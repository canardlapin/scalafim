package scalafim.fmri.fit

import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastWeights}
import scalafim.fmri.design.CoefficientAxis
import scalafim.fmri.model.FmriModel
import gale.linalg.{Cholesky, CholeskyOptions, DMat, DVec, Matrix, Vec}

private final case class ContrastVoxelSelection(
    positions: Vector[Int],
    voxelIndices: Vector[Int],
    exclusions: Vector[VoxelInferenceExclusion]
)

private object ContrastVoxelSelection:
  def from(
      contrastName: String,
      result: InferenceReadyDenseFit
  ): Either[FitError, ContrastVoxelSelection] =
    val positions = Vector.newBuilder[Int]
    val indices = Vector.newBuilder[Int]
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    exclusions ++= result.fitExclusions
    var voxel = 0
    while voxel < result.voxels do
      val status = result.voxelStatuses(voxel)
      if status.supportsInference then
        positions += voxel
        indices += result.voxelIndices(voxel)
      else
        exclusions += VoxelInferenceExclusion(result.voxelIndices(voxel), status)
      voxel += 1
    val kept = positions.result()
    val omitted = exclusions.result()
    if kept.isEmpty then
      val detail = omitted.map(exclusion => s"${exclusion.voxelIndex}:${exclusion.status.label}").mkString(", ")
      Left(FitError.NonEstimableContrast(contrastName, s"all voxels are excluded from inference ($detail)"))
    else Right(ContrastVoxelSelection(kept, indices.result(), omitted))

/** Compatibility-only rendered-name contrast.  New code should compile a
  * [[StructuralTContrast]] against a [[scalafim.fmri.design.DesignSchema]] so
  * structural column identity and estimability evidence are retained. */
final case class TContrast(name: String, weights: Map[String, Double]):
  require(name.nonEmpty, "contrast name must be non-empty")
  require(weights.nonEmpty, "contrast weights must be non-empty")
  require(weights.values.forall(_.isFinite), "contrast weights must be finite")

  def evaluate(result: DenseFmriFitResult): Either[FitError, TContrastResult] =
    result.inferenceReady.flatMap(evaluate)

  def evaluate(result: PatternedFmriFitResult): Either[FitError, PatternedTContrastResult] =
    result.densePatterns.flatMap { patterns =>
      traversePatterns(patterns) { (pattern, dense) =>
        evaluate(dense).map(pattern -> _)
      }.map(PatternedTContrastResult(name, _, result.voxelIndices, result.fitExclusions))
    }

  def evaluate(result: InferenceReadyDenseFit): Either[FitError, TContrastResult] =
    for
      weights <- weightVector(result.columnNames)
      _ <- result.inferenceScope.validateTContrast(name, weights, result.columnNames)
      selection <- ContrastVoxelSelection.from(name, result)
      evaluated <- evaluateEstimable(result, weights, selection, None)
    yield evaluated

  /** Align this named contrast to an explicit design axis. The resulting value
    * keeps the column identity beside its dense row, so downstream geometry
    * cannot silently reuse it against another ordering.
    */
  def align(columnNames: Vector[String]): Either[FitError, AlignedTContrast] =
    weightVector(columnNames).map: values =>
      val row = Matrix.newBuilder(1, values.length)
      var col = 0
      while col < values.length do
        row(0, col) = values(col)
        col += 1
      AlignedTContrast.unsafe(name, columnNames, row.result())

  private[fit] def evaluateAligned(
      result: InferenceReadyDenseFit,
      aligned: AlignedTContrast,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, TContrastResult] =
    val values = new Array[Double](aligned.weights.cols)
    var index = 0
    while index < aligned.weights.cols do
      values(index) = aligned.weights(0, index)
      index += 1
    for
      _ <- result.inferenceScope.validateTContrast(aligned.name, values, aligned.columnNames)
      selection <- ContrastVoxelSelection.from(name, result)
      evaluated <- evaluateEstimable(result, values, selection, metadata)
    yield evaluated

  private def evaluateEstimable(
      result: InferenceReadyDenseFit,
      weights: Array[Double],
      selection: ContrastVoxelSelection,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, TContrastResult] =
    val estimates = Vector.newBuilder[Double]
    val standardErrors = Vector.newBuilder[Double]
    val statistics = Vector.newBuilder[Double]
    var failure: Option[FitError] = None

    var retained = 0
    while retained < selection.positions.length && failure.isEmpty do
      val voxel = selection.positions(retained)
      var estimate = 0.0
      var predictor = 0
      while predictor < weights.length do
        estimate += weights(predictor) * result.coefficients(predictor, voxel)
        predictor += 1

      result.coefficientCovariance.matrixForVoxelPosition(voxel) match
        case Left(error) =>
          failure = Some(error)
        case Right(normalizedCovariance) =>
          val scale = contrastScale(weights, normalizedCovariance)
          if !(scale > 0.0 && scale.isFinite) then
            failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has contrast covariance scale $scale"))
          else
            val variance = scale * result.varianceScale(voxel)
            if !(variance > 0.0 && variance.isFinite) then
              failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has contrast variance $variance"))
            else
              val se = math.sqrt(variance)
              estimates += estimate
              standardErrors += se
              statistics += estimate / se
      retained += 1

    failure match
      case None =>
        Right(TContrastResult(
          name = name,
          estimates = DVec.fromSeq(estimates.result()),
          standardErrors = DVec.fromSeq(standardErrors.result()),
          statistics = DVec.fromSeq(statistics.result()),
          residualDegreesOfFreedom = result.residualDegreesOfFreedom,
          voxelIndices = selection.voxelIndices,
          hypothesis = metadata,
          excludedVoxels = selection.exclusions
        ))
      case Some(error) => Left(error)

  private def weightVector(columnNames: Vector[String]): Either[FitError, Array[Double]] =
    val known = columnNames.toSet
    weights.keys.find(name => !known.contains(name)) match
      case Some(unknown) => Left(FitError.UnknownContrastColumn(unknown))
      case None =>
        val out = new Array[Double](columnNames.length)
        var nonZero = false
        var i = 0
        while i < columnNames.length do
          val value = weights.getOrElse(columnNames(i), 0.0)
          out(i) = value
          if value != 0.0 then nonZero = true
          i += 1
        if nonZero then Right(out) else Left(FitError.EmptyContrast(name))

  private def contrastScale(weights: Array[Double], normalizedCovariance: DMat): Double =
    var out = 0.0
    var i = 0
    while i < weights.length do
      var j = 0
      while j < weights.length do
        out += weights(i) * normalizedCovariance(i, j) * weights(j)
        j += 1
      i += 1
    out

  private def traversePatterns[A](
      patterns: Vector[(ObservationPattern, DenseFmriFitResult)]
  )(
      evaluate: (ObservationPattern, DenseFmriFitResult) => Either[FitError, A]
  ): Either[FitError, Vector[A]] =
    val out = Vector.newBuilder[A]
    var index = 0
    while index < patterns.length do
      evaluate(patterns(index)._1, patterns(index)._2) match
        case Left(error)  => return Left(error)
        case Right(value) => out += value
      index += 1
    Right(out.result())

object TContrast:
  private[fit] def evaluateAligned(
      result: InferenceReadyDenseFit,
      aligned: AlignedTContrast,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, TContrastResult] =
    TContrast(aligned.name, Map(aligned.name -> 1.0)).evaluateAligned(result, aligned, metadata)

  def column(columnName: String): TContrast =
    TContrast(columnName, Map(columnName -> 1.0))

  def fromContrastWeights(name: String, weights: ContrastWeights): Vector[TContrast] =
    val out = Vector.newBuilder[TContrast]
    var col = 0
    while col < weights.weights.cols do
      val mapped = scala.collection.mutable.LinkedHashMap.empty[String, Double]
      var row = 0
      while row < weights.weights.rows do
        val value = weights.weights(row, col)
        if value != 0.0 then mapped.update(weights.condNames(row), value)
        row += 1

      val contrastName =
        if weights.weights.cols == 1 then name
        else weights.contrastNames.lift(col).map(c => s"$name#$c").getOrElse(s"$name#${col + 1}")

      out += TContrast(contrastName, mapped.toMap)
      col += 1
    out.result()

final case class AlignedTContrast private (
    name: String,
    columnNames: Vector[String],
    weights: DMat,
    coefficientAxis: Option[CoefficientAxis]
):
  require(name.nonEmpty, "aligned contrast name must be non-empty")
  require(columnNames.nonEmpty, "aligned contrast must have design columns")
  require(
    coefficientAxis.nonEmpty || columnNames.distinct.length == columnNames.length,
    "rendered-name aligned contrast columns must be unique"
  )
  require(weights.rows == 1, "aligned T contrast must contain exactly one row")
  require(weights.cols == columnNames.length, "aligned contrast weights must match design columns")
  require(coefficientAxis.forall(_.predictors == columnNames.length), "aligned contrast axis must match design columns")

object AlignedTContrast:
  private[fit] def unsafe(
      name: String,
      columnNames: Vector[String],
      weights: DMat,
      coefficientAxis: Option[CoefficientAxis] = None
  ): AlignedTContrast =
    new AlignedTContrast(name, columnNames, weights, coefficientAxis)

final case class TContrastResult(
    name: String,
    estimates: DVec,
    standardErrors: DVec,
    statistics: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int],
    hypothesis: Option[HypothesisMetadata] = None,
    excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(estimates.length == voxelIndices.length, "contrast estimates must match voxel indices")
  require(standardErrors.length == voxelIndices.length, "contrast standard errors must match voxel indices")
  require(statistics.length == voxelIndices.length, "contrast statistics must match voxel indices")
  require(excludedVoxels.map(_.voxelIndex).distinct.length == excludedVoxels.length, "excluded contrast voxels must be unique")
  require(excludedVoxels.forall(exclusion => !voxelIndices.contains(exclusion.voxelIndex)), "retained and excluded contrast voxels must be disjoint")
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)

final case class PatternedTContrastResult(
    name: String,
    patternResults: Vector[(ObservationPattern, TContrastResult)],
    voxelIndices: Vector[Int],
    excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(patternResults.nonEmpty, "patterned T contrast must contain at least one observation pattern")
  require(patternResults.forall(_._2.name == name), "patterned T contrast child names must match")
  require(voxelIndices.distinct.length == voxelIndices.length, "patterned T contrast voxels must be unique")
  require(
    patternResults.flatMap(_._2.voxelIndices).toSet == voxelIndices.toSet,
    "patterned T contrast children must cover exactly the retained voxels"
  )
  require(
    patternResults.flatMap(_._2.voxelIndices).distinct.length == voxelIndices.length,
    "patterned T contrast child voxel sets must be disjoint"
  )
  require(
    excludedVoxels.map(_.voxelIndex).distinct.length == excludedVoxels.length,
    "excluded patterned T contrast voxels must be unique"
  )
  require(
    excludedVoxels.forall(exclusion => !voxelIndices.contains(exclusion.voxelIndex)),
    "retained and excluded patterned T contrast voxels must be disjoint"
  )
  def residualDegreesOfFreedomByVoxel: Map[Int, ResidualDegreesOfFreedom] =
    patternResults.flatMap { case (_, result) =>
      result.voxelIndices.map(_ -> result.residualDegreesOfFreedom)
    }.toMap

/** Compatibility-only rendered-name F contrast.  New hypotheses should use
  * [[StructuralFContrast]] and compile it against a [[scalafim.fmri.design.DesignSchema]].
  */
final case class FContrast(name: String, weights: Vector[Map[String, Double]]):
  require(name.nonEmpty, "contrast name must be non-empty")
  require(weights.nonEmpty, "F contrast must contain at least one row")
  require(weights.forall(_.values.forall(_.isFinite)), "contrast weights must be finite")

  def align(columnNames: Vector[String]): Either[FitError, AlignedFContrast] =
    weightMatrix(columnNames).map(matrix => AlignedFContrast.unsafe(name, columnNames, matrix))

  def evaluate(result: DenseFmriFitResult): Either[FitError, FContrastResult] =
    result.inferenceReady.flatMap(evaluate)

  def evaluate(result: PatternedFmriFitResult): Either[FitError, PatternedFContrastResult] =
    result.densePatterns.flatMap { patterns =>
      patterns
        .foldLeft[Either[FitError, Vector[(ObservationPattern, FContrastResult)]]](Right(Vector.empty)) {
          case (accumulated, (pattern, dense)) =>
            for
              values <- accumulated
              value <- evaluate(dense)
            yield values :+ (pattern -> value)
        }
        .map(PatternedFContrastResult(name, _, result.voxelIndices, result.fitExclusions))
    }

  def evaluate(result: InferenceReadyDenseFit): Either[FitError, FContrastResult] =
    align(result.columnNames).flatMap(aligned => evaluateAligned(result, aligned, None))

  private[fit] def evaluateAligned(
      result: InferenceReadyDenseFit,
      aligned: AlignedFContrast,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, FContrastResult] =
    val w = aligned.weights
    for
      _ <- result.inferenceScope.validateFContrast(name, w, result.columnNames)
      selection <- ContrastVoxelSelection.from(name, result)
      evaluated <- result.coefficientCovariance.scope match
        case CoefficientCovarianceScope.Shared =>
          for
            covariance <- contrastCovariance(w, result.coefficientCovariance.canonicalMatrix)
            factor <- covariance
              .cholesky(CholeskyOptions(choleskyTolerance(covariance)))
              .left
              .map(error => FitError.NonEstimableContrast(name, error.getMessage))
            evaluated <- evaluateEstimable(result, w, factor, aligned.rank, selection, metadata)
          yield evaluated
        case CoefficientCovarianceScope.Voxelwise =>
          evaluateEstimableVoxelwise(result, w, aligned.rank, selection, metadata)
    yield evaluated

  private def evaluateEstimable(
      result: InferenceReadyDenseFit,
      weights: DMat,
      factor: Cholesky,
      numeratorRank: Int,
      selection: ContrastVoxelSelection,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, FContrastResult] =
    val estimates = contrastEstimates(weights, result.coefficients.value, selection.positions)
    factor
      .solve(estimates)
      .left
      .map(error => FitError.NonEstimableContrast(name, error.getMessage))
      .flatMap { solved =>
        val statistics = Vec.newBuilder(selection.positions.length)
        val q = weights.cols
        var failure: Option[FitError] = None

        var voxel = 0
        while voxel < selection.positions.length && failure.isEmpty do
          val sourceVoxel = selection.positions(voxel)
          val residualVariance = result.varianceScale(sourceVoxel)
          if !(residualVariance > 0.0 && residualVariance.isFinite) then
            failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(sourceVoxel)} has residual variance $residualVariance"))
          else
            var quadratic = 0.0
            var row = 0
            while row < q do
              quadratic += estimates(row, voxel) * solved(row, voxel)
              row += 1
            val statistic = quadratic / q.toDouble / residualVariance
            if !statistic.isFinite then
              failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(sourceVoxel)} produced non-finite F statistic"))
            else statistics(voxel) = statistic
          voxel += 1

        failure match
          case None =>
            Right(
              FContrastResult(
                name = name,
                estimates = estimates,
                statistics = statistics.result(),
                numeratorDegreesOfFreedom = numeratorRank,
                residualDegreesOfFreedom = result.residualDegreesOfFreedom,
                voxelIndices = selection.voxelIndices,
                hypothesis = metadata,
                excludedVoxels = selection.exclusions
              )
            )
          case Some(error) =>
            Left(error)
      }

  private def evaluateEstimableVoxelwise(
      result: InferenceReadyDenseFit,
      weights: DMat,
      numeratorRank: Int,
      selection: ContrastVoxelSelection,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, FContrastResult] =
    val estimates = contrastEstimates(weights, result.coefficients.value, selection.positions)
    val statistics = Vec.newBuilder(selection.positions.length)
    val q = weights.cols
    var failure: Option[FitError] = None

    var voxel = 0
    while voxel < selection.positions.length && failure.isEmpty do
      val sourceVoxel = selection.positions(voxel)
      val residualVariance = result.varianceScale(sourceVoxel)
      if !(residualVariance > 0.0 && residualVariance.isFinite) then
        failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(sourceVoxel)} has residual variance $residualVariance"))
      else
        result.coefficientCovariance.matrixForVoxelPosition(sourceVoxel) match
          case Left(error) =>
            failure = Some(error)
          case Right(normalizedCovariance) =>
            contrastCovariance(weights, normalizedCovariance) match
              case Left(error) =>
                failure = Some(error)
              case Right(covariance) =>
                val factorEither = covariance
                  .cholesky(CholeskyOptions(choleskyTolerance(covariance)))
                  .left
                  .map(error => FitError.NonEstimableContrast(name, error.getMessage))
                factorEither match
                  case Left(error) =>
                    failure = Some(error)
                  case Right(factor) =>
                    factor.solve(estimateColumn(estimates, voxel)) match
                      case Left(error) =>
                        failure = Some(FitError.NonEstimableContrast(name, error.getMessage))
                      case Right(solved) =>
                        var quadratic = 0.0
                        var row = 0
                        while row < q do
                          quadratic += estimates(row, voxel) * solved(row, 0)
                          row += 1
                        val statistic = quadratic / q.toDouble / residualVariance
                        if !statistic.isFinite then
                          failure = Some(FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(sourceVoxel)} produced non-finite F statistic"))
                        else statistics(voxel) = statistic
      voxel += 1

    failure match
      case None =>
        Right(
          FContrastResult(
            name = name,
            estimates = estimates,
            statistics = statistics.result(),
            numeratorDegreesOfFreedom = numeratorRank,
            residualDegreesOfFreedom = result.residualDegreesOfFreedom,
            voxelIndices = selection.voxelIndices,
            hypothesis = metadata,
            excludedVoxels = selection.exclusions
          )
        )
      case Some(error) =>
        Left(error)

  private def weightMatrix(columnNames: Vector[String]): Either[FitError, DMat] =
    val known = columnNames.toSet
    weights.iterator.flatMap(_.keysIterator).find(column => !known.contains(column)) match
      case Some(unknown) => Left(FitError.UnknownContrastColumn(unknown))
      case None =>
        val out = Matrix.newBuilder(columnNames.length, weights.length)
        var anyNonZero = false
        var contrast = 0
        while contrast < weights.length do
          var rowNonZero = false
          var predictor = 0
          while predictor < columnNames.length do
            val value = weights(contrast).getOrElse(columnNames(predictor), 0.0)
            out(predictor, contrast) = value
            if value != 0.0 then
              anyNonZero = true
              rowNonZero = true
            predictor += 1
          if !rowNonZero then return Left(FitError.EmptyContrast(s"$name#${contrast + 1}"))
          contrast += 1

        if anyNonZero then Right(out.result())
        else Left(FitError.EmptyContrast(name))

  private def contrastCovariance(weights: DMat, normalizedCovariance: DMat): Either[FitError, DMat] =
    if normalizedCovariance.rows != weights.rows || normalizedCovariance.cols != weights.rows then
      Left(FitError.NonEstimableContrast(name, "contrast weights and coefficient covariance have incompatible dimensions"))
    else
      val out = Matrix.newBuilder(weights.cols, weights.cols)
      var a = 0
      while a < weights.cols do
        var b = 0
        while b < weights.cols do
          var acc = 0.0
          var i = 0
          while i < weights.rows do
            val wi = weights(i, a)
            var j = 0
            while j < weights.rows do
              acc += wi * normalizedCovariance(i, j) * weights(j, b)
              j += 1
            i += 1
          out(a, b) = acc
          b += 1
        a += 1
      Right(out.result())

  private def contrastEstimates(weights: DMat, coefficients: DMat, voxelPositions: Vector[Int]): DMat =
    require(weights.rows == coefficients.rows, "contrast weights must match coefficient rows")
    require(voxelPositions.forall(voxel => voxel >= 0 && voxel < coefficients.cols), "contrast voxel position out of bounds")
    val out = Matrix.newBuilder(weights.cols, voxelPositions.length)
    var contrast = 0
    while contrast < weights.cols do
      var voxel = 0
      while voxel < voxelPositions.length do
        val sourceVoxel = voxelPositions(voxel)
        var estimate = 0.0
        var predictor = 0
        while predictor < weights.rows do
          estimate += weights(predictor, contrast) * coefficients(predictor, sourceVoxel)
          predictor += 1
        out(contrast, voxel) = estimate
        voxel += 1
      contrast += 1
    out.result()

  private def estimateColumn(estimates: DMat, voxel: Int): DMat =
    val out = Matrix.newBuilder(estimates.rows, 1)
    var row = 0
    while row < estimates.rows do
      out(row, 0) = estimates(row, voxel)
      row += 1
    out.result()

  private def choleskyTolerance(matrix: DMat): Double =
    var diagonalMax = 0.0
    var index = 0
    while index < matrix.rows do
      diagonalMax = math.max(diagonalMax, math.abs(matrix(index, index)))
      index += 1
    diagonalMax * 1e-12

object FContrast:
  private[fit] def evaluateAligned(
      result: InferenceReadyDenseFit,
      aligned: AlignedFContrast,
      metadata: Option[HypothesisMetadata]
  ): Either[FitError, FContrastResult] =
    FContrast(aligned.name, Vector(Map(aligned.name -> 1.0))).evaluateAligned(result, aligned, metadata)

  def fromContrastWeights(name: String, weights: ContrastWeights): FContrast =
    val rows = Vector.newBuilder[Map[String, Double]]
    var col = 0
    while col < weights.weights.cols do
      val mapped = scala.collection.mutable.LinkedHashMap.empty[String, Double]
      var row = 0
      while row < weights.weights.rows do
        val value = weights.weights(row, col)
        if value != 0.0 then mapped.update(weights.condNames(row), value)
        row += 1
      rows += mapped.toMap
      col += 1
    FContrast(name, rows.result())

final case class AlignedFContrast private (
    name: String,
    columnNames: Vector[String],
    weights: DMat,
    coefficientAxis: Option[CoefficientAxis],
    testRank: Int
):
  require(name.nonEmpty, "aligned F contrast name must be non-empty")
  require(columnNames.length == weights.rows, "aligned F contrast columns must match weight rows")
  require(weights.cols > 0, "aligned F contrast must contain at least one row")
  require(testRank > 0 && testRank <= weights.cols, "aligned F contrast test rank must be positive and no greater than supplied columns")

  def rank: Int = testRank

object AlignedFContrast:
  private[fit] def unsafe(name: String, columnNames: Vector[String], weights: DMat): AlignedFContrast =
    new AlignedFContrast(name, columnNames, weights, None, weights.cols)

  private[fit] def unsafe(
      name: String,
      columnNames: Vector[String],
      weights: DMat,
      coefficientAxis: Option[CoefficientAxis] = None,
      testRank: Int
  ): AlignedFContrast =
    new AlignedFContrast(name, columnNames, weights, coefficientAxis, testRank)

final case class FContrastResult(
    name: String,
    estimates: DMat,
    statistics: DVec,
    numeratorDegreesOfFreedom: Int,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int],
    hypothesis: Option[HypothesisMetadata] = None,
    excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(estimates.rows >= numeratorDegreesOfFreedom, "F contrast estimates must contain at least the effective numerator df")
  require(estimates.cols == voxelIndices.length, "F contrast estimates must match voxel indices")
  require(statistics.length == voxelIndices.length, "F statistics must match voxel indices")
  require(excludedVoxels.map(_.voxelIndex).distinct.length == excludedVoxels.length, "excluded F contrast voxels must be unique")
  require(excludedVoxels.forall(exclusion => !voxelIndices.contains(exclusion.voxelIndex)), "retained and excluded F contrast voxels must be disjoint")
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)

final case class PatternedFContrastResult(
    name: String,
    patternResults: Vector[(ObservationPattern, FContrastResult)],
    voxelIndices: Vector[Int],
    excludedVoxels: Vector[VoxelInferenceExclusion] = Vector.empty
):
  require(patternResults.nonEmpty, "patterned F contrast must contain at least one observation pattern")
  require(patternResults.forall(_._2.name == name), "patterned F contrast child names must match")
  require(voxelIndices.distinct.length == voxelIndices.length, "patterned F contrast voxels must be unique")
  require(
    patternResults.flatMap(_._2.voxelIndices).toSet == voxelIndices.toSet,
    "patterned F contrast children must cover exactly the retained voxels"
  )
  require(
    patternResults.flatMap(_._2.voxelIndices).distinct.length == voxelIndices.length,
    "patterned F contrast child voxel sets must be disjoint"
  )
  require(
    excludedVoxels.map(_.voxelIndex).distinct.length == excludedVoxels.length,
    "excluded patterned F contrast voxels must be unique"
  )
  require(
    excludedVoxels.forall(exclusion => !voxelIndices.contains(exclusion.voxelIndex)),
    "retained and excluded patterned F contrast voxels must be disjoint"
  )
  def residualDegreesOfFreedomByVoxel: Map[Int, ResidualDegreesOfFreedom] =
    patternResults.flatMap { case (_, result) =>
      result.voxelIndices.map(_ -> result.residualDegreesOfFreedom)
    }.toMap

object DesignContrasts:

  def attachedTContrasts(model: FmriModel): Vector[TContrast] =
    import ContrastRegistry.*

    model.eventModel.contrastWeights.iterator.flatMap { case (name, weights) =>
      TContrast.fromContrastWeights(name, weights.embedIn(model.columnNames))
    }.toVector

  def attachedFContrasts(model: FmriModel): Vector[FContrast] =
    import ContrastRegistry.*

    model.eventModel.contrastWeights.iterator.map { case (name, weights) =>
      FContrast.fromContrastWeights(name, weights.embedIn(model.columnNames))
    }.toVector

  def generatedFContrasts(model: FmriModel, maxInter: Int = 4): Vector[FContrast] =
    import ContrastRegistry.*

    model.eventModel.fContrastWeights(maxInter = maxInter).iterator.map { case (name, weights) =>
      FContrast.fromContrastWeights(name, weights.embedIn(model.columnNames))
    }.toVector
