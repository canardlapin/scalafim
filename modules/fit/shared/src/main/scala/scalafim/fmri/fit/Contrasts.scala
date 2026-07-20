package scalafim.fmri.fit

import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastWeights}
import scalafim.fmri.model.FmriModel
import gale.linalg.{Cholesky, CholeskyOptions, DMat, DVec, Matrix, Vec}

final case class TContrast(name: String, weights: Map[String, Double]):
  require(name.nonEmpty, "contrast name must be non-empty")
  require(weights.nonEmpty, "contrast weights must be non-empty")
  require(weights.values.forall(_.isFinite), "contrast weights must be finite")

  def evaluate(result: DenseFmriFitResult): Either[FitError, TContrastResult] =
    result.inferenceReady.flatMap(evaluate)

  def evaluate(result: InferenceReadyDenseFit): Either[FitError, TContrastResult] =
    for
      weights <- weightVector(result.columnNames)
      _ <- result.inferenceScope.validateTContrast(name, weights, result.columnNames)
      evaluated <- evaluateEstimable(result, weights)
    yield evaluated

  private def evaluateEstimable(
      result: InferenceReadyDenseFit,
      weights: Array[Double]
  ): Either[FitError, TContrastResult] =
    val estimates = Vec.newBuilder(result.voxels)
    val standardErrors = Vec.newBuilder(result.voxels)
    val statistics = Vec.newBuilder(result.voxels)
    var failure: FitError | Null = null

    var voxel = 0
    while voxel < result.voxels && failure == null do
      var estimate = 0.0
      var predictor = 0
      while predictor < weights.length do
        estimate += weights(predictor) * result.coefficients(predictor, voxel)
        predictor += 1

      result.coefficientCovariance.matrixForVoxelPosition(voxel) match
        case Left(error) =>
          failure = error
        case Right(normalizedCovariance) =>
          val scale = contrastScale(weights, normalizedCovariance)
          if !(scale > 0.0 && scale.isFinite) then
            failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has contrast covariance scale $scale")
          else
            val variance = scale * result.varianceScale(voxel)
            if !(variance > 0.0 && variance.isFinite) then
              failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has contrast variance $variance")
            else
              val se = math.sqrt(variance)
              estimates(voxel) = estimate
              standardErrors(voxel) = se
              statistics(voxel) = estimate / se
      voxel += 1

    failure match
      case null =>
        Right(TContrastResult(
          name = name,
          estimates = estimates.result(),
          standardErrors = standardErrors.result(),
          statistics = statistics.result(),
          residualDegreesOfFreedom = result.residualDegreesOfFreedom,
          voxelIndices = result.voxelIndices
        ))
      case error => Left(error)

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

object TContrast:
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

final case class TContrastResult(
    name: String,
    estimates: DVec,
    standardErrors: DVec,
    statistics: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int]
):
  require(estimates.length == voxelIndices.length, "contrast estimates must match voxel indices")
  require(standardErrors.length == voxelIndices.length, "contrast standard errors must match voxel indices")
  require(statistics.length == voxelIndices.length, "contrast statistics must match voxel indices")
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)

final case class FContrast(name: String, weights: Vector[Map[String, Double]]):
  require(name.nonEmpty, "contrast name must be non-empty")
  require(weights.nonEmpty, "F contrast must contain at least one row")
  require(weights.forall(_.values.forall(_.isFinite)), "contrast weights must be finite")

  def evaluate(result: DenseFmriFitResult): Either[FitError, FContrastResult] =
    result.inferenceReady.flatMap(evaluate)

  def evaluate(result: InferenceReadyDenseFit): Either[FitError, FContrastResult] =
    for
      w <- weightMatrix(result.columnNames)
      _ <- result.inferenceScope.validateFContrast(name, w, result.columnNames)
      evaluated <- result.coefficientCovariance.scope match
        case CoefficientCovarianceScope.Shared =>
          for
            covariance <- contrastCovariance(w, result.coefficientCovariance.canonicalMatrix)
            factor <- covariance
              .cholesky(CholeskyOptions(choleskyTolerance(covariance)))
              .left
              .map(error => FitError.NonEstimableContrast(name, error.getMessage))
            evaluated <- evaluateEstimable(result, w, factor)
          yield evaluated
        case CoefficientCovarianceScope.Voxelwise =>
          evaluateEstimableVoxelwise(result, w)
    yield evaluated

  private def evaluateEstimable(
      result: InferenceReadyDenseFit,
      weights: DMat,
      factor: Cholesky
  ): Either[FitError, FContrastResult] =
    val estimates = contrastEstimates(weights, result.coefficients.value)
    factor
      .solve(estimates)
      .left
      .map(error => FitError.NonEstimableContrast(name, error.getMessage))
      .flatMap { solved =>
        val statistics = Vec.newBuilder(result.voxels)
        val q = weights.cols
        var failure: FitError | Null = null

        var voxel = 0
        while voxel < result.voxels && failure == null do
          val residualVariance = result.varianceScale(voxel)
          if !(residualVariance > 0.0 && residualVariance.isFinite) then
            failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has residual variance $residualVariance")
          else
            var quadratic = 0.0
            var row = 0
            while row < q do
              quadratic += estimates(row, voxel) * solved(row, voxel)
              row += 1
            val statistic = quadratic / q.toDouble / residualVariance
            if !statistic.isFinite then
              failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} produced non-finite F statistic")
            else statistics(voxel) = statistic
          voxel += 1

        failure match
          case null =>
            Right(
              FContrastResult(
                name = name,
                estimates = estimates,
                statistics = statistics.result(),
                numeratorDegreesOfFreedom = q,
                residualDegreesOfFreedom = result.residualDegreesOfFreedom,
                voxelIndices = result.voxelIndices
              )
            )
          case error =>
            Left(error)
      }

  private def evaluateEstimableVoxelwise(
      result: InferenceReadyDenseFit,
      weights: DMat
  ): Either[FitError, FContrastResult] =
    val estimates = contrastEstimates(weights, result.coefficients.value)
    val statistics = Vec.newBuilder(result.voxels)
    val q = weights.cols
    var failure: FitError | Null = null

    var voxel = 0
    while voxel < result.voxels && failure == null do
      val residualVariance = result.varianceScale(voxel)
      if !(residualVariance > 0.0 && residualVariance.isFinite) then
        failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} has residual variance $residualVariance")
      else
        result.coefficientCovariance.matrixForVoxelPosition(voxel) match
          case Left(error) =>
            failure = error
          case Right(normalizedCovariance) =>
            contrastCovariance(weights, normalizedCovariance) match
              case Left(error) =>
                failure = error
              case Right(covariance) =>
                val factorEither = covariance
                  .cholesky(CholeskyOptions(choleskyTolerance(covariance)))
                  .left
                  .map(error => FitError.NonEstimableContrast(name, error.getMessage))
                factorEither match
                  case Left(error) =>
                    failure = error
                  case Right(factor) =>
                    factor.solve(estimateColumn(estimates, voxel)) match
                      case Left(error) =>
                        failure = FitError.NonEstimableContrast(name, error.getMessage)
                      case Right(solved) =>
                        var quadratic = 0.0
                        var row = 0
                        while row < q do
                          quadratic += estimates(row, voxel) * solved(row, 0)
                          row += 1
                        val statistic = quadratic / q.toDouble / residualVariance
                        if !statistic.isFinite then
                          failure = FitError.NonEstimableContrast(name, s"voxel ${result.voxelIndices(voxel)} produced non-finite F statistic")
                        else statistics(voxel) = statistic
      voxel += 1

    failure match
      case null =>
        Right(
          FContrastResult(
            name = name,
            estimates = estimates,
            statistics = statistics.result(),
            numeratorDegreesOfFreedom = q,
            residualDegreesOfFreedom = result.residualDegreesOfFreedom,
            voxelIndices = result.voxelIndices
          )
        )
      case error =>
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

  private def contrastEstimates(weights: DMat, coefficients: DMat): DMat =
    require(weights.rows == coefficients.rows, "contrast weights must match coefficient rows")
    val out = Matrix.newBuilder(weights.cols, coefficients.cols)
    var contrast = 0
    while contrast < weights.cols do
      var voxel = 0
      while voxel < coefficients.cols do
        var estimate = 0.0
        var predictor = 0
        while predictor < weights.rows do
          estimate += weights(predictor, contrast) * coefficients(predictor, voxel)
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

final case class FContrastResult(
    name: String,
    estimates: DMat,
    statistics: DVec,
    numeratorDegreesOfFreedom: Int,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    voxelIndices: Vector[Int]
):
  require(estimates.rows == numeratorDegreesOfFreedom, "F contrast estimates must match numerator df")
  require(estimates.cols == voxelIndices.length, "F contrast estimates must match voxel indices")
  require(statistics.length == voxelIndices.length, "F statistics must match voxel indices")
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)

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
