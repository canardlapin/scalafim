package scalafim.fmri.fit

import scalafim.fmri.design.contrast.{ContrastRegistry, ContrastWeights}
import scalafim.fmri.model.FmriModel
import scalafim.linalg.{Cholesky, DoubleMatrix, DoubleVector}

final case class TContrast(name: String, weights: Map[String, Double]):
  require(name.nonEmpty, "contrast name must be non-empty")
  require(weights.nonEmpty, "contrast weights must be non-empty")
  require(weights.values.forall(_.isFinite), "contrast weights must be finite")

  def evaluate(result: DenseFmriFitResult): Either[FitError, TContrastResult] =
    result.inferenceReady.flatMap(evaluate)

  def evaluate(result: InferenceReadyDenseFit): Either[FitError, TContrastResult] =
    weightVector(result.columnNames).flatMap { w =>
      val scale = contrastScale(w, result.normalizedCovariance)
      if !(scale > 0.0 && scale.isFinite) then
        Left(FitError.NonEstimableContrast(name, s"contrast variance is $scale"))
      else
        val estimates = new Array[Double](result.voxels)
        val standardErrors = new Array[Double](result.voxels)
        val statistics = new Array[Double](result.voxels)

        var voxel = 0
        while voxel < result.voxels do
          var estimate = 0.0
          var predictor = 0
          while predictor < w.length do
            estimate += w(predictor) * result.coefficients(predictor, voxel)
            predictor += 1

          val se = math.sqrt(scale * result.residualVariance(voxel))
          estimates(voxel) = estimate
          standardErrors(voxel) = se
          statistics(voxel) = estimate / se
          voxel += 1

        Right(TContrastResult(
          name = name,
          estimates = DoubleVector.unsafe(estimates),
          standardErrors = DoubleVector.unsafe(standardErrors),
          statistics = DoubleVector.unsafe(statistics),
          residualDegreesOfFreedom = result.residualDegreesOfFreedom,
          voxelIndices = result.voxelIndices
        ))
    }

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

  private def contrastScale(weights: Array[Double], normalizedCovariance: scalafim.linalg.DoubleMatrix): Double =
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
    estimates: DoubleVector,
    standardErrors: DoubleVector,
    statistics: DoubleVector,
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
      covariance <- contrastCovariance(w, result.normalizedCovariance)
      factor <- Cholesky
        .decompose(covariance)
        .left
        .map(error => FitError.NonEstimableContrast(name, error.message))
    yield
      val estimates = contrastEstimates(w, result.coefficients.value)
      val solved = factor.solve(estimates)
      val statistics = new Array[Double](result.voxels)
      val q = w.cols

      var voxel = 0
      while voxel < result.voxels do
        var quadratic = 0.0
        var row = 0
        while row < q do
          quadratic += estimates(row, voxel) * solved(row, voxel)
          row += 1
        statistics(voxel) = quadratic / q.toDouble / result.residualVariance(voxel)
        voxel += 1

      FContrastResult(
        name = name,
        estimates = estimates,
        statistics = DoubleVector.unsafe(statistics),
        numeratorDegreesOfFreedom = q,
        residualDegreesOfFreedom = result.residualDegreesOfFreedom,
        voxelIndices = result.voxelIndices
      )

  private def weightMatrix(columnNames: Vector[String]): Either[FitError, DoubleMatrix] =
    val known = columnNames.toSet
    weights.iterator.flatMap(_.keysIterator).find(column => !known.contains(column)) match
      case Some(unknown) => Left(FitError.UnknownContrastColumn(unknown))
      case None =>
        val out = new Array[Double](columnNames.length * weights.length)
        var anyNonZero = false
        var contrast = 0
        while contrast < weights.length do
          var rowNonZero = false
          var predictor = 0
          while predictor < columnNames.length do
            val value = weights(contrast).getOrElse(columnNames(predictor), 0.0)
            out(predictor * weights.length + contrast) = value
            if value != 0.0 then
              anyNonZero = true
              rowNonZero = true
            predictor += 1
          if !rowNonZero then return Left(FitError.EmptyContrast(s"$name#${contrast + 1}"))
          contrast += 1

        if anyNonZero then Right(DoubleMatrix.unsafe(columnNames.length, weights.length, out))
        else Left(FitError.EmptyContrast(name))

  private def contrastCovariance(weights: DoubleMatrix, normalizedCovariance: DoubleMatrix): Either[FitError, DoubleMatrix] =
    if normalizedCovariance.rows != weights.rows || normalizedCovariance.cols != weights.rows then
      Left(FitError.NonEstimableContrast(name, "contrast weights and coefficient covariance have incompatible dimensions"))
    else
      val out = new Array[Double](weights.cols * weights.cols)
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
          out(a * weights.cols + b) = acc
          b += 1
        a += 1
      Right(DoubleMatrix.unsafe(weights.cols, weights.cols, out))

  private def contrastEstimates(weights: DoubleMatrix, coefficients: DoubleMatrix): DoubleMatrix =
    require(weights.rows == coefficients.rows, "contrast weights must match coefficient rows")
    val out = new Array[Double](weights.cols * coefficients.cols)
    var contrast = 0
    while contrast < weights.cols do
      var voxel = 0
      while voxel < coefficients.cols do
        var estimate = 0.0
        var predictor = 0
        while predictor < weights.rows do
          estimate += weights(predictor, contrast) * coefficients(predictor, voxel)
          predictor += 1
        out(contrast * coefficients.cols + voxel) = estimate
        voxel += 1
      contrast += 1
    DoubleMatrix.unsafe(weights.cols, coefficients.cols, out)

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
    estimates: DoubleMatrix,
    statistics: DoubleVector,
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
