package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

final case class RdmVector private (items: Int, values: Vector[Double]):
  require(items >= 0, "item count must be non-negative")
  require(values.length == items * (items - 1) / 2, "RDM vector length does not match item count")

  def distance(row: Int, col: Int): Either[MvpaError, Double] =
    if row < 0 || row >= items then
      Left(MvpaError.InvalidRdmInput(s"RDM row $row out of bounds for $items items"))
    else if col < 0 || col >= items then
      Left(MvpaError.InvalidRdmInput(s"RDM column $col out of bounds for $items items"))
    else Right(unsafeDistance(row, col))

  private[mvpa] inline def unsafeDistance(row: Int, col: Int): Double =
    if row == col then 0.0
    else
      val high = if row > col then row else col
      val low = if row > col then col else row
      val offset = low * (2 * items - low - 1) / 2
      values(offset + high - low - 1)

object RdmVector:
  def unsafe(items: Int, values: Vector[Double]): RdmVector =
    new RdmVector(items, values)

final case class PartitionMeans private (
    conditions: Int,
    features: Int,
    folds: Int,
    data: Array[Double]
):
  require(conditions > 0, "conditions must be positive")
  require(features > 0, "features must be positive")
  require(folds > 1, "crossnobis requires at least two folds")
  require(data.length == conditions * features * folds, "partition mean array has wrong length")

  inline def apply(condition: Int, feature: Int, fold: Int): Double =
    data((fold * conditions + condition) * features + feature)

object PartitionMeans:
  def apply(conditions: Int, features: Int, folds: Int, data: Array[Double]): Either[MvpaError, PartitionMeans] =
    if conditions <= 0 then Left(MvpaError.InvalidRdmInput("conditions must be positive"))
    else if features <= 0 then Left(MvpaError.InvalidRdmInput("features must be positive"))
    else if folds <= 1 then Left(MvpaError.InvalidRdmInput("crossnobis requires at least two folds"))
    else if data.length != conditions * features * folds then
      Left(MvpaError.InvalidRdmInput("partition mean array has wrong length"))
    else Right(new PartitionMeans(conditions, features, folds, data))

  def unsafe(conditions: Int, features: Int, folds: Int, data: Array[Double]): PartitionMeans =
    apply(conditions, features, folds, data).fold(error => throw new IllegalArgumentException(error.message), identity)

object Rdm:
  def pairIndices(items: Int): Vector[(Int, Int)] =
    require(items >= 0, "item count must be non-negative")
    val out = Vector.newBuilder[(Int, Int)]
    var col = 0
    while col < items do
      var row = col + 1
      while row < items do
        out += ((row, col))
        row += 1
      col += 1
    out.result()

  def squaredEuclidean(
      matrix: DoubleMatrix,
      normalizeByFeatures: Boolean = false
  ): Either[MvpaError, RdmVector] =
    validatePatternMatrix(matrix).map { _ =>
      val pairs = pairIndices(matrix.rows)
      val out = new Array[Double](pairs.length)
      var p = 0
      while p < pairs.length do
        val (rowA, rowB) = pairs(p)
        var sum = 0.0
        var col = 0
        while col < matrix.cols do
          val diff = matrix.dataArray(rowA * matrix.cols + col) -
            matrix.dataArray(rowB * matrix.cols + col)
          sum += diff * diff
          col += 1
        out(p) = if normalizeByFeatures then sum / matrix.cols else sum
        p += 1
      RdmVector.unsafe(matrix.rows, out.toVector)
    }

  def euclidean(matrix: DoubleMatrix): Either[MvpaError, RdmVector] =
    squaredEuclidean(matrix).map(rdm => RdmVector.unsafe(rdm.items, rdm.values.map(v => math.sqrt(math.max(v, 0.0)))))

  def correlation(matrix: DoubleMatrix): Either[MvpaError, RdmVector] =
    validatePatternMatrix(matrix).flatMap { _ =>
      val means = new Array[Double](matrix.rows)
      val norms = new Array[Double](matrix.rows)
      var zeroVariance = false
      var row = 0
      while row < matrix.rows do
        var sum = 0.0
        var col = 0
        while col < matrix.cols do
          sum += matrix.dataArray(row * matrix.cols + col)
          col += 1
        val mean = sum / matrix.cols
        means(row) = mean
        var ss = 0.0
        col = 0
        while col < matrix.cols do
          val centered = matrix.dataArray(row * matrix.cols + col) - mean
          ss += centered * centered
          col += 1
        norms(row) = math.sqrt(ss)
        if norms(row) == 0.0 then
          zeroVariance = true
        row += 1

      if zeroVariance then
        Left(MvpaError.InvalidRdmInput("correlation distance is undefined for zero-variance rows"))
      else
        val pairs = pairIndices(matrix.rows)
        val out = new Array[Double](pairs.length)
        var p = 0
        while p < pairs.length do
          val (rowA, rowB) = pairs(p)
          var dot = 0.0
          var col = 0
          while col < matrix.cols do
            dot += (matrix.dataArray(rowA * matrix.cols + col) - means(rowA)) *
              (matrix.dataArray(rowB * matrix.cols + col) - means(rowB))
            col += 1
          val corr = dot / (norms(rowA) * norms(rowB))
          out(p) = 1.0 - corr
          p += 1
        Right(RdmVector.unsafe(matrix.rows, out.toVector))
    }

  def crossnobisDistances(means: PartitionMeans, normalizeByFeatures: Boolean = true): RdmVector =
    val gram = crossvalidatedGram(means)
    val pairs = pairIndices(means.conditions)
    val out = new Array[Double](pairs.length)
    var p = 0
    while p < pairs.length do
      val (rowA, rowB) = pairs(p)
      val distance = gram(rowA * means.conditions + rowA) +
        gram(rowB * means.conditions + rowB) -
        2.0 * gram(rowA * means.conditions + rowB)
      out(p) = if normalizeByFeatures then distance / means.features else distance
      p += 1
    RdmVector.unsafe(means.conditions, out.toVector)

  def crossvalidatedGram(means: PartitionMeans): Array[Double] =
    val conditions = means.conditions
    val features = means.features
    val folds = means.folds
    val sumPatterns = new Array[Double](conditions * features)

    var fold = 0
    while fold < folds do
      var condition = 0
      while condition < conditions do
        var feature = 0
        while feature < features do
          sumPatterns(condition * features + feature) += means(condition, feature, fold)
          feature += 1
        condition += 1
      fold += 1

    val gram = new Array[Double](conditions * conditions)
    var row = 0
    while row < conditions do
      var col = 0
      while col < conditions do
        var sumGram = 0.0
        var feature = 0
        while feature < features do
          sumGram += sumPatterns(row * features + feature) * sumPatterns(col * features + feature)
          feature += 1

        var within = 0.0
        fold = 0
        while fold < folds do
          feature = 0
          while feature < features do
            within += means(row, feature, fold) * means(col, feature, fold)
            feature += 1
          fold += 1

        gram(row * conditions + col) = (sumGram - within) / (folds * (folds - 1))
        col += 1
      row += 1
    gram

  private def validatePatternMatrix(matrix: DoubleMatrix): Either[MvpaError, Unit] =
    if matrix.rows < 2 then Left(MvpaError.InvalidRdmInput("RDM requires at least two rows"))
    else if matrix.cols < 1 then Left(MvpaError.InvalidRdmInput("RDM requires at least one feature"))
    else
      var i = 0
      while i < matrix.dataArray.length do
        if !matrix.dataArray(i).isFinite then
          return Left(MvpaError.InvalidRdmInput("RDM pattern matrix contains non-finite values"))
        i += 1
      Right(())
