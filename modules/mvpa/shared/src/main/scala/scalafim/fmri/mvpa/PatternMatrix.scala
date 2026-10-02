package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix}

final case class PatternMatrix(
    value: DMat,
    sampleIndices: Vector[SampleIndex],
    featureIndices: Vector[FeatureIndex]
):
  require(value.rows == sampleIndices.length, "matrix rows must match sample indices")
  require(value.cols == featureIndices.length, "matrix columns must match feature indices")

  def samples: Int = value.rows
  def features: Int = value.cols

  /** Select local row positions while preserving stored row indices. */
  def selectRowPositions(rows: IndexedSeq[Int]): Either[MvpaError, PatternMatrix] =
    if BigInt(rows.length) * value.cols > Int.MaxValue then
      return Left(MvpaError.MatrixShapeMismatch("selected matrix exceeds primitive capacity"))
    val localRows = rows
    localRows.find(i => i < 0 || i >= value.rows) match
      case Some(bad) =>
        Left(MvpaError.MatrixShapeMismatch(s"sample index $bad out of bounds for ${value.rows} rows"))
      case None =>
        val selected = Matrix.newBuilder(localRows.length, value.cols)
        var row = 0
        while row < localRows.length do
          var col = 0
          while col < value.cols do
            selected(row, col) = value(localRows(row), col)
            col += 1
          row += 1
        Right(
          PatternMatrix(
            value = selected.result(),
            sampleIndices = localRows.map(sampleIndices).toVector,
            featureIndices = featureIndices
          )
        )

  def selectColumns(columns: IndexedSeq[FeatureIndex]): Either[MvpaError, PatternMatrix] =
    if columns.isEmpty || columns.distinct.size != columns.size then
      return Left(MvpaError.MatrixShapeMismatch("selected feature indices must be nonempty and unique"))
    if BigInt(value.rows) * columns.size > Int.MaxValue then
      return Left(MvpaError.MatrixShapeMismatch("selected matrix exceeds primitive capacity"))
    val lookup = featureIndices.zipWithIndex.map { case (feature, position) => feature.value -> position }.toMap
    val positions = new Array[Int](columns.length)
    var i = 0
    while i < columns.length do
      val feature = columns(i)
      lookup.get(feature.value) match
        case Some(position) =>
          positions(i) = position
        case None =>
          return Left(MvpaError.MissingFeature(feature))
      i += 1

    val out = Matrix.newBuilder(value.rows, positions.length)
    var row = 0
    while row < value.rows do
      var col = 0
      while col < positions.length do
        out(row, col) = value(row, positions(col))
        col += 1
      row += 1

    Right(
      PatternMatrix(
        value = out.result(),
        sampleIndices = sampleIndices,
        featureIndices = columns.toVector
      )
    )

object PatternMatrix:
  def fromRows(rows: Seq[Seq[Double]]): PatternMatrix =
    require(rows.nonEmpty, "matrix rows must be non-empty")
    val rowVector = rows.map(_.toIndexedSeq).toIndexedSeq
    val cols = rowVector.head.length
    require(rowVector.forall(_.length == cols), "matrix rows must have equal length")
    val builder = Matrix.newBuilder(rowVector.length, cols)
    var row = 0
    while row < rowVector.length do
      var col = 0
      while col < cols do
        builder(row, col) = rowVector(row)(col)
        col += 1
      row += 1
    val matrix = builder.result()
    PatternMatrix(
      value = matrix,
      sampleIndices = (0 until matrix.rows).map(SampleIndex.unsafe).toVector,
      featureIndices = (0 until matrix.cols).map(FeatureIndex.unsafe).toVector
    )
