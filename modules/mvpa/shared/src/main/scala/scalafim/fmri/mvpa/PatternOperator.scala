package scalafim.fmri.mvpa

import gale.linalg.{DMat, DoubleLinearOperator, Matrix, LinearOperator}

/** Maximum returned dense-copy cells; this is not a process-memory limit. */
final case class PatternCopyBudget(maximumCells: Long):
  require(maximumCells >= 0L, "copy ceiling must be nonnegative")

enum PatternOperatorOrigin:
  case Dense
  case Composed
  case Mixed

sealed trait PatternOperatorProvenance:
  def origin: PatternOperatorOrigin
  def rowSelections: Int
  def featureSelections: Int
  def stackedParts: Int

  private[mvpa] final def afterRowSelection: PatternOperatorProvenance =
    PatternOperatorProvenance.rowSelected(this)

  private[mvpa] final def afterFeatureSelection: PatternOperatorProvenance =
    PatternOperatorProvenance.featureSelected(this)

object PatternOperatorProvenance:
  private case object Dense extends PatternOperatorProvenance:
    override val origin: PatternOperatorOrigin = PatternOperatorOrigin.Dense
    override val rowSelections: Int = 0
    override val featureSelections: Int = 0
    override val stackedParts: Int = 1

  private case object Composed extends PatternOperatorProvenance:
    override val origin: PatternOperatorOrigin = PatternOperatorOrigin.Composed
    override val rowSelections: Int = 0
    override val featureSelections: Int = 0
    override val stackedParts: Int = 1

  private final case class RowSelected(
      parent: PatternOperatorProvenance
  ) extends PatternOperatorProvenance:
    override def origin: PatternOperatorOrigin = parent.origin
    override def rowSelections: Int = parent.rowSelections + 1
    override def featureSelections: Int = parent.featureSelections
    override def stackedParts: Int = parent.stackedParts

  private final case class FeatureSelected(
      parent: PatternOperatorProvenance
  ) extends PatternOperatorProvenance:
    override def origin: PatternOperatorOrigin = parent.origin
    override def rowSelections: Int = parent.rowSelections
    override def featureSelections: Int = parent.featureSelections + 1
    override def stackedParts: Int = parent.stackedParts

  private final case class Stacked(
      parts: Vector[PatternOperatorProvenance]
  ) extends PatternOperatorProvenance:
    override val origin: PatternOperatorOrigin =
      val origins = parts.map(_.origin).distinct
      if origins.length == 1 then origins.head
      else PatternOperatorOrigin.Mixed
    override val rowSelections: Int = parts.map(_.rowSelections).sum
    override val featureSelections: Int = parts.map(_.featureSelections).sum
    override val stackedParts: Int = parts.map(_.stackedParts).sum

  val dense: PatternOperatorProvenance =
    Dense

  val composed: PatternOperatorProvenance =
    Composed

  private def rowSelected(parent: PatternOperatorProvenance): PatternOperatorProvenance =
    RowSelected(parent)

  private def featureSelected(parent: PatternOperatorProvenance): PatternOperatorProvenance =
    FeatureSelected(parent)

  private[mvpa] def stacked(parts: IndexedSeq[PatternOperatorProvenance]): PatternOperatorProvenance =
    require(parts.nonEmpty, "pattern provenance stack must be non-empty")
    Stacked(parts.toVector)

/** A sample-by-feature table represented as a linear map from feature weights
  * to sample scores.
  *
  * The public shape is therefore `samples x features`. Both the forward and
  * transpose actions are required so estimators can operate without demanding
  * a materialized sample-by-feature matrix.
  */
final class PatternOperator private (
    val samples: Int,
    val sampleIndices: Vector[SampleIndex],
    val featureIndices: Vector[FeatureIndex],
    val linear: DoubleLinearOperator,
    val provenance: PatternOperatorProvenance
):
  require(samples > 0, "operator samples must be positive")
  require(linear.rows == samples, "operator rows must match sample axis")
  require(sampleIndices.length == samples, "sample indices must match sample axis")
  require(sampleIndices.forall(_.value >= 0), "sample indices must be non-negative")
  require(sampleIndices.distinct.length == sampleIndices.length, "sample indices must be unique")
  require(linear.cols == featureIndices.length, "operator columns must match feature axis")
  require(featureIndices.nonEmpty, "pattern operator must contain at least one feature")
  require(featureIndices.forall(_.value >= 0), "feature indices must be non-negative")
  require(featureIndices.distinct.length == featureIndices.length, "feature indices must be unique")

  def features: Int = featureIndices.length
  def applyTo(featureWeights: DMat): Either[MvpaError, DMat] =
    if featureWeights.rows != features then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"feature-weight rows ${featureWeights.rows} do not match operator features $features"
        )
      )
    else if featureWeights.cols == 0 then
      Left(MvpaError.MatrixShapeMismatch("feature weights must contain at least one column"))
    else
      for
        _ <- validateFiniteOperator(featureWeights, "feature weights")
        output <- linear.applyTo(featureWeights).left.map(MvpaError.PatternOperatorFailed.apply)
        _ <- validateFiniteOperator(output, "pattern-score output")
      yield output

  def transposeApplyTo(sampleScores: DMat): Either[MvpaError, DMat] =
    if sampleScores.rows != samples then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"sample-score rows ${sampleScores.rows} do not match operator samples $samples"
        )
      )
    else if sampleScores.cols == 0 then
      Left(MvpaError.MatrixShapeMismatch("sample scores must contain at least one column"))
    else
      for
        _ <- validateFiniteOperator(sampleScores, "sample scores")
        output <- linear.transposeApplyTo(sampleScores).left.map(MvpaError.PatternOperatorFailed.apply)
        _ <- validateFiniteOperator(output, "feature-score output")
      yield output

  /** Explicit ceiling for the returned dense copy, excluding source/provider
    * storage and solver-private workspace. Copying never constructs a p-by-p identity.
    */
  def materialize(budget: PatternCopyBudget): Either[MvpaError, PatternMatrix] =
    val cells = BigInt(samples) * features
    if cells > budget.maximumCells || cells > Int.MaxValue then
      Left(MvpaError.PatternCopyBudgetExceeded(cells, budget.maximumCells))
    else
      val output = Matrix.newBuilder(samples, features)
      val basis = new Array[Double](features)
      var column = 0
      var failure: Option[MvpaError] = None
      while column < features && failure.isEmpty do
        basis(column) = 1.0
        applyTo(DMat.dense(features, 1, basis.toVector)) match
          case Left(error) => failure = Some(error)
          case Right(values) =>
            var row = 0
            while row < samples do
              output(row, column) = values(row, 0)
              row += 1
        basis(column) = 0.0
        column += 1
      failure.toLeft(()).map(_ => PatternMatrix(output.result(), sampleIndices, featureIndices))

  /** Select local row positions while preserving stored row indices. */
  def selectRowPositions(rows: IndexedSeq[Int]): Either[MvpaError, PatternOperator] =
    val positions = rows
    linear
      .restrictRows(positions)
      .left
      .map(MvpaError.PatternOperatorFailed.apply)
      .flatMap: restricted =>
        val selectedSamples = positions.map(sampleIndices).toVector
        PatternOperator.fromIndexedOperator(
          sampleIndices = selectedSamples,
          featureIndices = featureIndices,
          linear = restricted,
          provenance = provenance.afterRowSelection
        )

  def selectColumns(columns: IndexedSeq[FeatureIndex]): Either[MvpaError, PatternOperator] =
    if columns.isEmpty || columns.distinct.size != columns.size then
      return Left(MvpaError.MatrixShapeMismatch("selected feature indices must be nonempty and unique"))
    val lookup = featureIndices.zipWithIndex.map { case (feature, position) => feature.value -> position }.toMap
    val positions = new Array[Int](columns.length)
    var index = 0
    while index < columns.length do
      val feature = columns(index)
      lookup.get(feature.value) match
        case Some(position) => positions(index) = position
        case None           => return Left(MvpaError.MissingFeature(feature))
      index += 1

    linear
      .restrictColumns(positions.toIndexedSeq)
      .left
      .map(MvpaError.PatternOperatorFailed.apply)
      .flatMap: restricted =>
        PatternOperator.fromIndexedOperator(
          sampleIndices = sampleIndices,
          featureIndices = columns.toVector,
          linear = restricted,
          provenance = provenance.afterFeatureSelection
        )

object PatternOperator:
  def fromMatrix(patterns: PatternMatrix): Either[MvpaError, PatternOperator] =
    for
      _ <- validateFiniteOperator(patterns.value, "pattern matrix")
      operator <- fromIndexedOperator(
        sampleIndices = patterns.sampleIndices,
        featureIndices = patterns.featureIndices,
        linear = patterns.value,
        provenance = PatternOperatorProvenance.dense
      )
    yield operator

  def fromOperator(
      samples: Int,
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    if samples <= 0 then Left(MvpaError.MatrixShapeMismatch("pattern operator samples must be positive"))
    else if linear.rows != samples then Left(MvpaError.MatrixShapeMismatch(s"operator rows ${linear.rows} do not match sample axis $samples"))
    else checked(samples, Vector.tabulate(samples)(SampleIndex.unsafe), featureIndices, linear, provenance)

  def fromIndexedOperator(
      sampleIndices: Vector[SampleIndex],
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    checked(sampleIndices.length, sampleIndices, featureIndices, linear, provenance)

  private def checked(
      samples: Int,
      sampleIndices: Vector[SampleIndex],
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    if samples <= 0 then Left(MvpaError.MatrixShapeMismatch("pattern operator samples must be positive"))
    else if linear.rows != samples then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"operator rows ${linear.rows} do not match sample axis ${samples}"
        )
      )
    else if sampleIndices.length != samples then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"sample-index length ${sampleIndices.length} does not match sample axis ${samples}"
        )
      )
    else if sampleIndices.exists(_.value < 0) then
      Left(MvpaError.MatrixShapeMismatch("pattern operator sample indices must be non-negative"))
    else if sampleIndices.distinct.length != sampleIndices.length then
      Left(MvpaError.MatrixShapeMismatch("pattern operator sample indices must be unique"))
    else if featureIndices.isEmpty then
      Left(MvpaError.MatrixShapeMismatch("pattern operator requires at least one feature"))
    else if linear.cols != featureIndices.length then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"operator columns ${linear.cols} do not match feature axis ${featureIndices.length}"
        )
      )
    else if featureIndices.exists(_.value < 0) then
      Left(MvpaError.MatrixShapeMismatch("pattern operator feature indices must be non-negative"))
    else if featureIndices.distinct.length != featureIndices.length then
      Left(MvpaError.MatrixShapeMismatch("pattern operator feature indices must be unique"))
    else Right(new PatternOperator(samples, sampleIndices, featureIndices, linear, provenance))

  def stackRows(operators: IndexedSeq[PatternOperator]): Either[MvpaError, PatternOperator] =
    if operators.isEmpty then
      Left(MvpaError.MatrixShapeMismatch("pattern operator stack must be non-empty"))
    else
      val featureAxis = operators.head.featureIndices
      val mismatch = operators.indexWhere(_.featureIndices != featureAxis)
      if mismatch >= 0 then
        Left(MvpaError.MatrixShapeMismatch(s"pattern operator stack part $mismatch has a different feature axis"))
      else
        val total = operators.map(op => BigInt(op.samples)).sum
        if total > Int.MaxValue then Left(MvpaError.MatrixShapeMismatch("stacked sample count exceeds primitive capacity"))
        else for
          stacked <- LinearOperator
            .block(operators.map(operator => IndexedSeq(operator.linear)))
            .left
            .map(MvpaError.PatternOperatorFailed.apply)
          out <- fromOperator(
            samples = total.toInt,
            featureIndices = featureAxis,
            linear = stacked,
            provenance = PatternOperatorProvenance.stacked(operators.map(_.provenance))
          )
        yield out

private def validateFiniteOperator(matrix: DMat, label: String): Either[MvpaError, Unit] =
  var row = 0
  while row < matrix.rows do
    var col = 0
    while col < matrix.cols do
      if !matrix(row, col).isFinite then
        return Left(MvpaError.InvalidPatternOperatorInput(s"$label contains non-finite values"))
      col += 1
    row += 1
  Right(())
