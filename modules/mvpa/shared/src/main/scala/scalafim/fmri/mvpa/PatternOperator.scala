package scalafim.fmri.mvpa

import gale.linalg.{DMat, DoubleLinearOperator, LinearOperator}

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
    val sampleAxis: SampleAxis,
    val sampleIndices: Vector[SampleIndex],
    val featureIndices: Vector[FeatureIndex],
    val linear: DoubleLinearOperator,
    val provenance: PatternOperatorProvenance
):
  require(linear.rows == sampleAxis.samples, "operator rows must match sample axis")
  require(sampleIndices.length == sampleAxis.samples, "sample indices must match sample axis")
  require(sampleIndices.forall(_.value >= 0), "sample indices must be non-negative")
  require(sampleIndices.distinct.length == sampleIndices.length, "sample indices must be unique")
  require(linear.cols == featureIndices.length, "operator columns must match feature axis")
  require(featureIndices.nonEmpty, "pattern operator must contain at least one feature")
  require(featureIndices.forall(_.value >= 0), "feature indices must be non-negative")
  require(featureIndices.distinct.length == featureIndices.length, "feature indices must be unique")

  def samples: Int = sampleAxis.samples
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

  def materialize: Either[MvpaError, PatternMatrix] =
    applyTo(DMat.eye(features)).map: values =>
      PatternMatrix(
        value = values,
        sampleIndices = sampleIndices,
        featureIndices = featureIndices
      )

  def selectRows(rows: IndexedSeq[SampleIndex]): Either[MvpaError, PatternOperator] =
    val positions = rows.map(_.value)
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

  def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternOperator] =
    val lookup = featureIndices.zipWithIndex.map { case (feature, position) => feature.value -> position }.toMap
    val positions = new Array[Int](featureSet.featureIndices.length)
    var index = 0
    while index < featureSet.featureIndices.length do
      val feature = featureSet.featureIndices(index)
      lookup.get(feature.value) match
        case Some(position) => positions(index) = position
        case None           => return Left(MvpaError.MissingFeature(featureSet.id, feature))
      index += 1

    linear
      .restrictColumns(positions.toIndexedSeq)
      .left
      .map(MvpaError.PatternOperatorFailed.apply)
      .flatMap: restricted =>
        PatternOperator.fromIndexedOperator(
          sampleIndices = sampleIndices,
          featureIndices = featureSet.featureIndices,
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
      sampleAxis: SampleAxis,
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    checked(sampleAxis, sampleAxis.indices, featureIndices, linear, provenance)

  def fromIndexedOperator(
      sampleIndices: Vector[SampleIndex],
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    SampleAxis(sampleIndices.length).flatMap: sampleAxis =>
      checked(sampleAxis, sampleIndices, featureIndices, linear, provenance)

  private def checked(
      sampleAxis: SampleAxis,
      sampleIndices: Vector[SampleIndex],
      featureIndices: Vector[FeatureIndex],
      linear: DoubleLinearOperator,
      provenance: PatternOperatorProvenance
  ): Either[MvpaError, PatternOperator] =
    if linear.rows != sampleAxis.samples then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"operator rows ${linear.rows} do not match sample axis ${sampleAxis.samples}"
        )
      )
    else if sampleIndices.length != sampleAxis.samples then
      Left(
        MvpaError.MatrixShapeMismatch(
          s"sample-index length ${sampleIndices.length} does not match sample axis ${sampleAxis.samples}"
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
    else Right(new PatternOperator(sampleAxis, sampleIndices, featureIndices, linear, provenance))

  def stackRows(operators: IndexedSeq[PatternOperator]): Either[MvpaError, PatternOperator] =
    if operators.isEmpty then
      Left(MvpaError.MatrixShapeMismatch("pattern operator stack must be non-empty"))
    else
      val featureAxis = operators.head.featureIndices
      val mismatch = operators.indexWhere(_.featureIndices != featureAxis)
      if mismatch >= 0 then
        Left(MvpaError.MatrixShapeMismatch(s"pattern operator stack part $mismatch has a different feature axis"))
      else
        for
          axis <- SampleAxis(operators.map(_.samples).sum)
          stacked <- LinearOperator
            .block(operators.map(operator => IndexedSeq(operator.linear)))
            .left
            .map(MvpaError.PatternOperatorFailed.apply)
          out <- fromOperator(
            sampleAxis = axis,
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
