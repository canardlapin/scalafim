package scalafim.latent

import cats.instances.either.given
import scalafim.response.{
  DecodeConsistency,
  OrderedIndices,
  ReconstructionContract,
  ResolvedResponseSelection,
  ResponseBlock,
  ResponseSchema,
  ResponseSchemaId,
  SampleAxis,
  SelectionError,
  TimeAxis
}

enum RepresentationError:
  case Latent(error: LatentError)
  case InvalidSelection(error: SelectionError)
  case DecodeProgram(error: DecodePlanError)
  case SourceSchemaMismatch(
      expected: ResponseSchemaId,
      actual: ResponseSchemaId
  )
  case SourceMustCoverWholeSchema
  case OutputShapeOverflow(rows: Int, columns: Int)
  case SlotMismatch(expected: LogicalSlotId, actual: LogicalSlotId)
  case UnsupportedLogicalRead(slot: LogicalSlotId)
  case MissingLogicalPayload(slot: LogicalSlotId)
  case PayloadShapeMismatch(
      slot: LogicalSlotId,
      expected: Vector[Int],
      actual: Vector[Int]
  )
  case PayloadValueCountMismatch(
      label: String,
      expected: Int,
      actual: Int
  )
  case NonFinitePayloadValue(label: String, index: Int, value: Double)
  case ReconstructionContractViolation(
      index: Int,
      expected: Double,
      actual: Double,
      contract: ReconstructionContract
  )

  def message: String =
    this match
      case Latent(error) =>
        error.message
      case InvalidSelection(error) =>
        error.message
      case DecodeProgram(error) =>
        error.message
      case SourceSchemaMismatch(expected, actual) =>
        s"source response schema '${actual.value}' does not match '${expected.value}'"
      case SourceMustCoverWholeSchema =>
        "temporal DCT encoding requires the canonical whole-schema response block"
      case OutputShapeOverflow(rows, columns) =>
        s"decoded response shape ${rows}x$columns exceeds the primitive array limit"
      case SlotMismatch(expected, actual) =>
        s"logical read requested slot '${actual.value}', expected '${expected.value}'"
      case UnsupportedLogicalRead(slot) =>
        s"in-memory temporal DCT cannot interpret logical slot '${slot.value}'"
      case MissingLogicalPayload(slot) =>
        s"materialized temporal DCT is missing logical slot '${slot.value}'"
      case PayloadShapeMismatch(slot, expected, actual) =>
        s"logical slot '${slot.value}' expected shape " +
          s"${expected.mkString("[", ",", "]")} but got ${actual.mkString("[", ",", "]")}"
      case PayloadValueCountMismatch(label, expected, actual) =>
        s"$label requires $expected values but received $actual"
      case NonFinitePayloadValue(label, index, value) =>
        s"$label value $index must be finite, got $value"
      case ReconstructionContractViolation(index, expected, actual, contract) =>
        s"temporal DCT reconstruction violates $contract at value $index: " +
          s"expected $expected but got $actual"

final class TemporalBasisSlot private[latent] (
    val representation: RepresentationInstanceId,
    val timepoints: Int,
    val components: Int
) extends LogicalSlot[TemporalBasisValues]:
  val id: LogicalSlotId =
    LogicalSlotId.unsafe(s"${representation.value}/temporal-basis")

  val role: LogicalPayloadRole =
    LogicalPayloadRole.unsafe("org.scalafim/temporal-dct/basis")

  val dimensions: Vector[Int] =
    Vector(timepoints, components)

  override def equals(other: Any): Boolean =
    other match
      case that: TemporalBasisSlot =>
        representation == that.representation &&
          timepoints == that.timepoints &&
          components == that.components
      case _ =>
        false

  override def hashCode(): Int =
    (representation.value, timepoints, components).##

final class SpatialLoadingSlot private[latent] (
    val representation: RepresentationInstanceId,
    val samples: Int,
    val components: Int
) extends LogicalSlot[SpatialLoadingValues]:
  val id: LogicalSlotId =
    LogicalSlotId.unsafe(s"${representation.value}/spatial-loadings")

  val role: LogicalPayloadRole =
    LogicalPayloadRole.unsafe("org.scalafim/temporal-dct/loadings")

  val dimensions: Vector[Int] =
    Vector(samples, components)

  override def equals(other: Any): Boolean =
    other match
      case that: SpatialLoadingSlot =>
        representation == that.representation &&
          samples == that.samples &&
          components == that.components
      case _ =>
        false

  override def hashCode(): Int =
    (representation.value, samples, components).##

final class SampleOffsetSlot private[latent] (
    val representation: RepresentationInstanceId,
    val samples: Int
) extends LogicalSlot[SampleOffsetValues]:
  val id: LogicalSlotId =
    LogicalSlotId.unsafe(s"${representation.value}/sample-offset")

  val role: LogicalPayloadRole =
    LogicalPayloadRole.unsafe("org.scalafim/temporal-dct/offset")

  val dimensions: Vector[Int] =
    Vector(samples)

  override def equals(other: Any): Boolean =
    other match
      case that: SampleOffsetSlot =>
        representation == that.representation &&
          samples == that.samples
      case _ =>
        false

  override def hashCode(): Int =
    (representation.value, samples).##

final class TemporalBasisValues private (
    val rows: Int,
    val components: Int,
    private val ownedRowMajor: Array[Double]
):
  inline def apply(row: Int, component: Int): Double =
    ownedRowMajor(row * components + component)

  def rowMajorCopy: Array[Double] =
    ownedRowMajor.clone()

  private[latent] def selectRows(indices: Vector[Int]): TemporalBasisValues =
    val selected = new Array[Double](indices.length * components)
    var row = 0
    while row < indices.length do
      val sourceOffset = indices(row) * components
      val targetOffset = row * components
      var component = 0
      while component < components do
        selected(targetOffset + component) =
          ownedRowMajor(sourceOffset + component)
        component += 1
      row += 1
    new TemporalBasisValues(indices.length, components, selected)

object TemporalBasisValues:
  def copyFromRowMajor(
      rows: Int,
      components: Int,
      values: Array[Double]
  ): Either[RepresentationError, TemporalBasisValues] =
    validateTemporalDctMatrix("temporal basis", rows, components, values).map: _ =>
      new TemporalBasisValues(rows, components, values.clone())

  private[latent] def fromOwnedRowMajor(
      rows: Int,
      components: Int,
      values: Array[Double]
  ): Either[RepresentationError, TemporalBasisValues] =
    validateTemporalDctMatrix("temporal basis", rows, components, values).map: _ =>
      new TemporalBasisValues(rows, components, values)

final class SpatialLoadingValues private (
    val rows: Int,
    val components: Int,
    private val ownedRowMajor: Array[Double]
):
  inline def apply(row: Int, component: Int): Double =
    ownedRowMajor(row * components + component)

  def rowMajorCopy: Array[Double] =
    ownedRowMajor.clone()

  private[latent] def selectRows(indices: Vector[Int]): SpatialLoadingValues =
    val selected = new Array[Double](indices.length * components)
    var row = 0
    while row < indices.length do
      val sourceOffset = indices(row) * components
      val targetOffset = row * components
      var component = 0
      while component < components do
        selected(targetOffset + component) =
          ownedRowMajor(sourceOffset + component)
        component += 1
      row += 1
    new SpatialLoadingValues(indices.length, components, selected)

object SpatialLoadingValues:
  def copyFromRowMajor(
      rows: Int,
      components: Int,
      values: Array[Double]
  ): Either[RepresentationError, SpatialLoadingValues] =
    validateTemporalDctMatrix("spatial loadings", rows, components, values).map: _ =>
      new SpatialLoadingValues(rows, components, values.clone())

  private[latent] def fromOwnedRowMajor(
      rows: Int,
      components: Int,
      values: Array[Double]
  ): Either[RepresentationError, SpatialLoadingValues] =
    validateTemporalDctMatrix("spatial loadings", rows, components, values).map: _ =>
      new SpatialLoadingValues(rows, components, values)

final class SampleOffsetValues private (
    val length: Int,
    private val ownedValues: Array[Double]
):
  inline def apply(index: Int): Double =
    ownedValues(index)

  def valuesCopy: Array[Double] =
    ownedValues.clone()

  private[latent] def select(indices: Vector[Int]): SampleOffsetValues =
    val selected = new Array[Double](indices.length)
    var index = 0
    while index < indices.length do
      selected(index) = ownedValues(indices(index))
      index += 1
    new SampleOffsetValues(indices.length, selected)

object SampleOffsetValues:
  def copyFrom(
      values: Array[Double]
  ): Either[RepresentationError, SampleOffsetValues] =
    fromOwned(values.clone())

  private[latent] def fromOwned(
      values: Array[Double]
  ): Either[RepresentationError, SampleOffsetValues] =
    if values.isEmpty then
      Left(RepresentationError.PayloadValueCountMismatch(
        "sample offset",
        expected = 1,
        actual = 0
      ))
    else
      firstNonFiniteTemporalDctValue("sample offset", values) match
        case Some(error) =>
          Left(error)
        case None =>
          Right(new SampleOffsetValues(values.length, values))

sealed trait TemporalDctRead[A] extends LogicalPayloadRead[A]

object TemporalDctRead:
  final case class BasisRows private[latent] (
      slot: TemporalBasisSlot,
      timepoints: OrderedIndices[TimeAxis]
  ) extends TemporalDctRead[TemporalBasisValues]

  final case class LoadingRows private[latent] (
      slot: SpatialLoadingSlot,
      samples: OrderedIndices[SampleAxis]
  ) extends TemporalDctRead[SpatialLoadingValues]

  final case class OffsetEntries private[latent] (
      slot: SampleOffsetSlot,
      samples: OrderedIndices[SampleAxis]
  ) extends TemporalDctRead[SampleOffsetValues]

final class TemporalDctRepresentation private (
    val id: RepresentationInstanceId,
    val schema: ResponseSchema,
    val spec: DctSpec,
    val center: Boolean,
    val ridge: RidgePenalty,
    val reconstructionContract: ReconstructionContract,
    val decodeConsistency: DecodeConsistency
):
  val basisSlot: TemporalBasisSlot =
    new TemporalBasisSlot(id, schema.time.count, spec.components)

  val loadingSlot: SpatialLoadingSlot =
    new SpatialLoadingSlot(id, schema.samples.count, spec.components)

  val offsetSlot: Option[SampleOffsetSlot] =
    Option.when(center)(new SampleOffsetSlot(id, schema.samples.count))

  def compile(
      selection: ResolvedResponseSelection
  ): Either[RepresentationError, DecodePlan[ResponseBlock]] =
    for
      _ <- ResolvedResponseSelection
        .validateFor(schema, selection)
        .left
        .map(RepresentationError.InvalidSelection.apply)
      _ <-
        val values = selection.rows.toLong * selection.columns.toLong
        if values > Int.MaxValue.toLong then
          Left(RepresentationError.OutputShapeOverflow(
            selection.rows,
            selection.columns
          ))
        else Right(())
    yield
      val basis =
        DecodePlan.read(
          TemporalDctRead.BasisRows(basisSlot, selection.timepoints)
        )
      val loadings =
        DecodePlan.read(
          TemporalDctRead.LoadingRows(loadingSlot, selection.samples)
        )
      val offset: DecodePlan[Option[SampleOffsetValues]] =
        offsetSlot match
          case Some(slot) =>
            DecodePlan
              .read(TemporalDctRead.OffsetEntries(slot, selection.samples))
              .map(Some(_))
          case None =>
            DecodePlan.pure(None)
      DecodePlan.map3(basis, loadings, offset): (basis, loadings, offset) =>
        decodeBlock(basis, loadings, offset, selection)

  def encode(
      source: ResponseBlock
  ): Either[RepresentationError, MaterializedTemporalDct] =
    TemporalDctRepresentation.encode(this, source)

final class MaterializedTemporalDct private[latent] (
    val model: TemporalDctRepresentation,
    val basis: TemporalBasisValues,
    val loadings: SpatialLoadingValues,
    val offset: Option[SampleOffsetValues]
):
  def decode(
      selection: ResolvedResponseSelection
  ): Either[RepresentationError, ResponseBlock] =
    model.compile(selection).flatMap: plan =>
      type Result[A] = Either[RepresentationError, A]
      DecodePlan.runApplicative[Result, ResponseBlock](
        plan,
        new InMemoryTemporalDctInterpreter(this)
      ) match
        case Left(error) =>
          Left(RepresentationError.DecodeProgram(error))
        case Right(result) =>
          result

object TemporalDctRepresentation:
  def make(
      id: RepresentationInstanceId,
      schema: ResponseSchema,
      spec: DctSpec,
      center: Boolean,
      ridge: RidgePenalty,
      reconstructionContract: ReconstructionContract,
      decodeConsistency: DecodeConsistency
  ): Either[RepresentationError, TemporalDctRepresentation] =
    if spec.timepoints != schema.time.count then
      Left(RepresentationError.Latent(LatentError.DimensionMismatch(
        "DCT timepoints",
        schema.time.count,
        spec.timepoints
      )))
    else
      Right(new TemporalDctRepresentation(
        id,
        schema,
        spec,
        center,
        ridge,
        reconstructionContract,
        decodeConsistency
      ))

  def materialize(
      model: TemporalDctRepresentation,
      basis: TemporalBasisValues,
      loadings: SpatialLoadingValues,
      offset: Option[SampleOffsetValues]
  ): Either[RepresentationError, MaterializedTemporalDct] =
    checkedMaterialized(model, basis, loadings, offset)

  private def encode(
      model: TemporalDctRepresentation,
      source: ResponseBlock
  ): Either[RepresentationError, MaterializedTemporalDct] =
    if source.selection.schema != model.schema.id then
      Left(RepresentationError.SourceSchemaMismatch(
        model.schema.id,
        source.selection.schema
      ))
    else if !isCanonicalWholeSelection(model.schema, source.selection) then
      Left(RepresentationError.SourceMustCoverWholeSchema)
    else
      val sourceData = new Array[Double](source.rows * source.columns)
      var row = 0
      while row < source.rows do
        var column = 0
        while column < source.columns do
          sourceData(row * source.columns + column) = source(row, column)
          column += 1
        row += 1
      val matrix =
        LatentNumerics.matrixFromRowMajor(source.rows, source.columns, sourceData)
      for
        encoded <- TemporalBasisEncoder
          .encodeDctSpec(
            matrix,
            model.spec,
            center = model.center,
            ridge = model.ridge
          )
          .left
          .map(RepresentationError.Latent.apply)
        basis <- TemporalBasisValues.fromOwnedRowMajor(
          encoded.basis.rows,
          encoded.basis.cols,
          encoded.basis.copyData
        )
        loadings <- SpatialLoadingValues.fromOwnedRowMajor(
          encoded.loadings.rows,
          encoded.loadings.cols,
          encoded.loadings.copyData
        )
        offset <- encoded.offset match
          case Some(values) =>
            SampleOffsetValues.fromOwned(values.copyData).map(Some(_))
          case None =>
            Right(None)
        materialized <- materialize(model, basis, loadings, offset)
        reconstructed <- materialized.decode(source.selection)
        _ <- verifyReconstruction(
          source,
          reconstructed,
          model.reconstructionContract
        )
      yield materialized

  private def checkedMaterialized(
      model: TemporalDctRepresentation,
      basis: TemporalBasisValues,
      loadings: SpatialLoadingValues,
      offset: Option[SampleOffsetValues]
  ): Either[RepresentationError, MaterializedTemporalDct] =
    if basis.rows != model.basisSlot.timepoints ||
        basis.components != model.basisSlot.components
    then
      Left(RepresentationError.PayloadShapeMismatch(
        model.basisSlot.id,
        model.basisSlot.dimensions,
        Vector(basis.rows, basis.components)
      ))
    else if loadings.rows != model.loadingSlot.samples ||
        loadings.components != model.loadingSlot.components
    then
      Left(RepresentationError.PayloadShapeMismatch(
        model.loadingSlot.id,
        model.loadingSlot.dimensions,
        Vector(loadings.rows, loadings.components)
      ))
    else
      (model.offsetSlot, offset) match
        case (None, None) =>
          Right(new MaterializedTemporalDct(model, basis, loadings, offset))
        case (Some(slot), Some(values)) if values.length == slot.samples =>
          Right(new MaterializedTemporalDct(model, basis, loadings, offset))
        case (Some(slot), Some(values)) =>
          Left(RepresentationError.PayloadShapeMismatch(
            slot.id,
            slot.dimensions,
            Vector(values.length)
          ))
        case (Some(slot), None) =>
          Left(RepresentationError.MissingLogicalPayload(slot.id))
        case (None, Some(values)) =>
          Left(RepresentationError.PayloadShapeMismatch(
            LogicalSlotId.unsafe(s"${model.id.value}/sample-offset"),
            Vector.empty,
            Vector(values.length)
          ))

  private def isCanonicalWholeSelection(
      schema: ResponseSchema,
      selection: ResolvedResponseSelection
  ): Boolean =
    selection.schema == schema.id &&
      selection.timepoints.values == Vector.range(0, schema.time.count) &&
      selection.samples.values == Vector.range(0, schema.samples.count)

  private def verifyReconstruction(
      expected: ResponseBlock,
      actual: ResponseBlock,
      contract: ReconstructionContract
  ): Either[RepresentationError, Unit] =
    contract match
      case ReconstructionContract.ValidatedScientific(_, _) =>
        Right(())
      case _ =>
        var row = 0
        while row < expected.rows do
          var column = 0
          while column < expected.columns do
            val expectedValue = expected(row, column)
            val actualValue = actual(row, column)
            val agrees =
              contract match
                case ReconstructionContract.Exact =>
                  DecodeConsistency.ExactBits.agrees(expectedValue, actualValue)
                case ReconstructionContract.DeterministicBounded(bounds) =>
                  bounds.agrees(expectedValue, actualValue)
                case ReconstructionContract.ValidatedScientific(_, _) =>
                  true
            if !agrees then
              return Left(RepresentationError.ReconstructionContractViolation(
                row * expected.columns + column,
                expectedValue,
                actualValue,
                contract
              ))
            column += 1
          row += 1
        Right(())

private final class InMemoryTemporalDctInterpreter(
    materialized: MaterializedTemporalDct
) extends DecodePlan.Interpreter[
      [A] =>> Either[RepresentationError, A]
    ]:
  def apply[A](
      request: LogicalPayloadRead[A]
  ): Either[RepresentationError, A] =
    request match
      case TemporalDctRead.BasisRows(slot, timepoints) =>
        if slot != materialized.model.basisSlot then
          Left(RepresentationError.SlotMismatch(
            materialized.model.basisSlot.id,
            slot.id
          ))
        else Right(materialized.basis.selectRows(timepoints.values))
      case TemporalDctRead.LoadingRows(slot, samples) =>
        if slot != materialized.model.loadingSlot then
          Left(RepresentationError.SlotMismatch(
            materialized.model.loadingSlot.id,
            slot.id
          ))
        else Right(materialized.loadings.selectRows(samples.values))
      case TemporalDctRead.OffsetEntries(slot, samples) =>
        materialized.model.offsetSlot match
          case Some(expected) if slot != expected =>
            Left(RepresentationError.SlotMismatch(expected.id, slot.id))
          case Some(_) =>
            materialized.offset match
              case Some(values) =>
                Right(values.select(samples.values))
              case None =>
                Left(RepresentationError.MissingLogicalPayload(slot.id))
          case None =>
            Left(RepresentationError.UnsupportedLogicalRead(slot.id))
      case other =>
        Left(RepresentationError.UnsupportedLogicalRead(other.slot.id))

private def decodeBlock(
    basis: TemporalBasisValues,
    loadings: SpatialLoadingValues,
    offset: Option[SampleOffsetValues],
    selection: ResolvedResponseSelection
): ResponseBlock =
  require(
    basis.rows == selection.rows,
    "temporal basis rows must match selected response rows"
  )
  require(
    loadings.rows == selection.columns,
    "spatial loading rows must match selected response columns"
  )
  require(
    basis.components == loadings.components,
    "temporal basis and spatial loadings must share component count"
  )
  require(
    offset.forall(_.length == selection.columns),
    "sample offset length must match selected response columns"
  )
  val output = Array.ofDim[Double](selection.rows * selection.columns)
  var row = 0
  while row < selection.rows do
    var column = 0
    while column < selection.columns do
      var sum = offset.fold(0.0)(_(column))
      var component = 0
      while component < basis.components do
        sum += basis(row, component) * loadings(column, component)
        component += 1
      output(row * selection.columns + column) = sum
      column += 1
    row += 1
  ResponseBlock.unsafeFromOwnedRowMajor(output, selection)

private def validateTemporalDctMatrix(
    label: String,
    rows: Int,
    columns: Int,
    values: Array[Double]
): Either[RepresentationError, Unit] =
  if rows <= 0 then
    Left(RepresentationError.Latent(
      LatentError.NonPositiveDimension(s"$label rows", rows)
    ))
  else if columns <= 0 then
    Left(RepresentationError.Latent(
      LatentError.NonPositiveDimension(s"$label columns", columns)
    ))
  else
    val expected = rows.toLong * columns.toLong
    if expected > Int.MaxValue.toLong then
      Left(RepresentationError.OutputShapeOverflow(rows, columns))
    else if values.length != expected.toInt then
      Left(RepresentationError.PayloadValueCountMismatch(
        label,
        expected.toInt,
        values.length
      ))
    else
      firstNonFiniteTemporalDctValue(label, values) match
        case Some(error) =>
          Left(error)
        case None =>
          Right(())

private def firstNonFiniteTemporalDctValue(
    label: String,
    values: Array[Double]
): Option[RepresentationError] =
  var index = 0
  while index < values.length do
    val value = values(index)
    if !value.isFinite then
      return Some(RepresentationError.NonFinitePayloadValue(
        label,
        index,
        value
      ))
    index += 1
  None
