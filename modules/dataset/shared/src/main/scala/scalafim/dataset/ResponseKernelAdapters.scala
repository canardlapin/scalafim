package scalafim.dataset

import cats.data.EitherT
import cats.effect.kernel.Sync
import cats.syntax.all.*
import narr.NArray
import scalafim.response.*

enum ResponseAdapterError:
  case Kernel(error: ResponseFailure)
  case SamplingFrameCardinality(blocks: Int, origins: Int, intervals: Int)
  case TimeCountMismatch(schemaCount: Int, sourceCount: Int)
  case SampleCountMismatch(schemaCount: Int, sourceCount: Int)
  case SampleDomainMismatch(expected: SampleDomain, actual: SampleDomain)

  def message: String =
    this match
      case Kernel(error) =>
        error.message
      case SamplingFrameCardinality(blocks, origins, intervals) =>
        s"sampling frame has $blocks blocks, $origins origins, and $intervals intervals"
      case TimeCountMismatch(schemaCount, sourceCount) =>
        s"response schema has $schemaCount timepoints but source has $sourceCount"
      case SampleCountMismatch(schemaCount, sourceCount) =>
        s"response schema has $schemaCount samples but source has $sourceCount"
      case SampleDomainMismatch(expected, actual) =>
        s"response sample domain '${actual.id.value}' does not match source domain '${expected.id.value}'"

object DatasetResponseSchema:
  def fromDataset(
      dataset: FmriDataset,
      schemaId: ResponseSchemaId,
      signalUnits: UnitId,
      nonFinite: NonFinitePolicy = NonFinitePolicy.Preserve
  ): Either[ResponseAdapterError, ResponseSchema] =
    val timeId = DomainId.unsafe[TimeAxis](s"${schemaId.value}:time")
    val time: Either[ResponseAdapterError, TimeDomain] =
      if dataset.samplingFrame.nBlocks == 1 then
        (
          dataset.samplingFrame.startTime.headOption,
          dataset.samplingFrame.tr.headOption
        ) match
          case (Some(origin), Some(interval)) =>
            TimeDomain
              .regular(
                timeId,
                origin.value,
                interval.value,
                dataset.shape.timepoints,
                UnitId.unsafe("second")
              )
              .left
              .map(ResponseAdapterError.Kernel.apply)
          case _ =>
            Left(ResponseAdapterError.SamplingFrameCardinality(
              dataset.samplingFrame.nBlocks,
              dataset.samplingFrame.startTime.length,
              dataset.samplingFrame.tr.length
            ))
      else
        TimeDomain
          .explicit(
            timeId,
            dataset.samplingFrame.acquisitionOnsets().map(_.value),
            UnitId.unsafe("second")
          )
          .left
          .map(ResponseAdapterError.Kernel.apply)

    time
      .flatMap: checkedTime =>
        fromShapeAndDomain(
          dataset.shape,
          dataset.voxelDomain,
          schemaId,
          checkedTime,
          signalUnits,
          nonFinite
        )

  def fromBackend(
      backend: DatasetBackend,
      schemaId: ResponseSchemaId,
      time: TimeDomain,
      signalUnits: UnitId,
      nonFinite: NonFinitePolicy = NonFinitePolicy.Preserve
  ): Either[ResponseAdapterError, ResponseSchema] =
    fromShapeAndDomain(
      backend.shape,
      backend.voxelDomain,
      schemaId,
      time,
      signalUnits,
      nonFinite
    )

  def fromBlockSource(
      source: ResponseBlockSource,
      schemaId: ResponseSchemaId,
      time: TimeDomain,
      signalUnits: UnitId,
      nonFinite: NonFinitePolicy = NonFinitePolicy.Preserve
  ): Either[ResponseAdapterError, ResponseSchema] =
    fromShapeAndDomain(
      source.shape,
      source.voxelDomain,
      schemaId,
      time,
      signalUnits,
      nonFinite
    )

  private def fromShapeAndDomain(
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      schemaId: ResponseSchemaId,
      time: TimeDomain,
      signalUnits: UnitId,
      nonFinite: NonFinitePolicy
  ): Either[ResponseAdapterError, ResponseSchema] =
    if time.count != shape.timepoints then
      Left(ResponseAdapterError.TimeCountMismatch(time.count, shape.timepoints))
    else
      for
        samples <- sampleDomain(shape, voxelDomain, schemaId)
        schema <- ResponseSchema
          .make(
            schemaId,
            time,
            samples,
            SignalSchema(signalUnits, CalibrationState.Applied, nonFinite)
          )
          .left
          .map(error => ResponseAdapterError.Kernel(error))
      yield schema

  private[dataset] def sampleDomain(
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      schemaId: ResponseSchemaId
  ): Either[ResponseAdapterError, SampleDomain] =
    val sampleId = DomainId.unsafe[SampleAxis](s"${schemaId.value}:samples")
    val spaceReference = DomainReference.unsafe(
      "scalafim-image-space",
      exactSpaceReference(shape)
    )
    val orderingReference = DomainReference.unsafe(
      "scalafim-dataset-order",
      voxelDomain.indices.mkString(",")
    )
    val maskReference =
      if voxelDomain.isFullSpatial then None
      else
        Some(DomainReference.unsafe(
          "scalafim-dataset-mask",
          voxelDomain.indices.mkString(",")
        ))
    SampleDomain
      .make(
        sampleId,
        voxelDomain.nVoxels,
        SampleDomainKind.Volume(spaceReference, maskReference, orderingReference)
      )
      .left
      .map(ResponseAdapterError.Kernel.apply)

  private def exactSpaceReference(shape: DatasetShape): String =
    val space = shape.space
    val dimensions = space.dims.mkString("x")
    val spacing = space.spacing.map(rawHex).mkString(",")
    val origin = space.origin.map(rawHex).mkString(",")
    val transform =
      Vector.tabulate(space.trans.rows * space.trans.cols): index =>
        val row = index / space.trans.cols
        val column = index % space.trans.cols
        rawHex(space.trans(row, column))
    s"$dimensions:$spacing:$origin:${transform.mkString(",")}"

  private def rawHex(value: Double): String =
    java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))

final class DatasetResponseSource[F[_]] private (
    val sourceId: SourceId,
    val schema: ResponseSchema,
    voxelOrdering: Vector[VoxelIndex],
    readDataset: DataSelection => Either[DatasetError, FmriSeries],
    val consistency: DecodeConsistency,
    payloadId: PayloadId,
    objectId: ObjectId,
    val layout: OpenedLayout,
    val provenance: Provenance
)(using F: Sync[F]) extends scalafim.response.ResponseSource[F]:
  final case class DatasetReadPlan private[dataset] (
      selection: ResolvedResponseSelection,
      datasetSelection: DataSelection
  )

  type Plan = DatasetReadPlan

  val capabilities: ReadCapabilities =
    ReadCapabilities.uniform(PhysicalLocality.WholePayload)

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, DatasetReadPlan] =
    for
      _ <- ResolvedResponseSelection
        .validateFor(schema, selection)
        .left
        .map(ReadPlanningError.InvalidSelection.apply)
      _ <- rejectDuplicates(selection.timepoints, SelectionAxes.Time)
      _ <- rejectDuplicates(selection.samples, SelectionAxes.Samples)
      timeSelection <- TimepointSelection
        .fromInts(selection.timepoints.values*)
        .left
        .map(error => ReadPlanningError.Unsupported(error.message))
      sampleSelection =
        VoxelSelection.Indices(
          selection.samples.values.map(voxelOrdering)
        )
    yield
      DatasetReadPlan(
        selection,
        DataSelection(timeSelection, sampleSelection)
      )

  def summarize(plan: DatasetReadPlan): ReadPlanSummary =
    ReadPlanSummary(
      sourceId,
      schema.id,
      plan.selection.selectedAxes,
      plan.selection.rows,
      plan.selection.columns,
      capabilities.localityByAxes
    )

  def execute(
      plan: DatasetReadPlan
  ): EitherT[F, ReadError, ReadResult] =
    val evaluated =
      F.delay(readDataset(plan.datasetSelection)).attempt.map:
        case Left(error) =>
          Left(ReadError.SourceFailure(sourceId, Option(error.getMessage).getOrElse(error.getClass.getName)))
        case Right(Left(error)) =>
          Left(ReadError.SourceFailure(sourceId, error.message))
        case Right(Right(series)) =>
          adaptSeries(plan.selection, series)
    EitherT(evaluated)

  private def adaptSeries(
      selection: ResolvedResponseSelection,
      series: FmriSeries
  ): Either[ReadError, ReadResult] =
    if series.nTimepoints != selection.rows || series.nVoxels != selection.columns then
      Left(ReadError.ResultMismatch(
        ReadResultMismatch.BlockShape(
          selection.rows,
          selection.columns,
          series.nTimepoints,
          series.nVoxels
        )
      ))
    else
      val copied = NArray.ofSize[Double](selection.rows * selection.columns)
      var row = 0
      while row < selection.rows do
        var column = 0
        while column < selection.columns do
          val index = row * selection.columns + column
          val value = series.data(row, column)
          if schema.signal.nonFinite == NonFinitePolicy.Reject && !value.isFinite then
            return Left(ReadError.InvalidBlock(
              ResponseShapeError.NonFiniteValue(index, value)
            ))
          copied(index) = value
          column += 1
        row += 1
      for
        block <- ResponseBlock
          .copyFromRowMajor(copied, selection)
          .left
          .map(ReadError.InvalidBlock.apply)
        result <- ReadResult.make(
          block,
          provenance,
          datasetReceipt(selection),
          capabilities,
          layout
        )
      yield result

  private def datasetReceipt(
      selection: ResolvedResponseSelection
  ): ReadReceipt =
    val logicalBytes =
      selection.rows.toLong * selection.columns.toLong * java.lang.Double.BYTES.toLong
    val axes = selection.selectedAxes
    ReadReceipt(
      LogicalReadSummary(
        selection.schema,
        axes,
        selection.timepoints.values,
        selection.samples.values,
        logicalBytes
      ),
      PhysicalReadSummary(
        Vector(
          AxisReadEvidence(
            axes,
            PhysicalLocality.WholePayload,
            Vector(PhysicalReadUnit.Payload(payloadId)),
            Vector(PhysicalReadUnit.Payload(payloadId))
          )
        ),
        PhysicalByteEvidence.Unavailable(
          OperationId.unsafe("dataset-uninstrumented")
        ),
        cacheHits = 0
      ),
      Vector(IntegrityEvidence.NotChecked),
      Vector.empty
    )

  private def rejectDuplicates[A](
      indices: OrderedIndices[A],
      axes: SelectionAxes
  ): Either[ReadPlanningError, Unit] =
    indices.indexOfDuplicate match
      case Some(value) =>
        Left(ReadPlanningError.DuplicateSelectionUnsupported(axes, value))
      case None =>
        Right(())

object DatasetResponseSource:
  def fromDataset[F[_]: Sync](
      dataset: SynchronousFmriDataset,
      schemaId: ResponseSchemaId,
      sourceId: SourceId,
      signalUnits: UnitId,
      consistency: DecodeConsistency = DecodeConsistency.ExactBits
  ): Either[ResponseAdapterError, DatasetResponseSource[F]] =
    DatasetResponseSchema
      .fromDataset(dataset.dataset, schemaId, signalUnits)
      .flatMap(schema =>
        fromBackend(dataset.backend, schema, sourceId, consistency)
      )

  def fromBackend[F[_]: Sync](
      backend: DatasetBackend,
      schema: ResponseSchema,
      sourceId: SourceId,
      consistency: DecodeConsistency = DecodeConsistency.ExactBits
  ): Either[ResponseAdapterError, DatasetResponseSource[F]] =
    make(
      sourceId,
      schema,
      backend.shape,
      backend.voxelDomain,
      backend.readEither,
      consistency
    )

  def fromBlockSource[F[_]: Sync](
      source: ResponseBlockSource,
      schema: ResponseSchema,
      sourceId: SourceId,
      consistency: DecodeConsistency = DecodeConsistency.ExactBits
  ): Either[ResponseAdapterError, DatasetResponseSource[F]] =
    make(
      sourceId,
      schema,
      source.shape,
      source.voxelDomain,
      source.readBlock,
      consistency
    )

  private def make[F[_]: Sync](
      sourceId: SourceId,
      schema: ResponseSchema,
      shape: DatasetShape,
      voxelDomain: VoxelDomain,
      read: DataSelection => Either[DatasetError, FmriSeries],
      consistency: DecodeConsistency
  ): Either[ResponseAdapterError, DatasetResponseSource[F]] =
    if schema.time.count != shape.timepoints then
      Left(ResponseAdapterError.TimeCountMismatch(schema.time.count, shape.timepoints))
    else if schema.samples.count != voxelDomain.nVoxels then
      Left(
        ResponseAdapterError.SampleCountMismatch(
          schema.samples.count,
          voxelDomain.nVoxels
        )
      )
    else
      val payload = PayloadId.unsafe(s"${sourceId.value}:payload")
      val objectId = ObjectId.unsafe(s"${sourceId.value}:dataset-object")
      val sourceNode = ProvenanceId.unsafe(s"${sourceId.value}:source")
      val adapterNode = ProvenanceId.unsafe(s"${sourceId.value}:dataset-adapter")
      for
        expectedSamples <- DatasetResponseSchema.sampleDomain(
          shape,
          voxelDomain,
          schema.id
        )
        _ <-
          if expectedSamples == schema.samples then Right(())
          else
            Left(
              ResponseAdapterError.SampleDomainMismatch(
                expectedSamples,
                schema.samples
              )
            )
        layout <- OpenedLayout
          .make(Vector(PayloadLayout(payload, Vector(objectId), Vector.empty)))
          .left
          .map(ResponseAdapterError.Kernel.apply)
        provenance <- Provenance
          .make(
            Vector(
              ProvenanceNode(
                sourceNode,
                ProvenanceOperation.SourceRead(sourceId),
                Vector.empty,
                Vector(ProvenanceEvidence.NoneDeclared)
              ),
              ProvenanceNode(
                adapterNode,
                ProvenanceOperation.Adapter(OperationId.unsafe("dataset-response-adapter-v1")),
                Vector(sourceNode),
                Vector(ProvenanceEvidence.NoneDeclared)
              )
            ),
            Vector(adapterNode)
          )
          .left
          .map(ResponseAdapterError.Kernel.apply)
      yield
        new DatasetResponseSource(
          sourceId,
          schema,
          voxelDomain.voxels,
          read,
          consistency,
          payload,
          objectId,
          layout,
          provenance
        )
