package scalafim.dataset

import cats.Monad
import cats.data.{EitherT, NonEmptyChain, NonEmptyVector, Validated, ValidatedNec}
import scalafim.response.{
  CalibrationState,
  DomainReference,
  DuplicatePolicy,
  NonFinitePolicy,
  OperationId,
  OrderedIndices,
  Provenance,
  ProvenanceEvidence,
  ProvenanceId,
  ProvenanceOperation,
  ReadError,
  ReadPlanningError,
  ReadResult,
  ResolvedResponseSelection,
  ResponseSchema,
  ResponseSchemaId,
  ResponseSource,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId
}

opaque type ResponseKey = String

object ResponseKey:
  def fromString(value: String): Either[DatasetError, ResponseKey] =
    val normalized = value.trim
    if normalized.isEmpty then
      Left(DatasetError.InvalidLabel("ResponseKey", value, "must be non-empty"))
    else if normalized.exists(character => character.isWhitespace || character.isControl) then
      Left(DatasetError.InvalidLabel(
        "ResponseKey",
        value,
        "must not contain whitespace or control characters"
      ))
    else
      Right(normalized)

  def unsafe(value: String): ResponseKey =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (key: ResponseKey)
    inline def value: String =
      key

enum AssemblyPolicy:
  case Segmented
  case BlockConcatenate

final class SampleAlignment private (
    private val datasetToSource: Vector[Int]
):
  def size: Int =
    datasetToSource.length

  def sourceOrdinal(datasetOrdinal: Int): Int =
    datasetToSource(datasetOrdinal)

  def isIdentity: Boolean =
    var index = 0
    while index < datasetToSource.length do
      if datasetToSource(index) != index then return false
      index += 1
    true

  def values: Vector[Int] =
    datasetToSource

object SampleAlignment:
  def make(values: Vector[Int]): Either[AttachmentIssue, SampleAlignment] =
    if values.isEmpty then
      Left(AttachmentIssue.InvalidSampleAlignment(
        "sample alignment must contain at least one sample"
      ))
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var index = 0
      while index < values.length do
        val value = values(index)
        if value < 0 || value >= values.length then
          return Left(AttachmentIssue.InvalidSampleAlignment(
            s"source sample ordinal $value is outside [0, ${values.length})"
          ))
        if seen.contains(value) then
          return Left(AttachmentIssue.InvalidSampleAlignment(
            s"source sample ordinal $value appears more than once"
          ))
        seen += value
        index += 1
      Right(new SampleAlignment(values))

  def identity(size: Int): Either[AttachmentIssue, SampleAlignment] =
    if size <= 0 then
      Left(AttachmentIssue.InvalidSampleAlignment(
        s"sample count must be positive; got $size"
      ))
    else
      Right(new SampleAlignment(Vector.tabulate(size)(index => index)))

sealed trait DatasetSampleDomainAdapter:
  def count: Int

  def responseDomain(
      schema: ResponseSchemaId
  ): Either[AttachmentIssue, SampleDomain]

object DatasetSampleDomainAdapter:
  final class Volume private[dataset] (
      val shape: DatasetShape,
      val voxels: VoxelDomain
  ) extends DatasetSampleDomainAdapter:
    val count: Int =
      voxels.nVoxels

    def responseDomain(
        schema: ResponseSchemaId
    ): Either[AttachmentIssue, SampleDomain] =
      DatasetResponseSchema
        .sampleDomain(shape, voxels, schema)
        .left
        .map(error => AttachmentIssue.InvalidExpectedSchema(error.message))

  final class Surface private[dataset] (
      val count: Int,
      val surface: DomainReference,
      val topology: DomainReference,
      val ordering: DomainReference
  ) extends DatasetSampleDomainAdapter:
    def responseDomain(
        schema: ResponseSchemaId
    ): Either[AttachmentIssue, SampleDomain] =
      SampleDomain
        .make(
          scalafim.response.DomainId.unsafe[SampleAxis](
            s"${schema.value}:samples"
          ),
          count,
          SampleDomainKind.Surface(surface, topology, ordering)
        )
        .left
        .map(error => AttachmentIssue.InvalidExpectedSchema(error.message))

  def volume(
      shape: DatasetShape,
      voxels: VoxelDomain
  ): Either[AttachmentIssue, DatasetSampleDomainAdapter] =
    if voxels.spatialSize != shape.spatialSize then
      Left(AttachmentIssue.GeometryMismatch(
        s"voxel domain has spatial size ${voxels.spatialSize}, expected ${shape.spatialSize}"
      ))
    else
      Right(new Volume(shape, voxels))

  def surface(
      count: Int,
      surface: DomainReference,
      topology: DomainReference,
      ordering: DomainReference
  ): Either[AttachmentIssue, DatasetSampleDomainAdapter] =
    if count <= 0 then
      Left(AttachmentIssue.SampleCountMismatch(1, count))
    else
      Right(new Surface(count, surface, topology, ordering))

final class AcquisitionContext private (
    val datasetId: DatasetId,
    val datasetKey: DatasetKey,
    val runIds: Vector[RunId],
    val responseKey: ResponseKey,
    val expectedSchema: ResponseSchema,
    val samples: DatasetSampleDomainAdapter,
    val alignment: SampleAlignment
)

object AcquisitionContext:
  def volume(
      dataset: FmriDataset,
      datasetKey: DatasetKey,
      responseKey: ResponseKey,
      schemaId: ResponseSchemaId,
      signalUnits: UnitId,
      nonFinite: NonFinitePolicy = NonFinitePolicy.Preserve,
      alignment: Option[SampleAlignment] = None
  ): Either[AttachmentIssue, AcquisitionContext] =
    for
      sampleAdapter <- DatasetSampleDomainAdapter
        .volume(dataset.shape, dataset.voxelDomain)
      expected <- DatasetResponseSchema
        .fromDataset(dataset, schemaId, signalUnits, nonFinite)
        .left
        .map(error => AttachmentIssue.InvalidExpectedSchema(error.message))
      checkedAlignment <- alignment match
        case Some(found) =>
          if found.size == expected.samples.count then Right(found)
          else
            Left(AttachmentIssue.SampleAlignmentCardinality(
              expected.samples.count,
              found.size
            ))
        case None =>
          SampleAlignment.identity(expected.samples.count)
    yield
      new AcquisitionContext(
        dataset.id,
        datasetKey,
        dataset.timeAxis.runIds,
        responseKey,
        expected,
        sampleAdapter,
        checkedAlignment
      )

  def make(
      datasetId: DatasetId,
      datasetKey: DatasetKey,
      runIds: Vector[RunId],
      responseKey: ResponseKey,
      expectedSchema: ResponseSchema,
      samples: DatasetSampleDomainAdapter,
      alignment: SampleAlignment
  ): Either[AttachmentIssue, AcquisitionContext] =
    for
      _ <-
        if runIds.nonEmpty then Right(())
        else Left(AttachmentIssue.RunIdentityMismatch(Vector.empty, runIds))
      _ <-
        if runIds.distinct.length == runIds.length then Right(())
        else
          Left(AttachmentIssue.InvalidExpectedSchema(
            "acquisition run ids must be unique"
          ))
      expectedSamples <- samples.responseDomain(expectedSchema.id)
      _ <-
        if expectedSchema.samples == expectedSamples then Right(())
        else
          Left(AttachmentIssue.InvalidExpectedSchema(
            "sample-domain adapter does not reproduce the expected response sample domain"
          ))
      _ <-
        if alignment.size == samples.count then Right(())
        else
          Left(AttachmentIssue.SampleAlignmentCardinality(
            samples.count,
            alignment.size
          ))
    yield
      new AcquisitionContext(
        datasetId,
        datasetKey,
        runIds,
        responseKey,
        expectedSchema,
        samples,
        alignment
      )

enum AttachmentIssue:
  case DatasetIdentityMismatch(expected: DatasetId, actual: DatasetId)
  case RunIdentityMismatch(expected: Vector[RunId], actual: Vector[RunId])
  case SchemaMismatch(expected: ResponseSchemaId, actual: ResponseSchemaId)
  case TimeCountMismatch(expected: Int, actual: Int)
  case TimeUnitsMismatch(expected: UnitId, actual: UnitId)
  case TimeCoordinateMismatch(index: Int, expectedRawBits: Long, actualRawBits: Long)
  case SampleCountMismatch(expected: Int, actual: Int)
  case SampleDomainKindMismatch(expected: String, actual: String)
  case GeometryMismatch(detail: String)
  case MaskIdentityMismatch(expected: Option[DomainReference], actual: Option[DomainReference])
  case SampleOrderingMismatch(expected: DomainReference, actual: DomainReference)
  case SurfaceIdentityMismatch(expected: DomainReference, actual: DomainReference)
  case SurfaceTopologyMismatch(expected: DomainReference, actual: DomainReference)
  case SignalUnitsMismatch(expected: UnitId, actual: UnitId)
  case CalibrationMismatch(expected: CalibrationState, actual: CalibrationState)
  case NonFinitePolicyMismatch(expected: NonFinitePolicy, actual: NonFinitePolicy)
  case SampleAlignmentCardinality(expected: Int, actual: Int)
  case InvalidSampleAlignment(detail: String)
  case InvalidExpectedSchema(detail: String)

  def message: String =
    this match
      case DatasetIdentityMismatch(expected, actual) =>
        s"dataset identity '${actual.value}' does not match '${expected.value}'"
      case RunIdentityMismatch(expected, actual) =>
        s"run identities ${renderRuns(actual)} do not match ${renderRuns(expected)}"
      case SchemaMismatch(expected, actual) =>
        s"response schema '${actual.value}' does not match '${expected.value}'"
      case TimeCountMismatch(expected, actual) =>
        s"response time count $actual does not match $expected"
      case TimeUnitsMismatch(expected, actual) =>
        s"response time units '${actual.value}' do not match '${expected.value}'"
      case TimeCoordinateMismatch(index, expected, actual) =>
        s"response time coordinate $index has raw bits $actual, expected $expected"
      case SampleCountMismatch(expected, actual) =>
        s"response sample count $actual does not match $expected"
      case SampleDomainKindMismatch(expected, actual) =>
        s"response sample-domain kind '$actual' does not match '$expected'"
      case GeometryMismatch(detail) =>
        s"response geometry mismatch: $detail"
      case MaskIdentityMismatch(expected, actual) =>
        s"response mask identity ${renderReference(actual)} does not match ${renderReference(expected)}"
      case SampleOrderingMismatch(expected, actual) =>
        s"response sample ordering '$actual' does not match '$expected'"
      case SurfaceIdentityMismatch(expected, actual) =>
        s"response surface identity '$actual' does not match '$expected'"
      case SurfaceTopologyMismatch(expected, actual) =>
        s"response surface topology '$actual' does not match '$expected'"
      case SignalUnitsMismatch(expected, actual) =>
        s"response signal units '${actual.value}' do not match '${expected.value}'"
      case CalibrationMismatch(expected, actual) =>
        s"response calibration '$actual' does not match '$expected'"
      case NonFinitePolicyMismatch(expected, actual) =>
        s"response non-finite policy '$actual' does not match '$expected'"
      case SampleAlignmentCardinality(expected, actual) =>
        s"sample alignment has $actual entries, expected $expected"
      case InvalidSampleAlignment(detail) =>
        s"invalid sample alignment: $detail"
      case InvalidExpectedSchema(detail) =>
        s"invalid expected response schema: $detail"

private def renderRuns(values: Vector[RunId]): String =
  values.map(_.value).mkString("[", ",", "]")

private def renderReference(value: Option[DomainReference]): String =
  value.fold("<none>")(_.toString)

final case class ResponseRead(
    key: ResponseKey,
    run: RunKey,
    selection: ResolvedResponseSelection,
    partition: DatasetRunPartition,
    datasetVoxels: Vector[VoxelIndex]
)

final class ResolvedDatasetRead private (
    val attachment: DatasetAttachmentId,
    val reads: NonEmptyVector[ResponseRead],
    val assembly: AssemblyPolicy
)

object ResolvedDatasetRead:
  private[dataset] def make(
      attachment: DatasetAttachmentId,
      reads: NonEmptyVector[ResponseRead],
      assembly: AssemblyPolicy
  ): ResolvedDatasetRead =
    new ResolvedDatasetRead(attachment, reads, assembly)

final case class DatasetReadSegment(
    run: RunKey,
    partition: DatasetRunPartition,
    datasetVoxels: Vector[VoxelIndex],
    response: ReadResult
):
  def toFmriSeries(
      dataset: FmriDataset
  ): Either[DatasetError, FmriSeries] =
    val block = response.block
    if block.rows != partition.timepointIndices.length then
      Left(DatasetError.ShapeMismatch(
        s"response block has ${block.rows} rows but run partition has ${partition.timepointIndices.length}"
      ))
    else if block.columns != datasetVoxels.length then
      Left(DatasetError.ShapeMismatch(
        s"response block has ${block.columns} columns but dataset selection has ${datasetVoxels.length}"
      ))
    else
      FmriSeries.make(
        matrixFromRowMajor(block.rows, block.columns, block.rowMajorCopy),
        datasetVoxels,
        partition.timepointIndices,
        dataset.shape,
        dataset.metadata
      )

final class DatasetReadResult private (
    val segments: NonEmptyVector[DatasetReadSegment],
    val assembly: AssemblyPolicy
):
  def toFmriSeries(
      dataset: FmriDataset
  ): Either[DatasetError, FmriSeries] =
    val values = segments.toVector
    val expectedVoxels = values.head.datasetVoxels
    var segmentIndex = 1
    while segmentIndex < values.length do
      if values(segmentIndex).datasetVoxels != expectedVoxels then
        return Left(DatasetError.ShapeMismatch(
          "dataset read segments have different ordered sample selections"
        ))
      segmentIndex += 1

    val totalRowsLong =
      values.foldLeft(0L)(_ + _.response.block.rows.toLong)
    val totalValuesLong =
      totalRowsLong * expectedVoxels.length.toLong
    if totalRowsLong > Int.MaxValue.toLong ||
        totalValuesLong > Int.MaxValue.toLong
    then
      Left(DatasetError.ShapeMismatch(
        "dataset read result is too large for one in-memory series"
      ))
    else
      val totalRows = totalRowsLong.toInt
      val columns = expectedVoxels.length
      val copied = Array.ofDim[Double](totalValuesLong.toInt)
      val timepoints = Vector.newBuilder[TimepointIndex]
      timepoints.sizeHint(totalRows)
      var outputRow = 0
      segmentIndex = 0
      while segmentIndex < values.length do
        val segment = values(segmentIndex)
        val block = segment.response.block
        timepoints ++= segment.partition.timepointIndices
        var row = 0
        while row < block.rows do
          var column = 0
          while column < block.columns do
            copied((outputRow + row) * columns + column) =
              block(row, column)
            column += 1
          row += 1
        outputRow += block.rows
        segmentIndex += 1
      FmriSeries.make(
        matrixFromRowMajor(totalRows, columns, copied),
        expectedVoxels,
        timepoints.result(),
        dataset.shape,
        dataset.metadata
      )

  def toSegmentedFmriSeries(
      dataset: FmriDataset
  ): Either[DatasetError, SegmentedFmriSeries] =
    val out = Vector.newBuilder[FmriSeriesSegment]
    val values = segments.toVector
    var index = 0
    while index < values.length do
      val segment = values(index)
      segment.toFmriSeries(dataset) match
        case Left(error) =>
          return Left(error)
        case Right(series) =>
          FmriSeriesSegment.make(segment.run, segment.partition, series) match
            case Left(error) =>
              return Left(error)
            case Right(value) =>
              out += value
      index += 1
    SegmentedFmriSeries.make(out.result())

object DatasetReadResult:
  private[dataset] def make(
      segments: NonEmptyVector[DatasetReadSegment],
      assembly: AssemblyPolicy
  ): DatasetReadResult =
    new DatasetReadResult(segments, assembly)

opaque type DatasetAttachmentId = String

object DatasetAttachmentId:
  private[dataset] def from(
      dataset: FmriDataset,
      context: AcquisitionContext,
      source: ResponseSource[?]
  ): DatasetAttachmentId =
    s"${dataset.id.value}:${context.responseKey.value}:${source.sourceId.value}:${source.schema.id.value}"

  extension (id: DatasetAttachmentId)
    inline def value: String =
      id

trait OpenedDataset[F[_]]:
  def dataset: FmriDataset
  def acquisition: AcquisitionContext
  def attachmentId: DatasetAttachmentId

  def plan(
      query: DatasetRunQuery,
      selection: DataSelection = DataSelection.All,
      assembly: AssemblyPolicy = AssemblyPolicy.Segmented
  ): Either[DatasetError, ResolvedDatasetRead]

  def execute(
      plan: ResolvedDatasetRead
  )(using Monad[F]): EitherT[F, DatasetError, DatasetReadResult]

  final def read(
      query: DatasetRunQuery,
      selection: DataSelection = DataSelection.All,
      assembly: AssemblyPolicy = AssemblyPolicy.Segmented
  )(using Monad[F]): EitherT[F, DatasetError, DatasetReadResult] =
    EitherT
      .fromEither[F](plan(query, selection, assembly))
      .flatMap(execute)

object OpenedDataset:
  def attach[F[_]](
      dataset: FmriDataset,
      source: ResponseSource[F],
      acquisition: AcquisitionContext
  ): ValidatedNec[AttachmentIssue, OpenedDataset[F]] =
    val issues = attachmentIssues(dataset, source.schema, acquisition)
    NonEmptyChain.fromSeq(issues) match
      case Some(found) =>
        Validated.Invalid(found)
      case None =>
        Validated.Valid(
          new AttachedDataset(
            dataset,
            source,
            acquisition,
            DatasetAttachmentId.from(dataset, acquisition, source)
          )
        )

  private def attachmentIssues(
      dataset: FmriDataset,
      actual: ResponseSchema,
      context: AcquisitionContext
  ): Vector[AttachmentIssue] =
    val expected = context.expectedSchema
    val issues = Vector.newBuilder[AttachmentIssue]
    if dataset.id != context.datasetId then
      issues += AttachmentIssue.DatasetIdentityMismatch(
        context.datasetId,
        dataset.id
      )
    if dataset.timeAxis.runIds != context.runIds then
      issues += AttachmentIssue.RunIdentityMismatch(
        context.runIds,
        dataset.timeAxis.runIds
      )
    context.samples match
      case volume: DatasetSampleDomainAdapter.Volume =>
        if volume.shape != dataset.shape then
          issues += AttachmentIssue.GeometryMismatch(
            "volume-domain adapter shape does not match the dataset description"
          )
        if volume.voxels.indices != dataset.voxelDomain.indices then
          issues += AttachmentIssue.InvalidSampleAlignment(
            "volume-domain adapter voxel ordering does not match the dataset description"
          )
      case _: DatasetSampleDomainAdapter.Surface =>
        issues += AttachmentIssue.SampleDomainKindMismatch(
          "volume dataset",
          "surface adapter"
        )
    if actual.id != expected.id then
      issues += AttachmentIssue.SchemaMismatch(expected.id, actual.id)
    compareTime(expected.time, actual.time, issues)
    compareSamples(expected.samples, actual.samples, context.alignment, issues)
    compareSignal(expected.signal, actual.signal, issues)
    issues.result()

  private def compareTime(
      expected: TimeDomain,
      actual: TimeDomain,
      issues: scala.collection.mutable.Builder[AttachmentIssue, Vector[AttachmentIssue]]
  ): Unit =
    if actual.count != expected.count then
      issues += AttachmentIssue.TimeCountMismatch(expected.count, actual.count)
    if actual.units != expected.units then
      issues += AttachmentIssue.TimeUnitsMismatch(expected.units, actual.units)
    val count = math.min(expected.count, actual.count)
    var index = 0
    while index < count do
      val expectedCoordinate = expected.coordinate(
        scalafim.response.AxisIndex.fromInt[TimeAxis](index).toOption.get
      ).toOption.get
      val actualCoordinate = actual.coordinate(
        scalafim.response.AxisIndex.fromInt[TimeAxis](index).toOption.get
      ).toOption.get
      if expectedCoordinate.rawBits != actualCoordinate.rawBits then
        issues += AttachmentIssue.TimeCoordinateMismatch(
          index,
          expectedCoordinate.rawBits,
          actualCoordinate.rawBits
        )
      index += 1

  private def compareSamples(
      expected: SampleDomain,
      actual: SampleDomain,
      alignment: SampleAlignment,
      issues: scala.collection.mutable.Builder[AttachmentIssue, Vector[AttachmentIssue]]
  ): Unit =
    if actual.count != expected.count then
      issues += AttachmentIssue.SampleCountMismatch(expected.count, actual.count)
    if alignment.size != expected.count then
      issues += AttachmentIssue.SampleAlignmentCardinality(
        expected.count,
        alignment.size
      )
    (expected.kind, actual.kind) match
      case (
            SampleDomainKind.Volume(expectedSpace, expectedMask, expectedOrder),
            SampleDomainKind.Volume(actualSpace, actualMask, actualOrder)
          ) =>
        if expectedSpace != actualSpace then
          issues += AttachmentIssue.GeometryMismatch(
            s"volume space '$actualSpace' does not match '$expectedSpace'"
          )
        if expectedMask != actualMask then
          issues += AttachmentIssue.MaskIdentityMismatch(
            expectedMask,
            actualMask
          )
        if expectedOrder != actualOrder && alignment.isIdentity then
          issues += AttachmentIssue.SampleOrderingMismatch(
            expectedOrder,
            actualOrder
          )
      case (
            SampleDomainKind.Surface(expectedSurface, expectedTopology, expectedOrder),
            SampleDomainKind.Surface(actualSurface, actualTopology, actualOrder)
          ) =>
        if expectedSurface != actualSurface then
          issues += AttachmentIssue.SurfaceIdentityMismatch(
            expectedSurface,
            actualSurface
          )
        if expectedTopology != actualTopology then
          issues += AttachmentIssue.SurfaceTopologyMismatch(
            expectedTopology,
            actualTopology
          )
        if expectedOrder != actualOrder && alignment.isIdentity then
          issues += AttachmentIssue.SampleOrderingMismatch(
            expectedOrder,
            actualOrder
          )
      case (expectedKind, actualKind) =>
        issues += AttachmentIssue.SampleDomainKindMismatch(
          sampleKind(expectedKind),
          sampleKind(actualKind)
        )

  private def compareSignal(
      expected: SignalSchema,
      actual: SignalSchema,
      issues: scala.collection.mutable.Builder[AttachmentIssue, Vector[AttachmentIssue]]
  ): Unit =
    if actual.units != expected.units then
      issues += AttachmentIssue.SignalUnitsMismatch(expected.units, actual.units)
    if actual.calibration != expected.calibration then
      issues += AttachmentIssue.CalibrationMismatch(
        expected.calibration,
        actual.calibration
      )
    if actual.nonFinite != expected.nonFinite then
      issues += AttachmentIssue.NonFinitePolicyMismatch(
        expected.nonFinite,
        actual.nonFinite
      )

  private def sampleKind(value: SampleDomainKind): String =
    value match
      case _: SampleDomainKind.Volume  => "volume"
      case _: SampleDomainKind.Surface => "surface"

private final class AttachedDataset[F[_]](
    val dataset: FmriDataset,
    source: ResponseSource[F],
    val acquisition: AcquisitionContext,
    val attachmentId: DatasetAttachmentId
) extends OpenedDataset[F]:

  def plan(
      query: DatasetRunQuery,
      selection: DataSelection,
      assembly: AssemblyPolicy
  ): Either[DatasetError, ResolvedDatasetRead] =
    val selectedBlocks =
      dataset.timeAxis.blocks.filter: block =>
        query.matches(RunKey(acquisition.datasetKey, block.run))
    NonEmptyVector.fromVector(selectedBlocks) match
      case None =>
        Left(DatasetError.DatasetRunNotFound(query.label))
      case Some(blocks) =>
        val reads = Vector.newBuilder[ResponseRead]
        val values = blocks.toVector
        var index = 0
        while index < values.length do
          compileRun(values(index), selection) match
            case Left(error) =>
              return Left(error)
            case Right(read) =>
              reads += read
          index += 1
        Right(
          ResolvedDatasetRead.make(
            attachmentId,
            NonEmptyVector.fromVectorUnsafe(reads.result()),
            assembly
          )
        )

  def execute(
      plan: ResolvedDatasetRead
  )(using F: Monad[F]): EitherT[F, DatasetError, DatasetReadResult] =
    if plan.attachment != attachmentId then
      EitherT.leftT(DatasetError.AttachmentPlanMismatch(
        attachmentId,
        plan.attachment
      ))
    else
      val initial =
        EitherT.rightT[F, DatasetError](Vector.empty[DatasetReadSegment])
      val gathered =
        plan.reads.toVector.foldLeft(initial): (state, read) =>
          state.flatMap: completed =>
            source
              .read(read.selection)
              .leftMap(DatasetError.ResponseReadFailure.apply)
              .flatMap: result =>
                EitherT.fromEither[F](
                  attachProvenance(read, result)
                )
              .map: result =>
                completed :+ DatasetReadSegment(
                  read.run,
                  read.partition,
                  read.datasetVoxels,
                  result
                )
      gathered.map: segments =>
        DatasetReadResult.make(
          NonEmptyVector.fromVectorUnsafe(segments),
          plan.assembly
        )

  private def attachProvenance(
      read: ResponseRead,
      result: ReadResult
  ): Either[DatasetError, ReadResult] =
    val nodeId =
      ProvenanceId.unsafe(
        s"${attachmentId.value}:${read.run.label}:attachment"
      )
    for
      provenance <- Provenance
        .derive(
          result.provenance,
          nodeId,
          ProvenanceOperation.Adapter(
            OperationId.unsafe("opened-dataset-attachment-v1")
          ),
          Vector(ProvenanceEvidence.NoneDeclared)
        )
        .left
        .map(error =>
          DatasetError.ResponseReadFailure(
            ReadError.InvalidProvenance(error)
          )
        )
      attached <- ReadResult
        .make(
          result.block,
          provenance,
          result.receipt,
          source.capabilities,
          source.layout
        )
        .left
        .map(DatasetError.ResponseReadFailure.apply)
    yield attached

  private def compileRun(
      block: DatasetTimeBlock,
      selection: DataSelection
  ): Either[DatasetError, ResponseRead] =
    for
      localTimepoints <- selection.time.resolve(block.length)
      datasetVoxels <- dataset.voxelDomain.resolve(
        selection.voxels,
        dataset.shape.space
      )
      globalTimepoints =
        localTimepoints.map: local =>
          TimepointIndex.unsafe(block.startValue + local.value)
      sourceSamples <- sourceSampleOrdinals(datasetVoxels)
      responseTime <- OrderedIndices
        .fromInts[TimeAxis](
          source.schema.time.id,
          source.schema.time.count,
          globalTimepoints.map(_.value),
          DuplicatePolicy.Reject
        )
        .left
        .map(error => DatasetError.ResponseSelectionFailure(error.message))
      responseSamples <- OrderedIndices
        .fromInts[SampleAxis](
          source.schema.samples.id,
          source.schema.samples.count,
          sourceSamples,
          DuplicatePolicy.Reject
        )
        .left
        .map(error => DatasetError.ResponseSelectionFailure(error.message))
      responseSelection <- ResolvedResponseSelection
        .make(source.schema, responseTime, responseSamples)
        .left
        .map(error => DatasetError.ResponseSelectionFailure(error.message))
      partition <- DatasetRunPartition.make(
        block,
        Vector.tabulate(globalTimepoints.length)(index => index),
        globalTimepoints,
        localTimepoints.map(value => RunLocalTimepointIndex.unsafe(value.value))
      )
    yield
      ResponseRead(
        acquisition.responseKey,
        RunKey(acquisition.datasetKey, block.run),
        responseSelection,
        partition,
        datasetVoxels
      )

  private def sourceSampleOrdinals(
      voxels: Vector[VoxelIndex]
  ): Either[DatasetError, Vector[Int]] =
    val ordinals =
      dataset.voxelDomain.voxels.iterator.zipWithIndex
        .map((voxel, ordinal) => voxel.value -> ordinal)
        .toMap
    val out = Vector.newBuilder[Int]
    out.sizeHint(voxels.length)
    var index = 0
    while index < voxels.length do
      val voxel = voxels(index)
      ordinals.get(voxel.value) match
        case None =>
          return Left(DatasetError.VoxelOutsideMask(voxel.value))
        case Some(datasetOrdinal) =>
          out += acquisition.alignment.sourceOrdinal(datasetOrdinal)
      index += 1
    Right(out.result())
