package scalafim.fmri.mvpa.dataset

import scalafim.dataset.{
  DataSelection,
  DatasetId,
  DatasetShape,
  FmriDataset,
  FmriSeries,
  DatasetSeriesReader,
  RunId,
  SynchronousFmriDataset,
  TimepointIndex,
  VoxelIndex
}
import scalafim.fmri.mvpa.{AxisDigest, AxisRef, Column, EvidenceSource, FeatureIndex, Observations, ClassLabel, SampleIndex, PatternMatrix}
import gale.linalg.{DMat, Matrix}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

import scala.util.control.NonFatal

enum MvpaDatasetErrorCategory:
  case Metadata
  case SampleOrigin
  case FeatureMapping
  case PatternData
  case DatasetRead
  case Labels
  case Identifier

enum MvpaDatasetError:
  case EmptySampleTable
  case SampleCountMismatch(expected: Int, actual: Int)
  case SampleIndexMismatch(position: Int, actual: Int)
  case TimepointMismatch(position: Int, expected: Int, actual: Int)
  case MetadataLengthMismatch(column: String, expected: Int, actual: Int)
  case PatternRowCountMismatch(expected: Int, actual: Int)
  case PatternFeatureCountMismatch(expected: Int, actual: Int)
  case InvalidSampleOrigin(detail: String)
  case MissingCategoricalLabels
  case InvalidId(kind: String, value: String, detail: String)
  case InvalidFeatureMapping(detail: String)
  case InvalidFeatureSpace(detail: String)
  case DatasetReadFailed(dataset: String, detail: String)

  def category: MvpaDatasetErrorCategory =
    this match
      case EmptySampleTable | MetadataLengthMismatch(_, _, _) =>
        MvpaDatasetErrorCategory.Metadata
      case SampleCountMismatch(_, _) | SampleIndexMismatch(_, _) | TimepointMismatch(_, _, _) | InvalidSampleOrigin(_) =>
        MvpaDatasetErrorCategory.SampleOrigin
      case PatternRowCountMismatch(_, _) | PatternFeatureCountMismatch(_, _) =>
        MvpaDatasetErrorCategory.PatternData
      case MissingCategoricalLabels => MvpaDatasetErrorCategory.Labels
      case InvalidId(_, _, _) =>
        MvpaDatasetErrorCategory.Identifier
      case InvalidFeatureMapping(_) | InvalidFeatureSpace(_) =>
        MvpaDatasetErrorCategory.FeatureMapping
      case DatasetReadFailed(_, _) =>
        MvpaDatasetErrorCategory.DatasetRead

  def message: String =
    this match
      case EmptySampleTable =>
        "sample table must contain at least one row"
      case SampleCountMismatch(expected, actual) =>
        s"sample count mismatch: expected $expected, got $actual"
      case SampleIndexMismatch(position, actual) =>
        s"sample row $position has sample index $actual; row-aligned tables require index == row position"
      case TimepointMismatch(position, expected, actual) =>
        s"sample row $position has timepoint $actual, expected $expected"
      case MetadataLengthMismatch(column, expected, actual) =>
        s"$column metadata length mismatch: expected $expected, got $actual"
      case PatternRowCountMismatch(expected, actual) =>
        s"pattern row count mismatch: expected $expected, got $actual"
      case PatternFeatureCountMismatch(expected, actual) =>
        s"pattern feature count mismatch: expected $expected, got $actual"
      case InvalidSampleOrigin(detail) =>
        detail
      case MissingCategoricalLabels =>
        "categorical labels must be present for every dataset observation"
      case InvalidId(kind, value, detail) =>
        s"invalid $kind id '$value': $detail"
      case InvalidFeatureMapping(detail) =>
        detail
      case InvalidFeatureSpace(detail) =>
        detail
      case DatasetReadFailed(dataset, detail) =>
        s"could not read dataset '$dataset': $detail"

opaque type FeatureSpaceId = String

object FeatureSpaceId:
  def apply(value: String): Either[MvpaDatasetError, FeatureSpaceId] =
    checkedId("feature space", value)

  def unsafe(value: String): FeatureSpaceId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: FeatureSpaceId)
    inline def value: String = id

opaque type ItemId = String

object ItemId:
  def apply(value: String): Either[MvpaDatasetError, ItemId] =
    checkedId("item", value)

  def unsafe(value: String): ItemId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ItemId)
    inline def value: String = id

opaque type BlockId = String

object BlockId:
  def apply(value: String): Either[MvpaDatasetError, BlockId] =
    checkedId("block", value)

  def unsafe(value: String): BlockId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: BlockId)
    inline def value: String = id

opaque type EstimateId = String

object EstimateId:
  def apply(value: String): Either[MvpaDatasetError, EstimateId] =
    checkedId("estimate", value)

  def unsafe(value: String): EstimateId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: EstimateId)
    inline def value: String = id

opaque type SampleRowIndex = Int

object SampleRowIndex:
  def apply(value: Int): Either[MvpaDatasetError, SampleRowIndex] =
    if value < 0 then Left(MvpaDatasetError.InvalidId("sample row", value.toString, "must be non-negative"))
    else Right(value)

  def unsafe(value: Int): SampleRowIndex =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (index: SampleRowIndex)
    inline def value: Int = index

enum FeatureMapping:
  case VoxelBacked(
      mappingId: FeatureSpaceId,
      mappingDatasetId: Option[DatasetId],
      datasetShape: DatasetShape,
      voxelIndexValues: Vector[VoxelIndex],
      mappedFeatureIndices: Vector[FeatureIndex]
  )
  case Abstract(
      mappingId: FeatureSpaceId,
      mappingDatasetId: Option[DatasetId],
      mappedFeatureIndices: Vector[FeatureIndex]
  )

  def id: FeatureSpaceId =
    this match
      case VoxelBacked(mappingId, _, _, _, _) => mappingId
      case Abstract(mappingId, _, _) => mappingId

  def datasetId: Option[DatasetId] =
    this match
      case VoxelBacked(_, mappingDatasetId, _, _, _) => mappingDatasetId
      case Abstract(_, mappingDatasetId, _) => mappingDatasetId

  def shape: Option[DatasetShape] =
    this match
      case VoxelBacked(_, _, datasetShape, _, _) => Some(datasetShape)
      case Abstract(_, _, _) => None

  def featureIndices: Vector[FeatureIndex] =
    this match
      case VoxelBacked(_, _, _, _, mappedFeatureIndices) => mappedFeatureIndices
      case Abstract(_, _, mappedFeatureIndices) => mappedFeatureIndices

  def voxelIndices: Option[Vector[VoxelIndex]] =
    this match
      case VoxelBacked(_, _, _, voxelIndexValues, _) => Some(voxelIndexValues)
      case Abstract(_, _, _) => None

  def features: Int =
    featureIndices.length

  def isVoxelBacked: Boolean =
    voxelIndices.nonEmpty

object FeatureMapping:
  def fromSeries(
      series: FmriSeries,
      id: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, FeatureMapping] =
    voxelBacked(
      voxelIndices = series.voxelIndexValues,
      id = id,
      datasetId = datasetId,
      shape = series.shape
    )

  def voxelBacked(
      voxelIndices: Seq[VoxelIndex],
      id: FeatureSpaceId,
      datasetId: Option[DatasetId],
      shape: DatasetShape
  ): Either[MvpaDatasetError, FeatureMapping] =
    val voxels = voxelIndices.toVector
    validateFeatureValues(voxels.map(_.value), Some(shape)).map { values =>
      FeatureMapping.VoxelBacked(
        mappingId = id,
        mappingDatasetId = datasetId,
        datasetShape = shape,
        voxelIndexValues = values.map(VoxelIndex.unsafe),
        mappedFeatureIndices = values.map(FeatureIndex.apply)
      )
    }

  def voxelBackedFromInts(
      voxelIndices: Seq[Int],
      shape: DatasetShape,
      id: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, FeatureMapping] =
    validateFeatureValues(voxelIndices.toVector, Some(shape)).map { values =>
      FeatureMapping.VoxelBacked(
        mappingId = id,
        mappingDatasetId = datasetId,
        datasetShape = shape,
        voxelIndexValues = values.map(VoxelIndex.unsafe),
        mappedFeatureIndices = values.map(FeatureIndex.apply)
      )
    }

  def abstractFeatures(
      featureIndices: Seq[Int],
      id: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, FeatureMapping] =
    validateFeatureValues(featureIndices.toVector, None).map { values =>
      FeatureMapping.Abstract(
        mappingId = id,
        mappingDatasetId = datasetId,
        mappedFeatureIndices = values.map(FeatureIndex.apply)
      )
    }

  def fromFeatureValues(
      values: Seq[Int],
      id: FeatureSpaceId,
      datasetId: Option[DatasetId],
      shape: Option[DatasetShape]
  ): Either[MvpaDatasetError, FeatureMapping] =
    shape match
      case Some(datasetShape) =>
        voxelBackedFromInts(values, datasetShape, id, datasetId)
      case None =>
        abstractFeatures(values, id, datasetId)

  private def validateFeatureValues(
      values: Vector[Int],
      shape: Option[DatasetShape]
  ): Either[MvpaDatasetError, Vector[Int]] =
    if values.isEmpty then Left(MvpaDatasetError.InvalidFeatureMapping("feature mapping must contain at least one feature"))
    else if values.distinct.length != values.length then
      Left(MvpaDatasetError.InvalidFeatureMapping("feature mapping indices must be unique"))
    else
      values.find(_ < 0) match
        case Some(bad) =>
          Left(MvpaDatasetError.InvalidFeatureMapping(s"feature index $bad must be non-negative"))
        case None =>
          shape
            .flatMap(datasetShape => values.find(_ >= datasetShape.spatialSize).map(_ -> datasetShape.spatialSize))
            .map { case (bad, spatialSize) =>
              Left(MvpaDatasetError.InvalidFeatureMapping(s"voxel index $bad out of bounds for spatial size $spatialSize"))
            }
            .getOrElse(Right(values))

final case class FeatureSpaceRef private (private[dataset] val mapping: FeatureMapping):
  def id: FeatureSpaceId =
    mapping.id

  def datasetId: Option[DatasetId] =
    mapping.datasetId

  def shape: Option[DatasetShape] =
    mapping.shape

  def voxelIndices: Vector[Int] =
    mapping.voxelIndices match
      case Some(voxels) => voxels.map(_.value)
      case None => mapping.featureIndices.map(_.value)

  def featureIndices: Vector[FeatureIndex] =
    mapping.featureIndices

  def features: Int =
    mapping.features

  def isVoxelBacked: Boolean =
    mapping.isVoxelBacked

object FeatureSpaceRef:
  def fromMapping(mapping: FeatureMapping): FeatureSpaceRef =
    new FeatureSpaceRef(mapping)

  def fromSeries(
      series: FmriSeries,
      id: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, FeatureSpaceRef] =
    FeatureMapping.fromSeries(series, id, datasetId).map(fromMapping)

  def fromFeatures(
      voxelIndices: Seq[Int],
      id: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, FeatureSpaceRef] =
    FeatureMapping
      .fromFeatureValues(voxelIndices, id, datasetId, shape)
      .left
      .map {
        case MvpaDatasetError.InvalidFeatureMapping(detail) => MvpaDatasetError.InvalidFeatureSpace(detail)
        case error => error
      }
      .map(fromMapping)

enum SampleOrigin:
  case Timepoint(index: TimepointIndex)
  case Estimate(id: EstimateId, originRowIndex: SampleRowIndex)
  case Row(originRowIndex: SampleRowIndex)

  def sampleIndex: Int =
    this match
      case Timepoint(index) => index.value
      case Estimate(_, originRowIndex) => originRowIndex.value
      case Row(originRowIndex) => originRowIndex.value

  def timepoint: Option[Int] =
    this match
      case Timepoint(index) => Some(index.value)
      case Estimate(_, _) | Row(_) => None

  def estimateId: Option[EstimateId] =
    this match
      case Estimate(id, _) => Some(id)
      case Timepoint(_) | Row(_) => None

  def rowIndex: Option[Int] =
    this match
      case Estimate(_, originRowIndex) => Some(originRowIndex.value)
      case Row(originRowIndex) => Some(originRowIndex.value)
      case Timepoint(_) => None

object SampleOrigin:
  def fromTimepointIndex(index: TimepointIndex): Either[MvpaDatasetError, SampleOrigin] =
    Right(SampleOrigin.Timepoint(index))

  def timepoint(index: Int): Either[MvpaDatasetError, SampleOrigin] =
    TimepointIndex
      .make(index)
      .left
      .map(error => MvpaDatasetError.InvalidSampleOrigin(error.message))
      .map(SampleOrigin.Timepoint.apply)

  def fromEstimateId(id: EstimateId, rowIndex: SampleRowIndex): Either[MvpaDatasetError, SampleOrigin] =
    Right(SampleOrigin.Estimate(id, rowIndex))

  def estimate(name: String, rowIndex: Int): Either[MvpaDatasetError, SampleOrigin] =
    for
      id <- EstimateId(name).left.map(error => MvpaDatasetError.InvalidSampleOrigin(error.message))
      row <- SampleRowIndex(rowIndex).left.map(error => MvpaDatasetError.InvalidSampleOrigin(error.message))
    yield SampleOrigin.Estimate(id, row)

  def fromRowIndex(rowIndex: SampleRowIndex): Either[MvpaDatasetError, SampleOrigin] =
    Right(SampleOrigin.Row(rowIndex))

  def row(rowIndex: Int): Either[MvpaDatasetError, SampleOrigin] =
    SampleRowIndex(rowIndex)
      .left
      .map(error => MvpaDatasetError.InvalidSampleOrigin(error.message))
      .map(SampleOrigin.Row.apply)

final case class SampleMetadataRow(
    label: Option[ClassLabel],
    block: Option[BlockId],
    run: Option[RunId],
    item: Option[ItemId]
)

final case class SampleMetadata private (
    labels: Vector[Option[ClassLabel]],
    blocks: Vector[Option[BlockId]],
    runs: Vector[Option[RunId]],
    items: Vector[Option[ItemId]]
):
  require(blocks.length == labels.length, "block metadata length must match labels")
  require(runs.length == labels.length, "run metadata length must match labels")
  require(items.length == labels.length, "item metadata length must match labels")

  def size: Int =
    labels.length

  def row(index: Int): SampleMetadataRow =
    require(index >= 0 && index < size, "metadata row index out of bounds")
    SampleMetadataRow(labels(index), blocks(index), runs(index), items(index))


object SampleMetadata:
  def empty(size: Int): Either[MvpaDatasetError, SampleMetadata] =
    if size <= 0 then Left(MvpaDatasetError.EmptySampleTable)
    else fromTypedColumns(
      labels = Vector.fill(size)(None),
      blocks = Vector.fill(size)(None),
      runs = Vector.fill(size)(None),
      items = Vector.fill(size)(None)
    )

  def fromColumns(
      size: Int,
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): Either[MvpaDatasetError, SampleMetadata] =
    if size <= 0 then Left(MvpaDatasetError.EmptySampleTable)
    else
      for
        parsedLabels <- parseColumn("label", size, labels)(parseClassLabel)
        parsedBlocks <- parseColumn("block", size, blocks)(BlockId.apply)
        parsedRuns <- parseColumn("run", size, runs)(parseRunId)
        parsedItems <- parseColumn("item", size, items)(ItemId.apply)
        metadata <- fromTypedColumns(parsedLabels, parsedBlocks, parsedRuns, parsedItems)
      yield metadata

  def fromTypedColumns(
      labels: Vector[Option[ClassLabel]],
      blocks: Vector[Option[BlockId]],
      runs: Vector[Option[RunId]],
      items: Vector[Option[ItemId]]
  ): Either[MvpaDatasetError, SampleMetadata] =
    val size = labels.length
    if size <= 0 then Left(MvpaDatasetError.EmptySampleTable)
    else if blocks.length != size then Left(MvpaDatasetError.MetadataLengthMismatch("block", size, blocks.length))
    else if runs.length != size then Left(MvpaDatasetError.MetadataLengthMismatch("run", size, runs.length))
    else if items.length != size then Left(MvpaDatasetError.MetadataLengthMismatch("item", size, items.length))
    else Right(new SampleMetadata(labels, blocks, runs, items))

  def fromRecords(records: Seq[SampleRecord]): Either[MvpaDatasetError, SampleMetadata] =
    val rows = records.toVector
    if rows.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else
      fromTypedColumns(
        labels = rows.map(_.label),
        blocks = rows.map(_.block),
        runs = rows.map(_.run),
        items = rows.map(_.item)
      )

  private def parseColumn[A](
      name: String,
      size: Int,
      values: Option[Seq[String]]
  )(parse: String => Either[MvpaDatasetError, A]): Either[MvpaDatasetError, Vector[Option[A]]] =
    values match
      case None =>
        Right(Vector.fill(size)(None))
      case Some(raw) =>
        val rawVector = raw.toVector
        if rawVector.length != size then Left(MvpaDatasetError.MetadataLengthMismatch(name, size, rawVector.length))
        else
          val out = Vector.newBuilder[Option[A]]
          out.sizeHint(size)
          var i = 0
          while i < rawVector.length do
            parse(rawVector(i)) match
              case Right(value) =>
                out += Some(value)
              case Left(error) =>
                return Left(error)
            i += 1
          Right(out.result())

final class SampleMetadataRequest private (
    val labels: Option[Vector[String]],
    val blocks: Option[Vector[String]],
    val runs: Option[Vector[String]],
    val items: Option[Vector[String]]
):
  def resolve(size: Int): Either[MvpaDatasetError, SampleMetadata] =
    SampleMetadata.fromColumns(size, labels, blocks, runs, items)

object SampleMetadataRequest:
  val Empty: SampleMetadataRequest =
    apply()

  def apply(
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): SampleMetadataRequest =
    new SampleMetadataRequest(
      labels = labels.map(_.toVector),
      blocks = blocks.map(_.toVector),
      runs = runs.map(_.toVector),
      items = items.map(_.toVector)
    )

  def labeled(
      labels: Seq[String],
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): SampleMetadataRequest =
    apply(labels = Some(labels), blocks = blocks, runs = runs, items = items)

final case class DatasetPatternRequest(
    selection: DataSelection = DataSelection.All,
    metadata: SampleMetadataRequest = SampleMetadataRequest.Empty,
    featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels")
)

final case class SampleRecord private (
    index: SampleIndex,
    origin: SampleOrigin,
    metadata: SampleMetadataRow
):
  def rowOrdinal: Int =
    index.value

  def timepoint: Option[Int] =
    origin.timepoint

  def run: Option[RunId] =
    metadata.run

  def block: Option[BlockId] =
    metadata.block

  def item: Option[ItemId] =
    metadata.item

  def label: Option[ClassLabel] =
    metadata.label

object SampleRecord:
  def build(
      index: Int,
      origin: SampleOrigin,
      run: Option[RunId] = None,
      block: Option[BlockId] = None,
      item: Option[ItemId] = None,
      label: Option[ClassLabel] = None
  ): Either[MvpaDatasetError, SampleRecord] =
    build(index, origin, SampleMetadataRow(label = label, block = block, run = run, item = item))

  def build(
      index: Int,
      origin: SampleOrigin,
      metadata: SampleMetadataRow
  ): Either[MvpaDatasetError, SampleRecord] =
    if index < 0 then Left(MvpaDatasetError.InvalidId("sample", index.toString, "must be non-negative"))
    else
      Right(
        new SampleRecord(
          index = SampleIndex.unsafe(index),
          origin = origin,
          metadata = metadata
        )
      )

  def unsafe(
      index: Int,
      timepoint: Int,
      run: Option[RunId] = None,
      block: Option[BlockId] = None,
      item: Option[ItemId] = None,
      label: Option[ClassLabel] = None
  ): SampleRecord =
    SampleOrigin
      .timepoint(timepoint)
      .flatMap(origin => build(index, origin, run, block, item, label))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeEstimate(
      index: Int,
      name: String,
      run: Option[RunId] = None,
      block: Option[BlockId] = None,
      item: Option[ItemId] = None,
      label: Option[ClassLabel] = None
  ): SampleRecord =
    SampleOrigin
      .estimate(name, index)
      .flatMap(origin => build(index, origin, run, block, item, label))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SampleTable private (
    rows: Vector[SampleRecord],
    metadata: SampleMetadata
):
  require(rows.nonEmpty, "sample table must contain at least one row")
  require(rows.length == metadata.size, "sample metadata length must match rows")

  def size: Int =
    rows.length


  def validateAgainst(series: FmriSeries): Either[MvpaDatasetError, Unit] =
    if size != series.nTimepoints then Left(MvpaDatasetError.SampleCountMismatch(series.nTimepoints, size))
    else
      var i = 0
      while i < rows.length do
        val expected = series.timepoints(i)
        rows(i).timepoint match
          case Some(actual) =>
            if expected != actual then return Left(MvpaDatasetError.TimepointMismatch(i, expected, actual))
          case None =>
            return Left(MvpaDatasetError.InvalidSampleOrigin(s"sample row $i is not a timepoint-origin sample"))
        i += 1
      Right(())

object SampleTable:
  def build(rows: Seq[SampleRecord]): Either[MvpaDatasetError, SampleTable] =
    val rowVector = rows.toVector
    if rowVector.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else
      var i = 0
      while i < rowVector.length do
        val actual = rowVector(i).index.value
        if actual != i then return Left(MvpaDatasetError.SampleIndexMismatch(i, actual))
        i += 1
      SampleMetadata.fromRecords(rowVector).map(metadata => new SampleTable(rowVector, metadata))

  def fromSeries(
      series: FmriSeries,
      metadata: SampleMetadataRequest
  ): Either[MvpaDatasetError, SampleTable] =
    for
      resolved <- metadata.resolve(series.nTimepoints)
      records <- recordsFrom(series, resolved)
      table <- build(records, resolved)
      _ <- table.validateAgainst(series)
    yield table

  def fromSeries(
      series: FmriSeries,
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): Either[MvpaDatasetError, SampleTable] =
    fromSeries(series, SampleMetadataRequest(labels = labels, blocks = blocks, runs = runs, items = items))

  def fromRows(
      rowNames: Seq[String],
      metadata: SampleMetadataRequest
  ): Either[MvpaDatasetError, SampleTable] =
    val names = rowNames.toVector.map(_.trim)
    val size = names.length
    if names.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else if names.exists(_.isEmpty) then Left(MvpaDatasetError.InvalidSampleOrigin("row names must be non-empty"))
    else
      val effectiveMetadata =
        if metadata.items.isEmpty then
          SampleMetadataRequest(
            labels = metadata.labels,
            blocks = metadata.blocks,
            runs = metadata.runs,
            items = Some(names)
          )
        else metadata
      for
        resolved <- effectiveMetadata.resolve(size)
        records <- recordsFromRows(names, resolved)
        table <- build(records, resolved)
      yield table

  def fromRows(
      rowNames: Seq[String],
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): Either[MvpaDatasetError, SampleTable] =
    fromRows(rowNames, SampleMetadataRequest(labels = labels, blocks = blocks, runs = runs, items = items))

  private def build(
      rows: Vector[SampleRecord],
      metadata: SampleMetadata
  ): Either[MvpaDatasetError, SampleTable] =
    if rows.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else if rows.length != metadata.size then Left(MvpaDatasetError.SampleCountMismatch(metadata.size, rows.length))
    else
      var i = 0
      while i < rows.length do
        val actual = rows(i).index.value
        if actual != i then return Left(MvpaDatasetError.SampleIndexMismatch(i, actual))
        i += 1
      Right(new SampleTable(rows, metadata))

  private def recordsFrom(
      series: FmriSeries,
      metadata: SampleMetadata
  ): Either[MvpaDatasetError, Vector[SampleRecord]] =
    val out = Vector.newBuilder[SampleRecord]
    out.sizeHint(series.nTimepoints)
    var row = 0
    while row < series.nTimepoints do
      SampleOrigin.fromTimepointIndex(series.timepointIndices(row)).flatMap { origin =>
        SampleRecord.build(row, origin, metadata.row(row))
      } match
        case Right(record) =>
          out += record
        case Left(error) =>
          return Left(error)
      row += 1
    Right(out.result())

  private def recordsFromRows(
      rowNames: Vector[String],
      metadata: SampleMetadata
  ): Either[MvpaDatasetError, Vector[SampleRecord]] =
    val out = Vector.newBuilder[SampleRecord]
    out.sizeHint(rowNames.length)
    var row = 0
    while row < rowNames.length do
      SampleOrigin.estimate(rowNames(row), row).flatMap { origin =>
        SampleRecord.build(row, origin, metadata.row(row))
      } match
        case Right(record) =>
          out += record
        case Left(error) =>
          return Left(error)
      row += 1
    Right(out.result())

final case class PatternTable private (
    value: DMat,
    featureMapping: FeatureMapping
):
  require(value.rows > 0, "pattern table must contain at least one row")
  require(value.cols == featureMapping.features, "pattern table columns must match feature mapping")

  def rows: Int =
    value.rows

  def features: Int =
    value.cols

  def featureSpace: FeatureSpaceRef =
    FeatureSpaceRef.fromMapping(featureMapping)


object PatternTable:
  def build(value: DMat, featureMapping: FeatureMapping): Either[MvpaDatasetError, PatternTable] =
    if value.rows <= 0 then Left(MvpaDatasetError.EmptySampleTable)
    else if value.cols != featureMapping.features then
      Left(MvpaDatasetError.PatternFeatureCountMismatch(featureMapping.features, value.cols))
    else Right(new PatternTable(value, featureMapping))

  def build(value: DMat, featureSpace: FeatureSpaceRef): Either[MvpaDatasetError, PatternTable] =
    build(value, featureSpace.mapping)

  def fromMatrix(
      value: DMat,
      featureMapping: FeatureMapping
  ): Either[MvpaDatasetError, PatternTable] =
    build(value, featureMapping)

  def fromMatrix(
      value: DMat,
      voxelIndices: Seq[Int],
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, PatternTable] =
    for
      mapping <- FeatureMapping.fromFeatureValues(
        values = voxelIndices,
        id = featureSpaceId,
        datasetId = datasetId,
        shape = shape
      )
      table <- build(value, mapping)
    yield table

  def fromRows(
      rows: Seq[Seq[Double]],
      voxelIndices: Seq[Int],
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, PatternTable] =
    val rowVector = rows.toVector.map(_.toVector)
    if rowVector.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else
      val width = rowVector.head.length
      rowVector.collectFirst { case row if row.length != width => row.length } match
        case Some(actual) =>
          Left(MvpaDatasetError.PatternFeatureCountMismatch(width, actual))
        case None =>
          val builder = Matrix.newBuilder(rowVector.length, width)
          var row = 0
          while row < rowVector.length do
            var col = 0
            while col < width do
              builder(row, col) = rowVector(row)(col)
              col += 1
            row += 1
          fromMatrix(builder.result(), voxelIndices, featureSpaceId, datasetId, shape)

  def fromSeries(
      series: FmriSeries,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, PatternTable] =
    for
      mapping <- FeatureMapping.fromSeries(series, featureSpaceId, datasetId)
      table <- build(matrixFromSeries(series), mapping)
    yield table

  private def matrixFromSeries(series: FmriSeries): DMat =
    val out = Matrix.newBuilder(series.nTimepoints, series.nVoxels)
    var row = 0
    while row < series.nTimepoints do
      var col = 0
      while col < series.nVoxels do
        out(row, col) = series.data(row, col)
        col += 1
      row += 1
    out.result()


final class DatasetObservationEvidence private (
    val samples: AxisRef[String],
    val neural: AxisRef[String],
    val observations: Observations[samples.Id, neural.Id],
    val metadata: SampleTable,
    val featureMapping: FeatureMapping,
    private val labels: Option[Column[samples.Id, ClassLabel]],
    val sourceIdentity: String,
    val contentIdentity: String
):
  def featureSpace: FeatureSpaceRef =
    FeatureSpaceRef.fromMapping(featureMapping)

  def categoricalLabels: Either[MvpaDatasetError, Column[samples.Id, ClassLabel]] =
    labels.toRight(MvpaDatasetError.MissingCategoricalLabels)

object DatasetObservationEvidence:
  private def error(value: Any): MvpaDatasetError =
    MvpaDatasetError.InvalidSampleOrigin(value.toString)

  def build(
      table: PatternTable,
      sampleTable: SampleTable
  ): Either[MvpaDatasetError, DatasetObservationEvidence] =
    if table.rows != sampleTable.size then
      Left(MvpaDatasetError.PatternRowCountMismatch(sampleTable.size, table.rows))
    else
      val sampleKeys = sampleTable.rows.map(row => s"dataset-row:${row.rowOrdinal}")
      val neuralKeys = table.featureMapping.featureIndices.map(index => s"feature:${index.value}")
      for
        samples <- AxisRef
          .fromStableKeys(
            "dataset-observations",
            SpaceRole.Samples,
            sampleKeys,
            "dataset",
            "one",
            "raw"
          )
          .left
          .map(error)
        neural <- AxisRef
          .fromStableKeys(
            "dataset-features",
            SpaceRole.Observed,
            neuralKeys,
            table.featureMapping.id.value,
            "one",
            "raw"
          )
          .left
          .map(error)
        identity = EvidenceIdentity.content(table, sampleTable, sampleKeys, neuralKeys)
        sourceId = SourceId.unsafe(s"dataset-observations:$identity")
        source <- EvidenceSource(
          sourceId,
          Provenance.source(
            ProvenanceId.unsafe(s"dataset-observations-root:$identity"),
            sourceId
          )
        ).left.map(error)
        observations <- Observations
          .fromDense(
            samples,
            neural,
            table.value,
            ValueIdentity.source(ValueId.unsafe(identity)),
            source
          )
          .left
          .map(error)
        labelColumn <- labelsFor(samples, sampleTable, identity)
      yield new DatasetObservationEvidence(
        samples,
        neural,
        observations,
        sampleTable,
        table.featureMapping,
        labelColumn,
        sourceId.value,
        identity
      )

  private def labelsFor(
      samples: AxisRef[String],
      table: SampleTable,
      evidenceIdentity: String
  ): Either[MvpaDatasetError, Option[Column[samples.Id, ClassLabel]]] =
    val values = table.rows.map(_.label)
    if values.forall(_.isEmpty) then Right(None)
    else if values.exists(_.isEmpty) then Left(MvpaDatasetError.MissingCategoricalLabels)
    else
      val labelIdentity = EvidenceIdentity.labels(evidenceIdentity, values.flatten)
      Column
        .fromValues(
          samples,
          values.flatten,
          ValueIdentity.source(ValueId.unsafe(labelIdentity))
        )
        .left
        .map(error)
        .map(Some.apply)

  def fromSeries(
      series: FmriSeries,
      metadata: SampleMetadataRequest,
      featureSpaceId: FeatureSpaceId,
      datasetId: Option[DatasetId]
  ): Either[MvpaDatasetError, DatasetObservationEvidence] =
    for
      samples <- SampleTable.fromSeries(series, metadata)
      _ <- samples.validateAgainst(series)
      patterns <- PatternTable.fromSeries(series, featureSpaceId, datasetId)
      evidence <- build(patterns, samples)
    yield evidence

  def fromReader(
      reader: DatasetSeriesReader,
      request: DatasetPatternRequest
  ): Either[MvpaDatasetError, DatasetObservationEvidence] =
    reader
      .seriesEither(request.selection)
      .left
      .map(error => MvpaDatasetError.DatasetReadFailed(reader.dataset.id.value, error.message))
      .flatMap(series => fromSeries(series, request.metadata, request.featureSpaceId, Some(reader.dataset.id)))

  def fromDataset(
      dataset: FmriDataset,
      request: DatasetPatternRequest
  ): Either[MvpaDatasetError, DatasetObservationEvidence] =
    SynchronousFmriDataset
      .readerFor(dataset)
      .left
      .map(error => MvpaDatasetError.DatasetReadFailed(dataset.id.value, error.message))
      .flatMap(reader => fromReader(reader, request))

  def fromPatternRows(
      rows: Seq[Seq[Double]],
      rowNames: Seq[String],
      voxelIndices: Seq[Int],
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, DatasetObservationEvidence] =
    for
      patterns <- PatternTable.fromRows(rows, voxelIndices, featureSpaceId, datasetId, shape)
      samples <- SampleTable.fromRows(
        rowNames,
        SampleMetadataRequest(
          labels = labels,
          blocks = blocks,
          runs = runs,
          items = items
        )
      )
      evidence <- build(patterns, samples)
    yield evidence

private object EvidenceIdentity:
  def content(
      table: PatternTable,
      samples: SampleTable,
      sampleKeys: Vector[String],
      neuralKeys: Vector[String]
  ): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.dataset-observation-evidence.v2")
      writeMapping(writer, table.featureMapping)
      writer.intLE(sampleKeys.length)
      sampleKeys.foreach(writer.string)
      writer.intLE(neuralKeys.length)
      neuralKeys.foreach(writer.string)
      writer.intLE(samples.rows.length)
      samples.rows.foreach(writeSample(writer, _))
      writer.intLE(table.value.rows)
      writer.intLE(table.value.cols)
      var row = 0
      while row < table.value.rows do
        var column = 0
        while column < table.value.cols do
          writer.string(java.lang.Double.toHexString(table.value(row, column)))
          column += 1
        row += 1

  def labels(evidenceIdentity: String, labels: Vector[ClassLabel]): String =
    AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.dataset-observation-labels.v1")
      writer.string(evidenceIdentity)
      writer.intLE(labels.length)
      labels.foreach(label => writer.string(label.value))

  private def writeMapping(
      writer: AxisDigest.Writer,
      mapping: FeatureMapping
  ): Unit =
    writer.string(mapping.id.value)
    writeOption(writer, mapping.datasetId.map(_.value))
    mapping match
      case FeatureMapping.VoxelBacked(_, _, shape, voxels, features) =>
        writer.string("voxel-backed")
        writer.intLE(shape.timepoints)
        writer.intLE(shape.spatialDims.length)
        shape.spatialDims.foreach(writer.intLE)
        writer.intLE(voxels.length)
        voxels.foreach(voxel => writer.intLE(voxel.value))
        writer.intLE(features.length)
        features.foreach(feature => writer.intLE(feature.value))
      case FeatureMapping.Abstract(_, _, features) =>
        writer.string("abstract")
        writer.intLE(features.length)
        features.foreach(feature => writer.intLE(feature.value))

  private def writeSample(
      writer: AxisDigest.Writer,
      sample: SampleRecord
  ): Unit =
    writer.intLE(sample.rowOrdinal)
    sample.origin match
      case SampleOrigin.Timepoint(index) =>
        writer.string("timepoint")
        writer.intLE(index.value)
      case SampleOrigin.Estimate(id, row) =>
        writer.string("estimate")
        writer.string(id.value)
        writer.intLE(row.value)
      case SampleOrigin.Row(row) =>
        writer.string("row")
        writer.intLE(row.value)
    writeOption(writer, sample.label.map(_.value))
    writeOption(writer, sample.block.map(_.value))
    writeOption(writer, sample.run.map(_.value))
    writeOption(writer, sample.item.map(_.value))

  private def writeOption(
      writer: AxisDigest.Writer,
      value: Option[String]
  ): Unit =
    value match
      case Some(actual) =>
        writer.intLE(1)
        writer.string(actual)
      case None =>
        writer.intLE(0)

private def checkedId(kind: String, value: String): Either[MvpaDatasetError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(MvpaDatasetError.InvalidId(kind, value, "must be non-empty")) else Right(trimmed)

private def parseClassLabel(value: String): Either[MvpaDatasetError, ClassLabel] =
  try Right(ClassLabel(value))
  catch case NonFatal(error) => Left(MvpaDatasetError.InvalidId("class label", value, error.getMessage))

private def parseRunId(value: String): Either[MvpaDatasetError, RunId] =
  try Right(RunId(value))
  catch case NonFatal(error) => Left(MvpaDatasetError.InvalidId("run", value, error.getMessage))
