package scalafim.fmri.mvpa.dataset

import scalafim.dataset.{DataSelection, DatasetId, DatasetShape, FmriDataset, FmriSeries, RunId}
import scalafim.fmri.mvpa.*
import scalafim.linalg.DoubleMatrix

import scala.util.control.NonFatal

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
  case MissingFoldLabels(kind: String)
  case InvalidId(kind: String, value: String, detail: String)
  case InvalidResponse(detail: String)
  case InvalidFoldPlan(detail: String)
  case InvalidFeatureSpace(detail: String)
  case DatasetReadFailed(dataset: String, detail: String)

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
        "categorical response requires a label on every sample row"
      case MissingFoldLabels(kind) =>
        s"$kind fold plan requires a $kind label on every sample row"
      case InvalidId(kind, value, detail) =>
        s"invalid $kind id '$value': $detail"
      case InvalidResponse(detail) =>
        s"invalid MVPA response: $detail"
      case InvalidFoldPlan(detail) =>
        s"invalid MVPA fold plan: $detail"
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

final case class FeatureSpaceRef private (
    id: FeatureSpaceId,
    datasetId: Option[DatasetId],
    shape: Option[DatasetShape],
    voxelIndices: Vector[Int],
    featureIndices: Vector[FeatureIndex]
):
  require(voxelIndices.nonEmpty, "feature space must contain at least one voxel")
  require(voxelIndices.length == featureIndices.length, "voxel/feature index length mismatch")

  def features: Int =
    voxelIndices.length

object FeatureSpaceRef:
  def fromSeries(
      series: FmriSeries,
      id: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, FeatureSpaceRef] =
    fromFeatures(
      voxelIndices = series.voxelIndices,
      id = id,
      datasetId = datasetId,
      shape = Some(series.shape)
    )

  def fromFeatures(
      voxelIndices: Seq[Int],
      id: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, FeatureSpaceRef] =
    val voxels = voxelIndices.toVector
    if voxels.isEmpty then Left(MvpaDatasetError.InvalidFeatureSpace("feature space must contain at least one voxel"))
    else if voxels.distinct.length != voxels.length then
      Left(MvpaDatasetError.InvalidFeatureSpace("feature space voxel indices must be unique"))
    else if voxels.exists(_ < 0) then
      val bad = voxels.find(_ < 0).get
      Left(MvpaDatasetError.InvalidFeatureSpace(s"voxel index $bad must be non-negative"))
    else
      shape
        .flatMap(datasetShape => voxels.find(_ >= datasetShape.spatialSize).map(_ -> datasetShape.spatialSize))
        .map { case (bad, spatialSize) =>
          Left(MvpaDatasetError.InvalidFeatureSpace(s"voxel index $bad out of bounds for spatial size $spatialSize"))
        }
        .getOrElse(
          Right(
            new FeatureSpaceRef(
              id = id,
              datasetId = datasetId,
              shape = shape,
              voxelIndices = voxels,
              featureIndices = voxels.map(FeatureIndex.apply)
            )
          )
        )

enum SampleOrigin:
  case Timepoint(index: Int)
  case Estimate(name: String, rowIndex: Int)
  case Row(rowIndex: Int)

  def sampleIndex: Int =
    this match
      case Timepoint(index) => index
      case Estimate(_, rowIndex) => rowIndex
      case Row(rowIndex) => rowIndex

  def timepoint: Option[Int] =
    this match
      case Timepoint(index) => Some(index)
      case Estimate(_, _) | Row(_) => None

  this match
    case Timepoint(index) =>
      require(index >= 0, "timepoint origin must be non-negative")
    case Estimate(name, rowIndex) =>
      require(name.trim.nonEmpty, "estimate origin name must be non-empty")
      require(rowIndex >= 0, "estimate origin row index must be non-negative")
    case Row(rowIndex) =>
      require(rowIndex >= 0, "row origin row index must be non-negative")

object SampleOrigin:
  def timepoint(index: Int): Either[MvpaDatasetError, SampleOrigin] =
    if index < 0 then Left(MvpaDatasetError.InvalidSampleOrigin(s"timepoint origin $index must be non-negative"))
    else Right(SampleOrigin.Timepoint(index))

  def estimate(name: String, rowIndex: Int): Either[MvpaDatasetError, SampleOrigin] =
    val trimmed = name.trim
    if trimmed.isEmpty then Left(MvpaDatasetError.InvalidSampleOrigin("estimate origin name must be non-empty"))
    else if rowIndex < 0 then Left(MvpaDatasetError.InvalidSampleOrigin(s"estimate origin row index $rowIndex must be non-negative"))
    else Right(SampleOrigin.Estimate(trimmed, rowIndex))

  def row(rowIndex: Int): Either[MvpaDatasetError, SampleOrigin] =
    if rowIndex < 0 then Left(MvpaDatasetError.InvalidSampleOrigin(s"row origin row index $rowIndex must be non-negative"))
    else Right(SampleOrigin.Row(rowIndex))

final case class SampleRecord private (
    index: SampleIndex,
    origin: SampleOrigin,
    run: Option[RunId],
    block: Option[BlockId],
    item: Option[ItemId],
    label: Option[ClassLabel]
):
  def rowOrdinal: Int =
    index.value

  def timepoint: Option[Int] =
    origin.timepoint

object SampleRecord:
  def build(
      index: Int,
      origin: SampleOrigin,
      run: Option[RunId] = None,
      block: Option[BlockId] = None,
      item: Option[ItemId] = None,
      label: Option[ClassLabel] = None
  ): Either[MvpaDatasetError, SampleRecord] =
    if index < 0 then Left(MvpaDatasetError.InvalidId("sample", index.toString, "must be non-negative"))
    else
      Right(
        new SampleRecord(
          index = SampleIndex.unsafe(index),
          origin = origin,
          run = run,
          block = block,
          item = item,
          label = label
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
    build(index, SampleOrigin.Timepoint(timepoint), run, block, item, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafeEstimate(
      index: Int,
      name: String,
      run: Option[RunId] = None,
      block: Option[BlockId] = None,
      item: Option[ItemId] = None,
      label: Option[ClassLabel] = None
  ): SampleRecord =
    build(index, SampleOrigin.Estimate(name, index), run, block, item, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class SampleTable private (rows: Vector[SampleRecord]):
  require(rows.nonEmpty, "sample table must contain at least one row")

  def size: Int =
    rows.length

  def response: Either[MvpaDatasetError, Response] =
    if rows.exists(_.label.isEmpty) then Left(MvpaDatasetError.MissingCategoricalLabels)
    else
      val labels = rows.flatMap(_.label).map(_.value)
      Response
        .categorical(labels)
        .left
        .map(error => MvpaDatasetError.InvalidResponse(error.message))

  def foldPlanByBlock: Either[MvpaDatasetError, FoldPlan] =
    foldPlan("block", rows.map(_.block.map(_.value)))

  def foldPlanByRun: Either[MvpaDatasetError, FoldPlan] =
    foldPlan("run", rows.map(_.run.map(_.value)))

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

  private def foldPlan(kind: String, values: Vector[Option[String]]): Either[MvpaDatasetError, FoldPlan] =
    if values.exists(_.isEmpty) then Left(MvpaDatasetError.MissingFoldLabels(kind))
    else
      val labels = values.flatten
      val foldLabels = labels.distinct
      val folds = foldLabels.map { label =>
        val test = labels.zipWithIndex.collect { case (value, index) if value == label => index }
        val testSet = test.toSet
        val train = labels.indices.filterNot(testSet.contains).toVector
        Fold(s"$kind:$label", train, test)
      }
      folds.collectFirst { case Left(error) => error } match
        case Some(error) =>
          Left(MvpaDatasetError.InvalidFoldPlan(error.message))
        case None =>
          FoldPlan(folds.collect { case Right(fold) => fold }, rows.length)
            .left
            .map(error => MvpaDatasetError.InvalidFoldPlan(error.message))

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
      Right(new SampleTable(rowVector))

  def fromSeries(
      series: FmriSeries,
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): Either[MvpaDatasetError, SampleTable] =
    val size = series.nTimepoints
    for
      parsedLabels <- parseColumn("label", size, labels)(parseClassLabel)
      parsedBlocks <- parseColumn("block", size, blocks)(BlockId.apply)
      parsedRuns <- parseColumn("run", size, runs)(parseRunId)
      parsedItems <- parseColumn("item", size, items)(ItemId.apply)
      records <- recordsFrom(series, parsedLabels, parsedBlocks, parsedRuns, parsedItems)
      table <- build(records)
      _ <- table.validateAgainst(series)
    yield table

  def fromRows(
      rowNames: Seq[String],
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None
  ): Either[MvpaDatasetError, SampleTable] =
    val names = rowNames.toVector.map(_.trim)
    val size = names.length
    if names.isEmpty then Left(MvpaDatasetError.EmptySampleTable)
    else if names.exists(_.isEmpty) then Left(MvpaDatasetError.InvalidSampleOrigin("row names must be non-empty"))
    else
      val effectiveItems = items.orElse(Some(names))
      for
        parsedLabels <- parseColumn("label", size, labels)(parseClassLabel)
        parsedBlocks <- parseColumn("block", size, blocks)(BlockId.apply)
        parsedRuns <- parseColumn("run", size, runs)(parseRunId)
        parsedItems <- parseColumn("item", size, effectiveItems)(ItemId.apply)
        records <- recordsFromRows(names, parsedLabels, parsedBlocks, parsedRuns, parsedItems)
        table <- build(records)
      yield table

  private def recordsFrom(
      series: FmriSeries,
      labels: Vector[Option[ClassLabel]],
      blocks: Vector[Option[BlockId]],
      runs: Vector[Option[RunId]],
      items: Vector[Option[ItemId]]
  ): Either[MvpaDatasetError, Vector[SampleRecord]] =
    val out = Vector.newBuilder[SampleRecord]
    out.sizeHint(series.nTimepoints)
    var row = 0
    while row < series.nTimepoints do
      SampleRecord.build(
        index = row,
        origin = SampleOrigin.Timepoint(series.timepoints(row)),
        run = runs(row),
        block = blocks(row),
        item = items(row),
        label = labels(row)
      ) match
        case Right(record) =>
          out += record
        case Left(error) =>
          return Left(error)
      row += 1
    Right(out.result())

  private def recordsFromRows(
      rowNames: Vector[String],
      labels: Vector[Option[ClassLabel]],
      blocks: Vector[Option[BlockId]],
      runs: Vector[Option[RunId]],
      items: Vector[Option[ItemId]]
  ): Either[MvpaDatasetError, Vector[SampleRecord]] =
    val out = Vector.newBuilder[SampleRecord]
    out.sizeHint(rowNames.length)
    var row = 0
    while row < rowNames.length do
      SampleOrigin.estimate(rowNames(row), row).flatMap { origin =>
        SampleRecord.build(
          index = row,
          origin = origin,
          run = runs(row),
          block = blocks(row),
          item = items(row),
          label = labels(row)
        )
      } match
        case Right(record) =>
          out += record
        case Left(error) =>
          return Left(error)
      row += 1
    Right(out.result())

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

final case class PatternTable private (
    value: DoubleMatrix,
    featureSpace: FeatureSpaceRef
):
  require(value.rows > 0, "pattern table must contain at least one row")
  require(value.cols == featureSpace.features, "pattern table columns must match feature space")

  def rows: Int =
    value.rows

  def features: Int =
    value.cols

  def toPatternMatrix: PatternMatrix =
    PatternMatrix(
      value = value,
      sampleIndices = Vector.tabulate(value.rows)(SampleIndex.unsafe),
      featureIndices = featureSpace.featureIndices
    )

object PatternTable:
  def build(value: DoubleMatrix, featureSpace: FeatureSpaceRef): Either[MvpaDatasetError, PatternTable] =
    if value.rows <= 0 then Left(MvpaDatasetError.EmptySampleTable)
    else if value.cols != featureSpace.features then
      Left(MvpaDatasetError.PatternFeatureCountMismatch(featureSpace.features, value.cols))
    else Right(new PatternTable(value, featureSpace))

  def fromMatrix(
      value: DoubleMatrix,
      voxelIndices: Seq[Int],
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("features"),
      datasetId: Option[DatasetId] = None,
      shape: Option[DatasetShape] = None
  ): Either[MvpaDatasetError, PatternTable] =
    for
      featureSpace <- FeatureSpaceRef.fromFeatures(
        voxelIndices = voxelIndices,
        id = featureSpaceId,
        datasetId = datasetId,
        shape = shape
      )
      table <- build(value, featureSpace)
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
          fromMatrix(DoubleMatrix.fromRows(rowVector), voxelIndices, featureSpaceId, datasetId, shape)

  def fromSeries(
      series: FmriSeries,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, PatternTable] =
    for
      featureSpace <- FeatureSpaceRef.fromSeries(series, featureSpaceId, datasetId)
      table <- build(matrixFromSeries(series), featureSpace)
    yield table

  private def matrixFromSeries(series: FmriSeries): DoubleMatrix =
    val out = new Array[Double](series.nTimepoints * series.nVoxels)
    var row = 0
    while row < series.nTimepoints do
      var col = 0
      while col < series.nVoxels do
        out(row * series.nVoxels + col) = series.data(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(series.nTimepoints, series.nVoxels, out)

final case class MvpaDatasetView private (
    patterns: PatternMatrix,
    samples: SampleTable,
    featureSpace: FeatureSpaceRef
):
  require(patterns.samples == samples.size, "pattern rows must match sample table")
  require(patterns.features == featureSpace.features, "pattern columns must match feature space")

  def source: PatternSource =
    PatternSource.fromMatrix(patterns)

  def response: Either[MvpaDatasetError, Response] =
    samples.response

  def foldsByBlock: Either[MvpaDatasetError, FoldPlan] =
    samples.foldPlanByBlock

  def foldsByRun: Either[MvpaDatasetError, FoldPlan] =
    samples.foldPlanByRun

object MvpaDatasetView:
  def build(
      table: PatternTable,
      samples: SampleTable
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    if table.rows != samples.size then Left(MvpaDatasetError.PatternRowCountMismatch(samples.size, table.rows))
    else Right(new MvpaDatasetView(table.toPatternMatrix, samples, table.featureSpace))

  def build(
      series: FmriSeries,
      samples: SampleTable,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    for
      _ <- samples.validateAgainst(series)
      table <- PatternTable.fromSeries(series, featureSpaceId, datasetId)
      view <- build(table, samples)
    yield view

  def fromSeries(
      series: FmriSeries,
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels"),
      datasetId: Option[DatasetId] = None
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    for
      samples <- SampleTable.fromSeries(series, labels = labels, blocks = blocks, runs = runs, items = items)
      view <- build(series, samples, featureSpaceId, datasetId)
    yield view

  def fromDataset(
      dataset: FmriDataset,
      selection: DataSelection = DataSelection.All,
      labels: Option[Seq[String]] = None,
      blocks: Option[Seq[String]] = None,
      runs: Option[Seq[String]] = None,
      items: Option[Seq[String]] = None,
      featureSpaceId: FeatureSpaceId = FeatureSpaceId.unsafe("voxels")
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    try
      fromSeries(
        dataset.series(selection),
        labels = labels,
        blocks = blocks,
        runs = runs,
        items = items,
        featureSpaceId = featureSpaceId,
        datasetId = Some(dataset.id)
      )
    catch
      case NonFatal(error) =>
        Left(MvpaDatasetError.DatasetReadFailed(dataset.id.value, error.getMessage))

  def fromPatternTable(
      table: PatternTable,
      samples: SampleTable
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    build(table, samples)

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
  ): Either[MvpaDatasetError, MvpaDatasetView] =
    for
      table <- PatternTable.fromRows(rows, voxelIndices, featureSpaceId, datasetId, shape)
      samples <- SampleTable.fromRows(rowNames, labels = labels, blocks = blocks, runs = runs, items = items)
      view <- build(table, samples)
    yield view

private def checkedId(kind: String, value: String): Either[MvpaDatasetError, String] =
  val trimmed = value.trim
  if trimmed.isEmpty then Left(MvpaDatasetError.InvalidId(kind, value, "must be non-empty"))
  else Right(trimmed)

private def parseClassLabel(value: String): Either[MvpaDatasetError, ClassLabel] =
  try Right(ClassLabel(value))
  catch
    case NonFatal(error) =>
      Left(MvpaDatasetError.InvalidId("class label", value, error.getMessage))

private def parseRunId(value: String): Either[MvpaDatasetError, RunId] =
  try Right(RunId(value))
  catch
    case NonFatal(error) =>
      Left(MvpaDatasetError.InvalidId("run", value, error.getMessage))
