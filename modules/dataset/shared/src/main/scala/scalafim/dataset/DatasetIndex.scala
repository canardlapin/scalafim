package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.GridCompatibility

enum DatasetFieldCriterion[+A]:
  case Any
  case Missing
  case Is(value: A)

  def matches[B >: A](value: Option[B]): Boolean =
    this match
      case DatasetFieldCriterion.Any =>
        true
      case DatasetFieldCriterion.Missing =>
        value.isEmpty
      case DatasetFieldCriterion.Is(expected) =>
        value.contains(expected)

  def label[B >: A](render: B => String): String =
    this match
      case DatasetFieldCriterion.Any =>
        "*"
      case DatasetFieldCriterion.Missing =>
        "<missing>"
      case DatasetFieldCriterion.Is(value) =>
        render(value)

final case class DatasetKey(
    subject: SubjectId,
    session: Option[SessionId] = None,
    task: Option[TaskId] = None,
    space: Option[SpaceId] = None
):
  def label: String =
    Vector(
      Some(s"subject=${subject.value}"),
      session.map(value => s"session=${value.value}"),
      task.map(value => s"task=${value.value}"),
      space.map(value => s"space=${value.value}")
    ).flatten.mkString(",")

object DatasetKey:
  def fromStrings(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): Either[DatasetError, DatasetKey] =
    for
      subjectId <- SubjectId.make(subject)
      sessionId <- traverseOptional(session, SessionId.make)
      taskId <- traverseOptional(task, TaskId.make)
      spaceId <- traverseOptional(space, SpaceId.make)
    yield DatasetKey(subjectId, sessionId, taskId, spaceId)

  def unsafe(
      subject: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): DatasetKey =
    fromStrings(subject, session, task, space)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class RunKey(
    dataset: DatasetKey,
    run: RunId
):
  def label: String =
    s"${dataset.label},run=${run.value}"

object RunKey:
  def fromStrings(
      subject: String,
      run: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): Either[DatasetError, RunKey] =
    for
      datasetKey <- DatasetKey.fromStrings(subject, session, task, space)
      runId <- RunId.make(run)
    yield RunKey(datasetKey, runId)

  def unsafe(
      subject: String,
      run: String,
      session: Option[String] = None,
      task: Option[String] = None,
      space: Option[String] = None
  ): RunKey =
    fromStrings(subject, run, session, task, space)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final class DatasetRunDescriptor private (
    val key: RunKey,
    val datasetId: DatasetId,
    val shape: DatasetShape,
    val samplingFrame: SamplingFrame,
    val timeAxis: DatasetTimeAxis
):

  def subject: SubjectId =
    key.dataset.subject

  def session: Option[SessionId] =
    key.dataset.session

  def task: Option[TaskId] =
    key.dataset.task

  def space: Option[SpaceId] =
    key.dataset.space

  def run: RunId =
    key.run

object DatasetRunDescriptor:
  private[dataset] def fromRun(run: DatasetRun): DatasetRunDescriptor =
    val blockIndex = run.block.runIndex
    val frame =
      SamplingFrame
        .validated(
          blockLens = Seq(run.block.length),
          tr = Seq(run.dataset.samplingFrame.tr(blockIndex).value),
          startTime = Seq(run.dataset.samplingFrame.startTime(blockIndex).value),
          precision = run.dataset.samplingFrame.precision.value
        )
        .fold(error => throw new IllegalStateException(error.message), identity)
    new DatasetRunDescriptor(
      key = run.key,
      datasetId = run.dataset.id,
      shape = DatasetShape.unsafe(run.dataset.shape.space, run.block.length),
      samplingFrame = frame,
      timeAxis = DatasetTimeAxis.unsafe(frame, Vector(run.block.run))
    )

final class DatasetRun private (
    val key: RunKey,
    val dataset: FmriDataset,
    val block: DatasetTimeBlock
):
  def descriptor: DatasetRunDescriptor =
    DatasetRunDescriptor.fromRun(this)

  def id: DatasetId =
    dataset.id

  def seriesEither(
      readers: SynchronousDatasetReaders,
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, FmriSeries] =
    for
      reader <- readers.readerFor(dataset)
      translated <- translatedSelection(selection)
      series <- reader.seriesEither(translated)
    yield series

  def series(
      readers: SynchronousDatasetReaders,
      selection: DataSelection = DataSelection.All
  ): FmriSeries =
    seriesEither(readers, selection)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private[dataset] def partitionedSeriesEither(
      readers: SynchronousDatasetReaders,
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, (FmriSeries, DatasetRunPartition)] =
    for
      reader <- readers.readerFor(dataset)
      translated <- translatedSelection(selection)
      series <- reader.seriesEither(translated)
      partitions <- dataset.runPartitionsFor(series.timepointIndices)
      partition <- partitions match
        case Vector(single) if single.run == block.run =>
          Right(single)
        case found =>
          Left(DatasetError.InvalidTimeAxis(
            s"run-local selection for ${block.run.value} resolved to ${found.length} partitions"
          ))
    yield (series, partition)

  private def translatedSelection(
      selection: DataSelection
  ): Either[DatasetError, DataSelection] =
    selection.time.resolve(block.length).map: local =>
      val global =
        local.map(index => TimepointIndex.unsafe(block.startValue + index.value))
      DataSelection(
        time = TimepointSelection.Indices(global),
        voxels = selection.voxels
      )

object DatasetRun:
  def make(
      key: RunKey,
      dataset: FmriDataset
  ): Either[DatasetError, DatasetRun] =
    dataset.timeAxis.blocks match
      case Vector(block) if block.run == key.run =>
        Right(new DatasetRun(key, dataset, block))
      case Vector(block) =>
        Left(DatasetError.InvalidTimeAxis(
          s"run key ${key.run.value} does not match dataset run ${block.run.value}"
        ))
      case blocks =>
        Left(DatasetError.InvalidTimeAxis(
          s"direct DatasetRun construction requires exactly one time block, found ${blocks.length}"
        ))

  def fromDataset(
      key: DatasetKey,
      dataset: FmriDataset
  ): Vector[DatasetRun] =
    dataset.timeAxis.blocks.map: block =>
      new DatasetRun(RunKey(key, block.run), dataset, block)

  def unsafe(
      key: RunKey,
      dataset: FmriDataset
  ): DatasetRun =
    make(key, dataset)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class DatasetRunQuery(
    subject: Option[SubjectId] = None,
    session: DatasetFieldCriterion[SessionId] = DatasetFieldCriterion.Any,
    task: DatasetFieldCriterion[TaskId] = DatasetFieldCriterion.Any,
    space: DatasetFieldCriterion[SpaceId] = DatasetFieldCriterion.Any,
    run: Option[RunId] = None
):
  def matches(key: RunKey): Boolean =
    subject.forall(_ == key.dataset.subject) &&
      session.matches(key.dataset.session) &&
      task.matches(key.dataset.task) &&
      space.matches(key.dataset.space) &&
      run.forall(_ == key.run)

  def label: String =
    Vector(
      s"subject=${subject.map(_.value).getOrElse("*")}",
      s"session=${session.label(_.value)}",
      s"task=${task.label(_.value)}",
      s"space=${space.label(_.value)}",
      s"run=${run.map(_.value).getOrElse("*")}"
    ).mkString(",")

object DatasetRunQuery:
  val All: DatasetRunQuery = DatasetRunQuery()

  def forKey(key: RunKey): DatasetRunQuery =
    DatasetRunQuery(
      subject = Some(key.dataset.subject),
      session = key.dataset.session.fold(DatasetFieldCriterion.Missing)(DatasetFieldCriterion.Is.apply),
      task = key.dataset.task.fold(DatasetFieldCriterion.Missing)(DatasetFieldCriterion.Is.apply),
      space = key.dataset.space.fold(DatasetFieldCriterion.Missing)(DatasetFieldCriterion.Is.apply),
      run = Some(key.run)
    )

final class DatasetIndex private (
    val runs: Vector[DatasetRun]
):
  require(runs.nonEmpty, "dataset index must contain at least one run")

  def size: Int =
    runs.length

  def keys: Vector[RunKey] =
    runs.map(_.key)

  def descriptors: Vector[DatasetRunDescriptor] =
    runs.map(_.descriptor)

  def subjects: Vector[SubjectId] =
    runs.map(_.key.dataset.subject).distinct

  def get(key: RunKey): Option[DatasetRun] =
    runs.find(_.key == key)

  def resolve(query: DatasetRunQuery = DatasetRunQuery.All): Vector[DatasetRun] =
    runs.filter(run => query.matches(run.key))

  def resolveEither(query: DatasetRunQuery = DatasetRunQuery.All): Either[DatasetError, Vector[DatasetRun]] =
    val out = resolve(query)
    if out.isEmpty then Left(DatasetError.DatasetRunNotFound(query.label))
    else Right(out)

  def one(query: DatasetRunQuery): Either[DatasetError, DatasetRun] =
    resolve(query) match
      case Vector() =>
        Left(DatasetError.DatasetRunNotFound(query.label))
      case Vector(single) =>
        Right(single)
      case many =>
        Left(DatasetError.AmbiguousDatasetRun(query.label, many.length))

  def read(
      readers: SynchronousDatasetReaders,
      query: DatasetRunQuery,
      selection: DataSelection = DataSelection.All
  ): Either[DatasetError, SegmentedFmriSeries] =
    for
      selected <- resolveForRead(query, selection)
      segments <- readSegments(readers, selected, selection)
      result <- SegmentedFmriSeries.make(segments)
    yield result

  private[dataset] def resolveForRead(
      query: DatasetRunQuery,
      selection: DataSelection
  ): Either[DatasetError, Vector[DatasetRun]] =
    resolveEither(query).flatMap: selected =>
      selection.voxels match
        case coordinates: VoxelSelection.Coords =>
          validateCoordinateRuns(selected, coordinates)
        case _ =>
          Right(selected)

  private def validateCoordinateRuns(
      selected: Vector[DatasetRun],
      coordinates: VoxelSelection.Coords
  ): Either[DatasetError, Vector[DatasetRun]] =
    val expected = selected.head.dataset.shape.space
    var index = 0
    var failure = Option.empty[DatasetError]
    while index < selected.length && failure.isEmpty do
      val run = selected(index)
      GridCompatibility.exact(expected, run.dataset.shape.space) match
        case Left(error) =>
          failure = Some(DatasetError.ShapeMismatch(
            s"coordinate selection requires identical run grids: ${error.message}"
          ))
        case Right(_) =>
          run.dataset.voxelDomain.resolve(coordinates, run.dataset.shape.space) match
            case Left(error) => failure = Some(error)
            case Right(_)    => ()
      index += 1
    failure match
      case Some(error) => Left(error)
      case None        => Right(selected)

  private def readSegments(
      readers: SynchronousDatasetReaders,
      selected: Vector[DatasetRun],
      selection: DataSelection
  ): Either[DatasetError, Vector[FmriSeriesSegment]] =
    val segments = Vector.newBuilder[FmriSeriesSegment]
    segments.sizeHint(selected.length)
    var index = 0
    var failure = Option.empty[DatasetError]
    while index < selected.length && failure.isEmpty do
      val run = selected(index)
      val opened =
        run.partitionedSeriesEither(readers, selection).flatMap: (series, partition) =>
          FmriSeriesSegment.make(run.key, partition, series)
      opened match
        case Left(error)    => failure = Some(error)
        case Right(segment) => segments += segment
      index += 1
    failure match
      case Some(error) => Left(error)
      case None        => Right(segments.result())

object DatasetIndex:
  def fromRuns(runs: Vector[DatasetRun]): Either[DatasetError, DatasetIndex] =
    if runs.isEmpty then Left(DatasetError.EmptyDatasetIndex)
    else
      val seen = scala.collection.mutable.HashSet.empty[RunKey]
      var i = 0
      while i < runs.length do
        val key = runs(i).key
        if seen.contains(key) then return Left(DatasetError.DuplicateDatasetRun(key.label))
        seen += key
        i += 1
      Right(new DatasetIndex(runs))

  def single(
      key: RunKey,
      dataset: FmriDataset
  ): Either[DatasetError, DatasetIndex] =
    DatasetRun.make(key, dataset).flatMap(run => fromRuns(Vector(run)))

  def fromDataset(
      key: DatasetKey,
      dataset: FmriDataset
  ): Either[DatasetError, DatasetIndex] =
    fromRuns(DatasetRun.fromDataset(key, dataset))

  def unsafeSingle(key: RunKey, dataset: FmriDataset): DatasetIndex =
    single(key, dataset)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(runs: Vector[DatasetRun]): DatasetIndex =
    fromRuns(runs).fold(error => throw new IllegalArgumentException(error.message), identity)

final class SynchronousDatasetReaders private (
    private val byId: Map[DatasetId, DatasetSeriesReader]
):
  def ids: Vector[DatasetId] =
    byId.keys.toVector.sortBy(_.value)

  def readerFor(
      dataset: FmriDataset
  ): Either[DatasetError, DatasetSeriesReader] =
    byId
      .get(dataset.id)
      .toRight(DatasetError.SynchronousReaderNotFound(dataset.id))
      .flatMap: reader =>
        if reader.dataset.shape != dataset.shape then
          Left(DatasetError.ShapeMismatch(
            s"synchronous reader for '${dataset.id.value}' has a different dataset shape"
          ))
        else if reader.dataset.voxelDomain.indices != dataset.voxelDomain.indices then
          Left(DatasetError.SampleOrderingMismatch(
            dataset.voxelDomain.indices,
            reader.dataset.voxelDomain.indices
          ))
        else
          Right(reader)

object SynchronousDatasetReaders:
  def build(
      readers: DatasetSeriesReader*
  ): Either[DatasetError, SynchronousDatasetReaders] =
    val ordered = readers.toVector.sortBy(_.dataset.id.value)
    var index = 1
    while index < ordered.length do
      if ordered(index - 1).dataset.id == ordered(index).dataset.id then
        return Left(DatasetError.DuplicateSynchronousReader(
          ordered(index).dataset.id
        ))
      index += 1
    Right(new SynchronousDatasetReaders(
      ordered.iterator.map(reader => reader.dataset.id -> reader).toMap
    ))

  def one(
      reader: DatasetSeriesReader
  ): SynchronousDatasetReaders =
    new SynchronousDatasetReaders(Map(reader.dataset.id -> reader))

private def traverseOptional[A](
    value: Option[String],
    make: String => Either[DatasetError, A]
): Either[DatasetError, Option[A]] =
  value match
    case None => Right(None)
    case Some(text) => make(text).map(Some(_))
