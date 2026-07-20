package scalafim.dataset

import scalafim.fmri.hrf.design.SamplingFrame

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

final case class DatasetRunDescriptor(
    key: RunKey,
    datasetId: DatasetId,
    shape: DatasetShape,
    samplingFrame: SamplingFrame,
    timeAxis: DatasetTimeAxis
):
  require(samplingFrame.blockLens.sum == shape.timepoints, "run descriptor sampling frame rows must match dataset timepoints")
  require(timeAxis.timepoints == shape.timepoints, "run descriptor time axis must match dataset timepoints")

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
  def fromDataset(key: RunKey, dataset: FmriDataset): DatasetRunDescriptor =
    DatasetRunDescriptor(
      key = key,
      datasetId = dataset.id,
      shape = dataset.shape,
      samplingFrame = dataset.samplingFrame,
      timeAxis = dataset.timeAxis
    )

final case class DatasetRun(
    key: RunKey,
    dataset: FmriDataset
):
  def descriptor: DatasetRunDescriptor =
    DatasetRunDescriptor.fromDataset(key, dataset)

  def id: DatasetId =
    dataset.id

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

  def single(key: RunKey, dataset: FmriDataset): DatasetIndex =
    fromRuns(Vector(DatasetRun(key, dataset)))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def unsafe(runs: Vector[DatasetRun]): DatasetIndex =
    fromRuns(runs).fold(error => throw new IllegalArgumentException(error.message), identity)

private def traverseOptional[A](
    value: Option[String],
    make: String => Either[DatasetError, A]
): Either[DatasetError, Option[A]] =
  value match
    case None => Right(None)
    case Some(text) => make(text).map(Some(_))
