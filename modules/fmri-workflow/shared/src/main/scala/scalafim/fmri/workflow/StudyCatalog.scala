package scalafim.fmri.workflow

import bids4s.PipelineName
import scalafim.dataset.{DatasetId, DatasetShape, RunId, SessionId, SpaceId, SubjectId, TaskId}

opaque type RepetitionTime = Double

object RepetitionTime:
  def apply(value: Double): Either[WorkflowError, RepetitionTime] =
    if value > 0.0 && value.isFinite then Right(value)
    else Left(WorkflowError.InvalidValue("repetition time", value.toString, "must be positive and finite"))

  def unsafe(value: Double): RepetitionTime =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (time: RepetitionTime)
    inline def seconds: Double = time

/** Observed preprocessing metadata. Absence is unknown, not false.
  * Neither value declares the effective within-volume model reference.
  */
final case class RunTimingMetadata(sliceTimingCorrected: Option[Boolean] = None)

final case class RunInput private (
    id: RunId,
    repetitionTime: RepetitionTime,
    timepoints: Int,
    bold: WorkflowArtifactRef[BoldImageResource],
    events: WorkflowArtifactRef[EventsTableResource],
    confounds: Option[WorkflowArtifactRef[ConfoundsTableResource]],
    timing: RunTimingMetadata
):
  require(timepoints > 0, "run timepoints must be positive")

object RunInput:
  def make(
      id: RunId,
      repetitionTime: RepetitionTime,
      timepoints: Int,
      bold: WorkflowArtifactRef[BoldImageResource],
      events: WorkflowArtifactRef[EventsTableResource],
      confounds: Option[WorkflowArtifactRef[ConfoundsTableResource]] = None,
      timing: RunTimingMetadata = RunTimingMetadata()
  ): Either[WorkflowError, RunInput] =
    if timepoints <= 0 then Left(WorkflowError.InvalidRun(id.value, s"timepoints must be positive, got $timepoints"))
    else Right(new RunInput(id, repetitionTime, timepoints, bold, events, confounds, timing))

  def unsafe(
      id: RunId,
      repetitionTime: RepetitionTime,
      timepoints: Int,
      bold: WorkflowArtifactRef[BoldImageResource],
      events: WorkflowArtifactRef[EventsTableResource],
      confounds: Option[WorkflowArtifactRef[ConfoundsTableResource]] = None,
      timing: RunTimingMetadata = RunTimingMetadata()
  ): RunInput =
    make(id, repetitionTime, timepoints, bold, events, confounds, timing)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

sealed trait UnitMask:
  def artifacts: Vector[WorkflowArtifactRef[MaskImageResource]]

object UnitMask:
  final case class Single(artifact: WorkflowArtifactRef[MaskImageResource]) extends UnitMask:
    def artifacts: Vector[WorkflowArtifactRef[MaskImageResource]] = Vector(artifact)

  final case class Intersection private[workflow] (
      runMasks: Vector[(RunId, WorkflowArtifactRef[MaskImageResource])]
  ) extends UnitMask:
    require(runMasks.nonEmpty, "mask intersection must contain run masks")
    require(runMasks.map(_._1).distinct.length == runMasks.length, "mask intersection run ids must be unique")

    def artifacts: Vector[WorkflowArtifactRef[MaskImageResource]] =
      runMasks.map(_._2)

  def intersection(
      runMasks: Vector[(RunId, WorkflowArtifactRef[MaskImageResource])]
  ): Either[WorkflowError, UnitMask] =
    if runMasks.isEmpty then Left(WorkflowError.InvalidCatalog("mask intersection must contain at least one run mask"))
    else
      val duplicates = WorkflowValidation.duplicates(runMasks.map(_._1.value)).sorted
      if duplicates.nonEmpty then Left(WorkflowError.DuplicateValues("mask intersection run ids", duplicates))
      else Right(new Intersection(runMasks))

  def unsafeIntersection(
      runMasks: Vector[(RunId, WorkflowArtifactRef[MaskImageResource])]
  ): UnitMask =
    intersection(runMasks).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class FirstLevelUnit private (
    id: FirstLevelUnitId,
    subject: SubjectId,
    session: Option[SessionId],
    task: TaskId,
    space: SpaceId,
    shape: DatasetShape,
    runs: Vector[RunInput],
    mask: UnitMask,
    acquisition: Option[String],
    echo: Option[String],
    resolution: Option[String],
    pipeline: Option[PipelineName]
):
  require(runs.nonEmpty, "first-level unit must contain at least one run")
  require(runs.map(_.id).distinct.length == runs.length, "first-level run ids must be unique")
  require(runs.map(_.timepoints).sum == shape.timepoints, "run timepoints must match dataset shape")
  require(acquisition.forall(_.trim.nonEmpty), "acquisition entity must be non-empty when present")
  require(echo.forall(_.trim.nonEmpty), "echo entity must be non-empty when present")
  require(resolution.forall(_.trim.nonEmpty), "resolution entity must be non-empty when present")

  def totalTimepoints: Int =
    runs.map(_.timepoints).sum

  def runIds: Vector[RunId] =
    runs.map(_.id)

object FirstLevelUnit:
  def make(
      id: FirstLevelUnitId,
      subject: SubjectId,
      session: Option[SessionId],
      task: TaskId,
      space: SpaceId,
      shape: DatasetShape,
      runs: Vector[RunInput],
      mask: UnitMask,
      acquisition: Option[String] = None,
      echo: Option[String] = None,
      resolution: Option[String] = None,
      pipeline: Option[PipelineName] = None
  ): Either[WorkflowError, FirstLevelUnit] =
    if runs.isEmpty then Left(WorkflowError.InvalidUnit(id.value, "must contain at least one run"))
    else
      val duplicateRuns = WorkflowValidation.duplicates(runs.map(_.id.value)).sorted
      if duplicateRuns.nonEmpty then
        Left(WorkflowError.InvalidUnit(id.value, s"duplicate run ids: ${duplicateRuns.mkString(", ")}"))
      else
        val actualTimepoints = runs.map(_.timepoints).sum
        if actualTimepoints != shape.timepoints then
          Left(
            WorkflowError.InvalidUnit(
              id.value,
              s"run timepoints $actualTimepoints do not match dataset shape ${shape.timepoints}"
            )
          )
        else if acquisition.exists(_.trim.isEmpty) then
          Left(WorkflowError.InvalidUnit(id.value, "acquisition entity must be non-empty when present"))
        else if echo.exists(_.trim.isEmpty) then
          Left(WorkflowError.InvalidUnit(id.value, "echo entity must be non-empty when present"))
        else if resolution.exists(_.trim.isEmpty) then
          Left(WorkflowError.InvalidUnit(id.value, "resolution entity must be non-empty when present"))
        else Right(new FirstLevelUnit(id, subject, session, task, space, shape, runs, mask, acquisition, echo, resolution, pipeline))

  def unsafe(
      id: FirstLevelUnitId,
      subject: SubjectId,
      session: Option[SessionId],
      task: TaskId,
      space: SpaceId,
      shape: DatasetShape,
      runs: Vector[RunInput],
      mask: UnitMask,
      acquisition: Option[String] = None,
      echo: Option[String] = None,
      resolution: Option[String] = None,
      pipeline: Option[PipelineName] = None
  ): FirstLevelUnit =
    make(id, subject, session, task, space, shape, runs, mask, acquisition, echo, resolution, pipeline)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

final case class ParticipantRecord private (
    subject: SubjectId,
    values: Map[String, Option[String]]
):
  require(values.keys.forall(_.trim.nonEmpty), "participant field names must be non-empty")

object ParticipantRecord:
  def make(
      subject: SubjectId,
      values: Map[String, Option[String]]
  ): Either[WorkflowError, ParticipantRecord] =
    val invalid = values.keys.filter(_.trim.isEmpty).toVector
    if invalid.nonEmpty then Left(WorkflowError.InvalidCatalog("participant field names must be non-empty"))
    else Right(new ParticipantRecord(subject, values))

final case class StudyCatalog private (
    datasetId: DatasetId,
    units: Vector[FirstLevelUnit],
    participants: Vector[ParticipantRecord]
):
  require(units.map(_.id).distinct.length == units.length, "study catalog unit ids must be unique")
  require(units == units.sortBy(StudyCatalog.unitSortKey), "study catalog units must have canonical entity order")
  require(participants.map(_.subject).distinct.length == participants.length, "study catalog participant ids must be unique")
  require(participants == participants.sortBy(_.subject.value), "study catalog participants must have canonical subject order")
  require(participants.isEmpty || units.forall(unit => participants.exists(_.subject == unit.subject)), "participant records must cover catalog subjects")

  def unit(id: FirstLevelUnitId): Option[FirstLevelUnit] =
    units.find(_.id == id)

  def subjects: Vector[SubjectId] =
    units.map(_.subject).distinct

  def participant(subject: SubjectId): Option[ParticipantRecord] =
    participants.find(_.subject == subject)

  def isEmpty: Boolean =
    units.isEmpty

object StudyCatalog:
  def make(
      datasetId: DatasetId,
      units: Vector[FirstLevelUnit],
      participants: Vector[ParticipantRecord] = Vector.empty
  ): Either[WorkflowError, StudyCatalog] =
    val duplicateIds = WorkflowValidation.duplicates(units.map(_.id.value)).sorted
    val duplicateParticipants = WorkflowValidation.duplicates(participants.map(_.subject.value)).sorted
    val missingParticipants =
      if participants.isEmpty then Vector.empty
      else units.map(_.subject.value).distinct.filterNot(subject => participants.exists(_.subject.value == subject)).sorted
    if duplicateIds.nonEmpty then Left(WorkflowError.DuplicateValues("first-level unit ids", duplicateIds))
    else if duplicateParticipants.nonEmpty then Left(WorkflowError.DuplicateValues("participant ids", duplicateParticipants))
    else if missingParticipants.nonEmpty then Left(WorkflowError.InvalidCatalog(s"missing participant records: ${missingParticipants.mkString(", ")}"))
    else Right(new StudyCatalog(datasetId, units.sortBy(unitSortKey), participants.sortBy(_.subject.value)))

  def empty(datasetId: DatasetId): StudyCatalog =
    new StudyCatalog(datasetId, Vector.empty, Vector.empty)

  private[workflow] def unitSortKey(unit: FirstLevelUnit): String =
    Vector(
      unit.subject.value,
      unit.session.map(_.value).getOrElse(""),
      unit.task.value,
      unit.acquisition.getOrElse(""),
      unit.echo.getOrElse(""),
      unit.space.value,
      unit.resolution.getOrElse(""),
      unit.pipeline.map(_.value).getOrElse(""),
      unit.id.value
    ).mkString("\u0000")
