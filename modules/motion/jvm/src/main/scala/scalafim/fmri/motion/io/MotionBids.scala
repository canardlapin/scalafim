package scalafim.fmri.motion.io

import bids4s.*
import bids4s.io.{BidsLoadConfig, BidsProjectLoader}
import scalafim.fmri.motion.*
import scalafim.image.*

import java.nio.file.{Path, Paths}

object MotionBids:
  def loadProject(
      root: Path,
      config: BidsLoadConfig = BidsLoadConfig()
  ): Either[MotionIoError, BidsProject] =
    BidsProjectLoader
      .loadStrict(root, config)
      .left
      .map(error => MotionIoError.fromBids(error.message))

  def rawScans(
      project: BidsProject,
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      kind: String = "bold"
  ): Either[MotionIoError, Vector[MotionBidsScan]] =
    scans(project, project.funcScans(subid = subid, task = task, run = run, session = session, kind = kind))

  def preprocScans(
      project: BidsProject,
      subid: String = ".*",
      task: String = ".*",
      run: String = ".*",
      session: String = ".*",
      space: String = ".*",
      desc: String = "preproc",
      kind: String = "bold",
      pipeline: Option[PipelineName] = Some(PipelineName("fmriprep"))
  ): Either[MotionIoError, Vector[MotionBidsScan]] =
    scans(
      project,
      project.preprocScans(
        subid = subid,
        task = task,
        run = run,
        session = session,
        space = space,
        desc = desc,
        kind = kind,
        pipeline = pipeline
      )
    )

  def loadRun(scan: MotionBidsScan): Either[MotionIoError, SomeScalarSeries[Double]] =
    MotionNiftiIo.readRun(scan.path)

  private def scans(project: BidsProject, files: Vector[BidsFile]): Either[MotionIoError, Vector[MotionBidsScan]] =
    project
      .metadataRecords(files)
      .left
      .map(error => MotionIoError.fromBids(error.message))
      .flatMap { records =>
        traverse(records)(record => scanFromRecord(project, record))
      }

  private def scanFromRecord(
      project: BidsProject,
      record: BidsMetadataRecord
  ): Either[MotionIoError, MotionBidsScan] =
    for
      path <- resolveProjectPath(project, record.file)
      timing <- acquisitionTiming(record)
    yield
      MotionBidsScan(
        file = record.file,
        path = path,
        repetitionTimeSeconds = record.repetitionTime.map(_.value),
        acquisitionTiming = timing,
        metadata = record.metadata
      )

  private def acquisitionTiming(record: BidsMetadataRecord): Either[MotionIoError, AcquisitionTiming] =
    sliceTiming(record) match
      case Left(error) => Left(error)
      case Right(None) => Right(AcquisitionTiming.Volume)
      case Right(Some(offsets)) =>
        SliceTiming
          .make(offsets)
          .map(timing => AcquisitionTiming.Slice(timing))
          .left
          .map(err => MotionIoError.InvalidSidecar(Paths.get(record.path.value), err.message))

  private def sliceTiming(record: BidsMetadataRecord): Either[MotionIoError, Option[Vector[Double]]] =
    record.metadata.fields.get("SliceTiming") match
      case None => Right(None)
      case Some(JsonValue.Arr(values)) =>
        val offsets = Vector.newBuilder[Double]
        var i = 0
        while i < values.length do
          values(i).asNumber.filter(_.isFinite) match
            case Some(value) => offsets += value
            case None =>
              return Left(MotionIoError.InvalidSidecar(Paths.get(record.path.value), "SliceTiming must contain only finite numbers"))
          i += 1
        Right(Some(offsets.result()))
      case Some(_) =>
        Left(MotionIoError.InvalidSidecar(Paths.get(record.path.value), "SliceTiming must be a JSON number array"))

  private def resolveProjectPath(project: BidsProject, file: BidsFile): Either[MotionIoError, Path] =
    val root = Paths.get(project.root.value).toAbsolutePath.normalize()
    val resolved = root.resolve(file.path.value).normalize()
    if resolved.startsWith(root) then Right(resolved)
    else Left(MotionIoError.InvalidInput(Paths.get(file.path.value), s"path escapes project root '${project.root.value}'"))

  private def traverse[A, B](values: Vector[A])(f: A => Either[MotionIoError, B]): Either[MotionIoError, Vector[B]] =
    val out = Vector.newBuilder[B]
    var i = 0
    while i < values.length do
      f(values(i)) match
        case Left(error) => return Left(error)
        case Right(value) => out += value
      i += 1
    Right(out.result())
