package scalafim.dataset.io

import java.nio.file.Path
import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.LnaArchive
import scalafim.dataset.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.latent.{
  LatentArchiveKind,
  LatentArchivePlan,
  LatentArchiveRegistry
}
import scalafim.response.OperationId

enum LnaDatasetReadMode:
  case SelectionAware
  case WholeRunFallback

final case class LnaDatasetProvenance(
    archivePath: String,
    archiveRun: RunLabel,
    codecFamily: String,
    externalBasis: Option[String],
    readMode: LnaDatasetReadMode
) extends DatasetProvenance:
  val source: String = "lna"

extension (factory: FmriDataset.type)
  def openLna(
      root: Path,
      query: LnaDatasetQuery,
      timing: SamplingFrame,
      registry: LatentArchiveRegistry,
      events: DatasetEvents = DatasetEvents.Empty
  ): Either[DatasetError, SynchronousFmriDataset] =
    openLnaSingle(
      root,
      query,
      timing,
      archiveRun = None,
      registry = registry,
      events = events
    )

  def openLna(
      root: Path,
      query: LnaDatasetQuery,
      timing: SamplingFrame,
      archiveRun: RunLabel,
      registry: LatentArchiveRegistry,
      events: DatasetEvents
  ): Either[DatasetError, SynchronousFmriDataset] =
    openLnaSingle(
      root,
      query,
      timing,
      archiveRun = Some(archiveRun),
      registry = registry,
      events = events
    )

  def openLna(
      root: Path,
      query: LnaDatasetQuery,
      timing: SamplingFrame,
      runIds: Vector[RunId],
      archiveRun: Option[RunLabel],
      registry: LatentArchiveRegistry,
      events: DatasetEvents
  ): Either[DatasetError, SynchronousFmriDataset] =
    openLnaWithRuns(
      root,
      query,
      timing,
      runIds,
      archiveRun,
      registry,
      events
    )

private def openLnaSingle(
    root: Path,
    query: LnaDatasetQuery,
    timing: SamplingFrame,
    archiveRun: Option[RunLabel],
    registry: LatentArchiveRegistry,
    events: DatasetEvents
): Either[DatasetError, SynchronousFmriDataset] =
  if timing.nBlocks != 1 then
    Left(DatasetError.InvalidTimeAxis(
      s"single-run LNA open requires one sampling block, found ${timing.nBlocks}; supply explicit runIds for a multi-block frame"
    ))
  else
    for
      lna <- LnaDataset.open(root, registry).left.map(archiveFailure)
      path <- resolvePath(lna, query)
      runId <- lna.datasetRunId(path)
      dataset <- openResolvedLna(lna, path, query, timing, Vector(runId), archiveRun, events)
    yield dataset

private def openLnaWithRuns(
    root: Path,
    query: LnaDatasetQuery,
    timing: SamplingFrame,
    runIds: Vector[RunId],
    archiveRun: Option[RunLabel],
    registry: LatentArchiveRegistry,
    events: DatasetEvents
): Either[DatasetError, SynchronousFmriDataset] =
  for
    lna <- LnaDataset.open(root, registry).left.map(archiveFailure)
    path <- resolvePath(lna, query)
    dataset <- openResolvedLna(lna, path, query, timing, runIds, archiveRun, events)
  yield dataset

private def openResolvedLna(
    lna: LnaDataset,
    path: Path,
    query: LnaDatasetQuery,
    timing: SamplingFrame,
    runIds: Vector[RunId],
    requestedArchiveRun: Option[RunLabel],
    events: DatasetEvents
): Either[DatasetError, SynchronousFmriDataset] =
  for
    archive <- lna.readArchive(path).left.map(archiveFailure)
    archiveRun <- selectArchiveRun(archive, requestedArchiveRun, query)
    opened <- openBackend(lna, path, archive, archiveRun)
    dataset <- FmriDataset.open(opened, timing, runIds, events)
  yield dataset

private def resolvePath(
    lna: LnaDataset,
    query: LnaDatasetQuery
): Either[DatasetError, Path] =
  lna.resolveLnaFile(query).left.map:
    case LnaDatasetLookupError.ScanFailed(error) =>
      archiveFailure(error)
    case LnaDatasetLookupError.NoMatches(_) =>
      DatasetError.DatasetRunNotFound(query.label)
    case LnaDatasetLookupError.Ambiguous(_, matches) =>
      DatasetError.AmbiguousDatasetRun(query.label, matches.length)

private[io] def selectArchiveRun(
    archive: LnaArchive,
    requested: Option[RunLabel],
    query: LnaDatasetQuery
): Either[DatasetError, RunLabel] =
  requested match
    case Some(run) =>
      if archive.run(run).isDefined then Right(run)
      else Left(archiveFailure(
        ArchiveError.InvalidArchive(s"run '${run.value}' not found")
      ))
    case None =>
      archive.manifest.runs.map(_.label) match
        case Vector(run) =>
          Right(run)
        case runs =>
          Left(DatasetError.AmbiguousDatasetRun(
            s"${query.label}; internal LNA run",
            runs.length
          ))

private def openBackend(
    lna: LnaDataset,
    path: Path,
    archive: LnaArchive,
    archiveRun: RunLabel
): Either[DatasetError, DatasetBackend] =
  lna.registry
    .maybeOpenPlan(archive, archiveRun)
    .left
    .map(archiveFailure)
    .flatMap:
      case Some(plan) =>
        val provenance =
          LnaDatasetProvenance(
            archivePath = lna.root.relativize(path.toAbsolutePath.normalize()).toString.replace('\\', '/'),
            archiveRun = archiveRun,
            codecFamily = codecFamily(plan.kind),
            externalBasis = externalBasis(plan),
            readMode = LnaDatasetReadMode.SelectionAware
          )
        lna
          .latentBackendFromPlan(
            plan,
            path.toAbsolutePath.normalize(),
            DatasetId(LnaDataset.datasetIdFromPath(lna.root, path)),
            lna.metadataFor(path).withProvenance(provenance)
          )
          .left
          .map(archiveFailure)

      case None =>
        val provenance =
          LnaDatasetProvenance(
            archivePath = lna.root.relativize(path.toAbsolutePath.normalize()).toString.replace('\\', '/'),
            archiveRun = archiveRun,
            codecFamily = "lna-pipeline",
            externalBasis = None,
            readMode = LnaDatasetReadMode.WholeRunFallback
          )
        LatentArchiveDatasetBackend.make(
          id = DatasetId(LnaDataset.datasetIdFromPath(lna.root, path)),
          archive = archive,
          run = archiveRun,
          registry = lna.registry,
          metadata = lna.metadataFor(path).withProvenance(provenance)
        )

private def codecFamily(kind: LatentArchiveKind): String =
  kind match
    case LatentArchiveKind.Explicit     => "explicit"
    case LatentArchiveKind.TemporalDct  => "temporal-dct"
    case LatentArchiveKind.TemporalHaar => "temporal-haar"
    case LatentArchiveKind.SharedBasis  => "shared-basis"
    case LatentArchiveKind.Transport    => "transport"
    case LatentArchiveKind.BoldZip      => "boldzip"

private def externalBasis(plan: LatentArchivePlan): Option[String] =
  plan match
    case LatentArchivePlan.SharedBasis(_, _, archive) =>
      archive.basis.locator.map(_.value).orElse(Some(archive.basis.basisId.value))
    case _ =>
      None

private def archiveFailure(error: ArchiveError): DatasetError =
  DatasetError.AdapterFailure(
    OperationId.unsafe("lna-archive"),
    error.message
  )
