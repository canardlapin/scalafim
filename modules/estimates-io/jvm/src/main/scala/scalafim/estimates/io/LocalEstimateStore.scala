package scalafim.estimates.io

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.util.control.NonFatal
import scalafim.archive.io.{LocalObjectStore, LocalStoreError, StagedFile, VerifiedFileObject}
import scalafim.estimates.*

/** Local metadata publication and fully verified reopening. Collection coverage
  * remains distinct from a successfully published unit and consumer admission.
  */
final class LocalEstimateStore private[io] (private[io] val objects: LocalObjectStore) extends EstimateSetReader:
  val root: Path = objects.root

  private[io] def fromStore(error: LocalStoreError): EstimateError = error match
    case LocalStoreError.Invalid(detail) => EstimateError.Invalid(detail)
    case LocalStoreError.Integrity(detail) => EstimateError.Integrity(detail)
    case LocalStoreError.Conflict(detail) => EstimateError.Conflict(detail)
    case LocalStoreError.Io(detail) => EstimateError.Io(detail)

  private[io] def reference(value: VerifiedFileObject): FileReference = FileReference(value.path, value.digest, value.bytes)
  private[io] def verified(value: FileReference): VerifiedFileObject = VerifiedFileObject(value.path, value.digest, value.bytes)

  /** A declared design or estimable subspace is required scientific evidence,
    * not an optional external original input.
    */
  private[io] def verifyEstimability(unit: EstimateUnit): Either[EstimateError, Unit] =
    val reference = unit.estimability match
      case EstimabilityEvidence.Design(_, matrix) => Some(matrix)
      case EstimabilityEvidence.Subspace(_, basis, _, _, _) => Some(basis)
      case _ => None
    reference match
      case None => Right(())
      case Some(value) => objects.verify(verified(value)).left.map(fromStore)

  private[io] def protect[A](body: => Either[EstimateError, A]): Either[EstimateError, A] =
    try body
    catch case NonFatal(error) => Left(EstimateError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private[io] def text(ref: FileReference): Either[EstimateError, String] = protect:
    if ref.bytes > 16L * 1024L * 1024L then Left(EstimateError.Unsupported("metadata exceeds 16 MiB inspection budget"))
    else objects.verify(verified(ref)).left.map(fromStore).map(_ => Files.readString(root.resolve(ref.path), UTF_8))

  private[io] def writeText(path: String, text: String): Either[EstimateError, FileReference] =
    val bytes = text.getBytes(UTF_8)
    if bytes.length > 16 * 1024 * 1024 then Left(EstimateError.Unsupported("metadata exceeds 16 MiB inspection budget"))
    else objects.stage(".metadata").left.map(fromStore).flatMap: stage =>
      protect:
        try
          Files.write(stage.path, bytes, java.nio.file.StandardOpenOption.CREATE_NEW)
          publishStagedIdempotent(stage, path)
        finally
          // This exact staging path was allocated by this invocation. Identical
          // retry and conflict both release their aliases; never scan staging.
          Files.deleteIfExists(stage.path)
          Files.deleteIfExists(stage.path.getParent)

  /** A retry may reuse an immutable numerical object only when its staged bytes
    * match exactly. A different payload for the same revision remains a conflict.
    */
  private[io] def publishStagedIdempotent(stage: StagedFile, path: String): Either[EstimateError, FileReference] =
    objects.publishStaged(stage, path) match
      case Right(written) => Right(reference(written))
      case Left(LocalStoreError.Conflict(_)) => protect:
        objects.inspect(path).left.map(fromStore).flatMap: existing =>
          val digest = MessageDigest.getInstance("SHA-256")
          val input = Files.newInputStream(stage.path)
          val buffer = new Array[Byte](65536)
          var size = 0L
          try
            var count = input.read(buffer)
            while count >= 0 do
              digest.update(buffer, 0, count)
              size = Math.addExact(size, count.toLong)
              count = input.read(buffer)
          finally input.close()
          val hex = digest.digest().iterator.map(byte => f"${byte & 0xff}%02x").mkString
          if existing.bytes == size && existing.digest.value == hex then Right(reference(existing))
          else Left(EstimateError.Conflict(s"immutable numerical destination $path has different bytes"))
      case Left(error) => Left(fromStore(error))

  private[io] def publishUnit(unit: EstimateUnit, representations: Vector[NiftiRepresentation]): Either[EstimateError, PinnedUnit] =
    publishMixedUnit(unit, representations.map(EstimateRepresentation.Nifti.apply), false)

  private[io] def publishMixedUnit(unit: EstimateUnit, representations: Vector[EstimateRepresentation], compact: Boolean,
      status: Option[InferenceStatusRepresentation] = None): Either[EstimateError, PinnedUnit] =
    if representations.exists(_.isInstanceOf[EstimateRepresentation.Hdf5]) then
      return Left(EstimateError.Unsupported("NIfTI publication refuses HDF5 representations"))
    val prefix = s"units/${unit.revision.value}"
    // A fresh unit owns its catalog reference. Identical catalog bytes still
    // retain model identity; content-addressed deduplication is optional.
    if unit.inferenceEvidence.nonEmpty != status.nonEmpty then
      return Left(EstimateError.Invalid("inference evidence and status inventory must be present together"))
    if compact && status.nonEmpty then return Left(EstimateError.Unsupported("Core-3 refuses compact covariance coexistence"))
    for
      _ <- status match
        case Some(value) => value.validate(unit).flatMap(_ => objects.verify(verified(value.file)).left.map(fromStore))
        case None => Right(())
      catalog <- writeText(s"$prefix/estimands.json", EstimateMetadata.catalog(unit.catalog))
      estimands <- writeText(s"$prefix/estimands.tsv", EstimateMetadata.estimandsTsv(unit.catalog))
      observations <- writeText(s"$prefix/observations.tsv", EstimateMetadata.observationsTsv(unit))
      tables = EstimateIndexTables(estimands, observations)
      manifest <- writeText(s"$prefix/estimates.json", if status.nonEmpty then
        EstimateMetadata.inferenceUnit(unit, catalog, representations.collect { case EstimateRepresentation.Nifti(value) => value }, tables, status.get)
        else if compact then EstimateMetadata.compactUnit(unit, catalog, representations, tables)
        else EstimateMetadata.unit(unit, catalog, representations.collect { case EstimateRepresentation.Nifti(value) => value }, Some(tables)))
    yield PinnedUnit(unit.unit, unit.revision, manifest)

  /** Metadata-only hook. The backend must close and re-inspect its actual
    * datasets before supplying these digest-pinned container declarations.
    */
  private[io] def publishHdf5Unit(unit: EstimateUnit, records: Vector[Hdf5Representation]): Either[EstimateError, PinnedUnit] =
    val prefix = s"units/${unit.revision.value}"
    for
      _ <- Hdf5Representation.validateInventory(unit, records)
      _ <- verifyEstimability(unit)
      _ <- records.map(_.container).distinct.foldLeft[Either[EstimateError, Unit]](Right(())):
        (previous, container) => previous.flatMap(_ => objects.verify(verified(container)).left.map(fromStore))
      catalog <- writeText(s"$prefix/estimands.json", EstimateMetadata.catalog(unit.catalog))
      estimands <- writeText(s"$prefix/estimands.tsv", EstimateMetadata.estimandsTsv(unit.catalog))
      observations <- writeText(s"$prefix/observations.tsv", EstimateMetadata.observationsTsv(unit))
      encoded <- EstimateMetadata.hdf5Unit(unit, catalog, records, EstimateIndexTables(estimands, observations))
      manifest <- writeText(s"$prefix/estimates.json", encoded)
    yield PinnedUnit(unit.unit, unit.revision, manifest)

  private[io] def inspectWithRepresentations(reference: PinnedUnit): Either[EstimateError, (EstimateUnit, Vector[EstimateRepresentation], Option[InferenceStatusRepresentation])] =
    for
      manifest <- text(reference.manifest)
      catalogRef <- EstimateMetadata.catalogReference(manifest)
      catalogText <- text(catalogRef)
      catalog <- EstimateMetadata.readCatalog(catalogText)
      schema <- EstimateMetadata.schema(manifest, "unit")
      catalogSchema <- EstimateMetadata.schema(catalogText, "catalog")
      _ <- if schema == EstimateMetadata.developmentSchema || catalogSchema == EstimateMetadata.coreSchema then Right(())
           else Left(EstimateError.Integrity("Core-NIfTI unit requires a Core-NIfTI catalog"))
      unit <- EstimateMetadata.readUnit(manifest, catalog)
      tables <- EstimateMetadata.indexTables(manifest)
      _ <- tables match
        case None => Right(()) // development-1 legacy documents predate the TSV projection
        case Some(index) =>
          for
            estimands <- text(index.estimands)
            observations <- text(index.observations)
            _ <- if estimands == EstimateMetadata.estimandsTsv(catalog) &&
                    observations == EstimateMetadata.observationsTsv(unit) then Right(())
                 else Left(EstimateError.Integrity("TSV axis projections disagree with authoritative JSON"))
          yield ()
      _ <- if unit.unit == reference.unit && unit.revision == reference.revision then Right(())
           else Left(EstimateError.Integrity("pinned unit identity does not match the manifest"))
      representations <- EstimateMetadata.allRepresentations(manifest)
      status <- EstimateMetadata.inferenceStatus(manifest)
      _ <- if schema == EstimateMetadata.developmentSchema || representations.forall {
             case EstimateRepresentation.Nifti(value) => value.storedDatatype.nonEmpty
             case EstimateRepresentation.SharedNormalizedUpperTriangle(_) => true
             case EstimateRepresentation.Hdf5(_) => schema == EstimateMetadata.hdf5Schema
           } then Right(())
           else Left(EstimateError.Integrity("Core-NIfTI representations require explicit physical stored datatype"))
      _ <- if schema == EstimateMetadata.developmentSchema then Right(())
           else
             val expected = unit.products.flatMap(p => p.observations.map(o => p.id -> o)).toSet
             val actual = representations.map(r => r.product -> r.observation)
             if actual.distinct.size == actual.size && actual.toSet == expected then Right(())
             else Left(EstimateError.Integrity("Core-NIfTI representation inventory must exactly cover declared pairs"))
    yield (unit, representations, status)

  private def niftiOnly(representations: Vector[EstimateRepresentation]): Either[EstimateError, Unit] =
    if representations.exists(_.isInstanceOf[EstimateRepresentation.Hdf5]) then
      Left(EstimateError.Unsupported("default NIfTI store refuses HDF5 units"))
    else Right(())

  def inspect(reference: PinnedUnit): Either[EstimateError, EstimateUnit] =
    inspectWithRepresentations(reference).flatMap((unit, records, _) => niftiOnly(records).map(_ => unit))

  def open(reference: PinnedUnit, limits: ReadLimits): Either[EstimateError, EstimateSource] =
    inspectWithRepresentations(reference).flatMap: (unit, representations, status) =>
      NiftiEstimateSource.preflight(this, unit, representations, limits, status)
        .flatMap(_ => verifyEstimability(unit)).flatMap(_ => NiftiEstimateSource.openMixed(this, unit, representations, limits, status))

  def newSink(unit: EstimateUnit, maximumBlockCells: Int): Either[EstimateError, EstimateSink] =
    verifyEstimability(unit).flatMap(_ => NiftiEstimateSink.open(this, unit, maximumBlockCells))

  def newSink(unit: EstimateUnit, maximumBlockCells: Int, covarianceLayout: CovarianceLayout): Either[EstimateError, EstimateSink] =
    verifyEstimability(unit).flatMap(_ => NiftiEstimateSink.open(this, unit, maximumBlockCells, covarianceLayout))

  /** Explicit evidence-bearing PairNifti route. Default factories refuse evidence. */
  def newInferenceSink(unit: EstimateUnit, maximumBlockCells: Int,
      covarianceLayout: CovarianceLayout = CovarianceLayout.PairNifti): Either[EstimateError, InferenceEvidenceSink] =
    verifyEstimability(unit).flatMap(_ => NiftiEstimateSink.open(this, unit, maximumBlockCells, covarianceLayout, inference = true))

  private def inspectDefaultPublished(reference: PinnedUnit): Either[EstimateError, EstimateUnit] =
    inspectWithRepresentations(reference).flatMap: (unit, records, status) =>
      for
        _ <- niftiOnly(records)
        _ <- status match
          case Some(value) => objects.verify(verified(value.file)).left.map(fromStore)
          case None => Right(())
      yield unit

  private val collections = new LocalEstimateCollections(this, inspectDefaultPublished)

  def publishCollection(collection: EstimateCollection): Either[EstimateError, PinnedEstimateSet] = collections.publish(collection)
  def openCollection(reference: PinnedEstimateSet): Either[EstimateError, EstimateCollection] = collections.open(reference)
  def current(): Either[EstimateError, Option[(PinnedEstimateSet, scalafim.archive.ContentDigest)]] = collections.current()
  def discover(reference: PinnedEstimateSet, expectedPointerDigest: Option[scalafim.archive.ContentDigest]): Either[EstimateError, Unit] =
    collections.discover(reference, expectedPointerDigest)

object LocalEstimateStore:
  def open(root: Path): Either[EstimateError, LocalEstimateStore] =
    LocalObjectStore.open(root).left.map(e => EstimateError.Io(e.message)).map(new LocalEstimateStore(_))
