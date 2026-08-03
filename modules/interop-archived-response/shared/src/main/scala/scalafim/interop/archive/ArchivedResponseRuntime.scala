package scalafim.interop.archive

import cats.arrow.FunctionK
import cats.data.{EitherT, NonEmptyChain}
import cats.effect.{Async, Resource}
import cats.{Applicative, Functor}
import scalafim.archive.{
  ArchiveDriver,
  ArchiveError,
  ArchiveLocation,
  ArchiveResource,
  ArchiveRevision,
  ArchiveRevisionId,
  ArchiveValidation,
  ArchiveValidationScope,
  CanonicalValue,
  ContentDigest,
  OpenArchive,
  PayloadDescriptor,
  PayloadExecutor,
  PayloadId,
  PublicationStatus,
  RepresentationKey
}
import scalafim.dataset.{
  AcquisitionContext,
  AttachmentIssue,
  FmriDataset,
  OpenedDataset
}
import scalafim.response.ResponseSource

enum RuntimeOpenError:
  case Archive(error: ArchiveError)
  case Attachment(issues: NonEmptyChain[AttachmentIssue])

  def message: String =
    this match
      case Archive(error) =>
        error.message
      case Attachment(issues) =>
        issues.toNonEmptyList.toList.map(_.message).mkString("; ")

type RuntimeResource[F[_], A] =
  Resource[[X] =>> EitherT[F, RuntimeOpenError, X], A]

final class PayloadCatalog private (
    val values: Vector[PayloadDescriptor]
):
  private val byId: Map[PayloadId, PayloadDescriptor] =
    values.iterator.map(value => value.id -> value).toMap

  def get(id: PayloadId): Option[PayloadDescriptor] =
    byId.get(id)

object PayloadCatalog:
  def from(values: Iterable[PayloadDescriptor]): PayloadCatalog =
    new PayloadCatalog(values.toVector.sortBy(_.id.value))

final case class RepresentationEnvelope(
    key: RepresentationKey,
    descriptor: CanonicalValue,
    payloads: PayloadCatalog,
    outputSchema: CanonicalValue
)

object RepresentationEnvelope:
  def fromRevision(
      revision: ArchiveRevision
  ): Either[ArchiveError, RepresentationEnvelope] =
    revision.manifest.representation
      .toRight(
        ArchiveError.InvalidArchive(
          "response archive has no representation descriptor envelope"
        )
      )
      .map: persisted =>
        RepresentationEnvelope(
          persisted.key,
          persisted.descriptor,
          PayloadCatalog.from(revision.manifest.payloads),
          persisted.outputSchema
        )

final class ArchiveResponseAccess[F[_]] private[archive] (
    val location: ArchiveLocation,
    val revisionId: ArchiveRevisionId,
    val rootDigest: ContentDigest,
    val payloads: PayloadExecutor[F],
    contentValidation: EitherT[F, ArchiveError, ArchiveValidation]
):
  def validateContents: EitherT[F, ArchiveError, ArchiveValidation] =
    contentValidation

trait ArchivedResponseFamily[F[_]]:
  def key: RepresentationKey

  def open(
      envelope: RepresentationEnvelope,
      archive: ArchiveResponseAccess[F]
  ): ArchiveResource[F, ResponseSource[F]]

enum RuntimeRegistryError:
  case DuplicateRepresentation(key: RepresentationKey)
  case DuplicateArchiveDriver(id: ArchiveDriverId)

  def message: String =
    this match
      case DuplicateRepresentation(key) =>
        s"duplicate archived-response family '${key.value}'"
      case DuplicateArchiveDriver(id) =>
        s"duplicate archive driver '${id.value}'"

final class ArchivedResponseRegistry[F[_]] private (
    private val entries: Map[RepresentationKey, ArchivedResponseFamily[F]],
    val supportedKeys: Vector[RepresentationKey]
):
  def resolve(
      key: RepresentationKey
  ): Either[ArchiveError, ArchivedResponseFamily[F]] =
    entries
      .get(key)
      .toRight(ArchiveError.UnsupportedRepresentation(key, supportedKeys))

object ArchivedResponseRegistry:
  def build[F[_]](
      families: ArchivedResponseFamily[F]*
  ): Either[RuntimeRegistryError, ArchivedResponseRegistry[F]] =
    val ordered = families.toVector.sortBy(_.key.value)
    firstDuplicate(ordered.map(_.key.value)) match
      case Some(value) =>
        Left(RuntimeRegistryError.DuplicateRepresentation(
          RepresentationKey.unsafe(value)
        ))
      case None =>
        Right(new ArchivedResponseRegistry(
          ordered.iterator.map(family => family.key -> family).toMap,
          ordered.map(_.key)
        ))

  private def firstDuplicate(
      ordered: Vector[String]
  ): Option[String] =
    var index = 1
    while index < ordered.length do
      if ordered(index - 1) == ordered(index) then
        return Some(ordered(index))
      index += 1
    None

opaque type ArchiveDriverId = String

object ArchiveDriverId:
  def fromString(value: String): Either[ArchiveError, ArchiveDriverId] =
    val normalized = value.trim
    if normalized.isEmpty then
      Left(ArchiveError.InvalidArchive("archive driver id must be non-empty"))
    else if normalized.exists(character =>
        character.isWhitespace || character.isControl
      )
    then
      Left(ArchiveError.InvalidArchive(
        "archive driver id must not contain whitespace or control characters"
      ))
    else Right(normalized)

  def unsafe(value: String): ArchiveDriverId =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ArchiveDriverId)
    inline def value: String =
      id

final class ArchiveDriverRegistration[F[_]] private (
    val id: ArchiveDriverId,
    val driver: ArchiveDriver[F],
    acceptsLocation: ArchiveLocation => Boolean
):
  def accepts(location: ArchiveLocation): Boolean =
    acceptsLocation(location)

object ArchiveDriverRegistration:
  def make[F[_]](
      id: ArchiveDriverId,
      driver: ArchiveDriver[F],
      accepts: ArchiveLocation => Boolean
  ): ArchiveDriverRegistration[F] =
    new ArchiveDriverRegistration(id, driver, accepts)

final class ArchiveDrivers[F[_]] private (
    val entries: Vector[ArchiveDriverRegistration[F]]
):
  def select(
      location: ArchiveLocation
  ): Either[ArchiveError, ArchiveDriver[F]] =
    entries.filter(_.accepts(location)) match
      case Vector(found) =>
        Right(found.driver)
      case Vector() =>
        Left(ArchiveError.UnsupportedStorage(
          s"no installed archive driver accepts '${location.value}'"
        ))
      case many =>
        Left(ArchiveError.UnsupportedStorage(
          s"multiple archive drivers accept '${location.value}': " +
            many.map(_.id.value).mkString(", ")
        ))

  def open(
      location: ArchiveLocation
  )(using Applicative[F]): ArchiveResource[F, OpenArchive[F]] =
    Resource
      .eval(EitherT.fromEither[F](select(location)))
      .flatMap(_.open(location))

object ArchiveDrivers:
  def build[F[_]](
      registrations: ArchiveDriverRegistration[F]*
  ): Either[RuntimeRegistryError, ArchiveDrivers[F]] =
    val ordered = registrations.toVector.sortBy(_.id.value)
    var index = 1
    while index < ordered.length do
      if ordered(index - 1).id == ordered(index).id then
        return Left(RuntimeRegistryError.DuplicateArchiveDriver(
          ordered(index).id
        ))
      index += 1
    Right(new ArchiveDrivers(ordered))

final class ScalafimRuntime[F[_]] private (
    val archives: ArchiveDrivers[F],
    val responses: ArchivedResponseRegistry[F]
)(using F: Async[F]):
  def openResponse(
      location: ArchiveLocation
  ): ArchiveResource[F, ResponseSource[F]] =
    archives.open(location).flatMap: opened =>
      Resource
        .eval(EitherT.fromEither[F](
          for
            _ <- ScalafimRuntime.validateStructure(opened)
            digest <- ScalafimRuntime.validatePublication(opened)
            envelope <- RepresentationEnvelope.fromRevision(opened.revision)
            family <- responses.resolve(envelope.key)
            access = new ArchiveResponseAccess(
              location,
              opened.revision.id,
              digest,
              opened.payloads,
              opened.validateContents
            )
          yield (envelope, access, family)
        ))
        .flatMap: (envelope, access, family) =>
          family.open(envelope, access)

  def openDataset(
      location: ArchiveLocation,
      dataset: FmriDataset,
      acquisition: AcquisitionContext
  ): RuntimeResource[F, OpenedDataset[F]] =
    openResponse(location)
      .mapK(ScalafimRuntime.liftArchiveErrors[F])
      .flatMap: source =>
        Resource.eval(
          EitherT.fromEither[F](
            OpenedDataset
              .attach(dataset, source, acquisition)
              .toEither
              .left
              .map(RuntimeOpenError.Attachment.apply)
          )
        )

object ScalafimRuntime:
  def make[F[_]: Async](
      archives: ArchiveDrivers[F],
      responses: ArchivedResponseRegistry[F]
  ): ScalafimRuntime[F] =
    new ScalafimRuntime(archives, responses)

  private def liftArchiveErrors[F[_]: Functor]
      : FunctionK[
        [X] =>> EitherT[F, ArchiveError, X],
        [X] =>> EitherT[F, RuntimeOpenError, X]
      ] =
    new FunctionK[
      [X] =>> EitherT[F, ArchiveError, X],
      [X] =>> EitherT[F, RuntimeOpenError, X]
    ]:
      def apply[A](
          effect: EitherT[F, ArchiveError, A]
      ): EitherT[F, RuntimeOpenError, A] =
        effect.leftMap(RuntimeOpenError.Archive.apply)

  private def validateStructure(
      opened: OpenArchive[?]
  ): Either[ArchiveError, Unit] =
    val expected =
      opened.revision.manifest.payloads.map(_.id.value).sorted
    val checked =
      opened.structure.checkedPayloads.map(_.value).sorted
    if opened.structure.scope != ArchiveValidationScope.Structure then
      Left(ArchiveError.InvalidArchive(
        "archive driver did not report structural validation"
      ))
    else if checked != expected then
      Left(ArchiveError.InvalidArchive(
        "archive structural validation does not cover the declared payload catalog"
      ))
    else Right(())

  private def validatePublication(
      opened: OpenArchive[?]
  ): Either[ArchiveError, ContentDigest] =
    opened.revision.publication match
      case PublicationStatus.Published(digest) =>
        Right(digest)
      case PublicationStatus.Staging =>
        Left(ArchiveError.IncompletePublication(
          "staging revisions cannot open as responses"
        ))
      case PublicationStatus.Incomplete(reason) =>
        Left(ArchiveError.IncompletePublication(reason.value))
