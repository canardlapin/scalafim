package scalafim.archive

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.all.*

final case class CanonicalPayload private (
    descriptor: PayloadDescriptor,
    bytes: Vector[Byte]
)

object CanonicalPayload:
  def from(
      descriptor: PayloadDescriptor,
      bytes: Iterable[Byte]
  ): Either[ArchiveError, CanonicalPayload] =
    val copied = bytes.toVector
    if copied.isEmpty then
      Left(ArchiveError.InvalidArchive(
        s"canonical payload '${descriptor.id.value}' must contain logical bytes"
      ))
    else Right(CanonicalPayload(descriptor, copied))

final class CanonicalArchiveDocument private (
    val manifest: ArchiveManifest,
    val payloads: Vector[CanonicalPayload]
):
  def payload(id: PayloadId): Option[CanonicalPayload] =
    payloads.find(_.descriptor.id == id)

object CanonicalArchiveDocument:
  def from(
      manifest: ArchiveManifest,
      payloads: Iterable[CanonicalPayload]
  ): Either[ArchiveError, CanonicalArchiveDocument] =
    val copied = payloads.toVector.sortBy(_.descriptor.id.value)
    val actual = copied.map(_.descriptor.id.value)
    val expected = manifest.payloads.map(_.id.value)
    val duplicates =
      actual.groupBy(identity).collectFirst:
        case (id, values) if values.lengthCompare(1) > 0 => id
    duplicates match
      case Some(id) =>
        Left(ArchiveError.InvalidArchive(
          s"canonical document contains duplicate payload '$id'"
        ))
      case None if actual != expected =>
        Left(ArchiveError.InvalidArchive(
          "canonical document payload ids do not exactly match the manifest"
        ))
      case None =>
        val mismatch =
          copied.iterator
            .zip(manifest.payloads.iterator)
            .collectFirst:
              case (payload, descriptor)
                  if payload.descriptor != descriptor =>
                descriptor.id
        mismatch match
          case Some(id) =>
            Left(ArchiveError.InvalidArchive(
              s"canonical payload '${id.value}' descriptor differs from its manifest entry"
            ))
          case None =>
            Right(new CanonicalArchiveDocument(manifest, copied))

enum CanonicalArchiveWriteStep:
  case BeginStaging
  case WritePayload(id: PayloadId)
  case WriteManifest
  case Publish

final class CanonicalArchiveWritePlan private (
    val document: CanonicalArchiveDocument,
    val encodedManifest: String,
    val steps: Vector[CanonicalArchiveWriteStep]
)

object CanonicalArchiveWritePlan:
  def from(
      document: CanonicalArchiveDocument
  ): Either[ArchiveError, CanonicalArchiveWritePlan] =
    val unqualifiedRole =
      document.manifest.payloads.collectFirst:
        case payload if !payload.role.isNamespaced => payload.role
    unqualifiedRole match
      case Some(role) =>
        Left(ArchiveError.InvalidArchive(
          s"canonical writing requires a namespaced payload role; found '${role.value}'"
        ))
      case None =>
        val encoded =
          CanonicalArchiveManifestCodec.render(document.manifest)
        CanonicalArchiveManifestCodec.parse(encoded).map: _ =>
          val steps =
            Vector(CanonicalArchiveWriteStep.BeginStaging) ++
              document.payloads.map(payload =>
                CanonicalArchiveWriteStep.WritePayload(
                  payload.descriptor.id
                )
              ) ++
              Vector(
                CanonicalArchiveWriteStep.WriteManifest,
                CanonicalArchiveWriteStep.Publish
              )
          new CanonicalArchiveWritePlan(document, encoded, steps)

trait CanonicalArchiveTransaction[F[_]]:
  def writePayload(
      payload: CanonicalPayload
  ): EitherT[F, ArchiveError, Unit]

  def writeManifest(
      encoded: String
  ): EitherT[F, ArchiveError, Unit]

  def publish: EitherT[F, ArchiveError, ContentDigest]

trait CanonicalArchiveSink[F[_]]:
  def stage(
      plan: CanonicalArchiveWritePlan
  ): ArchiveResource[F, CanonicalArchiveTransaction[F]]

enum CanonicalArchiveWriteVisibility:
  case Absent
  case Published(digest: ContentDigest)

final case class CanonicalArchiveWriteReceipt(
    completed: Vector[CanonicalArchiveWriteStep],
    visibility: CanonicalArchiveWriteVisibility,
    logicalPayloads: Vector[LogicalPayloadIdentity]
)

object CanonicalArchiveWriter:
  def execute[F[_]: Async](
      plan: CanonicalArchiveWritePlan,
      sink: CanonicalArchiveSink[F]
  ): EitherT[F, ArchiveError, CanonicalArchiveWriteReceipt] =
    sink.stage(plan).use: transaction =>
      runSteps(
        plan,
        transaction,
        plan.steps
      ).flatMap:
        case Some(digest) =>
          EitherT.rightT(
            CanonicalArchiveWriteReceipt(
              plan.steps,
              CanonicalArchiveWriteVisibility.Published(digest),
              logicalPayloads(plan)
            )
          )
        case None =>
          EitherT.leftT(ArchiveError.IncompletePublication(
            "canonical write completed without publication"
          ))

  def executeProperPrefix[F[_]: Async](
      plan: CanonicalArchiveWritePlan,
      sink: CanonicalArchiveSink[F],
      completedSteps: Int
  ): EitherT[F, ArchiveError, CanonicalArchiveWriteReceipt] =
    if completedSteps < 0 || completedSteps >= plan.steps.length then
      EitherT.leftT(ArchiveError.InvalidArchive(
        s"canonical write prefix $completedSteps must be in [0, ${plan.steps.length})"
      ))
    else if completedSteps == 0 then
      EitherT.rightT(
        CanonicalArchiveWriteReceipt(
          Vector.empty,
          CanonicalArchiveWriteVisibility.Absent,
          logicalPayloads(plan)
        )
      )
    else
      val completed = plan.steps.take(completedSteps)
      sink.stage(plan).use: transaction =>
        runSteps(plan, transaction, completed).flatMap:
          case None =>
            EitherT.rightT(
              CanonicalArchiveWriteReceipt(
                completed,
                CanonicalArchiveWriteVisibility.Absent,
                logicalPayloads(plan)
              )
            )
          case Some(_) =>
            EitherT.leftT(ArchiveError.InvalidArchive(
              "a proper canonical write prefix must not publish"
            ))

  private def runSteps[F[_]: Async](
      plan: CanonicalArchiveWritePlan,
      transaction: CanonicalArchiveTransaction[F],
      steps: Vector[CanonicalArchiveWriteStep]
  ): EitherT[F, ArchiveError, Option[ContentDigest]] =
    steps.foldLeft(
      EitherT.rightT[F, ArchiveError](Option.empty[ContentDigest])
    ): (state, step) =>
      state.flatMap:
        case published @ Some(_) =>
          EitherT.leftT(ArchiveError.InvalidArchive(
            s"canonical write contains step '$step' after publication"
          ))
        case None =>
          step match
            case CanonicalArchiveWriteStep.BeginStaging =>
              EitherT.rightT(None)
            case CanonicalArchiveWriteStep.WritePayload(id) =>
              plan.document
                .payload(id)
                .toRight(ArchiveError.InvalidArchive(
                  s"canonical write plan references unknown payload '${id.value}'"
                ))
                .fold(
                  error =>
                    EitherT.leftT[
                      F,
                      Option[ContentDigest]
                    ](error),
                  payload => transaction.writePayload(payload).as(None)
                )
            case CanonicalArchiveWriteStep.WriteManifest =>
              transaction.writeManifest(plan.encodedManifest).as(None)
            case CanonicalArchiveWriteStep.Publish =>
              transaction.publish.map(Some(_))

  private def logicalPayloads(
      plan: CanonicalArchiveWritePlan
  ): Vector[LogicalPayloadIdentity] =
    plan.document.manifest.payloads.map(_.logicalIdentity)
