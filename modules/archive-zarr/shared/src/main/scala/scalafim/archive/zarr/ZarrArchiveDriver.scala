package scalafim.archive.zarr

import cats.arrow.FunctionK
import cats.data.EitherT
import cats.effect.{Async, Resource}
import cats.effect.std.Semaphore
import cats.syntax.all.*
import scalafim.archive.{
  ArchiveDriver,
  ArchiveError,
  ArchiveFormatKey,
  ArchiveLocation,
  ArchiveManifest,
  ArchiveProvenance,
  ArchiveReadReceipt,
  ArchiveResource,
  ArchiveRevision,
  ArchiveRevisionId,
  ArchiveValidation,
  ArchiveValidationScope,
  CanonicalKey,
  CanonicalValue,
  ContentDigest,
  IntegrityManifest,
  LocalityClaim,
  ObjectKey,
  ObjectTypeId,
  Observed,
  OpenArchive,
  PayloadDescriptor,
  PayloadExecutor,
  PayloadId as ArchivePayloadId,
  PayloadPlan,
  PayloadPlanSummary,
  PhysicalLocality,
  PhysicalObjectId,
  ReadObservation,
  ReceiptAccumulator,
  RepresentationKey,
  ScalarTypeId,
  SelectionAxes
}
import scalafim.zarr.*

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

final case class ZarrPayloadPlan private (
    selection: ArraySelection,
    limits: ReadLimits,
    summary: PayloadPlanSummary
) extends PayloadPlan[ReadResult]

object ZarrPayloadPlan:
  def from(
      payload: ArchivePayloadId,
      selection: ArraySelection,
      axes: SelectionAxes,
      coveringObjects: Iterable[PhysicalObjectId],
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): Either[ArchiveError, ZarrPayloadPlan] =
    chunkBounded(payload, selection, axes, coveringObjects, limits)

  def chunkBounded(
      payload: ArchivePayloadId,
      selection: ArraySelection,
      axes: SelectionAxes,
      coveringObjects: Iterable[PhysicalObjectId],
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): Either[ArchiveError, ZarrPayloadPlan] =
    checkedLimits(limits).flatMap: checked =>
      LocalityClaim.chunkBounded(axes, coveringObjects).map: locality =>
        ZarrPayloadPlan(
          selection,
          checked,
          PayloadPlanSummary(payload, "zarr-read", locality)
        )

  def byteRangeBounded(
      payload: ArchivePayloadId,
      selection: ArraySelection,
      axes: SelectionAxes,
      coveringRanges: Iterable[scalafim.archive.ObjectRange],
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): Either[ArchiveError, ZarrPayloadPlan] =
    checkedLimits(limits).flatMap: checked =>
      LocalityClaim.byteRangeBounded(axes, coveringRanges).map: locality =>
        ZarrPayloadPlan(
          selection,
          checked,
          PayloadPlanSummary(payload, "zarr-read", locality)
        )

  private def checkedLimits(
      limits: ReadLimits
  ): Either[ArchiveError, ReadLimits] =
    if limits.maxConcurrentRequests != 1 then
      Left(ArchiveError.InvalidArchive(
        "receipt-bearing Zarr plans currently require maxConcurrentRequests = 1 " +
          "to preserve deterministic physical-attempt order"
      ))
    else Right(limits)

/** Resource-safe adapter over the portable asynchronous Zarr path.
  *
  * Platform modules supply the byte-codec runtime: the JVM uses
  * `JvmAsyncCodecRuntime`, while Scala.js uses `BrowserCodecRuntime`.
  */
final class ZarrArchiveDriver[F[_]] private (
    openStore: ArchiveLocation => Resource[F, AsyncObjectReader],
    capabilities: ZarrCapabilities,
    limits: ProfileOpenLimits,
    runtime: AsyncCodecRuntime
)(using F: Async[F]) extends ArchiveDriver[F]:

  def open(location: ArchiveLocation): ArchiveResource[F, OpenArchive[F]] =
    val liftedStore = openStore(location).mapK:
      new FunctionK[F, [A] =>> EitherT[F, ArchiveError, A]]:
        def apply[A](effect: F[A]): EitherT[F, ArchiveError, A] =
          EitherT.liftF(effect)

    liftedStore.flatMap: store =>
      Resource.eval(EitherT:
        for
          executionContext <- F.executionContext
          result <- F.delay(
            new ObservingAsyncObjectReader(store)(using executionContext)
          ).flatMap: observing =>
            F.fromFuture(F.delay:
              AsyncNeuroArchiveZarr.openCanonical(
                observing,
                capabilities,
                limits,
                runtime
              )(using executionContext)
            ).map:
              _.left
                .map(ZarrArchiveDriver.archiveError)
                .flatMap: opened =>
                  observing.drain().flatMap: _ =>
                    ZarrArchiveDriver
                      .revision(opened)
                      .map((observing, opened, _))
        yield result
      ).flatMap: (observing, opened, revision) =>
        Resource
          .eval(EitherT.liftF(Semaphore[F](1)))
          .map: gate =>
            new OpenedZarrArchive[F](observing, opened, revision, gate)

object ZarrArchiveDriver:
  def using[F[_]: Async](
      openStore: ArchiveLocation => Resource[F, AsyncObjectReader],
      runtime: AsyncCodecRuntime,
      capabilities: ZarrCapabilities = ZarrCapabilities(),
      limits: ProfileOpenLimits = ProfileOpenLimits.default
  ): ZarrArchiveDriver[F] =
    new ZarrArchiveDriver[F](openStore, capabilities, limits, runtime)

  def fixed[F[_]: Async](
      store: AsyncObjectReader,
      runtime: AsyncCodecRuntime,
      capabilities: ZarrCapabilities = ZarrCapabilities(),
      limits: ProfileOpenLimits = ProfileOpenLimits.default
  ): ZarrArchiveDriver[F] =
    using(_ => Resource.pure(store), runtime, capabilities, limits)

  private def revision(
      opened: AsyncOpenedCanonicalBold
  ): Either[ArchiveError, ArchiveRevision] =
    val profile = opened.canonical
    val profileManifest = profile.manifest
    val payloadId = ArchivePayloadId.unsafe(profileManifest.payloadId.value)
    for
      attributes <- DenseBoldRevisionMetadata.attributes(opened)
      representation <-
        DenseBoldRevisionMetadata.persistedRepresentation(opened)
      descriptor <- PayloadDescriptor.from(
        payloadId,
        DenseBoldRevisionMetadata.PayloadRole,
        ScalarTypeId.unsafe(profile.array.dataType.name),
        profile.array.shape.toVector
      )
      key <- ObjectKey.from(
        ObjectTypeId.unsafe("org.scalafim/fmri-response"),
        1,
        Some(DenseBoldRevisionMetadata.Representation)
      )
      manifest <- ArchiveManifest.from(
        ArchiveFormatKey.unsafe("neuroarchive-zarr@1"),
        key,
        Some(representation),
        attributes,
        Vector(descriptor),
        IntegrityManifest.Empty
      )
      rootDigest <- ContentDigest.sha256(profileManifest.contentRevision.value)
      revisionId <- ArchiveRevisionId.fromString(
        s"zarr-${profileManifest.contentRevision.value}"
      )
      creator <- scalafim.archive.CreatorId(opened.publication.writerVersion)
      revision <- ArchiveRevision.from(
        revisionId,
        manifest,
        scalafim.archive.PublicationStatus.Published(rootDigest),
        ArchiveProvenance(creator)
      )
    yield revision

  private def archiveError(error: NeuroArchiveZarrError): ArchiveError =
    error match
      case NeuroArchiveZarrError.IncompletePublication(detail) =>
        ArchiveError.IncompletePublication(detail)
      case NeuroArchiveZarrError.PublicationFailure(detail) =>
        ArchiveError.IncompletePublication(detail)
      case other =>
        ArchiveError.InvalidArchive(other.message)

private final class OpenedZarrArchive[F[_]](
    reader: ObservingAsyncObjectReader,
    opened: AsyncOpenedCanonicalBold,
    val revision: ArchiveRevision,
    gate: Semaphore[F]
)(using F: Async[F]) extends OpenArchive[F]:

  val structure: ArchiveValidation =
    ArchiveValidation(
      ArchiveValidationScope.Structure,
      revision.manifest.payloads.map(_.id),
      Vector.empty
    )

  val payloads: PayloadExecutor[F] =
    new PayloadExecutor[F]:
      def execute[A](
          plan: PayloadPlan[A]
      ): EitherT[F, ArchiveError, Observed[A]] =
        plan match
          case concrete: ZarrPayloadPlan =>
            EitherT(gate.permit.use(_ => executeZarr(concrete)))
          case other =>
            EitherT.leftT(ArchiveError.UnsupportedPayloadPlan(
              "neuroarchive-zarr",
              other.summary.operation
            ))

      private def executeZarr(
          plan: ZarrPayloadPlan
      ): F[Either[ArchiveError, Observed[ReadResult]]] =
        for
          _ <- F.delay(reader.clear())
          executionContext <- F.executionContext
          read <- F.fromFuture(F.delay:
            plan.selection match
              case ArraySelection.RegionSelection(region) =>
                opened.array.readRegion(region, plan.limits)
              case ArraySelection.PointSelection(points) =>
                opened.array.readPoints(points, plan.limits)
              case ArraySelection.Factored(selection) =>
                opened.array.read(selection, plan.limits)
          )
          result <- F.delay:
            for
              readResult <- read.left.map(error =>
                ArchiveError.UnsupportedStorage(error.message)
              )
              observations <- reader.drain()
              receipt <- ArchiveReadReceipt.from(
                plan.summary,
                plan.summary.locality.locality,
                ReceiptAccumulator.from(observations)
              )
              _ <-
                if readResult.receipt.bytesRead == receipt.physicalBytes then Right(())
                else Left(ArchiveError.ReceiptMismatch(
                  s"Zarr reported ${readResult.receipt.bytesRead} bytes but observations " +
                    s"recorded ${receipt.physicalBytes}"
                ))
            yield Observed(readResult, receipt)
        yield result

  val validateContents: EitherT[F, ArchiveError, ArchiveValidation] =
    EitherT(gate.permit.use: _ =>
      for
        _ <- F.delay(reader.clear())
        executionContext <- F.executionContext
        result <- F.fromFuture(F.delay:
          validatePublishedObjects(using executionContext)
        )
        _ <- F.delay(reader.clear())
      yield result
    )

  private def validatePublishedObjects(
      using ExecutionContext
  ): Future[Either[ArchiveError, ArchiveValidation]] =
    def loop(index: Int): Future[Either[ArchiveError, ArchiveValidation]] =
      if index >= opened.publication.objects.length then
        Future.successful(Right(ArchiveValidation(
          ArchiveValidationScope.Contents,
          revision.manifest.payloads.map(_.id),
          Vector.empty
        )))
      else
        val expected = opened.publication.objects(index)
        reader.readAll(expected.key, expected.length).flatMap:
          case Left(error) =>
            Future.successful(Left(ArchiveError.UnsupportedStorage(error.message)))
          case Right(bytes) =>
            val actual = Sha256.digest(bytes.toArray)
            if actual != expected.sha256 then
              Future.successful(Left(ArchiveError.ValidationFailed(Vector(
                scalafim.archive.ArchiveValidationIssue(
                  scalafim.archive.ArchiveValidationLayer.Checksum,
                  s"${expected.key.value} SHA-256 mismatch"
                )
              ))))
            else loop(index + 1)
    loop(0)

private final case class ObservationSlot(
    var completed: Boolean,
    var observation: Option[ReadObservation]
)

private final class ObservingAsyncObjectReader(
    underlying: AsyncObjectReader
)(using ExecutionContext) extends AsyncObjectReader:
  private val observations = mutable.ArrayBuffer.empty[ObservationSlot]
  private var observationError = Option.empty[ArchiveError]

  def read(
      key: StoreKey,
      range: scalafim.zarr.ByteRange
  ): Future[Either[StoreError, OwnedBytes]] =
    val slot = reserve()
    underlying.read(key, range).map: result =>
      result match
        case Right(bytes) =>
          complete(
            slot,
            for
              objectId <- PhysicalObjectId.fromString(key.value)
              interval <- scalafim.archive.ByteInterval.from(
                range.offset,
                range.length.toLong
              )
              observation <- ReadObservation.byteRange(
                objectId,
                interval,
                bytes.byteCount.toLong
              )
            yield Some(observation)
          )
        case Left(_) =>
          complete(slot, Right(None))
      result

  def readAll(
      key: StoreKey,
      maxBytes: ByteCount
  ): Future[Either[StoreError, OwnedBytes]] =
    val slot = reserve()
    underlying.readAll(key, maxBytes).map: result =>
      result match
        case Right(bytes) =>
          complete(
            slot,
            PhysicalObjectId
              .fromString(key.value)
              .flatMap(ReadObservation.wholeObject(_, bytes.byteCount.toLong))
              .map(Some(_))
          )
        case Left(_) =>
          complete(slot, Right(None))
      result

  def length(key: StoreKey): Future[Either[StoreError, Long]] =
    val slot = reserve()
    underlying.length(key).map: result =>
      result match
        case Right(value) =>
          complete(
            slot,
            PhysicalObjectId
              .fromString(key.value)
              .flatMap(ReadObservation.objectLength(_, value))
              .map(Some(_))
          )
        case Left(_) =>
          complete(slot, Right(None))
      result

  def clear(): Unit = this.synchronized:
    observations.clear()
    observationError = None

  def drain(): Either[ArchiveError, Vector[ReadObservation]] =
    this.synchronized:
      observationError match
        case Some(error) =>
          clear()
          Left(error)
        case None if observations.exists(!_.completed) =>
          clear()
          Left(ArchiveError.ReceiptMismatch(
            "asynchronous store observation was incomplete after read completion"
          ))
        case None =>
          val result = observations.iterator.flatMap(_.observation).toVector
          clear()
          Right(result)

  private def reserve(): Int = this.synchronized:
    val index = observations.length
    observations += ObservationSlot(completed = false, observation = None)
    index

  private def complete(
      index: Int,
      result: Either[ArchiveError, Option[ReadObservation]]
  ): Unit = this.synchronized:
    val slot = observations(index)
    slot.completed = true
    result match
      case Right(observation) => slot.observation = observation
      case Left(error) if observationError.isEmpty => observationError = Some(error)
      case Left(_) => ()
