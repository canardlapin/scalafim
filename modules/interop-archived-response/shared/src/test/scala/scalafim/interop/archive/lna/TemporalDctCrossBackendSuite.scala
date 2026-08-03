package scalafim.interop.archive.lna

import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.archive.{
  ArchiveError,
  ArchiveLocation,
  ArchiveReadReceipt,
  ArchiveRevisionId,
  ArchiveValidation,
  ArchiveValidationScope,
  ContentDigest,
  Observed,
  PayloadExecutor,
  PayloadId as ArchivePayloadId,
  PayloadPlan,
  PayloadPlanSummary,
  PhysicalLocality as ArchivePhysicalLocality,
  PhysicalObjectId,
  ReadObservation,
  ReceiptAccumulator
}
import scalafim.archive.lna.{
  DatasetRole,
  LnaDoubleMatrixPlan,
  LnaDoubleVectorPlan,
  Payload
}
import scalafim.archive.zarr.{
  TestZarrRuntime,
  ZarrPayloadPlan
}
import scalafim.interop.archive.ArchiveResponseAccess
import scalafim.interop.archive.zarr.{
  TemporalDctZarrLayout,
  TemporalDctZarrObject,
  TemporalDctZarrPayload,
  TemporalDctZarrSource
}
import scalafim.latent.DecodePlan
import scalafim.response.{
  DecodeConsistency,
  PhysicalLocality,
  ResponseBlock,
  SelectionAxes,
  SourceId
}
import scalafim.response.laws.ResponseLawChecks
import zarr4s.{
  ArrayDescriptor,
  ArraySelection,
  AsyncChunkProvider,
  AsyncMemoryStore,
  AsyncOpenedArray,
  AsyncZarr,
  AsyncZarrWriter,
  ChunkCoordinate,
  ChunkPayload,
  ObjectRequest,
  OwnedDoubles,
  PhysicalLayout,
  PrimitiveBlock,
  ReadResult as ZarrReadResult,
  Shape,
  StoreKey,
  WriteOutcome,
  ZarrMetadata,
  ZarrNodeMetadata,
  ZarrPath
}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global

class TemporalDctCrossBackendSuite extends munit.FunSuite:
  test("one temporal-DCT decode plan runs unchanged through memory, LNA, and Zarr"):
    val fixture =
      TemporalDctLnaFixtures.fixture("cross-backend")
    val requested =
      TemporalDctLnaFixtures.selection(
        fixture.schema,
        Vector(3, 1),
        Vector(2, 0)
      )
    val memory =
      fixture.materialized
        .decode(requested)
        .fold(error => fail(error.message), identity)
    val lnaPlan =
      TemporalDctLnaWritePlan
        .create(fixture.materialized, fixture.space)
        .fold(error => fail(error.message), identity)
    val lnaLayout =
      TemporalDctLnaLayout(
        rolePath(lnaPlan, DatasetRole.TemporalBasis),
        rolePath(lnaPlan, DatasetRole.Loadings),
        Some(rolePath(lnaPlan, DatasetRole.SampleOffset))
      )
    val lnaPayloadIds =
      lnaPlan.archive.manifest.datasets.map(reference =>
        ArchivePayloadId.unsafe(reference.path.value)
      )
    val lnaAccess =
      access(
        "lna",
        new FixtureLnaExecutor(lnaPlan.archive),
        lnaPayloadIds
      )
    val lnaSource =
      ArchivedTemporalDctSource
        .make[IO](
          SourceId.unsafe("cross-backend:lna"),
          fixture.model,
          lnaAccess,
          lnaLayout
        )
        .fold(error => fail(error.message), identity)

    val result =
      for
        zarr <- createZarrFixture(fixture.materialized)
        zarrAccess = access(
          "zarr",
          zarr.executor,
          zarr.layout.payloads.map(_.payload)
        )
        zarrSource <- lift(
          TemporalDctZarrSource.make[IO](
            SourceId.unsafe("cross-backend:zarr"),
            fixture.model,
            zarrAccess,
            zarr.layout
          )
        )
        lnaReadPlan <- lift(
          lnaSource
            .plan(requested)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
        )
        zarrReadPlan <- lift(
          zarrSource
            .plan(requested)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
        )
        lnaRead <- lnaSource
          .executeObserved(lnaReadPlan)
          .value
          .flatMap(lift)
        zarrRead <- zarrSource
          .executeObserved(zarrReadPlan)
          .value
          .flatMap(lift)
      yield
        val lnaInspection =
          DecodePlan.inspect(lnaReadPlan.decode)
        val zarrInspection =
          DecodePlan.inspect(zarrReadPlan.decode)
        assertEquals(
          lnaInspection.requests.map(_.slot.id.value),
          zarrInspection.requests.map(_.slot.id.value)
        )
        assertEquals(lnaInspection.dependentBarriers, 0)
        assertEquals(zarrInspection.dependentBarriers, 0)
        assertBlocksAgree(
          fixture.model.decodeConsistency,
          memory,
          lnaRead.result.block
        )
        assertBlocksAgree(
          fixture.model.decodeConsistency,
          memory,
          zarrRead.result.block
        )
        assertBlocksAgree(
          fixture.model.decodeConsistency,
          lnaRead.result.block,
          zarrRead.result.block
        )
        assertEquals(lnaRead.nativeReceipts.length, 3)
        assert(
          lnaRead.nativeReceipts.forall(
            _.observedLocality ==
              ArchivePhysicalLocality.WholePayload
          )
        )
        assertEquals(zarrRead.nativeReceipts.length, 3)
        assertEquals(
          zarrRead.nativeReceipts.map(_.observedLocality),
          Vector(
            ArchivePhysicalLocality.ByteRangeBounded,
            ArchivePhysicalLocality.ChunkBounded,
            ArchivePhysicalLocality.ChunkBounded
          )
        )
        assertEquals(
          zarrSource.capabilities.forAxes(SelectionAxes.Time),
          Some(PhysicalLocality.ByteRangeBounded)
        )
        assertEquals(
          zarrSource.capabilities.forAxes(SelectionAxes.Samples),
          Some(PhysicalLocality.ChunkBounded)
        )
        assertEquals(
          zarrSource.capabilities.forAxes(SelectionAxes.TimeAndSamples),
          None
        )
        assertEquals(
          zarrRead.result.receipt.physical.byAxes.map(value =>
            value.axes -> value.observed
          ),
          Vector(
            SelectionAxes.Time ->
              PhysicalLocality.ByteRangeBounded,
            SelectionAxes.Samples ->
              PhysicalLocality.ChunkBounded
          )
        )
        assertEquals(zarrRead.result.receipt.fallbacks, Vector.empty)

    run(result)

  private final case class ZarrFixture(
      layout: TemporalDctZarrLayout,
      executor: PayloadExecutor[IO]
  )

  private def createZarrFixture(
      value: scalafim.latent.MaterializedTemporalDct
  ): IO[ZarrFixture] =
    val basisDescriptor = descriptor(
      """{"zarr_format":3,"node_type":"array","shape":[4,4],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[4,4]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":0.0,"codecs":[{"name":"sharding_indexed","configuration":{"chunk_shape":[2,2],"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"index_codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"crc32c"}],"index_location":"start"}}],"attributes":{},"storage_transformers":[]}"""
    )
    val loadingDescriptor = descriptor(
      """{"zarr_format":3,"node_type":"array","shape":[3,4],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2,2]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":0.0,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"attributes":{},"storage_transformers":[]}"""
    )
    val offsetDescriptor = descriptor(
      """{"zarr_format":3,"node_type":"array","shape":[3],"data_type":"float64","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[2]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":0.0,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"gzip","configuration":{"level":1}},{"name":"crc32c"}],"attributes":{},"storage_transformers":[]}"""
    )
    val basisPath = path("basis")
    val loadingPath = path("loadings")
    val offsetPath = path("offset")
    val basisId = ArchivePayloadId.unsafe("basis")
    val loadingId = ArchivePayloadId.unsafe("loadings")
    val offsetId = ArchivePayloadId.unsafe("offset")

    for
      store <- liftZarr(AsyncMemoryStore(Map.empty))
      _ <- write(
        store,
        basisPath,
        basisDescriptor,
        value.basis.rowMajorCopy
      )
      _ <- write(
        store,
        loadingPath,
        loadingDescriptor,
        value.loadings.rowMajorCopy
      )
      _ <- write(
        store,
        offsetPath,
        offsetDescriptor,
        value.offset
          .getOrElse(fail("cross-backend fixture requires offsets"))
          .valuesCopy
      )
      basisOpened <- open(store, basisPath)
      loadingsOpened <- open(store, loadingPath)
      offsetOpened <- open(store, offsetPath)
      basis <- lift(
        TemporalDctZarrPayload.make(
          basisId,
          basisDescriptor,
          basisPath,
          objects(store, basisPath),
          ArchivePhysicalLocality.ByteRangeBounded
        )
      )
      loadings <- lift(
        TemporalDctZarrPayload.make(
          loadingId,
          loadingDescriptor,
          loadingPath,
          objects(store, loadingPath),
          ArchivePhysicalLocality.ChunkBounded
        )
      )
      offset <- lift(
        TemporalDctZarrPayload.make(
          offsetId,
          offsetDescriptor,
          offsetPath,
          objects(store, offsetPath),
          ArchivePhysicalLocality.ChunkBounded
        )
      )
      layout <- lift(
        TemporalDctZarrLayout.make(
          value.model,
          basis,
          loadings,
          Some(offset)
        )
      )
      executor = new FixtureZarrExecutor(
        store,
        Map(
          basisId -> basisOpened,
          loadingId -> loadingsOpened,
          offsetId -> offsetOpened
        )
      )
    yield ZarrFixture(layout, executor)

  private def write(
      store: AsyncMemoryStore,
      path: ZarrPath,
      descriptor: ArrayDescriptor,
      values: Array[Double]
  ): IO[Unit] =
    val provider =
      new ArrayChunkProvider(descriptor, values)
    IO.fromFuture(IO(
      AsyncZarrWriter.create(
        store,
        descriptor,
        provider,
        path,
        runtime = TestZarrRuntime.runtime
      )
    )).flatMap:
      case WriteOutcome.Complete(_) =>
        IO.unit
      case WriteOutcome.Incomplete(_, error) =>
        IO.raiseError(new IllegalArgumentException(error.message))

  private def open(
      store: AsyncMemoryStore,
      path: ZarrPath
  ): IO[AsyncOpenedArray] =
    IO.fromFuture(IO(
      AsyncZarr.openArray(
        store,
        path,
        runtime = TestZarrRuntime.runtime
      )
    )).flatMap(liftZarr)

  private def objects(
      store: AsyncMemoryStore,
      path: ZarrPath
  ): Vector[TemporalDctZarrObject] =
    store.snapshot.iterator
      .collect:
        case (key, bytes)
            if key.startsWith(s"${path.value}/") &&
              !key.endsWith("/zarr.json") =>
          TemporalDctZarrObject(
            PhysicalObjectId.unsafe(key),
            bytes.byteCount.toLong
          )
      .toVector
      .sortBy(_.id.value)

  private final class ArrayChunkProvider(
      descriptor: ArrayDescriptor,
      source: Array[Double]
  ) extends AsyncChunkProvider:
    def chunk(
        coordinate: ChunkCoordinate,
        storedShape: Shape
    )(using ExecutionContext): Future[Either[zarr4s.ZarrError, ChunkPayload]] =
      storedShape.elementCount match
        case Left(error) =>
          Future.successful(Left(error))
        case Right(count) if count > Int.MaxValue.toLong =>
          Future.successful(Left(zarr4s.ZarrError.ResourceLimit(
            "test chunk values",
            Int.MaxValue,
            count
          )))
        case Right(count) =>
          val values = new Array[Double](count.toInt)
          val cursor = new Array[Long](storedShape.rank.toInt)
          var linear = 0
          while linear < values.length do
            var sourceOffset = 0L
            var inside = true
            var axis = 0
            while axis < cursor.length do
              val global =
                coordinate.axis(axis) * storedShape.axis(axis) + cursor(axis)
              if global >= descriptor.shape.axis(axis) then inside = false
              sourceOffset =
                sourceOffset * descriptor.shape.axis(axis) + global
              axis += 1
            if inside then values(linear) = source(sourceOffset.toInt)
            advance(cursor, storedShape)
            linear += 1
          Future.successful(Right(ChunkPayload.Values(
            PrimitiveBlock.Float64(OwnedDoubles.copyOf(values))
          )))

    private def advance(
        cursor: Array[Long],
        shape: Shape
    ): Unit =
      var axis = cursor.length - 1
      var advanced = false
      while axis >= 0 && !advanced do
        cursor(axis) += 1L
        if cursor(axis) < shape.axis(axis) then
          advanced = true
        else
          cursor(axis) = 0L
          axis -= 1

  private final class FixtureZarrExecutor(
      store: AsyncMemoryStore,
      arrays: Map[ArchivePayloadId, AsyncOpenedArray]
  ) extends PayloadExecutor[IO]:
    def execute[A](
        plan: PayloadPlan[A]
    ): EitherT[IO, ArchiveError, Observed[A]] =
      plan match
        case concrete: ZarrPayloadPlan =>
          EitherT(executeZarr(concrete))
        case other =>
          EitherT.leftT(ArchiveError.UnsupportedPayloadPlan(
            "typed-zarr-fixture",
            other.summary.operation
          ))

    private def executeZarr(
        plan: ZarrPayloadPlan
    ): IO[Either[ArchiveError, Observed[ZarrReadResult]]] =
      arrays.get(plan.summary.payload) match
        case None =>
          IO.pure(Left(ArchiveError.MissingPayload(
            scalafim.archive.ArchivePath(plan.summary.payload.value)
          )))
        case Some(array) =>
          for
            _ <- IO(store.clearTrace())
            read <- IO.fromFuture(IO(
              plan.selection match
                case ArraySelection.RegionSelection(region) =>
                  array.readRegion(region, plan.limits)
                case ArraySelection.PointSelection(points) =>
                  array.readPoints(points, plan.limits)
                case ArraySelection.Factored(selection) =>
                  array.read(selection, plan.limits)
            ))
            result <- IO:
              for
                value <- read.left.map(error =>
                  ArchiveError.UnsupportedStorage(error.message)
                )
                observations <- observedRequests(store)
                receipt <- ArchiveReadReceipt.from(
                  plan.summary,
                  plan.summary.locality.locality,
                  ReceiptAccumulator.from(observations)
                )
                _ <-
                  if value.receipt.bytesRead == receipt.physicalBytes then
                    Right(())
                  else
                    Left(ArchiveError.ReceiptMismatch(
                      s"typed Zarr fixture recorded ${receipt.physicalBytes} " +
                        s"bytes but Zarr reported ${value.receipt.bytesRead}"
                    ))
              yield Observed(value, receipt)
          yield result

  private def observedRequests(
      store: AsyncMemoryStore
  ): Either[ArchiveError, Vector[ReadObservation]] =
    val snapshot = store.snapshot
    val result = Vector.newBuilder[ReadObservation]
    var index = 0
    val requests = store.trace
    while index < requests.length do
      val observation =
        requests(index) match
          case ObjectRequest.Whole(key) =>
            for
              objectId <- PhysicalObjectId.fromString(key.value)
              bytes <- snapshot.get(key.value).toRight(
                ArchiveError.InvalidArchive(
                  s"typed Zarr fixture lost '${key.value}'"
                )
              )
              value <- ReadObservation.wholeObject(
                objectId,
                bytes.byteCount.toLong
              )
            yield value
          case ObjectRequest.Range(key, requested) =>
            for
              objectId <- PhysicalObjectId.fromString(key.value)
              interval <- scalafim.archive.ByteInterval.from(
                requested.offset,
                requested.length.toLong
              )
              value <- ReadObservation.byteRange(
                objectId,
                interval,
                requested.length.toLong
              )
            yield value
          case ObjectRequest.Length(key) =>
            for
              objectId <- PhysicalObjectId.fromString(key.value)
              bytes <- snapshot.get(key.value).toRight(
                ArchiveError.InvalidArchive(
                  s"typed Zarr fixture lost '${key.value}'"
                )
              )
              value <- ReadObservation.objectLength(
                objectId,
                bytes.byteCount.toLong
              )
            yield value
      observation match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          result += value
      index += 1
    Right(result.result())

  private final class FixtureLnaExecutor(
      archive: scalafim.archive.lna.LnaArchive
  ) extends PayloadExecutor[IO]:
    def execute[A](
        plan: PayloadPlan[A]
    ): EitherT[IO, ArchiveError, Observed[A]] =
      plan match
        case concrete: LnaDoubleMatrixPlan =>
          executePayload(
            concrete.path,
            concrete.summary,
            {
              case value: Payload.DoubleMatrix =>
                Right(value)
              case other =>
                Left(ArchiveError.ShapeMismatch(
                  s"${concrete.path.value} is $other, expected DoubleMatrix"
                ))
            }
          )
        case concrete: LnaDoubleVectorPlan =>
          executePayload(
            concrete.path,
            concrete.summary,
            {
              case value: Payload.DoubleVector =>
                Right(value)
              case other =>
                Left(ArchiveError.ShapeMismatch(
                  s"${concrete.path.value} is $other, expected DoubleVector"
                ))
            }
          )
        case other =>
          EitherT.leftT(ArchiveError.UnsupportedPayloadPlan(
            "typed-lna-fixture",
            other.summary.operation
          ))

    private def executePayload[A](
        path: scalafim.archive.ArchivePath,
        summary: PayloadPlanSummary,
        refine: Payload => Either[ArchiveError, A]
    ): EitherT[IO, ArchiveError, Observed[A]] =
      EitherT.fromEither:
        for
          payload <- archive
            .payload(path)
            .toRight(ArchiveError.MissingPayload(path))
          value <- refine(payload)
          observation <- ReadObservation.wholePayload(
            summary.payload,
            payloadBytes(payload)
          )
          receipt <- ArchiveReadReceipt.from(
            summary,
            ArchivePhysicalLocality.WholePayload,
            ReceiptAccumulator.from(Vector(observation))
          )
        yield Observed(value, receipt)

    private def payloadBytes(
        payload: Payload
    ): Long =
      payload.dims.foldLeft(payload.dtype.bytes.toLong): (total, dimension) =>
        Math.multiplyExact(total, dimension.toLong)

  private def access(
      id: String,
      executor: PayloadExecutor[IO],
      payloads: Vector[ArchivePayloadId]
  ): ArchiveResponseAccess[IO] =
    new ArchiveResponseAccess(
      ArchiveLocation.unsafe(s"memory://$id"),
      ArchiveRevisionId.unsafe(s"$id-revision"),
      ContentDigest
        .sha256(if id == "lna" then "33" * 32 else "44" * 32)
        .fold(error => fail(error.message), identity),
      executor,
      EitherT.rightT(
        ArchiveValidation(
          ArchiveValidationScope.Contents,
          payloads,
          Vector.empty
        )
      )
    )

  private def rolePath(
      plan: TemporalDctLnaWritePlan,
      role: DatasetRole
  ): scalafim.archive.ArchivePath =
    plan.archive.manifest.datasets
      .find(_.role == role)
      .map(_.path)
      .getOrElse(fail(s"missing ${role.value} payload"))

  private def descriptor(
      metadata: String
  ): ArrayDescriptor =
    ZarrMetadata.parse(metadata) match
      case Right(ZarrNodeMetadata.Array(found)) =>
        ArrayDescriptor
          .compile(found)
          .fold(error => fail(error.message), identity)
      case Right(_) =>
        fail("expected Zarr array metadata")
      case Left(error) =>
        fail(error.message)

  private def path(
      value: String
  ): ZarrPath =
    ZarrPath(value).fold(error => fail(error.message), identity)

  private def lift[A](
      value: Either[ArchiveError, A]
  ): IO[A] =
    value.fold(
      error => IO.raiseError(new IllegalArgumentException(error.message)),
      IO.pure
    )

  private def liftZarr[A](
      value: Either[zarr4s.ZarrError, A]
  ): IO[A] =
    value.fold(
      error => IO.raiseError(new IllegalArgumentException(error.message)),
      IO.pure
    )

  private def assertBlocksAgree(
      consistency: DecodeConsistency,
      left: ResponseBlock,
      right: ResponseBlock
  ): Unit =
    val law =
      ResponseLawChecks.blockAgreement(
        "temporal DCT cross-backend",
        left,
        right,
        consistency
      )
    assert(
      law.isSuccess,
      law.failures.map(_.message).mkString("; ")
    )

  private def run[A](
      program: IO[A]
  ): Future[A] =
    program.unsafeToFuture()
