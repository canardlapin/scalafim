package scalafim.archive.zarr

import java.nio.file.Files
import scala.jdk.CollectionConverters.*
import scalafim.zarr.*

class JvmPublicationSuite extends munit.FunSuite:
  private def value[A](result: Either[NeuroArchiveZarrError, A]): A = result match
    case Right(found) => found
    case Left(error) => fail(error.message)

  test("JVM publisher creates an immutable revision that opens and reads"):
    val parent = Files.createTempDirectory("scalafim-neuroarchive-publish")
    val target = parent.resolve("revision.zarr")
    val descriptor = ProfileFixtures.descriptor()
    val manifest = ProfileFixtures.manifest
    val receipt = value(JvmNeuroArchivePublisher.create(
      target,
      descriptor,
      manifest,
      sequentialProvider(descriptor)
    ))
    assertEquals(receipt.expectedOuterObjects, 8L)
    assertEquals(receipt.objects.length, 8)
    assert(Files.isRegularFile(target.resolve("publication.json")))
    assert(JvmNeuroArchivePublisher.create(target, descriptor, manifest, sequentialProvider(descriptor)).isLeft)

    val store = JvmFileStore.open(target).fold(fail(_), identity)
    val opened = value(NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable))
    assert(JvmNeuroArchiveAudit.verifyObjects(
      store,
      receipt,
      ByteCount(1024L).fold(error => fail(error.message), identity)
    ).isRight)
    val origin = Coordinate(0L, 0L, 0L, 0L).fold(error => fail(error.message), identity)
    val region = Region.within(descriptor.shape, origin, descriptor.shape)
      .fold(error => fail(error.message), identity)
    val result = opened.array.readRegion(region).fold(error => fail(error.message), identity)
    result.block match
      case PrimitiveBlock.Int16(values) =>
        assertEquals(values.toArray.toVector, Vector.tabulate(24)(_.toShort))
      case _ => fail("expected int16 values")
    assertEquals(opened.canonical.manifest.calibration(8.0), 0.0)

  test("missing receipt-listed object makes a completed revision unopenable"):
    val parent = Files.createTempDirectory("scalafim-neuroarchive-missing")
    val target = parent.resolve("revision.zarr")
    val descriptor = ProfileFixtures.descriptor()
    val receipt = value(JvmNeuroArchivePublisher.create(
      target,
      descriptor,
      ProfileFixtures.manifest,
      sequentialProvider(descriptor)
    ))
    Files.delete(target.resolve(receipt.objects.head.key.value))
    val store = JvmFileStore.open(target).fold(fail(_), identity)
    assert(NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable).isLeft)

  test("start-indexed sharded canonical revision publishes as one complete outer object"):
    val parent = Files.createTempDirectory("scalafim-neuroarchive-sharded")
    val target = parent.resolve("revision.zarr")
    val descriptor = ProfileFixtures.descriptor(ProfileFixtures.startIndexedMetadata)
    val receipt = value(JvmNeuroArchivePublisher.create(
      target,
      descriptor,
      ProfileFixtures.manifest,
      sequentialProvider(descriptor)
    ))
    assertEquals(receipt.expectedOuterObjects, 1L)
    assertEquals(receipt.objects.map(_.key.value), Vector("canonical/c/0/0/0/0"))
    val store = JvmFileStore.open(target).fold(fail(_), identity)
    val opened = value(NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable))
    val origin = Coordinate(0L, 0L, 0L, 0L).fold(error => fail(error.message), identity)
    val region = Region.within(descriptor.shape, origin, descriptor.shape)
      .fold(error => fail(error.message), identity)
    opened.array.readRegion(region).fold(error => fail(error.message), identity).block match
      case PrimitiveBlock.Int16(values) =>
        assertEquals(values.toArray.toVector, Vector.tabulate(24)(_.toShort))
      case _ => fail("expected int16 values")

  test("same-length object corruption is separated from bounded opening by full audit"):
    val parent = Files.createTempDirectory("scalafim-neuroarchive-corrupt")
    val target = parent.resolve("revision.zarr")
    val descriptor = ProfileFixtures.descriptor()
    val receipt = value(JvmNeuroArchivePublisher.create(
      target,
      descriptor,
      ProfileFixtures.manifest,
      sequentialProvider(descriptor)
    ))
    val corrupted = target.resolve(receipt.objects.head.key.value)
    val bytes = Files.readAllBytes(corrupted)
    bytes(0) = (bytes(0) ^ 1).toByte
    Files.write(corrupted, bytes)
    val store = JvmFileStore.open(target).fold(fail(_), identity)
    assert(NeuroArchiveZarr.openCanonical(store, runtime = JvmCodecRuntime.portable).isRight)
    assert(JvmNeuroArchiveAudit.verifyObjects(
      store,
      receipt,
      ByteCount(1024L).fold(error => fail(error.message), identity)
    ).isLeft)

  test("provider interruption and direct fill omission never publish a revision"):
    val parent = Files.createTempDirectory("scalafim-neuroarchive-interrupt")
    val descriptor = ProfileFixtures.descriptor()
    val interrupted = parent.resolve("interrupted.zarr")
    val failing = new ChunkProvider:
      private var calls = 0
      def chunk(coordinate: ChunkCoordinate, storedShape: Shape): Either[ZarrError, ChunkPayload] =
        calls += 1
        if calls == 1 then sequentialProvider(descriptor).chunk(coordinate, storedShape)
        else Left(ZarrError.WriteFailure("simulated interruption"))
    assert(JvmNeuroArchivePublisher.create(
      interrupted,
      descriptor,
      ProfileFixtures.manifest,
      failing
    ).isLeft)
    assert(!Files.exists(interrupted))

    val incomplete = parent.resolve("fill-omitted.zarr")
    val fill = new ChunkProvider:
      def chunk(coordinate: ChunkCoordinate, storedShape: Shape): Either[ZarrError, ChunkPayload] =
        if coordinate.toVector == Vector(0L, 0L, 0L, 0L) then Right(ChunkPayload.Fill)
        else sequentialProvider(descriptor).chunk(coordinate, storedShape)
    assert(JvmNeuroArchivePublisher.create(
      incomplete,
      descriptor,
      ProfileFixtures.manifest,
      fill
    ).isLeft)
    assert(!Files.exists(incomplete))

    val stream = Files.list(parent)
    try assertEquals(stream.iterator().asScala.toVector, Vector.empty)
    finally stream.close()

  private def sequentialProvider(descriptor: ArrayDescriptor): ChunkProvider = new ChunkProvider:
    private val chunkShape = descriptor.layout match
      case PhysicalLayout.Direct(_) => descriptor.grid.chunkShape
      case PhysicalLayout.Sharded(sharded, _, _, _, _) => sharded.innerChunkShape

    def chunk(coordinate: ChunkCoordinate, storedShape: Shape): Either[ZarrError, ChunkPayload] =
      val count = storedShape.elementCount.fold(error => throw IllegalArgumentException(error.message), identity)
      val values = Array.fill[Short](count.toInt)(-9)
      val cursor = new Array[Long](storedShape.rank.toInt)
      var element = 0
      while element < values.length do
        var linear = 0L
        var inside = true
        var axis = 0
        while axis < cursor.length do
          val global = coordinate.axis(axis) * chunkShape.axis(axis) + cursor(axis)
          if global >= descriptor.shape.axis(axis) then inside = false
          linear = linear * descriptor.shape.axis(axis) + global
          axis += 1
        if inside then values(element) = linear.toShort
        advance(cursor, storedShape)
        element += 1
      Right(ChunkPayload.Values(PrimitiveBlock.Int16(OwnedShorts.copyOf(values))))

  private def advance(cursor: Array[Long], shape: Shape): Unit =
    var axis = cursor.length - 1
    var advanced = false
    while axis >= 0 && !advanced do
      cursor(axis) += 1L
      if cursor(axis) < shape.axis(axis) then advanced = true
      else
        cursor(axis) = 0L
        axis -= 1
