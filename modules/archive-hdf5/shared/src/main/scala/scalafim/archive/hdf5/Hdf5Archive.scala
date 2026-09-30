package scalafim.archive.hdf5

/** Physical archive errors. No scientific schema or publication semantics. */
enum Hdf5Error:
  case InvalidName(detail: String)
  case InvalidShape(detail: String)
  case InvalidChunk(detail: String)
  case InvalidSlab(detail: String)
  case InvalidBlock(detail: String)
  case Overflow(detail: String)
  case TypeMismatch(expected: Hdf5DType, actual: Hdf5DType)
  case Capacity(required: Long, available: Int)
  case AlreadyExists(path: String)
  case DuplicateDataset(name: String)
  case UnsupportedStored(detail: String)
  case ResourceLimit(detail: String)
  case Closed(detail: String)
  case Cancelled
  case NativeFailure(operation: String, detail: String, cleanup: Vector[Hdf5Error] = Vector.empty)
  case ScopeFailure(primary: Hdf5Error, cleanup: Vector[Hdf5Error])
  case MissingCapability(provider: String, host: String, jdk: String, detail: String)

  def message: String = toString

private[hdf5] object Checked:
  def product(values: Vector[Long], context: String): Either[Hdf5Error, Long] =
    if values.contains(0L) then Right(0L)
    else values.foldLeft[Either[Hdf5Error, Long]](Right(1L))((acc, v) => acc.flatMap(multiply(_, v, context)))

  def multiply(a: Long, b: Long, context: String): Either[Hdf5Error, Long] =
    if a < 0 || b < 0 || (b != 0 && a > Long.MaxValue / b) then Left(Hdf5Error.Overflow(context))
    else Right(a * b)

  def add(a: Long, b: Long, context: String): Either[Hdf5Error, Long] =
    if a < 0 || b < 0 || a > Long.MaxValue - b then Left(Hdf5Error.Overflow(context))
    else Right(a + b)

opaque type Hdf5DatasetName = String
object Hdf5DatasetName:
  def apply(value: String): Either[Hdf5Error, Hdf5DatasetName] =
    if value == null || value.length > 128 || !value.matches("[A-Za-z][A-Za-z0-9_]*") then
      Left(Hdf5Error.InvalidName("flat ASCII name, 1..128 characters"))
    else Right(value)
  extension (name: Hdf5DatasetName) def value: String = name

/** Exact reachable root namespace, limited to one or two distinct flat dataset names.
  * This does not describe deleted/unreachable file bytes or dataset shape, dtype or payload.
  */
final class Hdf5FlatInventory private (val expected: Vector[Hdf5DatasetName])
object Hdf5FlatInventory:
  def apply(expected: Vector[Hdf5DatasetName]): Either[Hdf5Error, Hdf5FlatInventory] =
    if expected == null || expected.isEmpty || expected.size > 2 then
      Left(Hdf5Error.InvalidName("flat inventory requires 1..2 distinct names"))
    else
      for
        _ <- expected.foldLeft[Either[Hdf5Error, Unit]](Right(())):
          (acc, name) => acc.flatMap(_ => Hdf5DatasetName(name.value).map(_ => ()))
        _ <- if expected.distinct.size == expected.size then Right(())
          else Left(Hdf5Error.InvalidName("flat inventory requires distinct names"))
      yield new Hdf5FlatInventory(expected)

opaque type Hdf5Extent = Vector[Long]
object Hdf5Extent:
  def apply(dimensions: Vector[Long]): Either[Hdf5Error, Hdf5Extent] =
    if dimensions == null || dimensions.isEmpty || dimensions.size > 3 || dimensions.exists(_ <= 0) then
      Left(Hdf5Error.InvalidShape("rank 1..3 with positive fixed Long dimensions"))
    else Checked.product(dimensions, "extent elements").map(_ => dimensions)
  extension (extent: Hdf5Extent)
    def dimensions: Vector[Long] = extent
    def rank: Int = extent.size
    def elements: Long = extent.product // Proven by constructor.

final class Hdf5Slab private (
    val offset: Vector[Long], val count: Vector[Long], val elements: Long
)
object Hdf5Slab:
  def apply(offset: Vector[Long], count: Vector[Long]): Either[Hdf5Error, Hdf5Slab] =
    if offset == null || count == null || count.isEmpty || count.size > 3 || offset.size != count.size ||
        offset.exists(_ < 0) || count.exists(_ < 0) then
      Left(Hdf5Error.InvalidSlab("equal rank 1..3; nonnegative offset/count"))
    else
      val sums = offset.zip(count).foldLeft[Either[Hdf5Error, Unit]](Right(())):
        case (acc, (o, c)) => acc.flatMap(_ => Checked.add(o, c, "slab end").map(_ => ()))
      for
        _ <- sums
        n <- Checked.product(count, "selected elements")
      yield new Hdf5Slab(offset, count, n)

  def within(slab: Hdf5Slab, extent: Hdf5Extent): Either[Hdf5Error, Unit] =
    if slab == null || slab.count.size != extent.rank then Left(Hdf5Error.InvalidSlab("rank mismatch"))
    else if slab.offset.zip(slab.count).zip(extent.dimensions).exists:
        case ((o, c), d) => o + c > d
    then Left(Hdf5Error.InvalidSlab("outside fixed extent"))
    else Right(())

enum Hdf5DType(val bytes: Int):
  case Float32 extends Hdf5DType(4)
  case Float64 extends Hdf5DType(8)
  case UInt8 extends Hdf5DType(1)

enum Hdf5Filter:
  case None
  case Deflate(level: Int)

  def validate: Either[Hdf5Error, Unit] = this match
    case None => Right(())
    case Deflate(level) =>
      if level >= 0 && level <= 9 then Right(())
      else Left(Hdf5Error.InvalidChunk("DEFLATE level must be 0..9"))

/** The arrays are caller-owned, contiguous, and borrowed only during a synchronous slab. */
enum Hdf5Block:
  case Float32(values: Array[Float])
  case Float64(values: Array[Double])
  case UInt8(values: Array[Byte])

  def dtype: Hdf5DType = this match
    case Float32(_) => Hdf5DType.Float32
    case Float64(_) => Hdf5DType.Float64
    case UInt8(_) => Hdf5DType.UInt8
  def capacity: Either[Hdf5Error, Int] = this match
    case Float32(a) => if a == null then Left(Hdf5Error.InvalidBlock("null float buffer")) else Right(a.length)
    case Float64(a) => if a == null then Left(Hdf5Error.InvalidBlock("null double buffer")) else Right(a.length)
    case UInt8(a) => if a == null then Left(Hdf5Error.InvalidBlock("null byte buffer")) else Right(a.length)

final case class Hdf5Limits(
    maxBlockElements: Int, rawCacheBytes: Long, rawCacheSlots: Int,
    metadataCacheBytes: Long, maxFiles: Int, maxDatasetsPerFile: Int, maxNativeIds: Int
):
  require(maxBlockElements > 0 && maxBlockElements <= 65536, "block ceiling 1..65536")
  require(rawCacheBytes > 0 && rawCacheBytes <= 1048576 && rawCacheSlots > 0 && rawCacheSlots <= 521, "raw cache ceiling")
  require(metadataCacheBytes >= 1048576 && metadataCacheBytes <= 4194304, "metadata ceiling 1..4 MiB")
  require(maxFiles > 0 && maxFiles <= 2 && maxDatasetsPerFile > 0 && maxDatasetsPerFile <= 2, "handle ceilings")
  require(maxNativeIds >= 8 && maxNativeIds <= 24, "ID ceiling 8..24")

object Hdf5Limits:
  val bounded: Hdf5Limits = Hdf5Limits(65536, 1048576, 521, 4194304, 2, 2, 24)

final case class Hdf5DatasetInfo(
    name: Hdf5DatasetName, dtype: Hdf5DType, extent: Hdf5Extent,
    chunks: Hdf5Extent, filter: Hdf5Filter
)

object Hdf5Plan:
  def dataset(info: Hdf5DatasetInfo, limits: Hdf5Limits): Either[Hdf5Error, Unit] =
    if info == null || info.dtype == null || info.filter == null || limits == null then Left(Hdf5Error.InvalidChunk("null dataset plan"))
    else if info.name.value == null || info.extent.dimensions == null || info.chunks.dimensions == null then
      Left(Hdf5Error.InvalidChunk("null dataset fields"))
    else if info.chunks.rank != info.extent.rank || info.chunks.dimensions.zip(info.extent.dimensions).exists((c, d) => c > d) then
      Left(Hdf5Error.InvalidChunk("same rank and chunks <= extent"))
    else
      for
        _ <- Hdf5DatasetName(info.name.value)
        _ <- info.filter.validate
        _ <- Checked.multiply(info.extent.elements, info.dtype.bytes, "extent bytes")
        bytes <- Checked.multiply(info.chunks.elements, info.dtype.bytes, "chunk bytes")
        _ <- if bytes <= limits.rawCacheBytes then Right(()) else Left(Hdf5Error.InvalidChunk("chunk exceeds raw-cache cap"))
      yield ()

  def slab(info: Hdf5DatasetInfo, slab: Hdf5Slab, block: Hdf5Block, limits: Hdf5Limits): Either[Hdf5Error, Long] =
    for
      _ <- dataset(info, limits)
      _ <- Hdf5Slab.within(slab, info.extent)
      _ <- if block == null then Left(Hdf5Error.InvalidBlock("null block")) else Right(())
      _ <- if block.dtype == info.dtype then Right(()) else Left(Hdf5Error.TypeMismatch(info.dtype, block.dtype))
      capacity <- block.capacity
      _ <- if slab.elements <= limits.maxBlockElements then Right(()) else Left(Hdf5Error.ResourceLimit("selected block elements"))
      _ <- if slab.elements <= capacity then Right(()) else Left(Hdf5Error.Capacity(slab.elements, capacity))
      bytes <- Checked.multiply(slab.elements, info.dtype.bytes, "selected bytes")
    yield bytes

/** Process-lifetime counters across adapter instances; limits describe this adapter's configuration.
  * Selection counts and configured caches are not physical IO or native allocator measurements.
  */
final case class Hdf5Receipt(
    attempts: Long, payloadCalls: Long, successfulCalls: Long, selectedElements: Long,
    selectedBytes: Long, failures: Long, openFiles: Int, openDatasets: Int,
    ownedIds: Int, peakOwnedIds: Int, providerOpenIds: Option[Int],
    metadataObservedPeakBytes: Long, heapSamplePeakBytes: Long, limits: Hdf5Limits
)

trait Hdf5Dataset:
  def info: Hdf5DatasetInfo
  def write(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean = () => false): Either[Hdf5Error, Unit]
  def readInto(slab: Hdf5Slab, block: Hdf5Block, cancelled: () => Boolean = () => false): Either[Hdf5Error, Unit]
  def close(): Either[Hdf5Error, Unit]

trait Hdf5File:
  def create(info: Hdf5DatasetInfo): Either[Hdf5Error, Hdf5Dataset]
  def inspect(name: Hdf5DatasetName): Either[Hdf5Error, Hdf5Dataset]
  /** Verify exact root links are singly linked hard datasets, without reading payload.
    * Dataset metadata validation remains the separate inspect capability.
    */
  def verifyFlatInventory(inventory: Hdf5FlatInventory): Either[Hdf5Error, Unit] =
    Left(Hdf5Error.UnsupportedStored("flat inventory capability unavailable"))
  def close(): Either[Hdf5Error, Unit]

trait Hdf5Archive:
  def createExclusive(path: String): Either[Hdf5Error, Hdf5File]
  def openReadOnly(path: String): Either[Hdf5Error, Hdf5File]
  def receipt: Hdf5Receipt

object Hdf5Scope:
  /** Scoped callbacks preserve a primary error and all cleanup errors. */
  def file[A](opened: Either[Hdf5Error, Hdf5File])(f: Hdf5File => Either[Hdf5Error, A]): Either[Hdf5Error, A] =
    opened.flatMap(handle => use(f(handle), () => handle.close()))
  def dataset[A](opened: Either[Hdf5Error, Hdf5Dataset])(f: Hdf5Dataset => Either[Hdf5Error, A]): Either[Hdf5Error, A] =
    opened.flatMap(handle => use(f(handle), () => handle.close()))
  private def use[A](body: => Either[Hdf5Error, A], close: () => Either[Hdf5Error, Unit]): Either[Hdf5Error, A] =
    val primary = try body catch
      case scala.util.control.NonFatal(e) => Left(Hdf5Error.NativeFailure("callback", e.toString))
    val cleanup = try close() catch
      case scala.util.control.NonFatal(e) => Left(Hdf5Error.NativeFailure("close callback", e.toString))
    (primary, cleanup) match
      case (Left(e), Left(c)) => Left(Hdf5Error.ScopeFailure(e, Vector(c)))
      case (Right(_), Left(c)) => Left(c)
      case _ => primary
