package scalafim.archive.zarr

import zarr4s.*

/** A measured physical layout for canonical `[t,z,y,x]` BOLD.
  *
  * This value belongs to the neuroimaging profile, not the rank-generic Zarr
  * kernel.  Construction proves rank, positivity, and shard divisibility;
  * compilation proves agreement with a particular acquisition shape.
  */
final case class CanonicalChunkProfile private (
    name: String,
    innerChunkShape: Shape,
    shardShape: Shape
):
  def compile(arrayShape: Shape): Either[NeuroArchiveZarrError, ShardedGrid] =
    if arrayShape.rank.toInt != 4 then
      Left(NeuroArchiveZarrError.InvalidProfileLayout(
        "canonical chunk profiles require a rank-four [t,z,y,x] array"
      ))
    else
      RegularGrid(arrayShape, shardShape)
        .flatMap(outer => ShardedGrid(outer, innerChunkShape))
        .left.map(NeuroArchiveZarrError.Kernel.apply)

  def sizing(
      arrayShape: Shape,
      dataType: DataTypeCapability
  ): Either[NeuroArchiveZarrError, CanonicalLayoutSizing] =
    for
      sharded <- compile(arrayShape)
      innerElements <- innerChunkShape.elementCount.left.map(NeuroArchiveZarrError.Kernel.apply)
      shardElements <- shardShape.elementCount.left.map(NeuroArchiveZarrError.Kernel.apply)
      innerBytes <- checkedBytes(innerElements, dataType.byteWidth, "inner chunk")
      shardBytes <- checkedBytes(shardElements, dataType.byteWidth, "shard")
      indexBytes <- ShardIndexCodec.encodedLength(sharded.innerChunksPerShard)
        .left.map(NeuroArchiveZarrError.Kernel.apply)
    yield CanonicalLayoutSizing(innerBytes, shardBytes, indexBytes, sharded.innerChunksPerShard)

  private def checkedBytes(
      elements: Long,
      width: Int,
      subject: String
  ): Either[NeuroArchiveZarrError, ByteCount] =
    if elements > Long.MaxValue / width.toLong then
      Left(NeuroArchiveZarrError.InvalidProfileLayout(s"$subject byte size overflows Long"))
    else ByteCount(elements * width.toLong).left.map(NeuroArchiveZarrError.Kernel.apply)

final case class CanonicalLayoutSizing(
    innerChunkBytes: ByteCount,
    shardBytes: ByteCount,
    shardIndexBytes: ByteCount,
    innerChunksPerShard: Shape
)

object CanonicalChunkProfile:
  /** Canonical volumetric default selected by the Z5 workload corpus.
    *
    * For int16 this is a 768 KiB inner chunk and an 81 MiB uncompressed shard;
    * float32 doubles those byte sizes without changing request geometry.
    */
  val balancedV01: CanonicalChunkProfile =
    unsafe("canonical-balanced-0.1", Vector(16L, 24L, 32L, 32L), Vector(64L, 72L, 96L, 96L))

  def apply(
      name: String,
      innerChunkShape: Shape,
      shardShape: Shape
  ): Either[NeuroArchiveZarrError, CanonicalChunkProfile] =
    val normalized = name.trim
    if normalized.isEmpty || !normalized.forall(portableNameCharacter) then
      Left(NeuroArchiveZarrError.InvalidProfileLayout(
        "chunk profile name must contain only letters, digits, '.', '_', or '-'"
      ))
    else if innerChunkShape.rank.toInt != 4 then
      Left(NeuroArchiveZarrError.InvalidProfileLayout("inner chunk shape must have rank four"))
    else if shardShape.rank.toInt != 4 then
      Left(NeuroArchiveZarrError.InvalidProfileLayout("shard shape must have rank four"))
    else
      var axis = 0
      while axis < 4 do
        val inner = innerChunkShape.axis(axis)
        val shard = shardShape.axis(axis)
        if inner <= 0L || shard <= 0L then
          return Left(NeuroArchiveZarrError.InvalidProfileLayout(
            s"chunk and shard dimensions must be positive on axis $axis"
          ))
        if shard % inner != 0L then
          return Left(NeuroArchiveZarrError.InvalidProfileLayout(
            s"inner chunk dimension $inner does not divide shard dimension $shard on axis $axis"
          ))
        axis += 1
      Right(new CanonicalChunkProfile(normalized, innerChunkShape, shardShape))

  private def unsafe(
      name: String,
      inner: Vector[Long],
      shard: Vector[Long]
  ): CanonicalChunkProfile =
    val innerShape = Shape.from(inner).fold(error => throw IllegalArgumentException(error.message), identity)
    val shardShape = Shape.from(shard).fold(error => throw IllegalArgumentException(error.message), identity)
    apply(name, innerShape, shardShape)
      .fold(error => throw IllegalArgumentException(error.message), identity)

  private def portableNameCharacter(character: Char): Boolean =
    character.isLetterOrDigit || character == '.' || character == '_' || character == '-'
