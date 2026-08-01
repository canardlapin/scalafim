package scalafim.archive.zarr

import java.nio.file.Path
import zarr4s.*

object PublicationFixtureMain:
  def main(arguments: Array[String]): Unit =
    require(arguments.length == 1, "expected output path")
    val descriptor = ProfileFixtures.descriptor(ProfileFixtures.startIndexedMetadata)
    JvmNeuroArchivePublisher.create(
      Path.of(arguments(0)),
      descriptor,
      ProfileFixtures.manifest,
      provider(descriptor)
    ) match
      case Left(error) => throw IllegalStateException(error.message)
      case Right(_) => ()

  private def provider(descriptor: ArrayDescriptor): ChunkProvider = new ChunkProvider:
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
