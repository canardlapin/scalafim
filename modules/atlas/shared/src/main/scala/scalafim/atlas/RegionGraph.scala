package scalafim.atlas

import scalafim.image.Indexing

enum VoxelConnectivity:
  case Connect6, Connect18, Connect26

final case class RegionEdge(from: Region, to: Region, weight: Int)

object RegionGraph:
  def adjacency(atlas: VolumeAtlas, connectivity: VoxelConnectivity = VoxelConnectivity.Connect6): Vector[RegionEdge] =
    val vol = atlas.labelVolume
    val dims = atlas.space.spatialDims
    val counts = scala.collection.mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    val offsets = positiveOffsets(connectivity)

    var z = 0
    while z < dims(2) do
      var y = 0
      while y < dims(1) do
        var x = 0
        while x < dims(0) do
          val a = vol.linear(Indexing.gridToIndex3D(dims, x, y, z))
          if a != 0 then
            offsets.foreach { off =>
              val x2 = x + off(0)
              val y2 = y + off(1)
              val z2 = z + off(2)
              if x2 >= 0 && x2 < dims(0) && y2 >= 0 && y2 < dims(1) && z2 >= 0 && z2 < dims(2) then
                val b = vol.linear(Indexing.gridToIndex3D(dims, x2, y2, z2))
                if b != 0 && b != a then
                  val key = if a < b then (a, b) else (b, a)
                  counts.update(key, counts(key) + 1)
            }
          x += 1
        y += 1
      z += 1

    counts.toVector.flatMap { case ((a, b), weight) =>
      for
        r1 <- atlas.region(RegionId(a))
        r2 <- atlas.region(RegionId(b))
      yield RegionEdge(r1, r2, weight)
    }.sortBy(e => (e.from.id.value, e.to.id.value))

  private def positiveOffsets(connectivity: VoxelConnectivity): Vector[Vector[Int]] =
    val face = Vector(
      Vector(1, 0, 0),
      Vector(0, 1, 0),
      Vector(0, 0, 1)
    )
    val edge =
      if connectivity == VoxelConnectivity.Connect18 || connectivity == VoxelConnectivity.Connect26 then
        Vector(
          Vector(1, 1, 0),
          Vector(1, -1, 0),
          Vector(1, 0, 1),
          Vector(1, 0, -1),
          Vector(0, 1, 1),
          Vector(0, 1, -1)
        )
      else Vector.empty
    val corner =
      if connectivity == VoxelConnectivity.Connect26 then
        Vector(
          Vector(1, 1, 1),
          Vector(1, 1, -1),
          Vector(1, -1, 1),
          Vector(1, -1, -1)
        )
      else Vector.empty
    face ++ edge ++ corner
