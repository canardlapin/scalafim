package scalafim.atlas

import cats.Hash
import graph4s.{Graph, Link}
import graph4s.data.{EdgeField, WeightedGraph}
import scalafim.image.Indexing
import scalafim.locus.{IndexedField, Relation}

enum VoxelConnectivity:
  case Connect6, Connect18, Connect26

final case class RegionEdge(
    from: AtlasRegionMetadata,
    to: AtlasRegionMetadata,
    weight: Int
)

final case class ParcelContact(
    from: AtlasRegionMetadata,
    to: AtlasRegionMetadata,
    boundaryCount: Int
)

trait ParcelAdjacencyRelation:
  type P
  val relation: Relation[P, P]
  val regionIds: IndexedField[P, RegionId]

object RegionGraph:
  def topology(
      atlas: VolumeAtlas,
      connectivity: VoxelConnectivity = VoxelConnectivity.Connect6
  ): WeightedGraph[RegionId, Int] =
    given Hash[RegionId] =
      Hash.by(_.value)
    val vertices = atlas.regions.ids
    val edges = adjacency(atlas, connectivity).map(edge => (edge.from.id, edge.to.id, edge.weight))
    Graph
      .of(vertices, edges.map((from, to, _) => Link(from, to)))
      .toEither match
      case Right(graph) =>
        val weights =
          edges.map: (from, to, weight) =>
            val key =
              if from.value < to.value then from.value -> to.value
              else to.value -> from.value
            key -> weight
        .toMap
        WeightedGraph.from:
          EdgeField.total(graph): edge =>
            val key =
              if edge.first.value < edge.second.value then
                edge.first.value -> edge.second.value
              else
                edge.second.value -> edge.first.value
            weights(key)
      case Left(errors) =>
        throw new IllegalStateException(
          s"validated atlas adjacency violated graph4s invariants: " +
            errors.toNonEmptyList.toList.mkString(", ")
        )

  def adjacency(atlas: VolumeAtlas, connectivity: VoxelConnectivity = VoxelConnectivity.Connect6): Vector[RegionEdge] =
    contactCounts(atlas, connectivity).map: contact =>
      RegionEdge(contact.from, contact.to, contact.boundaryCount)

  /** Optimized weighted boundary-contact counts. This is distinct from the
    * extensional parcel adjacency relation returned by `relation`.
    */
  def contactCounts(
      atlas: VolumeAtlas,
      connectivity: VoxelConnectivity = VoxelConnectivity.Connect6
  ): Vector[ParcelContact] =
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
      yield ParcelContact(r1, r2, weight)
    }.sortBy(e => (e.from.id.value, e.to.id.value))

  /** Semantic reference implementation of
    * `p.converse ; voxelAdjacency ; p`, with self-edges removed.
    */
  def relation(
      atlas: VolumeAtlas,
      connectivity: VoxelConnectivity = VoxelConnectivity.Connect6
  ): ParcelAdjacencyRelation =
    val quotient = atlas.quotient
    val voxelRelation =
      ambientRelation(
        quotient.parcellation.ambient,
        atlas.space.spatialDims,
        connectivity
      )
    // Composition is total when the shared boundary type matches, which it
    // does here by construction: parcel -> voxel -> voxel -> parcel.
    val projected =
      quotient.parcellation.quotientRelation.converse
        .andThen(voxelRelation)
        .andThen(quotient.parcellation.quotientRelation)
    val withoutSelf =
      val rows =
        Iterator.tabulate(quotient.parcellation.parcels.size): source =>
          projected
            .row(quotient.parcellation.parcels.indexOption(source).get)
            .ordinalsInDomainOrder
            .filter(_ != source)
            .iterator
      Relation
        .fromOrdinalRows(
          quotient.parcellation.parcels,
          quotient.parcellation.parcels,
          rows
        )
        .toOption
        .get

    new ParcelAdjacencyRelation:
      type P = quotient.P
      val relation: Relation[P, P] = withoutSelf
      val regionIds: IndexedField[P, RegionId] = quotient.regionIds

  private def ambientRelation[X](
      space: scalafim.locus.FiniteDomain[X],
      dims: Vector[Int],
      connectivity: VoxelConnectivity
  ): Relation[X, X] =
    val offsets = allOffsets(connectivity)
    val rows =
      Array.tabulate(space.size): source =>
        val xyz = Indexing.indexToGrid3D(dims, source)
        val targets = Array.newBuilder[Int]
        offsets.foreach: offset =>
          val x = xyz(0) + offset(0)
          val y = xyz(1) + offset(1)
          val z = xyz(2) + offset(2)
          if
            x >= 0 && x < dims(0) &&
            y >= 0 && y < dims(1) &&
            z >= 0 && z < dims(2)
          then
            targets += Indexing.gridToIndex3D(dims, x, y, z)
        targets.result()
    Relation
      .fromOrdinalRows(space, space, rows.iterator.map(_.iterator))
      .toOption
      .get

  private def allOffsets(
      connectivity: VoxelConnectivity
  ): Vector[Vector[Int]] =
    (for
      z <- -1 to 1
      y <- -1 to 1
      x <- -1 to 1
      manhattan = math.abs(x) + math.abs(y) + math.abs(z)
      if manhattan > 0
      if connectivity match
        case VoxelConnectivity.Connect6 => manhattan == 1
        case VoxelConnectivity.Connect18 => manhattan <= 2
        case VoxelConnectivity.Connect26 => true
    yield Vector(x, y, z)).toVector

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
