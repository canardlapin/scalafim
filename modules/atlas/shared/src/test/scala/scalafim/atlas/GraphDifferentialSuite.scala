package scalafim.atlas

import scala.collection.mutable

import scalafim.image.Indexing
import scalafim.image.PrimitiveBuffers
import scalafim.image.{SampleSpaces, SomeSampleSpace}

class GraphDifferentialSuite extends munit.FunSuite:
  private val dimensions = Vector(3, 3, 2)
  private val labels = Vector(
    1, 1, 2,
    1, 3, 2,
    0, 3, 4,
    1, 1, 2,
    0, 3, 2,
    0, 4, 4
  )

  private val regions = RegionIndex(
    Vector(
      AtlasRegionMetadata(RegionId(1), "A"),
      AtlasRegionMetadata(RegionId(2), "B"),
      AtlasRegionMetadata(RegionId(3), "C"),
      AtlasRegionMetadata(RegionId(4), "D")
    )
  )

  private val atlas =
    val space = SampleSpaces(dimensions)
    val ref = AtlasRef.volume(
      family = "graph-differential",
      model = "GraphDifferential",
      templateSpace = SpaceId.Custom,
      coordSpace = SpaceId.MNI152,
      confidence = Confidence.Exact
    )
    VolumeAtlas.fromLabelVolume(
      ref,
      regions,
      AtlasTestImages.labelVolume(
        space,
        PrimitiveBuffers.fromArray(labels.toArray),
        "graph-differential"
      )
    )

  test("region contact counts match an independent full-neighborhood oracle"):
    Vector(VoxelConnectivity.Connect6, VoxelConnectivity.Connect18, VoxelConnectivity.Connect26).foreach: connectivity =>
      val actual = RegionGraph.adjacency(atlas, connectivity).map: edge =>
        (edge.from.id.value, edge.to.id.value) -> edge.weight
      .toMap

      assertEquals(actual, oracleContacts(connectivity), clue = connectivity.toString)

  test("region adjacency lowers to canonical graph topology without changing the atlas API"):
    val regionEdges = RegionGraph.adjacency(atlas, VoxelConnectivity.Connect6)
    val graph = RegionGraph.topology(atlas, VoxelConnectivity.Connect6)

    assertEquals(graph.topology.vertices.iterator.toSet, regions.ids.toSet)
    assertEquals(
      graph.weights.iterator.map: (edge, weight) =>
        val from = Math.min(edge.first.value, edge.second.value)
        val to = Math.max(edge.first.value, edge.second.value)
        (from, to, weight)
      .toVector
      .sortBy((from, to, _) => from -> to),
      regionEdges.map(edge => (edge.from.id.value, edge.to.id.value, edge.weight))
    )

  test("optimized contacts have exactly the relational quotient adjacency"):
    Vector(
      VoxelConnectivity.Connect6,
      VoxelConnectivity.Connect18,
      VoxelConnectivity.Connect26
    ).foreach: connectivity =>
      val projected = RegionGraph.relation(atlas, connectivity)
      val relationPairs =
        (for
          source <- projected.relation.from.indices
          target <- projected.relation
            .row(source)
            .indicesInDomainOrder
          if source.value < target.value
        yield
          (
            projected.regionIds(source).value,
            projected.regionIds(target).value
          )).toSet
      val optimizedPairs =
        RegionGraph
          .contactCounts(atlas, connectivity)
          .map(contact => (contact.from.id.value, contact.to.id.value))
          .toSet

      assertEquals(relationPairs, optimizedPairs, clue = connectivity.toString)

  private def oracleContacts(connectivity: VoxelConnectivity): Map[(Int, Int), Int] =
    val counts = mutable.Map.empty[(Int, Int), Int].withDefaultValue(0)
    var z = 0
    while z < dimensions(2) do
      var y = 0
      while y < dimensions(1) do
        var x = 0
        while x < dimensions(0) do
          val sourceLinear = Indexing.gridToIndex3D(dimensions, x, y, z)
          val source = labels(sourceLinear)
          if source != 0 then
            var dz = -1
            while dz <= 1 do
              var dy = -1
              while dy <= 1 do
                var dx = -1
                while dx <= 1 do
                  val manhattan = Math.abs(dx) + Math.abs(dy) + Math.abs(dz)
                  if manhattan > 0 && accepts(connectivity, manhattan) then
                    val nx = x + dx
                    val ny = y + dy
                    val nz = z + dz
                    if nx >= 0 && nx < dimensions(0) && ny >= 0 && ny < dimensions(1) && nz >= 0 && nz < dimensions(2) then
                      val targetLinear = Indexing.gridToIndex3D(dimensions, nx, ny, nz)
                      if targetLinear > sourceLinear then
                        val target = labels(targetLinear)
                        if target != 0 && target != source then
                          val key = if source < target then source -> target else target -> source
                          counts(key) += 1
                  dx += 1
                dy += 1
              dz += 1
          x += 1
        y += 1
      z += 1
    counts.toMap

  private def accepts(connectivity: VoxelConnectivity, manhattan: Int): Boolean =
    connectivity match
      case VoxelConnectivity.Connect6  => manhattan == 1
      case VoxelConnectivity.Connect18 => manhattan <= 2
      case VoxelConnectivity.Connect26 => true
