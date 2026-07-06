package scalafim.surface

enum DistanceMetric:
  case Euclidean, Geodesic, Spherical

final case class NeighborHit(source: VertexId, target: VertexId, distance: Double)

final case class DistanceMatrix(
  rowVertices: Vector[VertexId],
  colVertices: Vector[VertexId],
  data: Vector[Double]
):
  require(data.length == rowVertices.length * colVertices.length, "distance matrix shape mismatch")

  def rows: Int =
    rowVertices.length

  def cols: Int =
    colVertices.length

  def apply(row: Int, col: Int): Double =
    require(row >= 0 && row < rows, "distance matrix row out of range")
    require(col >= 0 && col < cols, "distance matrix column out of range")
    data(row * cols + col)

final class GeodesicCache:
  private val store = scala.collection.mutable.Map.empty[GeodesicCache.Key, DistanceMatrix]

  def size: Int =
    store.size

  def clear(): Unit =
    store.clear()

  private[surface] def get(key: GeodesicCache.Key): Option[DistanceMatrix] =
    store.get(key)

  private[surface] def put(key: GeodesicCache.Key, value: DistanceMatrix): DistanceMatrix =
    store.update(key, value)
    value

object GeodesicCache:
  final case class Key(
    metric: DistanceMetric,
    rows: Vector[Int],
    cols: Vector[Int],
    weightsHash: Int,
    vertexCount: Int,
    edgeCount: Int
  )

enum CachePolicy:
  case Disabled
  case Use(cache: GeodesicCache)

object CachePolicy:
  val NoCache: CachePolicy = Disabled

object SurfaceGeodesics:

  def distanceMatrix(
    topology: MeshTopology,
    sources: Seq[VertexId],
    targets: Seq[VertexId],
    metric: DistanceMetric = DistanceMetric.Geodesic,
    edgeWeights: Option[Seq[Double]] = None,
    chunkSize: Int = 2000,
    cachePolicy: CachePolicy = CachePolicy.Disabled
  ): DistanceMatrix =
    require(sources.nonEmpty, "sources must be non-empty")
    require(targets.nonEmpty, "targets must be non-empty")
    require(chunkSize > 0, "chunkSize must be positive")

    val src = sources.toVector
    val tgt = targets.toVector
    validateVertices(topology, src, "source")
    validateVertices(topology, tgt, "target")

    val weights = weightsFor(topology, edgeWeights, metric)
    val key =
      GeodesicCache.Key(
        metric = metric,
        rows = src.map(_.index),
        cols = tgt.map(_.index),
        weightsHash = weightsHash(weights),
        vertexCount = topology.mesh.vertexCount,
        edgeCount = topology.edgeCount
      )

    cachePolicy match
      case CachePolicy.Use(cache) =>
        cache.get(key).getOrElse {
          cache.put(key, computeMatrix(topology, src, tgt, metric, weights, chunkSize))
        }
      case CachePolicy.Disabled =>
        computeMatrix(topology, src, tgt, metric, weights, chunkSize)

  def allPairsDistanceMatrix(
    topology: MeshTopology,
    metric: DistanceMetric = DistanceMetric.Geodesic
  ): DistanceMatrix =
    val all = Vector.tabulate(topology.mesh.vertexCount)(VertexId.unsafe)
    distanceMatrix(topology, all, all, metric)

  def distances(
    topology: MeshTopology,
    source: VertexId,
    targets: Seq[VertexId],
    metric: DistanceMetric = DistanceMetric.Geodesic,
    edgeWeights: Option[Seq[Double]] = None
  ): Vector[Double] =
    distanceMatrix(topology, Vector(source), targets, metric, edgeWeights).data

  def neighborsWithin(
    topology: MeshTopology,
    radius: Double,
    sources: Seq[VertexId],
    metric: DistanceMetric = DistanceMetric.Geodesic,
    edgeWeights: Option[Seq[Double]] = None
  ): Vector[NeighborHit] =
    require(radius > 0.0 && radius.isFinite, "radius must be positive and finite")
    val allTargets = Vector.tabulate(topology.mesh.vertexCount)(VertexId.unsafe)
    val matrix = distanceMatrix(topology, sources, allTargets, metric, edgeWeights)
    val hits = Vector.newBuilder[NeighborHit]

    var r = 0
    while r < matrix.rows do
      var c = 0
      while c < matrix.cols do
        val d = matrix(r, c)
        if d < radius then hits += NeighborHit(matrix.rowVertices(r), matrix.colVertices(c), d)
        c += 1
      r += 1

    hits.result()

  private def computeMatrix(
    topology: MeshTopology,
    sources: Vector[VertexId],
    targets: Vector[VertexId],
    metric: DistanceMetric,
    weights: Vector[Double],
    chunkSize: Int
  ): DistanceMatrix =
    val out = Array.fill(sources.length * targets.length)(Double.PositiveInfinity)
    val targetIndex = targets.zipWithIndex.map { case (id, i) => id.index -> i }.toMap

    var chunkStart = 0
    while chunkStart < sources.length do
      val chunkEnd = math.min(sources.length, chunkStart + chunkSize)
      var row = chunkStart
      while row < chunkEnd do
        metric match
          case DistanceMetric.Euclidean =>
            fillDirect(topology, sources(row), targets, row, out, euclideanDistance)
          case DistanceMetric.Spherical =>
            fillDirect(topology, sources(row), targets, row, out, sphericalDistance(topology))
          case DistanceMetric.Geodesic =>
            val d = dijkstra(topology, sources(row), weights)
            targetIndex.foreach { case (vertex, col) =>
              out(row * targets.length + col) = d(vertex)
            }
        row += 1
      chunkStart = chunkEnd

    DistanceMatrix(sources, targets, out.toVector)

  private def fillDirect(
    topology: MeshTopology,
    source: VertexId,
    targets: Vector[VertexId],
    row: Int,
    out: Array[Double],
    distance: (Point3D, Point3D) => Double
  ): Unit =
    val srcPoint = topology.mesh.vertex(source)
    var col = 0
    while col < targets.length do
      out(row * targets.length + col) = distance(srcPoint, topology.mesh.vertex(targets(col)))
      col += 1

  private def dijkstra(topology: MeshTopology, source: VertexId, weights: Vector[Double]): Array[Double] =
    val adjacency = weightedAdjacency(topology, weights)
    val distances = Array.fill(topology.mesh.vertexCount)(Double.PositiveInfinity)
    val visited = Array.fill(topology.mesh.vertexCount)(false)
    val queue =
      scala.collection.mutable.PriorityQueue.empty[(Double, Int)](
        Ordering.by[(Double, Int), Double] { case (distance, _) => -distance }
      )

    distances(source.index) = 0.0
    queue.enqueue((0.0, source.index))

    while queue.nonEmpty do
      val (dist, vertex) = queue.dequeue()
      if !visited(vertex) then
        visited(vertex) = true
        adjacency(vertex).foreach { case (neighbor, weight) =>
          val alt = dist + weight
          if alt < distances(neighbor) then
            distances(neighbor) = alt
            queue.enqueue((alt, neighbor))
        }

    distances

  private def weightedAdjacency(topology: MeshTopology, weights: Vector[Double]): Vector[Vector[(Int, Double)]] =
    val rows = Array.fill(topology.mesh.vertexCount)(Vector.newBuilder[(Int, Double)])
    var i = 0
    while i < topology.edges.length do
      val edge = topology.edges(i)
      val w = weights(i)
      rows(edge.a.index) += ((edge.b.index, w))
      rows(edge.b.index) += ((edge.a.index, w))
      i += 1
    rows.toVector.map(_.result())

  private def euclideanDistance(a: Point3D, b: Point3D): Double =
    (a - b).norm

  private def sphericalDistance(topology: MeshTopology)(a: Point3D, b: Point3D): Double =
    val radius = sphereRadius(topology)
    val an = a.norm
    val bn = b.norm
    require(an > 0.0 && bn > 0.0, "spherical distance requires non-zero vertex coordinates")
    val cosine = clamp(a.dot(b) / (an * bn), -1.0, 1.0)
    radius * math.acos(cosine)

  private def sphereRadius(topology: MeshTopology): Double =
    val radii = topology.mesh.vertices.map(_.norm)
    val radius = radii.sum / radii.length
    require(radius > 0.0 && radius.isFinite, "spherical distance requires a positive finite radius")
    radius

  private def weightsFor(
    topology: MeshTopology,
    edgeWeights: Option[Seq[Double]],
    metric: DistanceMetric
  ): Vector[Double] =
    val weights =
      edgeWeights match
        case Some(values) => values.toVector
        case None => topology.edgeLengths
    require(weights.length == topology.edgeCount, "edgeWeights length must equal topology edge count")
    require(weights.forall(w => w.isFinite && w >= 0.0), "edgeWeights must be finite and non-negative")
    if metric == DistanceMetric.Geodesic then
      require(weights.exists(_ > 0.0), "geodesic edgeWeights must contain at least one positive value")
    weights

  private def validateVertices(topology: MeshTopology, vertices: Seq[VertexId], role: String): Unit =
    vertices.foreach { vertex =>
      require(vertex.index >= 0 && vertex.index < topology.mesh.vertexCount, s"$role vertex id out of range")
    }

  private def weightsHash(weights: Vector[Double]): Int =
    var h = 1
    weights.foreach { weight =>
      val bits = java.lang.Double.doubleToLongBits(if weight == 0.0 then 0.0 else weight)
      h = 31 * h + (bits ^ (bits >>> 32)).toInt
    }
    h

  private def clamp(value: Double, low: Double, high: Double): Double =
    math.max(low, math.min(high, value))
