package scalafim.image

import ravel.Array1
import ravel.DType.given
import ravel.NDArray as RavelArray
import ravel.Shape

final case class ClusteredNeuroVol(
  mask: NeuroVol[Boolean],
  clusters: Array1[Int],
  labelMap: Map[Int, String] = Map.empty,
  label: String = ""
):
  val space: NeuroSpace = mask.space
  private val activeIdx: Array1[Int] = Mask.indices(mask)
  require(clusters.size == activeIdx.size, "clusters length must equal mask cardinality")
  private val firstInvalidClusterId =
    var invalid = Option.empty[Int]
    var i = 0
    while i < clusters.size && invalid.isEmpty do
      if clusters(i) <= 0 then invalid = Some(clusters(i))
      i += 1
    invalid
  require(
    firstInvalidClusterId.isEmpty,
    firstInvalidClusterId.map(value => ClusterIdError.NonPositive(value).message).getOrElse("")
  )

  private val ids: Vector[Int] =
    Vector.tabulate(clusters.size)(i => clusters(i)).distinct.sorted

  val labels: Map[Int, String] =
    if labelMap.isEmpty then ids.map(id => id -> s"Clus_$id").toMap
    else
      require(labelMap.keySet == ids.toSet, "labelMap keys must match cluster ids")
      labelMap

  val clusterMap: Map[Int, Array1[Int]] =
    ids.map { id =>
      val buf = Array.newBuilder[Int]
      var i = 0
      while i < clusters.size do
        if clusters(i) == id then buf += activeIdx(i)
        i += 1
      val indices = buf.result()
      id -> RavelArray.fromSeq(Shape(indices.length), indices)
    }.toMap

  def clusterIds: Vector[Int] = ids

  def typedClusterIds: Vector[ClusterId] =
    ids.map(ClusterId.unsafe)

  def indices(id: ClusterId): Array1[Int] =
    clusterMap(id.value)

  def numClusters: Int = ids.length

  def toDense: NeuroVol[Int] =
    val shape = space.spatialShape
    val lookup = IndexLookupVol(space, activeIdx)
    val full =
      RavelArray.tabulate[Int](shape.x, shape.y, shape.z):
        (x, y, z) =>
          val linear = Indexing.gridToIndex3D(shape, x, y, z)
          val position = lookup.lookup(linear)
          if position < 0 then 0 else clusters(position)
    NeuroVol.fromRavel(full, space, label)

  def splitClusters: Vector[ROIVol[Int]] =
    ids.map { id =>
      val idx = clusterMap(id)
      val coords = Vector.tabulate(idx.size)(i => Indexing.indexToGrid3D(space.spatialDims, idx(i)))
      val vals: Array1[Int] =
        RavelArray.fill(Shape(idx.size), id)
      ROIVol[Int](space, coords, vals)
    }

  /** Cluster centroids (center-of-mass) in grid coordinates by default. */
  def centroids(real: Boolean = false): Vector[Vector[Double]] =
    centroids(
      ClusteredNeuroVol.CentroidType.CenterOfMass,
      if real then SpatialCoordinateFrame.World else SpatialCoordinateFrame.Grid,
      eps = 1e-6,
      maxIter = 500
    )

  def centroids(frame: SpatialCoordinateFrame): Vector[Vector[Double]] =
    centroids(ClusteredNeuroVol.CentroidType.CenterOfMass, frame, eps = 1e-6, maxIter = 500)

  /** Cluster centroids.
    *
    * `CentroidType.Medoid` matches neuroim2's `"medoid"` option: geometric median
    * (Weiszfeld) of cluster coordinates.
    */
  def centroids(centroidType: ClusteredNeuroVol.CentroidType): Vector[Vector[Double]] =
    centroids(centroidType, SpatialCoordinateFrame.Grid, eps = 1e-6, maxIter = 500)

  def centroids(
    centroidType: ClusteredNeuroVol.CentroidType,
    real: Boolean,
    eps: Double,
    maxIter: Int
  ): Vector[Vector[Double]] =
    centroids(
      centroidType,
      if real then SpatialCoordinateFrame.World else SpatialCoordinateFrame.Grid,
      eps,
      maxIter
    )

  def centroids(
    centroidType: ClusteredNeuroVol.CentroidType,
    frame: SpatialCoordinateFrame,
    eps: Double,
    maxIter: Int
  ): Vector[Vector[Double]] =
    centroidType match
      case ClusteredNeuroVol.CentroidType.CenterOfMass =>
        ids.map { id =>
          val idx = clusterMap(id)
          require(idx.size > 0, "empty cluster")
          var sx = 0.0
          var sy = 0.0
          var sz = 0.0
          var i = 0
          while i < idx.size do
            val g = Indexing.indexToGrid3D(space.spatialDims, idx(i))
            if frame == SpatialCoordinateFrame.World then
              val r = space.indexToCoord(g.map(_.toDouble))
              sx += r(0); sy += r(1); sz += r(2)
            else
              sx += g(0).toDouble; sy += g(1).toDouble; sz += g(2).toDouble
            i += 1
          val n = idx.size.toDouble
          Vector(sx / n, sy / n, sz / n)
        }
      case ClusteredNeuroVol.CentroidType.Medoid =>
        ids.map { id =>
          val idx = clusterMap(id)
          require(idx.size > 0, "empty cluster")
          val m = idx.size
          if m == 1 then
            val g = Indexing.indexToGrid3D(space.spatialDims, idx(0))
            if frame == SpatialCoordinateFrame.World then space.indexToCoord(g.map(_.toDouble)) else g.map(_.toDouble)
          else
            val pts = Array.ofDim[Double](m, 3)
            var i = 0
            while i < m do
              val g = Indexing.indexToGrid3D(space.spatialDims, idx(i))
              val v =
                if frame == SpatialCoordinateFrame.World then space.indexToCoord(g.map(_.toDouble))
                else Vector(g(0).toDouble, g(1).toDouble, g(2).toDouble)
              pts(i)(0) = v(0); pts(i)(1) = v(1); pts(i)(2) = v(2)
              i += 1

            var x0 = 0.0; var y0 = 0.0; var z0 = 0.0
            i = 0
            while i < m do
              x0 += pts(i)(0); y0 += pts(i)(1); z0 += pts(i)(2)
              i += 1
            var x = x0 / m; var y = y0 / m; var z = z0 / m

            val zeroTol = 1e-12
            var iter = 0
            var converged = false
            while iter < maxIter && !converged do
              var numX = 0.0; var numY = 0.0; var numZ = 0.0
              var denom = 0.0
              var hit = -1
              i = 0
              while i < m && hit == -1 do
                val dx = pts(i)(0) - x
                val dy = pts(i)(1) - y
                val dz = pts(i)(2) - z
                val d = math.sqrt(dx * dx + dy * dy + dz * dz)
                if d < zeroTol then hit = i
                else
                  val w = 1.0 / d
                  numX += w * pts(i)(0)
                  numY += w * pts(i)(1)
                  numZ += w * pts(i)(2)
                  denom += w
                i += 1

              if hit >= 0 || denom == 0.0 then
                x = pts(math.max(hit, 0))(0)
                y = pts(math.max(hit, 0))(1)
                z = pts(math.max(hit, 0))(2)
                converged = true
              else
                val nx = numX / denom
                val ny = numY / denom
                val nz = numZ / denom
                val shift =
                  val dx = nx - x
                  val dy = ny - y
                  val dz = nz - z
                  math.sqrt(dx * dx + dy * dy + dz * dz)
                x = nx; y = ny; z = nz
                if shift < eps then converged = true

              iter += 1

            Vector(x, y, z)
        }

object ClusteredNeuroVol:
  private def ravelClusters(values: Array[Int]): Array1[Int] =
    RavelArray.fromSeq(Shape(values.length), values)

  def apply(
      mask: NeuroVol[Boolean],
      clusters: Array[Int]
  ): ClusteredNeuroVol =
    new ClusteredNeuroVol(mask, ravelClusters(clusters))

  def apply(
      mask: NeuroVol[Boolean],
      clusters: Array[Int],
      labelMap: Map[Int, String]
  ): ClusteredNeuroVol =
    new ClusteredNeuroVol(mask, ravelClusters(clusters), labelMap)

  def apply(
      mask: NeuroVol[Boolean],
      clusters: Array[Int],
      labelMap: Map[Int, String],
      label: String
  ): ClusteredNeuroVol =
    new ClusteredNeuroVol(
      mask,
      ravelClusters(clusters),
      labelMap,
      label
    )

  enum CentroidType:
    case CenterOfMass, Medoid

  export ConnComp.Connectivity

  def fromMask(
    mask: NeuroVol[Boolean],
    connectivity: Connectivity = Connectivity.Connect26,
    labelMap: Map[Int, String] = Map.empty,
    label: String = ""
  ): ClusteredNeuroVol =
    val (idxVol, _) = ConnComp.connComp3D(mask, connectivity, label)
    val activeIdx = Mask.indices(mask)
    val clusters =
      RavelArray.tabulate[Int](activeIdx.size): i =>
        idxVol.valueAtCanonicalOrdinal(activeIdx(i))
    ClusteredNeuroVol(mask, clusters, labelMap, label)

  def fromThreshold(
    vol: NeuroVol[Double],
    thr: Double,
    connectivity: Connectivity = Connectivity.Connect26,
    labelMap: Map[Int, String] = Map.empty,
    label: String = ""
  ): ClusteredNeuroVol =
    val mask = vol.mapValues(_ > thr).copy(label = label)
    fromMask(mask, connectivity, labelMap, label)
