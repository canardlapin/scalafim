package scalafim.image

import ravel.DType.given
import ravel.NDArray as RavelArray
import ravel.Shape

object Searchlight:

  /** Cluster-centroid searchlight over cluster time series.
    *
    * For each seed cluster, returns a ROIVec whose columns are the time series
    * of the nearest clusters (including the seed itself).
    */
  def clusterSearchlightSeries(
    x: ClusteredNeuroVec[Double],
    k: Int = 10,
    radius: Option[Double] = None,
    realDistances: Boolean = true,
    label: String = ""
  ): Vector[ROIVec[Double]] =
    clusterSearchlightSeries(
      x,
      k,
      radius,
      if realDistances then SpatialCoordinateFrame.World else SpatialCoordinateFrame.Grid,
      label
    )

  def clusterSearchlightSeries(
    x: ClusteredNeuroVec[Double],
    k: Int,
    radius: Option[Double],
    frame: SpatialCoordinateFrame,
    label: String
  ): Vector[ROIVec[Double]] =
    require(k > 0, "k must be positive")
    val cvol = x.cvol
    val centGrid = cvol.centroids(SpatialCoordinateFrame.Grid)
    val centDist =
      if frame == SpatialCoordinateFrame.Grid then centGrid
      else cvol.centroids(frame)
    val K = centGrid.length
    val tLen = x.nVolumes
    val gridInt = centGrid.map { c =>
      Vector(math.round(c(0)).toInt, math.round(c(1)).toInt, math.round(c(2)).toInt)
    }

    val kEff = math.min(k, K)

    def dist(i: Int, j: Int): Double =
      val a = centDist(i)
      val b = centDist(j)
      val dx = a(0) - b(0)
      val dy = a(1) - b(1)
      val dz = a(2) - b(2)
      math.sqrt(dx * dx + dy * dy + dz * dz)

    Vector.tabulate(K) { seed =>
      val dists = Array.ofDim[Double](K)
      var j = 0
      while j < K do
        dists(j) = dist(seed, j)
        j += 1

      val neigh =
        radius match
          case Some(r) =>
            Vector.tabulate(K)(identity).filter(j => dists(j) <= r).sortBy(j => dists(j))
          case None =>
            Vector.tabulate(K)(identity).sortBy(j => dists(j)).take(kEff)

      val nNeigh = neigh.length
      val coords = neigh.map(gridInt)
      val mat =
        RavelArray.tabulate[Double](tLen, nNeigh) { (time, column) =>
          x.ts(time, neigh(column))
        }
      ROIVec(x.space, coords, mat)
    }

  def clusterSearchlightSeries(
    vec: NeuroVec[Double],
    cvol: ClusteredNeuroVol,
    k: Int,
    radius: Option[Double],
    realDistances: Boolean,
    label: String
  ): Vector[ROIVec[Double]] =
    import spire.std.double.given
    val cv = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol, label = label)
    clusterSearchlightSeries(cv, k, radius, realDistances, label)

  /** Iterator over cluster ROIs (one per cluster).
    *
    * Equivalent to R `clustered_searchlight` when `cvol` is provided.
    */
  def clusteredSearchlight(
    cvol: ClusteredNeuroVol,
    fill: Int = 1,
    label: String = ""
  ): Iterator[ROIVol[Int]] =
    val sp = cvol.space
    cvol.clusterIds.iterator.map { id =>
      val idx = cvol.clusterMap(id)
      val coords = Vector.tabulate(idx.size)(i => Indexing.indexToGrid3D(sp.spatialDims, idx(i)))
      val data = RavelArray.fill(Shape(idx.size), fill)
      ROIVol[Int](sp, coords, data)
    }
