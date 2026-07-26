package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.Ring
import scala.util.Random

object Searchlight:

  private def checkedCenter(space: NeuroSpace, center: Vector[Int]): SearchlightCenter =
    val voxel =
      VoxelCoord
        .fromVector(center, "searchlight center")
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    SearchlightCenter
      .make(space, voxel)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def centerRow(coords: Vector[Vector[Int]], center: VoxelCoord): Int =
    val row = coords.indexWhere(_ == center.toVector)
    if row >= 0 then row
    else throw new IllegalArgumentException(SearchlightError.CenterExcluded(center).message)

  private def validateMask(space: NeuroSpace, mask: Option[NeuroVol[Boolean]]): Unit =
    mask.foreach(value => GridCompatibility.requireSpatial(space, value.space))

  private def validateRadius(
      space: NeuroSpace,
      radius: SearchlightRadius
  ): Either[SearchlightError, Unit] =
    val minimumSpacing = space.spacing.min
    if radius.millimeters >= minimumSpacing then Right(())
    else Left(SearchlightError.RadiusBelowVoxelSpacing(radius.millimeters, minimumSpacing))

  /** Checked spherical extraction with a grid-bound center and explicit value support. */
  def sphericalRoiChecked[A: Ring](
      vol: NeuroVol[A],
      center: SearchlightCenter,
      radius: SearchlightRadius,
      fill: Option[A] = None,
      support: SearchlightValueSupport = SearchlightValueSupport.AllValues,
      label: String = ""
  )(using ClassTag[A]): Either[SearchlightError, ROIVolWindow[A]] =
    for
      volumeSpace <- VolumeSpace
        .fromSpatialPart(vol.space)
        .left
        .map(SearchlightError.InvalidSpace.apply)
      _ <- GridCompatibility
        .volume(volumeSpace, center.space)
        .left
        .map(SearchlightError.Grid.apply)
      _ <- validateRadius(vol.space, radius)
      _ <-
        val centerValue = fill.getOrElse(vol.linear(center.linearIndex))
        if support == SearchlightValueSupport.NonZero &&
            centerValue == summon[Ring[A]].zero
        then Left(SearchlightError.CenterExcluded(center.voxel))
        else Right(())
    yield sphericalRoi(
      vol,
      center.voxel.toVector,
      radius.millimeters,
      fill,
      nonzero = support == SearchlightValueSupport.NonZero,
      label
    )

  /** Checked searchlight construction with explicit center and support policies.
    *
    * Mask-constrained support requires mask-voxel centers so every yielded
    * window contains its center.
    */
  def searchlightChecked(
      mask: NeuroVol[Boolean],
      radius: SearchlightRadius,
      centerDomain: SearchlightCenterDomain,
      support: SearchlightSupport,
      label: String = ""
  ): Either[SearchlightError, Iterator[ROIVolWindow[Int]]] =
    if centerDomain == SearchlightCenterDomain.AllVoxels &&
        support == SearchlightSupport.InsideMask
    then Left(SearchlightError.IncompatiblePolicies(centerDomain, support))
    else
      validateRadius(mask.space, radius).map { _ =>
        val spatialNels = mask.space.spatialDims.product
        val centers =
          centerDomain match
            case SearchlightCenterDomain.AllVoxels =>
              Iterator.range(0, spatialNels)
            case SearchlightCenterDomain.MaskVoxels =>
              val indices = Mask.indices(mask)
              Iterator.tabulate(indices.length)(indices.apply)
        val selectedMask =
          support match
            case SearchlightSupport.FullNeighborhood => None
            case SearchlightSupport.InsideMask => Some(mask)

        centers.map { linearIndex =>
          val center = Indexing.indexToGrid3D(mask.space.spatialDims, linearIndex)
          sphericalRoi(
            mask.space,
            center,
            radius.millimeters,
            fill = 1,
            mask = selectedMask,
            label = label
          )
        }
      }

  def sphericalRoi[A: Ring](
    vol: NeuroVol[A],
    center: Vector[Int],
    radius: Double,
    fill: Option[A] = None,
    nonzero: Boolean = false,
    label: String = ""
  )(using ClassTag[A]): ROIVolWindow[A] =
    val sp = vol.space
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims

    val spacing = sp.spacing
    require(radius >= spacing.min, "radius too small relative to voxel spacing")
    val deltas = spacing.map(s => math.ceil(radius / s).toInt)
    val r2 = radius * radius
    val zero = summon[Ring[A]].zero

    val pairs = Vector.newBuilder[(Vector[Int], A)]
    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val dx = (x - center(0)) * spacing(0)
                val dy = (y - center(1)) * spacing(1)
                val dz = (z - center(2)) * spacing(2)
                if dx * dx + dy * dy + dz * dz <= r2 then
                  val coord = Vector(x, y, z)
                  val lin = Indexing.gridToIndex3D(dims, x, y, z)
                  val v = fill.getOrElse(vol.linear(lin))
                  if !nonzero || v != zero then pairs += ((coord, v))
              z += 1
          y += 1
      x += 1

    val sorted = pairs.result().sortBy { case (c, _) => (c(0), c(1), c(2)) }
    val coords = sorted.map(_._1)
    val dataArr = NArray.ofSize[A](coords.length)
    var i = 0
    while i < coords.length do
      dataArr(i) = sorted(i)._2
      i += 1

    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned(sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def sphericalRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]],
    label: String
  ): ROIVolWindow[Int] =
    val sp = space.spatialSpace
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims
    val spacing = sp.spacing
    require(radius >= spacing.min, "radius too small relative to voxel spacing")
    validateMask(sp, mask)

    val deltas = spacing.map(s => math.ceil(radius / s).toInt)
    val r2 = radius * radius

    val coordsBuf = Vector.newBuilder[Vector[Int]]
    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val dx = (x - center(0)) * spacing(0)
                val dy = (y - center(1)) * spacing(1)
                val dz = (z - center(2)) * spacing(2)
                if dx * dx + dy * dy + dz * dz <= r2 then
                  val coord = Vector(x, y, z)
                  val lin = Indexing.gridToIndex3D(dims, x, y, z)
                  val keep =
                    mask match
                      case Some(m) => m.linear(lin)
                      case None => true
                  if keep then coordsBuf += coord
              z += 1
          y += 1
      x += 1

    val coords = coordsBuf.result().sortBy(c => (c(0), c(1), c(2)))
    val dataArr = NArrayUtil.fillConst[Int](coords.length, fill)
    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned[Int](sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def sphericalRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double
  ): ROIVolWindow[Int] =
    sphericalRoi(space, center, radius, fill = 1, mask = None, label = "")

  def sphericalRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int
  ): ROIVolWindow[Int] =
    sphericalRoi(space, center, radius, fill, None, "")

  def sphericalRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]]
  ): ROIVolWindow[Int] =
    sphericalRoi(space, center, radius, fill, mask, "")

  def ellipsoidRoi[A: Ring](
    vol: NeuroVol[A],
    center: Vector[Int],
    radius: Double,
    scales: Vector[Double] = Vector(1.0, 1.0, 1.0),
    jitter: Double = 0.0,
    fill: Option[A] = None,
    nonzero: Boolean = false,
    rng: Random,
    label: String = ""
  )(using ClassTag[A]): ROIVolWindow[A] =
    require(scales.length == 3 && scales.forall(_ > 0), "scales must be length-3 positive")
    require(jitter >= 0, "jitter must be >= 0")
    val sp = vol.space
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims
    val spacing = sp.spacing
    val zero = summon[Ring[A]].zero

    val sc =
      if jitter == 0.0 then scales
      else
        scales.map { s =>
          math.max(s * (1.0 + rng.nextGaussian() * jitter), 1e-12)
        }

    val deltas = Vector.tabulate(3)(d => math.ceil(radius / (spacing(d) * sc(d))).toInt)
    val r2 = radius * radius
    val pairs = Vector.newBuilder[(Vector[Int], A)]

    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val dx = (x - center(0)) * spacing(0) * sc(0)
                val dy = (y - center(1)) * spacing(1) * sc(1)
                val dz = (z - center(2)) * spacing(2) * sc(2)
                if dx * dx + dy * dy + dz * dz <= r2 then
                  val coord = Vector(x, y, z)
                  val lin = Indexing.gridToIndex3D(dims, x, y, z)
                  val v = fill.getOrElse(vol.linear(lin))
                  if !nonzero || v != zero then pairs += ((coord, v))
              z += 1
          y += 1
      x += 1

    val sorted = pairs.result().sortBy { case (c, _) => (c(0), c(1), c(2)) }
    val coords = sorted.map(_._1)
    val dataArr = NArray.ofSize[A](coords.length)
    var i = 0
    while i < coords.length do
      dataArr(i) = sorted(i)._2
      i += 1

    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned(sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def ellipsoidRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    scales: Vector[Double],
    jitter: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]],
    rng: Random,
    label: String
  ): ROIVolWindow[Int] =
    val sp = space.spatialSpace
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims
    val spacing = sp.spacing
    require(scales.length == 3 && scales.forall(_ > 0), "scales must be length-3 positive")
    require(jitter >= 0, "jitter must be >= 0")
    validateMask(sp, mask)

    val sc =
      if jitter == 0.0 then scales
      else scales.map(s => math.max(s * (1.0 + rng.nextGaussian() * jitter), 1e-12))

    val deltas = Vector.tabulate(3)(d => math.ceil(radius / (spacing(d) * sc(d))).toInt)
    val r2 = radius * radius
    val coordsBuf = Vector.newBuilder[Vector[Int]]

    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val dx = (x - center(0)) * spacing(0) * sc(0)
                val dy = (y - center(1)) * spacing(1) * sc(1)
                val dz = (z - center(2)) * spacing(2) * sc(2)
                if dx * dx + dy * dy + dz * dz <= r2 then
                  val coord = Vector(x, y, z)
                  val lin = Indexing.gridToIndex3D(dims, x, y, z)
                  val keep = mask.forall(_.linear(lin))
                  if keep then coordsBuf += coord
              z += 1
          y += 1
      x += 1

    val coords = coordsBuf.result().sortBy(c => (c(0), c(1), c(2)))
    val dataArr = NArrayUtil.fillConst[Int](coords.length, fill)
    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned[Int](sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def ellipsoidRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double
  ): ROIVolWindow[Int] =
    ellipsoidRoi(space, center, radius, Vector(1.0, 1.0, 1.0), 0.0, 1, None, new Random(0L), "")

  def ellipsoidRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    scales: Vector[Double]
  ): ROIVolWindow[Int] =
    ellipsoidRoi(space, center, radius, scales, 0.0, 1, None, new Random(0L), "")

  def cubeRoi[A: Ring](
    vol: NeuroVol[A],
    center: Vector[Int],
    radius: Double,
    fill: Option[A] = None,
    nonzero: Boolean = false,
    label: String = ""
  )(using ClassTag[A]): ROIVolWindow[A] =
    val sp = vol.space
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims
    val spacing = sp.spacing
    val deltas = spacing.map(s => math.ceil(radius / s).toInt)
    val zero = summon[Ring[A]].zero

    val pairs = Vector.newBuilder[(Vector[Int], A)]
    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val coord = Vector(x, y, z)
                val lin = Indexing.gridToIndex3D(dims, x, y, z)
                val v = fill.getOrElse(vol.linear(lin))
                if !nonzero || v != zero then pairs += ((coord, v))
              z += 1
          y += 1
      x += 1

    val sorted = pairs.result().sortBy { case (c, _) => (c(0), c(1), c(2)) }
    val coords = sorted.map(_._1)
    val dataArr = NArray.ofSize[A](coords.length)
    var i = 0
    while i < coords.length do
      dataArr(i) = sorted(i)._2
      i += 1
    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned(sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def cubeRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]],
    label: String
  ): ROIVolWindow[Int] =
    val sp = space.spatialSpace
    val typedCenter = checkedCenter(sp, center)
    val dims = sp.spatialDims
    val spacing = sp.spacing
    validateMask(sp, mask)
    val deltas = spacing.map(s => math.ceil(radius / s).toInt)

    val coordsBuf = Vector.newBuilder[Vector[Int]]
    var x = center(0) - deltas(0)
    while x <= center(0) + deltas(0) do
      if x >= 0 && x < dims(0) then
        var y = center(1) - deltas(1)
        while y <= center(1) + deltas(1) do
          if y >= 0 && y < dims(1) then
            var z = center(2) - deltas(2)
            while z <= center(2) + deltas(2) do
              if z >= 0 && z < dims(2) then
                val lin = Indexing.gridToIndex3D(dims, x, y, z)
                val keep = mask.forall(_.linear(lin))
                if keep then coordsBuf += Vector(x, y, z)
              z += 1
          y += 1
      x += 1

    val coords = coordsBuf.result().sortBy(c => (c(0), c(1), c(2)))
    val dataArr = NArrayUtil.fillConst[Int](coords.length, fill)
    val centerIndex = centerRow(coords, typedCenter.voxel)
    ROIVolWindow
      .fromOwned[Int](sp, ROICoords(coords), dataArr, centerIndex, typedCenter.linearIndex, label)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def cubeRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double
  ): ROIVolWindow[Int] =
    cubeRoi(space, center, radius, fill = 1, mask = None, label = "")

  def cubeRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int
  ): ROIVolWindow[Int] =
    cubeRoi(space, center, radius, fill, None, "")

  def cubeRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]]
  ): ROIVolWindow[Int] =
    cubeRoi(space, center, radius, fill, mask, "")

  def blobbyRoi[A: Ring](
    vol: NeuroVol[A],
    center: Vector[Int],
    radius: Double,
    drop: Double = 0.3,
    edgeFraction: Double = 0.7,
    fill: Option[A] = None,
    nonzero: Boolean = false,
    rng: Random,
    label: String = ""
  )(using ClassTag[A]): ROIVolWindow[A] =
    require(drop >= 0 && drop <= 1.0, "drop must be in [0,1]")
    require(edgeFraction > 0 && edgeFraction <= 1.0, "edgeFraction must be in (0,1]")
    val base = sphericalRoi(vol, center, radius, fill = fill, nonzero = false, label = label)
    val coords0 = base.coords.coords
    if coords0.isEmpty then base
    else
      val dists = coords0.map { c =>
        val dx = c(0) - center(0)
        val dy = c(1) - center(1)
        val dz = c(2) - center(2)
        math.sqrt(dx * dx + dy * dy + dz * dz)
      }
      val sorted = dists.sorted
      val thr = sorted(math.floor(edgeFraction * (sorted.length - 1)).toInt)

      val keepIdx = coords0.indices.filter { i =>
        val isEdge = dists(i) >= thr
        !isEdge || rng.nextDouble() >= drop
      }

      val coords = keepIdx.map(coords0)
      val dataArr = NArray.ofSize[A](coords.length)
      var i = 0
      val zero = summon[Ring[A]].zero
      while i < coords.length do
        val lin = Indexing.gridToIndex3D(vol.space.spatialDims, coords(i)(0), coords(i)(1), coords(i)(2))
        val v = fill.getOrElse(vol.linear(lin))
        dataArr(i) = v
        i += 1

      val filteredPairs =
        if nonzero then
          val buf = Vector.newBuilder[(Vector[Int], A)]
          var j = 0
          while j < coords.length do
            if dataArr(j) != zero then buf += ((coords(j), dataArr(j)))
            j += 1
          buf.result()
        else coords.zip(Vector.tabulate(coords.length)(i => dataArr(i)))

      val fcoords = filteredPairs.map(_._1)
      val fdata = NArray.ofSize[A](filteredPairs.length)
      var k = 0
      while k < filteredPairs.length do
        fdata(k) = filteredPairs(k)._2
        k += 1

      val typedCenter = checkedCenter(vol.space, center)
      val centerIndex = centerRow(fcoords.toVector, typedCenter.voxel)
      ROIVolWindow
        .fromOwned(
          vol.space,
          ROICoords(fcoords.toVector),
          fdata,
          centerIndex,
          typedCenter.linearIndex,
          label
        )
        .fold(error => throw new IllegalArgumentException(error.message), identity)

  def blobbyRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    drop: Double,
    edgeFraction: Double,
    fill: Int,
    mask: Option[NeuroVol[Boolean]],
    rng: Random,
    label: String
  ): ROIVolWindow[Int] =
    require(drop >= 0 && drop <= 1.0, "drop must be in [0,1]")
    require(edgeFraction > 0 && edgeFraction <= 1.0, "edgeFraction must be in (0,1]")
    validateMask(space.spatialSpace, mask)
    val base = sphericalRoi(space, center, radius, fill, mask, label)
    val coords0 = base.coords.coords
    if coords0.isEmpty then base
    else
      val dists = coords0.map { c =>
        val dx = c(0) - center(0)
        val dy = c(1) - center(1)
        val dz = c(2) - center(2)
        math.sqrt(dx * dx + dy * dy + dz * dz)
      }
      val sorted = dists.sorted
      val thr = sorted(math.floor(edgeFraction * (sorted.length - 1)).toInt)

      val coords = coords0.indices.filter { i =>
        val isEdge = dists(i) >= thr
        !isEdge || rng.nextDouble() >= drop
      }.map(coords0)

      val dataArr = NArrayUtil.fillConst[Int](coords.length, fill)
      val centerIndex = centerRow(coords.toVector, checkedCenter(base.space, center).voxel)
      val parentIdx = base.parentIndex
      ROIVolWindow
        .fromOwned[Int](base.space, ROICoords(coords.toVector), dataArr, centerIndex, parentIdx, label)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

  def blobbyRoi(
    space: NeuroSpace,
    center: Vector[Int],
    radius: Double,
    rng: Random
  ): ROIVolWindow[Int] =
    blobbyRoi(space, center, radius, drop = 0.3, edgeFraction = 0.7, fill = 1, mask = None, rng = rng, label = "")

  /** Exhaustive spherical searchlight over voxel centers.
    *
    * If `nonzero` is false, centers cover the entire volume; otherwise only non-zero mask voxels.
    * Each element is the coordinates of voxels inside the searchlight sphere.
    */
  def searchlightCoords(
    mask: NeuroVol[Boolean],
    radius: Double,
    nonzero: Boolean = false
  ): Iterator[ROICoords] =
    val sp = mask.space
    val spatialNels = sp.spatialDims.product
    val centers: Iterator[Int] =
      if nonzero then
        val idx = Mask.indices(mask)
        Iterator.tabulate(idx.length)(i => idx(i))
      else Iterator.range(0, spatialNels)

    centers.map { lin =>
      val center = Indexing.indexToGrid3D(sp.spatialDims, lin)
      sphericalRoi(sp, center, radius, fill = 1, mask = if nonzero then Some(mask) else None, label = "").coords
    }

  /** Exhaustive spherical searchlight iterator over active mask voxels. */
  def searchlight(
    mask: NeuroVol[Boolean],
    radius: Double,
    nonzero: Boolean = false,
    label: String = ""
  ): Iterator[ROIVolWindow[Int]] =
    val sp = mask.space
    val idx = Mask.indices(mask)
    Iterator.tabulate(idx.length) { i =>
      val lin = idx(i)
      val center = Indexing.indexToGrid3D(sp.spatialDims, lin)
      sphericalRoi(sp, center, radius, fill = 1, mask = if nonzero then Some(mask) else None, label = label)
    }

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
    val ids = cvol.clusterIds

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
      val outData = narr.NArray.ofSize[Double](tLen * nNeigh)
      var c = 0
      while c < nNeigh do
        val col = neigh(c)
        val srcOff = col * tLen
        val dstOff = c * tLen
        var t = 0
        while t < tLen do
          outData(dstOff + t) = x.ts.data(srcOff + t)
          t += 1
        c += 1

      val coords = neigh.map(gridInt)
      val mat = NDArray[Double](outData, Vector(tLen, nNeigh))
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
      val coords = Vector.tabulate(idx.length)(i => Indexing.indexToGrid3D(sp.spatialDims, idx(i)))
      val data = NArrayUtil.fillConst[Int](idx.length, fill)
      ROIVol[Int](sp, coords, data)
    }
