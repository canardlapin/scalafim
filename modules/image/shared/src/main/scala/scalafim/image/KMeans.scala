package scalafim.image

import ravel.NDArray as RavelArray
import ravel.Shape

object KMeans:

  enum Init:
    case Random, KMeansPlusPlus

  private final class Lcg(private var state: Int):
    def nextInt(bound: Int): Int =
      state = state * 1664525 + 1013904223
      val n = (state >>> 1) % bound
      if n < 0 then -n else n
    def nextDouble(): Double =
      nextInt(Int.MaxValue).toDouble / Int.MaxValue.toDouble

  final case class Result(
    labels: Array[Int],               // 1-based cluster ids, length = n points
    centers: Vector[Vector[Double]],   // k x 3 centers
    iterations: Int
  )

  def fit(
    points: Vector[Vector[Double]],
    k: Int,
    iterMax: Int = 200,
    seed: Int = 0,
    tol: Double = 1e-6,
    init: Init = Init.Random
  ): Result =
    require(k > 1, "k must be > 1")
    require(points.nonEmpty, "points must be non-empty")
    val n = points.length
    require(k <= n, "k must be <= number of points")

    val pts = Array.ofDim[Double](n, 3)
    var i = 0
    while i < n do
      val p = points(i)
      pts(i)(0) = p(0); pts(i)(1) = p(1); pts(i)(2) = p(2)
      i += 1

    val rng = new Lcg(seed)

    val centers = Array.ofDim[Double](k, 3)

    def initRandom(): Unit =
      val idx = Array.tabulate(n)(identity)
      var j = n - 1
      while j > 0 do
        val r = rng.nextInt(j + 1)
        val tmp = idx(j)
        idx(j) = idx(r)
        idx(r) = tmp
        j -= 1
      var c = 0
      while c < k do
        val p = pts(idx(c))
        centers(c)(0) = p(0); centers(c)(1) = p(1); centers(c)(2) = p(2)
        c += 1

    def initPlusPlus(): Unit =
      // first center random
      val first = rng.nextInt(n)
      centers(0)(0) = pts(first)(0); centers(0)(1) = pts(first)(1); centers(0)(2) = pts(first)(2)
      val dist2 = Array.ofDim[Double](n)
      var c = 1
      while c < k do
        var sum = 0.0
        i = 0
        while i < n do
          var best = Double.PositiveInfinity
          var j = 0
          while j < c do
            val dx = pts(i)(0) - centers(j)(0)
            val dy = pts(i)(1) - centers(j)(1)
            val dz = pts(i)(2) - centers(j)(2)
            val d = dx * dx + dy * dy + dz * dz
            if d < best then best = d
            j += 1
          dist2(i) = best
          sum += best
          i += 1
        val target = rng.nextDouble() * sum
        var acc = 0.0
        var chosen = n - 1
        i = 0
        while i < n && acc < target do
          acc += dist2(i)
          if acc >= target then chosen = i
          i += 1
        centers(c)(0) = pts(chosen)(0); centers(c)(1) = pts(chosen)(1); centers(c)(2) = pts(chosen)(2)
        c += 1

    init match
      case Init.Random => initRandom()
      case Init.KMeansPlusPlus => initPlusPlus()

    val labelsArr = Array.ofDim[Int](n)
    val sums = Array.ofDim[Double](k, 3)
    val counts = Array.ofDim[Int](k)

    var iter = 0
    var continue = true
    while iter < iterMax && continue do
      var anyChange = false
      // assignment
      i = 0
      while i < n do
        var best = 0
        var bestDist =
          val dx = pts(i)(0) - centers(0)(0)
          val dy = pts(i)(1) - centers(0)(1)
          val dz = pts(i)(2) - centers(0)(2)
          dx * dx + dy * dy + dz * dz
        var c = 1
        while c < k do
          val dx = pts(i)(0) - centers(c)(0)
          val dy = pts(i)(1) - centers(c)(1)
          val dz = pts(i)(2) - centers(c)(2)
          val d = dx * dx + dy * dy + dz * dz
          if d < bestDist then
            bestDist = d
            best = c
          c += 1
        if labelsArr(i) != best then
          labelsArr(i) = best
          anyChange = true
        i += 1

      // reset accumulators
      var c = 0
      while c < k do
        sums(c)(0) = 0.0; sums(c)(1) = 0.0; sums(c)(2) = 0.0
        counts(c) = 0
        c += 1

      // accumulate
      i = 0
      while i < n do
        val lab = labelsArr(i)
        counts(lab) += 1
        sums(lab)(0) += pts(i)(0)
        sums(lab)(1) += pts(i)(1)
        sums(lab)(2) += pts(i)(2)
        i += 1

      var maxShift = 0.0
      c = 0
      while c < k do
        if counts(c) == 0 then
          val r = rng.nextInt(n)
          val nx = pts(r)(0); val ny = pts(r)(1); val nz = pts(r)(2)
          val dx = nx - centers(c)(0)
          val dy = ny - centers(c)(1)
          val dz = nz - centers(c)(2)
          val shift = math.sqrt(dx * dx + dy * dy + dz * dz)
          if shift > maxShift then maxShift = shift
          centers(c)(0) = nx; centers(c)(1) = ny; centers(c)(2) = nz
        else
          val nx = sums(c)(0) / counts(c)
          val ny = sums(c)(1) / counts(c)
          val nz = sums(c)(2) / counts(c)
          val dx = nx - centers(c)(0)
          val dy = ny - centers(c)(1)
          val dz = nz - centers(c)(2)
          val shift = math.sqrt(dx * dx + dy * dy + dz * dz)
          if shift > maxShift then maxShift = shift
          centers(c)(0) = nx; centers(c)(1) = ny; centers(c)(2) = nz
        c += 1

      iter += 1
      continue = anyChange && maxShift > tol

    val outLabels = Array.ofDim[Int](n)
    i = 0
    while i < n do
      outLabels(i) = labelsArr(i) + 1
      i += 1

    val outCenters =
      Vector.tabulate(k) { c =>
        Vector(centers(c)(0), centers(c)(1), centers(c)(2))
      }

    Result(outLabels, outCenters, iter)

  /** Partition a boolean mask into k clusters using k-means on real coordinates.
    * Mirrors neuroim2 `partition(LogicalNeuroVol, k)`.
    */
  def partition(
    mask: NeuroVol[Boolean],
    k: Int,
    iterMax: Int = 200,
    seed: Int = 0,
    init: Init = Init.Random,
    label: String = ""
  ): ClusteredNeuroVol =
    val sp = mask.space
    val activeIdx = Mask.indices(mask)
    require(activeIdx.size >= k, "k must be <= number of active voxels")

    val pts = Vector.tabulate(activeIdx.size) { i =>
      val lin = activeIdx(i)
      val g = Indexing.indexToGrid3D(sp.spatialDims, lin)
      sp.indexToCoord(g.map(_.toDouble))
    }
    val res = fit(pts, k, iterMax = iterMax, seed = seed, init = init)
    ClusteredNeuroVol(
      mask,
      RavelArray.fromSeq(Shape(res.labels.length), res.labels),
      label = label
    )

  /** Partition a numeric volume by clustering non-zero voxels (as.logical in R). */
  def partitionNonZero(
    vol: NeuroVol[Double],
    k: Int,
    iterMax: Int,
    seed: Int,
    init: Init,
    label: String
  ): ClusteredNeuroVol =
    val flags = Array.ofDim[Boolean](vol.values.size)
    var i = 0
    while i < flags.length do
      flags(i) = vol.linear(i) != 0.0
      i += 1
    val mask = NeuroVol.fromLinear[Boolean](flags, vol.space.spatialSpace, label)
    partition(mask, k, iterMax, seed, init, label)
