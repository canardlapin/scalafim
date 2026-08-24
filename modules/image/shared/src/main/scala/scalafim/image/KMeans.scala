package scalafim.image

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import locus4s.Region
import locus4s.SpaceMismatch

final case class KMeansParcelMetadata(label: String):
  require(label.nonEmpty, "k-means parcel label must be non-empty")

enum KMeansPartitionError:
  case WrongVoxelOwner(error: SpaceMismatch)
  case InvalidClusterCount(value: Int, activeVoxels: Int)
  case InvalidMaximumIterations(value: Int)
  case InvalidTolerance(value: Double)
  case InvalidGridIndex(error: GridDomainError)
  case InvalidWorldCoordinate(error: GeometryError)
  case InvalidParcellation(error: VolumeParcellationError)

  def message: String =
    this match
      case WrongVoxelOwner(error) => error.message
      case InvalidClusterCount(value, activeVoxels) =>
        s"k-means cluster count must be in [1, $activeVoxels], found $value"
      case InvalidMaximumIterations(value) =>
        s"k-means maximum iterations must be positive, found $value"
      case InvalidTolerance(value) =>
        s"k-means tolerance must be finite and non-negative, found $value"
      case InvalidGridIndex(error) => error.message
      case InvalidWorldCoordinate(error) => error.message
      case InvalidParcellation(error) => error.message

sealed trait KMeansParcellationResolution[F <: Frame[D3], S]:
  type P
  val value: VolumeParcellation[F, S, P, KMeansParcelMetadata]
  val iterations: Int

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

  /** Partition an exact voxel region by world-coordinate k-means.
    *
    * The returned assignment is the sole cluster-membership representation;
    * centers are derived from its fibers when needed.
    */
  def partitionRegion[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      active: Region[T],
      k: Int,
      iterMax: Int = 200,
      seed: Int = 0,
      tolerance: Double = 1e-6,
      init: Init = Init.Random,
      parcelDomainName: String = "k-means parcels"
  ): Either[
    KMeansPartitionError,
    KMeansParcellationResolution[F, S]
  ] =
    Region
      .whole(domain.space)
      .intersectChecked(active)
      .left
      .map(KMeansPartitionError.WrongVoxelOwner.apply)
      .flatMap: exactActive =>
        if k <= 0 || k > exactActive.cardinality then
          Left(
            KMeansPartitionError.InvalidClusterCount(
              k,
              exactActive.cardinality
            )
          )
        else if iterMax <= 0 then
          Left(KMeansPartitionError.InvalidMaximumIterations(iterMax))
        else if !tolerance.isFinite || tolerance < 0.0 then
          Left(KMeansPartitionError.InvalidTolerance(tolerance))
        else
          worldCoordinates(domain, exactActive).flatMap: points =>
            val result =
              if k == 1 then
                Result(
                  Array.fill(points.length)(1),
                  Vector(coordinateMean(points)),
                  0
                )
              else
                fit(
                  points,
                  k,
                  iterMax = iterMax,
                  seed = seed,
                  tol = tolerance,
                  init = init
                )
            val targetOrdinals =
              Array.fill[Option[Int]](domain.space.size)(None)
            val activeIndices = exactActive.indicesInDomainOrder
            var position = 0
            while activeIndices.hasNext do
              targetOrdinals(activeIndices.next().ordinal) =
                Some(result.labels(position) - 1)
              position += 1
            val metadata =
              Vector.tabulate(k): parcelOrdinal =>
                KMeansParcelMetadata(s"Cluster_${parcelOrdinal + 1}")
            VolumeParcellation
              .resolve(
                domain,
                parcelDomainName,
                metadata,
                targetOrdinals
              )
              .left
              .map(KMeansPartitionError.InvalidParcellation.apply)
              .map: resolved =>
                new KMeansParcellationResolution[F, S]:
                  type P = resolved.P
                  val value = resolved.value
                  val iterations = result.iterations

  private def worldCoordinates[F <: Frame[D3], S](
      domain: GridDomain[F, D3, S],
      active: Region[S]
  ): Either[KMeansPartitionError, Vector[Vector[Double]]] =
    val output = Vector.newBuilder[Vector[Double]]
    val indices = active.indicesInDomainOrder
    var error = Option.empty[KMeansPartitionError]
    while indices.hasNext && error.isEmpty do
      domain.indexOf(indices.next()) match
        case Left(gridError) =>
          error = Some(KMeansPartitionError.InvalidGridIndex(gridError))
        case Right(lattice) =>
          domain.grid.indexToFrame.apply(lattice.values.map(_.toDouble)) match
            case Left(geometryError) =>
              error = Some(
                KMeansPartitionError.InvalidWorldCoordinate(geometryError)
              )
            case Right(point) => output += point
    error match
      case Some(value) => Left(value)
      case None        => Right(output.result())

  private def coordinateMean(
      points: Vector[Vector[Double]]
  ): Vector[Double] =
    val sums = Array.ofDim[Double](3)
    var point = 0
    while point < points.length do
      var axis = 0
      while axis < 3 do
        sums(axis) += points(point)(axis)
        axis += 1
      point += 1
    Vector.tabulate(3)(axis => sums(axis) / points.length.toDouble)

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
