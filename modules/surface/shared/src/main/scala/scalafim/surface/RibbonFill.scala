package scalafim.surface

import image4s.SampleSpace
import image4s.geometry.{D3, Frame}
import ravel.DType.given
import scalafim.image.{GridSpec, MaskVolume, NeuroVolume, ScalarVolume, SomeScalarVolume}
import scalafim.image.SomeNeuroVolume.*

/** Failures of compiling or applying a cortical-ribbon operator or mask. */
enum RibbonError derives CanEqual:
  case InvalidSteps(steps: Int)
  case TopologyMismatch(reason: String)
  case RoleMismatch(reason: String)
  case FrameMismatch(reason: String)
  case OpenSurface(role: String, badEdges: Int)
  case LengthMismatch(what: String, expected: Int, actual: Int)
  case VolumeGridMismatch(reason: String)
  case Volume(reason: String)

  def message: String =
    this match
      case InvalidSteps(steps)                => s"ribbon sampling needs 1 to ${RibbonSteps.Max} steps, got $steps"
      case RoleMismatch(reason)               => s"white and pial surfaces are not an inner/outer pair: $reason"
      case TopologyMismatch(reason)           => s"white and pial surfaces do not correspond: $reason"
      case FrameMismatch(reason)              => s"ribbon inputs are not in one frame: $reason"
      case OpenSurface(role, badEdges)        => s"$role surface is not closed: $badEdges edges are not shared by exactly two faces"
      case LengthMismatch(what, expected, actual) => s"$what: expected $expected values, got $actual"
      case VolumeGridMismatch(reason)         => s"volume does not match the ribbon grid: $reason"
      case Volume(reason)                     => s"ribbon volume construction failed: $reason"

/** Number of equal intervals each white->pial segment is cut into; `n` steps sample `n + 1` points, fractions `s / n`. */
opaque type RibbonSteps = Int

object RibbonSteps:
  /** neurotransform's `n_ribbon_samples` default. */
  val Default: RibbonSteps = 6

  /** Upper bound: far beyond any useful sub-voxel density, and small enough that row scratch space stays tiny. */
  val Max: Int = 1024

  def apply(steps: Int): Either[RibbonError, RibbonSteps] =
    if steps >= 1 && steps <= Max then Right(steps) else Left(RibbonError.InvalidSteps(steps))

  extension (steps: RibbonSteps)
    inline def value: Int = steps

/** How surface values are written back into ribbon voxels. */
enum RibbonProjection derives CanEqual:
  /** The exact adjoint `Wᵀ s`: each voxel receives the weighted sum of the vertex values that sampled it. */
  case Adjoint

  /** `Wᵀ s / Wᵀ 1`: each voxel receives the weighted mean of the vertex values that sampled it. */
  case WeightedMean

/** The compiled vertex-by-voxel ribbon sampling operator `W` for one white/pial pair on one grid.
  *
  * Row `v` averages trilinear weights over `steps + 1` equally spaced points on the segment from white vertex `v` to
  * pial vertex `v`; points outside the half-voxel-padded grid are skipped, corner indices are clamped to the grid, and
  * each nonempty row sums to one. This is neurotransform's `cpp_ribbon_weights`. Volume-to-surface sampling applies
  * `W`; surface-to-volume ribbon fill applies its adjoint `Wᵀ` ([[adjoint]], [[project]]).
  *
  * Voxel columns are canonical ordinals of `grid` (`(x * ny + y) * nz + z`). The triplets are ordered by vertex, then
  * voxel.
  */
final class RibbonOperator[F <: Frame[D3]] private (
    val grid: GridSpec[F],
    val steps: RibbonSteps,
    val vertexCount: Int,
    rowStart: Array[Int],
    colIndex: Array[Int],
    weight: Array[Double]
):
  /** The grid's sample space as a stable value, so volumes built on it are typed by it. */
  val sampleSpace: SampleSpace[F, D3] = grid.sampleSpace

  def voxelCount: Int = grid.nVoxels

  def nonZeros: Int = weight.length

  /** Row (vertex) of each stored weight. */
  def rows: IArray[Int] =
    val out = new Array[Int](weight.length)
    var v = 0
    while v < vertexCount do
      var k = rowStart(v)
      while k < rowStart(v + 1) do
        out(k) = v
        k += 1
      v += 1
    IArray.unsafeFromArray(out)

  /** Column (canonical voxel ordinal) of each stored weight. */
  def cols: IArray[Int] = IArray.unsafeFromArray(colIndex.clone())

  def vals: IArray[Double] = IArray.unsafeFromArray(weight.clone())

  /** Whether any sample of vertex `v`'s segment landed in the grid. */
  def sampled(vertex: VertexId): Boolean =
    vertex.index < vertexCount && rowStart(vertex.index + 1) > rowStart(vertex.index)

  /** `Wᵀ 1`: the total weight each voxel receives from all vertices. */
  lazy val coverage: IArray[Double] =
    val out = new Array[Double](voxelCount)
    var k = 0
    while k < weight.length do
      out(colIndex(k)) += weight(k)
      k += 1
    IArray.unsafeFromArray(out)

  /** Voxels with positive coverage: the ribbon as the operator sees it. This includes the clamped and fractional
    * trilinear corners of every sample, so it is a dilated, sampling-dependent ribbon (it can include corners whose
    * weight is a rounding residue); use [[RibbonMask]] for the geometric ribbon.
    */
  def support: IArray[Boolean] = IArray.unsafeFromArray(supportArray)

  private lazy val supportArray: Array[Boolean] = Array.tabulate(voxelCount)(i => coverage(i) > 0.0)

  /** Volume -> surface: `W v`, one value per vertex; vertices whose segment never entered the grid get NaN. */
  def sample(volume: Array[Double]): Either[RibbonError, Array[Double]] =
    if volume.length != voxelCount then Left(RibbonError.LengthMismatch("ribbon sample volume", voxelCount, volume.length))
    else
      val out = new Array[Double](vertexCount)
      var v = 0
      while v < vertexCount do
        val (start, end) = (rowStart(v), rowStart(v + 1))
        if start == end then out(v) = Double.NaN
        else
          var sum = 0.0
          var k = start
          while k < end do
            sum += weight(k) * volume(colIndex(k))
            k += 1
          out(v) = sum
        v += 1
      Right(out)

  /** Surface -> volume: `Wᵀ s`, one value per voxel; voxels no segment sampled are zero. */
  def adjoint(values: Array[Double]): Either[RibbonError, Array[Double]] =
    if values.length != vertexCount then Left(RibbonError.LengthMismatch("ribbon adjoint surface values", vertexCount, values.length))
    else
      val out = new Array[Double](voxelCount)
      var v = 0
      while v < vertexCount do
        var k = rowStart(v)
        while k < rowStart(v + 1) do
          out(colIndex(k)) += weight(k) * values(v)
          k += 1
        v += 1
      Right(out)

  /** Ribbon fill of per-vertex values; voxels outside the operator's support receive `outside`. */
  def project(values: Array[Double], projection: RibbonProjection, outside: Double = 0.0): Either[RibbonError, Array[Double]] =
    adjoint(values).map: filled =>
      val cover = coverage
      var i = 0
      while i < filled.length do
        if cover(i) <= 0.0 then filled(i) = outside
        else if projection == RibbonProjection.WeightedMean then filled(i) = filled(i) / cover(i)
        i += 1
      filled

  /** [[sample]] of a volume, which must lie on exactly this operator's grid (same frame, shape and affine). */
  def sampleVolume(volume: SomeScalarVolume[Double]): Either[RibbonError, Array[Double]] =
    for
      actual <- GridSpec.fromSpaceEither(volume.space).left.map(error => RibbonError.VolumeGridMismatch(error.message))
      _ <- RibbonGrid.requireSame(grid, actual)
      values <- sample(volume.copyToCanonicalArray)
    yield values

  /** [[project]] as a scalar volume on this operator's grid. */
  def projectVolume(
      values: Array[Double],
      projection: RibbonProjection,
      outside: Double = 0.0
  ): Either[RibbonError, ScalarVolume[sampleSpace.type, Double]] =
    project(values, projection, outside).flatMap: data =>
      NeuroVolume.copyContinuousFromCanonicalArray(sampleSpace, data).left.map(error => RibbonError.Volume(error.message))

  /** [[support]] as a mask volume on this operator's grid. */
  def supportVolume: Either[RibbonError, MaskVolume[sampleSpace.type]] =
    NeuroVolume.copyMaskFromCanonicalArray(sampleSpace, supportArray).left.map(error => RibbonError.Volume(error.message))

object RibbonOperator:
  /** Compile `W` for `white`/`pial` (one vertex-matched mesh pair) on `grid`, all in frame `F`. */
  def compile[F <: Frame[D3]](
      white: FramedSurface[F],
      pial: FramedSurface[F],
      grid: GridSpec[F],
      steps: RibbonSteps = RibbonSteps.Default
  ): Either[RibbonError, RibbonOperator[F]] =
    for
      _ <- if white.hasSameTopology(pial) then Right(()) else Left(RibbonError.TopologyMismatch("white and pial must share vertex count and ordered faces"))
      _ <- RibbonGrid.requireRoles(white, pial)
      _ <- RibbonGrid.requireFrame("white", white.frame, grid)
      _ <- RibbonGrid.requireFrame("pial", pial.frame, grid)
    yield build(white.coordinates, pial.coordinates, grid, steps)

  private def build[F <: Frame[D3]](white: IArray[Double], pial: IArray[Double], grid: GridSpec[F], steps: RibbonSteps): RibbonOperator[F] =
    val w2v = grid.affine.inverse.rowMajor.toArray
    val Vector(nx, ny, nz) = grid.dims
    val n = white.length / 3
    val capacity = (steps.value + 1) * 8
    val scratchCols = new Array[Int](capacity)
    val scratchVals = new Array[Double](capacity)
    val rowStart = new Array[Int](n + 1)
    var cols = new Array[Int](math.max(16, n * 8))
    var vals = new Array[Double](cols.length)
    var nnz = 0
    var v = 0
    while v < n do
      val (x0, y0, z0) = (white(3 * v), white(3 * v + 1), white(3 * v + 2))
      val (x1, y1, z1) = (pial(3 * v), pial(3 * v + 1), pial(3 * v + 2))
      var used = 0
      var s = 0
      while s <= steps.value do
        val alpha = s.toDouble / steps.value.toDouble
        val wx = x0 + (x1 - x0) * alpha
        val wy = y0 + (y1 - y0) * alpha
        val wz = z0 + (z1 - z0) * alpha
        val vx = w2v(0) * wx + w2v(1) * wy + w2v(2) * wz + w2v(3)
        val vy = w2v(4) * wx + w2v(5) * wy + w2v(6) * wz + w2v(7)
        val vz = w2v(8) * wx + w2v(9) * wy + w2v(10) * wz + w2v(11)
        if !(vx < -0.5 || vy < -0.5 || vz < -0.5 || vx > nx - 0.5 || vy > ny - 0.5 || vz > nz - 0.5) then
          val (xf, yf, zf) = (math.floor(vx).toInt, math.floor(vy).toInt, math.floor(vz).toInt)
          val (fx, fy, fz) = (vx - xf, vy - yf, vz - zf)
          var iz = 0
          while iz < 2 do
            var iy = 0
            while iy < 2 do
              var ix = 0
              while ix < 2 do
                val w = (if ix == 0 then 1.0 - fx else fx) * (if iy == 0 then 1.0 - fy else fy) * (if iz == 0 then 1.0 - fz else fz)
                if w != 0.0 then
                  val xi = math.min(math.max(xf + ix, 0), nx - 1)
                  val yi = math.min(math.max(yf + iy, 0), ny - 1)
                  val zi = math.min(math.max(zf + iz, 0), nz - 1)
                  val col = (xi * ny + yi) * nz + zi
                  var j = 0
                  while j < used && scratchCols(j) != col do j += 1
                  if j == used then
                    scratchCols(used) = col
                    scratchVals(used) = w
                    used += 1
                  else scratchVals(j) += w
                ix += 1
              iy += 1
            iz += 1
        s += 1
      sortPairs(scratchCols, scratchVals, used)
      var total = 0.0
      var j = 0
      while j < used do
        total += scratchVals(j)
        j += 1
      if total > 0.0 then
        if nnz + used > cols.length then
          val grown = math.max(cols.length * 2, nnz + used)
          cols = java.util.Arrays.copyOf(cols, grown)
          vals = java.util.Arrays.copyOf(vals, grown)
        j = 0
        while j < used do
          cols(nnz) = scratchCols(j)
          vals(nnz) = scratchVals(j) / total
          nnz += 1
          j += 1
      rowStart(v + 1) = nnz
      v += 1
    new RibbonOperator(grid, steps, n, rowStart, java.util.Arrays.copyOf(cols, nnz), java.util.Arrays.copyOf(vals, nnz))

  /** Insertion sort of the first `size` (column, value) pairs by column; rows hold at most `8 * (steps + 1)` entries. */
  private def sortPairs(cols: Array[Int], vals: Array[Double], size: Int): Unit =
    var i = 1
    while i < size do
      val (c, w) = (cols(i), vals(i))
      var j = i - 1
      while j >= 0 && cols(j) > c do
        cols(j + 1) = cols(j)
        vals(j + 1) = vals(j)
        j -= 1
      cols(j + 1) = c
      vals(j + 1) = w
      i += 1

/** Voxels of `grid` whose centres lie in the solid cortical ribbon: inside the closed pial surface and outside the
  * closed white surface (FreeSurfer's `ribbon.mgz` definition). Unlike [[RibbonOperator.support]] this does not depend on
  * vertex density.
  */
final class RibbonMask[F <: Frame[D3]] private (val grid: GridSpec[F], voxels: Array[Boolean]):
  val sampleSpace: SampleSpace[F, D3] = grid.sampleSpace

  /** Membership per canonical voxel ordinal. */
  def values: IArray[Boolean] = IArray.unsafeFromArray(voxels.clone())

  lazy val count: Int = voxels.count(identity)

  def contains(x: Int, y: Int, z: Int): Boolean =
    val Vector(nx, ny, nz) = grid.dims
    x >= 0 && y >= 0 && z >= 0 && x < nx && y < ny && z < nz && voxels((x * ny + y) * nz + z)

  def toVolume: Either[RibbonError, MaskVolume[sampleSpace.type]] =
    NeuroVolume.copyMaskFromCanonicalArray(sampleSpace, voxels.clone()).left.map(error => RibbonError.Volume(error.message))

object RibbonMask:
  /** Voxel centres inside `pial` and not inside `white`; both surfaces must be closed. Order matters: swapped
    * surfaces would give an empty ribbon, so surfaces whose kinds say they are swapped are rejected.
    */
  def fill[F <: Frame[D3]](white: FramedSurface[F], pial: FramedSurface[F], grid: GridSpec[F]): Either[RibbonError, RibbonMask[F]] =
    for
      _ <- RibbonGrid.requireRoles(white, pial)
      outer <- insideArray("pial", pial, grid)
      inner <- insideArray("white", white, grid)
    yield
      val out = new Array[Boolean](outer.length)
      var i = 0
      while i < out.length do
        out(i) = outer(i) && !inner(i)
        i += 1
      new RibbonMask(grid, out)

  /** Voxel centres inside one closed surface. */
  def inside[F <: Frame[D3]](surface: FramedSurface[F], grid: GridSpec[F]): Either[RibbonError, RibbonMask[F]] =
    insideArray("surface", surface, grid).map(new RibbonMask(grid, _))

  // Rays run along the grid's first index axis through every (j, k) lattice line, shifted by an irrational-looking
  // offset so they almost surely miss mesh vertices and edges; the parity of crossings beyond a voxel centre decides it.
  private val JitterJ = 1.2345678901e-7
  private val JitterK = 7.6543210987e-8

  private def insideArray[F <: Frame[D3]](role: String, surface: FramedSurface[F], grid: GridSpec[F]): Either[RibbonError, Array[Boolean]] =
    for
      _ <- RibbonGrid.requireFrame(role, surface.frame, grid)
      _ <- requireClosed(role, surface.mesh)
    yield scan(surface.mesh, grid)

  private def scan[F <: Frame[D3]](mesh: TriangleMesh, grid: GridSpec[F]): Array[Boolean] =
    val w2v = grid.affine.inverse.rowMajor.toArray
    val Vector(nx, ny, nz) = grid.dims
    val c = mesh.coordinates
    val n = mesh.vertexCount
    val vox = new Array[Double](3 * n)
    var v = 0
    while v < n do
      val (x, y, z) = (c(3 * v), c(3 * v + 1), c(3 * v + 2))
      vox(3 * v) = w2v(0) * x + w2v(1) * y + w2v(2) * z + w2v(3)
      vox(3 * v + 1) = w2v(4) * x + w2v(5) * y + w2v(6) * z + w2v(7) - JitterJ
      vox(3 * v + 2) = w2v(8) * x + w2v(9) * y + w2v(10) * z + w2v(11) - JitterK
      v += 1
    val hits = new Array[Array[Double]](ny * nz)
    val counts = new Array[Int](ny * nz)
    val f = mesh.faceIndices
    var t = 0
    while t < mesh.faceCount do
      val (a, b, d) = (3 * f(3 * t), 3 * f(3 * t + 1), 3 * f(3 * t + 2))
      val (ja, ka, jb, kb, jd, kd) = (vox(a + 1), vox(a + 2), vox(b + 1), vox(b + 2), vox(d + 1), vox(d + 2))
      val det = (jb - ja) * (kd - ka) - (jd - ja) * (kb - ka)
      if det != 0.0 then
        val jlo = math.max(0, math.ceil(math.min(ja, math.min(jb, jd))).toInt)
        val jhi = math.min(ny - 1, math.floor(math.max(ja, math.max(jb, jd))).toInt)
        val klo = math.max(0, math.ceil(math.min(ka, math.min(kb, kd))).toInt)
        val khi = math.min(nz - 1, math.floor(math.max(ka, math.max(kb, kd))).toInt)
        var j = jlo
        while j <= jhi do
          var k = klo
          while k <= khi do
            val la = ((jb - j) * (kd - k) - (jd - j) * (kb - k)) / det
            val lb = ((jd - j) * (ka - k) - (ja - j) * (kd - k)) / det
            val ld = 1.0 - la - lb
            if la >= 0.0 && lb >= 0.0 && ld >= 0.0 then
              val line = j * nz + k
              if hits(line) == null then hits(line) = new Array[Double](4)
              else if counts(line) == hits(line).length then hits(line) = java.util.Arrays.copyOf(hits(line), hits(line).length * 2)
              hits(line)(counts(line)) = la * vox(a) + lb * vox(b) + ld * vox(d)
              counts(line) += 1
            k += 1
          j += 1
      t += 1
    val out = new Array[Boolean](nx * ny * nz)
    var line = 0
    while line < hits.length do
      val size = counts(line)
      if size > 0 then
        val xs = hits(line)
        java.util.Arrays.sort(xs, 0, size)
        val (j, k) = (line / nz, line % nz)
        var p = 0
        var i = 0
        while i < nx do
          while p < size && xs(p) <= i do p += 1
          if ((size - p) & 1) == 1 then out((i * ny + j) * nz + k) = true
          i += 1
      line += 1
    out

  private def requireClosed(role: String, mesh: TriangleMesh): Either[RibbonError, Unit] =
    // Undirected edge keys `min * n + max` are exact in a Double (n² < 2^53) and sort fast on both platforms.
    val n = mesh.vertexCount.toDouble
    val f = mesh.faceIndices
    val keys = new Array[Double](f.length)
    var e = 0
    while e < f.length do
      val (p, q) = (f(e), f(if e % 3 == 2 then e - 2 else e + 1))
      keys(e) = math.min(p, q) * n + math.max(p, q)
      e += 1
    java.util.Arrays.sort(keys)
    var bad = 0
    var start = 0
    while start < keys.length do
      var end = start + 1
      while end < keys.length && keys(end) == keys(start) do end += 1
      if end - start != 2 then bad += 1
      start = end
    if bad == 0 then Right(()) else Left(RibbonError.OpenSurface(role, bad))

private object RibbonGrid:
  def requireRoles(white: FramedSurface[?], pial: FramedSurface[?]): Either[RibbonError, Unit] =
    if white.hemisphere != pial.hemisphere then
      Left(RibbonError.RoleMismatch(s"hemispheres differ: ${white.hemisphere.code} vs ${pial.hemisphere.code}"))
    else if white.kind == SurfaceKind.Pial || pial.kind == SurfaceKind.White then
      Left(RibbonError.RoleMismatch(s"inner surface is ${white.kind.label} and outer surface is ${pial.kind.label}"))
    else Right(())

  def requireFrame[F <: Frame[D3]](role: String, frame: F, grid: GridSpec[F]): Either[RibbonError, Unit] =
    Frame
      .alignOwners[D3, F, F](frame, grid.frame)
      .left
      .map(error => RibbonError.FrameMismatch(s"$role surface frame vs grid frame: ${error.message}"))
      .map(_ => ())

  def requireSame(expected: GridSpec[?], actual: GridSpec[?]): Either[RibbonError, Unit] =
    if expected.dims != actual.dims then Left(RibbonError.VolumeGridMismatch(s"shape ${actual.dims} vs ${expected.dims}"))
    else if expected.affine.rowMajor != actual.affine.rowMajor then Left(RibbonError.VolumeGridMismatch("voxel-to-world affines differ"))
    else
      Frame
        .alignOwners[D3, Frame[D3], Frame[D3]](expected.providerFrame, actual.providerFrame)
        .left
        .map(error => RibbonError.VolumeGridMismatch(error.message))
        .map(_ => ())
