package scalafim.surface

import image4s.geometry.{Affine, D3, Frame}
import scalafim.image.{GridSpec, SpatialDims}
import scalafim.image.SomeNeuroVolume
import scalafim.image.SomeNeuroVolume.*
import scalafim.image.world.*
import scalafim.surface.fixtures.{RibbonRFixture, SphereMeshes}

class RibbonFillSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val scanner: Frame[D3] = ok(WorldSpace.declare("ribbon test scanner RAS").map(FrameCatalog.frame))

  // ---- neurotransform parity ------------------------------------------------------------------------------------

  private object Parity:
    val grid = ok(GridSpec.in(scanner)(SpatialDims(7, 6, 5), ok(Affine.fromRowMajor[D3](RibbonRFixture.voxelToWorldRowMajor))))
    private val faces = Vector((0, 1, 2), (3, 4, 5))
    val white = ok(FramedSurface.in(scanner)(SurfaceGeometry(TriangleMesh.fromRows(RibbonRFixture.white, faces)), SurfacePlacement.StoredCoordinates))
    val pial = ok(FramedSurface.in(scanner)(SurfaceGeometry(TriangleMesh.fromRows(RibbonRFixture.pial, faces)), SurfacePlacement.StoredCoordinates))
    val operator = ok(RibbonOperator.compile(white, pial, grid, ok(RibbonSteps(RibbonRFixture.steps))))

    def ordinal(i: Double, j: Double, k: Double): Int =
      (i.toInt * 6 + j.toInt) * 5 + k.toInt

  test("ribbon weights match neurotransform cpp_ribbon_weights entry for entry"):
    val op = Parity.operator
    val (rows, cols, vals) = (op.rows, op.cols, op.vals)
    val actual = rows.indices.map(k => (rows(k), cols(k)) -> vals(k)).toMap
    val expected = RibbonRFixture.weights.map(r => (r(0).toInt, Parity.ordinal(r(1), r(2), r(3))) -> r(4)).toMap
    // Corners whose trilinear weight is a rounding residue (~1e-16) may be stored on one side only, because R's
    // `solve` and image4s's inverse differ in the last bits; every stored weight must still agree to 1e-12.
    (actual.keySet ++ expected.keySet).foreach: key =>
      assertEqualsDouble(actual.getOrElse(key, 0.0), expected.getOrElse(key, 0.0), 1e-12, s"entry $key")
    val substantive = (m: Map[(Int, Int), Double]) => m.filter(_._2 > 1e-12).keySet
    assertEquals(substantive(actual), substantive(expected))

  test("surface-to-volume fill matches neurotransform backproject_surface_to_volume(method = \"ribbon\")"):
    val filled = ok(Parity.operator.adjoint(RibbonRFixture.surfaceValues.toArray))
    val expected = RibbonRFixture.adjoint.map(r => Parity.ordinal(r(0), r(1), r(2)) -> r(3)).toMap
    filled.indices.foreach: voxel =>
      assertEqualsDouble(filled(voxel), expected.getOrElse(voxel, 0.0), 1e-12, s"voxel $voxel")

  test("rows sum to one, and a segment that never enters the grid samples to NaN"):
    val op = Parity.operator
    val (rows, vals) = (op.rows, op.vals)
    val sums = new Array[Double](op.vertexCount)
    rows.indices.foreach(k => sums(rows(k)) += vals(k))
    (0 until op.vertexCount).foreach: v =>
      if v == 3 then
        assert(!op.sampled(VertexId(v)))
        assertEqualsDouble(sums(v), 0.0, 0.0)
      else assertEqualsDouble(sums(v), 1.0, 1e-12)
    val ones = ok(op.sample(Array.fill(op.voxelCount)(1.0)))
    assert(ones(3).isNaN)
    (0 until op.vertexCount).filter(_ != 3).foreach(v => assertEqualsDouble(ones(v), 1.0, 1e-12))

  test("the fill is the exact adjoint of volume-to-surface sampling"):
    val op = Parity.operator
    val rng = new scala.util.Random(8011)
    val x = Array.fill(op.voxelCount)(rng.nextGaussian())
    val s = Array.fill(op.vertexCount)(rng.nextGaussian())
    val wx = ok(op.sample(x)).map(v => if v.isNaN then 0.0 else v)
    val wts = ok(op.adjoint(s))
    val left = wx.indices.map(i => wx(i) * s(i)).sum
    val right = x.indices.map(i => x(i) * wts(i)).sum
    assertEqualsDouble(left, right, 1e-12)

  // ---- analytic concentric spheres -------------------------------------------------------------------------------

  private object Shell:
    val center = (0.31, -0.23, 0.17)
    val (whiteRadius, pialRadius) = (10.0, 16.0)
    // Oblique, anisotropic grid: rotation about z by 25 degrees and x by 10 degrees, spacing (1.0, 1.1, 0.9).
    val voxelToWorld: Vector[Double] =
      val (cz, sz, cx, sx) = (math.cos(0.4363), math.sin(0.4363), math.cos(0.1745), math.sin(0.1745))
      val rz = Vector(Vector(cz, -sz, 0.0), Vector(sz, cz, 0.0), Vector(0.0, 0.0, 1.0))
      val rx = Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, cx, -sx), Vector(0.0, sx, cx))
      val spacing = Vector(1.0, 1.1, 0.9)
      val r = Vector.tabulate(3, 3)((i, j) => (0 until 3).map(k => rz(i)(k) * rx(k)(j)).sum * spacing(j))
      Vector.tabulate(3)(i => r(i) ++ Vector(-20.0 * r(i).sum)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0)
    val dims = SpatialDims(40, 40, 44)
    val grid = ok(GridSpec.in(scanner)(dims, ok(Affine.fromRowMajor[D3](voxelToWorld))))

    def world(i: Int, j: Int, k: Int): (Double, Double, Double) =
      val m = voxelToWorld
      (m(0) * i + m(1) * j + m(2) * k + m(3), m(4) * i + m(5) * j + m(6) * k + m(7), m(8) * i + m(9) * j + m(10) * k + m(11))

    def radius(i: Int, j: Int, k: Int): Double =
      val (x, y, z) = world(i, j, k)
      math.sqrt(math.pow(x - center._1, 2) + math.pow(y - center._2, 2) + math.pow(z - center._3, 2))

    def surface(radius: Double, levels: Int): FramedSurface[scanner.type] =
      ok(FramedSurface.in(scanner)(SurfaceGeometry(SphereMeshes.icosphere(levels, radius, center)), SurfacePlacement.StoredCoordinates))

    def voxels: Iterator[(Int, Int, Int, Int)] =
      for
        i <- Iterator.range(0, dims.x)
        j <- Iterator.range(0, dims.y)
        k <- Iterator.range(0, dims.z)
      yield (i, j, k, (i * dims.y + j) * dims.z + k)

  test("ribbon mask of concentric spheres is the lattice shell between their radii"):
    val mask = ok(RibbonMask.fill(Shell.surface(Shell.whiteRadius, 4), Shell.surface(Shell.pialRadius, 4), Shell.grid))
    var expected = 0
    Shell.voxels.foreach: (i, j, k, _) =>
      val r = Shell.radius(i, j, k)
      if r > Shell.whiteRadius && r <= Shell.pialRadius then expected += 1
      if r > Shell.whiteRadius + 0.2 && r < Shell.pialRadius - 0.2 then assert(mask.contains(i, j, k), s"($i,$j,$k) r=$r")
      if r < Shell.whiteRadius - 0.2 || r > Shell.pialRadius + 0.2 then assert(!mask.contains(i, j, k), s"($i,$j,$k) r=$r")
    val analytic = 4.0 / 3.0 * math.Pi * (math.pow(Shell.pialRadius, 3) - math.pow(Shell.whiteRadius, 3)) / (1.0 * 1.1 * 0.9)
    assert(math.abs(mask.count - expected).toDouble / expected < 0.01, s"mask ${mask.count} vs lattice $expected")
    assert(math.abs(expected - analytic) / analytic < 0.01, s"lattice $expected vs analytic $analytic")
    val volume = SomeNeuroVolume.eraseSpace(ok(mask.toVolume))
    assertEquals(volume.copyToCanonicalArray.count(identity), mask.count)

  test("the ribbon mask requires closed surfaces"):
    val open = TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(5.0, 0.0, 0.0), Vector(0.0, 5.0, 0.0)), Vector((0, 1, 2)))
    val sheet = ok(FramedSurface.in(scanner)(SurfaceGeometry(open), SurfacePlacement.StoredCoordinates))
    assertEquals(RibbonMask.inside(sheet, Shell.grid).map(_.count), Left(RibbonError.OpenSurface("surface", 3)))

  test("ribbon sampling of a radial volume recovers the mid-ribbon radius, and the mean fill of ones is one"):
    val white = Shell.surface(Shell.whiteRadius, 3)
    val pial = Shell.surface(Shell.pialRadius, 3)
    val op = ok(RibbonOperator.compile(white, pial, Shell.grid, ok(RibbonSteps(8))))
    val radial = new Array[Double](op.voxelCount)
    Shell.voxels.foreach((i, j, k, o) => radial(o) = Shell.radius(i, j, k))
    val sampled = ok(op.sample(radial))
    val mid = (Shell.whiteRadius + Shell.pialRadius) / 2.0
    sampled.foreach(v => assertEqualsDouble(v, mid, 0.08))
    val filled = ok(op.project(Array.fill(op.vertexCount)(1.0), RibbonProjection.WeightedMean, outside = Double.NaN))
    val support = op.support
    Shell.voxels.foreach: (i, j, k, o) =>
      if support(o) then
        assertEqualsDouble(filled(o), 1.0, 1e-12)
        val r = Shell.radius(i, j, k)
        assert(r > Shell.whiteRadius - 1.6 && r < Shell.pialRadius + 1.6, s"support voxel at r=$r")
      else assert(filled(o).isNaN)

  test("volume adapters keep the operator's grid and reject other grids"):
    val op = ok(RibbonOperator.compile(Shell.surface(Shell.whiteRadius, 2), Shell.surface(Shell.pialRadius, 2), Shell.grid))
    val values = Array.tabulate(op.vertexCount)(_.toDouble)
    val filled = ok(op.projectVolume(values, RibbonProjection.Adjoint))
    val erased = SomeNeuroVolume.eraseSpace(filled)
    assertEquals(erased.copyToCanonicalArray.toVector, ok(op.project(values, RibbonProjection.Adjoint)).toVector)
    assertEquals(ok(op.sampleVolume(erased)).toVector, ok(op.sample(erased.copyToCanonicalArray)).toVector)
    val supportVolume = SomeNeuroVolume.eraseSpace(ok(op.supportVolume))
    assertEquals(supportVolume.grid.shape, Vector(40, 40, 44))
    assertEquals(supportVolume.copyToCanonicalArray.toVector, op.support.toVector)
    val other = ok(GridSpec.in(scanner)(SpatialDims(7, 6, 5), Affine.identity[D3]))
    val otherOp = ok(RibbonOperator.compile(Parity.white, Parity.pial, other))
    assert(otherOp.sampleVolume(erased).left.exists(_.isInstanceOf[RibbonError.VolumeGridMismatch]))
    assertEquals(op.sample(Array(1.0)), Left(RibbonError.LengthMismatch("ribbon sample volume", op.voxelCount, 1)))

  // ---- contracts --------------------------------------------------------------------------------------------------

  test("ribbon inputs must share a frame: statically when frames are typed, at runtime when they are erased"):
    val errors = compileErrors(
      """
      val elsewhere = FrameCatalog.frame(WorldSpace.Unresolved)
      val grid = GridSpec.in(elsewhere)(SpatialDims(7, 6, 5), Affine.identity[D3]).toOption.get
      RibbonOperator.compile(Parity.white, Parity.pial, grid)
      """
    )
    assert(errors.contains("Found:"), errors)
    // Simulate owners erased at a dynamic boundary, where only the runtime check stands between the inputs.
    val other: Frame[D3] = ok(WorldSpace.declare("another scanner").map(FrameCatalog.frame))
    val erasedGrid = ok(GridSpec.in(other)(SpatialDims(7, 6, 5), Affine.identity[D3])).asInstanceOf[GridSpec[Frame[D3]]]
    val erasedWhite: FramedSurface[Frame[D3]] = Parity.white.asInstanceOf[FramedSurface[Frame[D3]]]
    val erasedPial: FramedSurface[Frame[D3]] = Parity.pial.asInstanceOf[FramedSurface[Frame[D3]]]
    assert(RibbonOperator.compile(erasedWhite, erasedPial, erasedGrid).left.exists(_.isInstanceOf[RibbonError.FrameMismatch]))
    assert(RibbonMask.fill(erasedWhite, erasedPial, erasedGrid).left.exists(_.isInstanceOf[RibbonError.FrameMismatch]))

  test("white and pial must be vertex-matched, and steps must be positive"):
    val white = Shell.surface(Shell.whiteRadius, 2)
    val pial = Shell.surface(Shell.pialRadius, 3)
    assert(RibbonOperator.compile(white, pial, Shell.grid).left.exists(_.isInstanceOf[RibbonError.TopologyMismatch]))
    assertEquals(RibbonSteps(0), Left(RibbonError.InvalidSteps(0)))
    assertEquals(RibbonSteps.Default.value, 6)
