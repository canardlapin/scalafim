package scalafim.image.world

import image4s.geometry.{Affine, D3}
import munit.ScalaCheckSuite
import org.scalacheck.{Gen, Prop}

class ToolCoordinatesSuite extends ScalaCheckSuite:
  private def affine(values: Double*): Affine[D3] =
    Affine.fromRowMajor[D3](values.toVector).fold(e => fail(e.message), identity)

  private def apply(a: Affine[D3], p: Vector[Double]): Vector[Double] =
    a(p).fold(e => fail(e.message), identity)

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-9)(using munit.Location): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tol))

  private val obliqueRas = affine(0.9, -0.2, 0.1, 12.0, 0.15, 1.1, 0.05, -30.0, -0.1, 0.02, 2.4, 8.5, 0, 0, 0, 1)
  private val obliqueLas = affine(-0.9, 0.2, 0.1, 50.0, 0.15, 1.1, 0.05, -30.0, 0.1, 0.02, 2.4, 8.5, 0, 0, 0, 1)
  private def fsl(v2w: Affine[D3]) =
    FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 0, None, 2, Some(v2w)).fold(e => fail(e.message), identity)

  private val coordinate = Gen.choose(-200.0, 200.0)
  private val point = for x <- coordinate; y <- coordinate; z <- coordinate yield Vector(x, y, z)

  property("every convention round-trips through RAS"):
    val systems = Vector(
      ToolCoordinates.RasMm,
      ToolCoordinates.LpsMm,
      ToolCoordinates.FslScaledVoxel(fsl(obliqueRas)),
      ToolCoordinates.FslScaledVoxel(fsl(obliqueLas)),
      ToolCoordinates.TkRas(FreeSurferVolumeGeometry(Vector(64, 72, 50), obliqueRas)),
      ToolCoordinates.VoxelIndex(obliqueLas)
    )
    Prop.forAll(point): p =>
      systems.foreach: system =>
        val back = apply(ToolCoordinates.fromRas(system), apply(ToolCoordinates.toRas(system), p))
        assertClose(back, p, 1e-8)
      true

  property("the LPS flip negates x and y only and is self-inverse"):
    Prop.forAll(point): p =>
      assertClose(apply(ToolCoordinates.LpsToRas, p), Vector(-p(0), -p(1), p(2)))
      assertClose(apply(ToolCoordinates.LpsToRas, apply(ToolCoordinates.LpsToRas, p)), p)
      true

  test("FSL mirrors x exactly when the selected affine is neurological"):
    val neuro = fsl(obliqueRas)
    val radio = fsl(obliqueLas)
    assert(neuro.neurological)
    assert(!radio.neurological)
    assertClose(apply(neuro.voxelToFsl, Vector(0.0, 1.0, 2.0)), Vector(6 * 1.1, 0.8, 4.6))
    assertClose(apply(radio.voxelToFsl, Vector(0.0, 1.0, 2.0)), Vector(0.0, 0.8, 4.6))

  test("FSL selects the sform, then the qform, then scaling"):
    val both = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueLas), 2, Some(obliqueRas))
    val qformOnly = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueLas), 0, Some(obliqueRas))
    val neither = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(-1.1, 0.8, 2.3), 0, None, 0, None)
    assertEquals(both.map(_.selected), Right(FslAffineSource.Sform))
    assertEquals(qformOnly.map(_.selected), Right(FslAffineSource.Qform))
    assertEquals(neither.map(_.selected), Right(FslAffineSource.Scaling))
    assertEquals(neither.map(_.pixdim), Right(Vector(1.1, 0.8, 2.3)))
    assert(FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 0, None, 3, None).isLeft)

  test("tkRAS to scanner is a translation by c_ras, even for an oblique volume"):
    val geometry = FreeSurferVolumeGeometry(Vector(64, 72, 50), obliqueRas)
    val m = geometry.tkrToScanner.rowMajor
    assertClose(Vector(m(0), m(1), m(2), m(4), m(5), m(6), m(8), m(9), m(10)), Vector(1.0, 0, 0, 0, 1, 0, 0, 0, 1), 1e-12)
    assertClose(Vector(m(3), m(7), m(11)), geometry.centerRas, 1e-9)
    assertClose(apply(geometry.torig, Vector(32.0, 36.0, 25.0)), Vector(0.0, 0.0, 0.0), 1e-9)

  test("between composes into and out of RAS"):
    val system = ToolCoordinates.FslScaledVoxel(fsl(obliqueRas))
    val p = Vector(3.0, -4.5, 7.25)
    assertClose(apply(ToolCoordinates.between(system, ToolCoordinates.LpsMm), p), apply(ToolCoordinates.LpsToRas, apply(ToolCoordinates.toRas(system), p)))
