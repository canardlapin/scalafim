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
    val both = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueRas), 2, Some(obliqueRas))
    val qformOnly = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueLas), 0, Some(obliqueRas))
    val neither = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(-1.1, 0.8, 2.3), 0, None, 0, None)
    assertEquals(both.map(_.selected), Right(FslAffineSource.Sform))
    assertEquals(qformOnly.map(_.selected), Right(FslAffineSource.Qform))
    assertEquals(neither.map(_.selected), Right(FslAffineSource.Scaling))
    assertEquals(neither.map(_.pixdim), Right(Vector(1.1, 0.8, 2.3)))
    assertEquals(neither.map(_.storageOrder), Right(FslStorageOrder.Radiological))
    assertClose(apply(neither.toOption.get.voxelToFsl, Vector(0.0, 1.0, 2.0)), Vector(0.0, 0.8, 4.6))
    assert(FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 0, None, 3, None).isLeft)

  test("opposite active q/sform handedness is a typed refusal; historical compatibility must be explicit"):
    val conflict = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueLas), 2, Some(obliqueRas))
    conflict match
      case Left(SpaceError.FslHandednessConflict(qcode, qdet, scode, sdet)) =>
        assertEquals((qcode, scode), (1, 2))
        assert(qdet < 0.0 && sdet > 0.0)
      case other => fail(s"expected opposite-handed forms to be refused, got $other")
    val historical = FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(obliqueLas), 2, Some(obliqueRas), FslHeaderPolicy.FslpyCompatibility)
    assertEquals(historical.map(_.storageOrder), Right(FslStorageOrder.Neurological))
    val translated = affine(0.9, -0.2, 0.1, 99, 0.15, 1.1, 0.05, -77, -0.1, 0.02, 2.4, 66, 0, 0, 0, 1)
    assert(FslVolumeGeometry.fromHeader(Vector(7, 9, 6), Vector(1.1, 0.8, 2.3), 1, Some(translated), 2, Some(obliqueRas)).isRight)

  test("Torig is FreeSurfer's fixed LIA tkregister geometry; tkRAS to scanner is a translation only for LIA volumes"):
    val oblique = FreeSurferVolumeGeometry(Vector(64, 72, 50), obliqueRas)
    val torig = oblique.torig.rowMajor
    val Vector(xs, ys, zs) = oblique.voxelSizes
    assertClose(Vector(torig(0), torig(1), torig(2), torig(4), torig(5), torig(6), torig(8), torig(9), torig(10)), Vector(-xs, 0, 0, 0, 0, zs, 0, -ys, 0), 1e-12)
    assertClose(apply(oblique.torig, Vector(32.0, 36.0, 25.0)), Vector(0.0, 0.0, 0.0), 1e-9)
    // oblique: the tkr->scanner map rotates as well as translates, and still sends the centre to c_ras
    val m = oblique.tkrToScanner.rowMajor
    assert(math.abs(m(0) - 1.0) > 1e-3, "non-LIA volumes need the full Norig * inv(Torig)")
    assertClose(apply(oblique.tkrToScanner, Vector(0.0, 0.0, 0.0)), oblique.centerRas, 1e-9)
    // LIA (conformed-style) volume: pure translation by c_ras
    val lia = FreeSurferVolumeGeometry(Vector(256, 256, 256), affine(-1, 0, 0, 140.3, 0, 0, 1, -110.6, 0, -1, 0, 135.8, 0, 0, 0, 1))
    val t = lia.tkrToScanner.rowMajor
    assertClose(Vector(t(0), t(1), t(2), t(4), t(5), t(6), t(8), t(9), t(10)), Vector(1.0, 0, 0, 0, 1, 0, 0, 0, 1), 1e-12)
    assertClose(Vector(t(3), t(7), t(11)), lia.centerRas, 1e-9)

  test("between composes into and out of RAS"):
    val system = ToolCoordinates.FslScaledVoxel(fsl(obliqueRas))
    val p = Vector(3.0, -4.5, 7.25)
    assertClose(apply(ToolCoordinates.between(system, ToolCoordinates.LpsMm), p), apply(ToolCoordinates.LpsToRas, apply(ToolCoordinates.toRas(system), p)))
