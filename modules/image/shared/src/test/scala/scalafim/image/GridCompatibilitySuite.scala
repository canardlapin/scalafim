package scalafim.image

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import Ops.*
import spire.std.double.given

class GridCompatibilitySuite extends munit.FunSuite:

  private val translatedAffine =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 10.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private val reflectedAffine =
    DMat.fromRows(
      Vector(
        Vector(-1.0, 0.0, 0.0, 1.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  test("exact compatibility rejects equal-shaped spaces with different affines") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))

    assert(GridCompatibility.exact(expected, translated).isLeft)
    assert(GridCompatibility.spatial(expected, translated).isLeft)
  }

  test("spatial compatibility ignores only non-spatial series extent") {
    val first = NeuroSpace(Vector(2, 2, 1, 3))
    val second = NeuroSpace(Vector(2, 2, 1, 5))

    assert(GridCompatibility.exact(first, second).isLeft)
    assertEquals(GridCompatibility.spatial(first, second), Right(()), clue = "")
  }

  test("certified congruence is explicit evidence bound to its exact live grids") {
    def liveSpace(label: String): NeuroSpace =
      val frame =
        Frame.named[D3](label).fold(error => fail(error.message), identity)
      val grid =
        Grid
          .in(frame)(Vector(2, 2, 1), Affine.identity[D3])
          .fold(error => fail(error.message), identity)
      NeuroSpace.fromCanonical(SampleSpace.create(grid, NonSpatialAxes.empty))

    val expected = liveSpace("certified-grid-expected")
    val actual = liveSpace("certified-grid-actual")
    val unrelated = liveSpace("certified-grid-unrelated")

    assert(GridCompatibility.exact(expected, actual).isLeft)
    val certificate =
      GridCompatibility
        .certifySpatialCongruence(expected, actual, tolerance = 0.0)
        .fold(error => fail(error.message), identity)

    assertEquals(
      GridCompatibility.acceptCertifiedSpatial(certificate, expected, actual),
      Right(())
    )
    assert(
      GridCompatibility
        .acceptCertifiedSpatial(certificate, expected, unrelated)
        .isLeft,
      clue = "a certificate must not be reusable for a third grid"
    )
    assert(
      GridCompatibility.acceptCertifiedExact(certificate, expected, actual).isLeft,
      clue = "a spatial certificate must not satisfy an exact-axis admission"
    )
  }

  test("pointwise volume arithmetic rejects a different physical grid") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val reflected = NeuroSpace(Vector(2, 2, 1), trans = Some(reflectedAffine))
    val left = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 1.0), expected)
    val right = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 2.0), reflected)

    intercept[IllegalArgumentException] {
      left + right
    }
  }

  test("volume concatenation rejects a different physical grid") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))
    val left = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 1.0), expected)
    val right = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 2.0), translated)

    intercept[IllegalArgumentException] {
      left.concat(right)
    }
  }

  test("selected-volume gather rejects a different exact grid owner") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))
    val sampleSpace = NeuroSpace.requireSpatialD3(expected).toOption.get
    val volume =
      NeuroVolume
        .continuous(
          sampleSpace,
          ravel.NDArray.fill(ravel.Shape(2, 2, 1), 1.0)
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(expected),
          "grid compatibility expected",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val foreign =
      VolumeDomain
        .register(
          VolumeSpace(translated),
          "grid compatibility translated",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    val selection =
      locus4s.Selection
        .fromOrdinals(foreign.value.space, Vector(0, 1))
        .toOption
        .get

    assert(SelectedVolume.gather(domain, volume, selection).isLeft)
  }
