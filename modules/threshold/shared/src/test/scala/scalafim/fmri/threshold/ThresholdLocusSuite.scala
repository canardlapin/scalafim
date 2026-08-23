package scalafim.fmri.threshold

import scalafim.locus.mapping
import scalafim.image.{Mask, NeuroSpace, NeuroVol}

class ThresholdLocusSuite extends munit.FunSuite:

  private def field(
      active: Int*
  ): MaskedField =
    val space = NeuroSpace(Vector(4, 1, 1))
    val stat =
      NeuroVol.copyFromCanonicalArray[Double](
        Array(1.0, 2.0, 3.0, 4.0),
        space
      )
    val mask = Mask.fromIndices(space, Array[Int](active*))
    MaskedField
      .fromVolume(stat, mask, Tail.Positive)
      .fold(error => fail(error.message), identity)

  test("masked fields expose ordered support and exact compact-to-volume injection"):
    val masked = field(0, 2, 3)

    assertEquals(masked.activeSelection.ordinals.toVector, Vector(0, 2, 3))
    assertEquals(masked.support.ordinalsInDomainOrder.toVector, Vector(0, 2, 3))
    assertEquals(
      masked.activeToFull.mapping.targetOrdinals.toVector,
      Vector(0, 2, 3)
    )
    assertEquals(
      masked.volumeIndices(Array(2, 0)).map(_.toVector),
      Right(Vector(3, 0))
    )

  test("threshold regions carry locus membership and reject foreign compact support"):
    val first = field(0, 2, 3)
    val second = field(0, 1, 3)
    val priors =
      PriorWeights
        .uniform(first.size)
        .fold(error => fail(error.message), identity)
    val region =
      ThresholdRegion
        .fromIndices(7, Array(2, 0), first, priors)
        .fold(error => fail(error.message), identity)

    assertEquals(region.membership.ordinalsInDomainOrder.toVector, Vector(0, 2))
    assertEquals(region.indicesVector, Vector(0, 2))
    assert(ScoringInput(first, priors, region).isRight)
    assert(ScoringInput(second, priors, region).isLeft)

  test("threshold regions reject duplicate compact indices"):
    val masked = field(0, 2, 3)
    val priors =
      PriorWeights
        .uniform(masked.size)
        .fold(error => fail(error.message), identity)

    assertEquals(
      ThresholdRegion
        .fromIndices(1, Array(0, 0), masked, priors)
        .left
        .toOption,
      Some(ThresholdError.DuplicateRegionIndex(0))
    )
