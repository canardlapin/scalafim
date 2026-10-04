package scalafim.fmri.mvpa

import multivar.core.SpaceRole
import resample4s.core.*
import resample4s.designs.{Bootstrap, KFold, PermutationDesign as ProviderPermutationDesign}
import scala.compiletime.testing.typeCheckErrors

class EvidenceDesignSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(keys: String*): AxisRef[String] =
    right(
      AxisRef.fromStableKeys(
        "trials",
        SpaceRole.Samples,
        keys.toVector,
        "acquisition-order",
        "trial",
        "unscaled",
        Vector("source:run-1")
      )
    )

  test("exact-once cross-fit covers every identified sample exactly once"):
    val samples = axis("a", "b", "c", "d", "e", "f")
    val root = ScientificSeed.fromLong(90210L)
    val bound = right(CrossFitDesign.bind(samples, KFold.ordered(3), root))
    val validation = right(ValidationDesign.bind(samples, KFold.ordered(3), root))
    val units = bound.keys.map(key => right(bound.at(key)))
    val assessed = units.flatMap(_.assessment.members.map(_.parentStableKey))

    assertEquals(bound.plan.shape.repeats, 1)
    assertEquals(bound.plan.shape.foldsPerRepeat, 3)
    assertEquals(assessed.sorted, samples.toRecord.stableKeys.sorted)
    assertEquals(assessed.distinct.length, samples.size)
    units.foreach: unit =>
      val analysis = unit.analysis.responseSelection.values.toSet
      val assessment = unit.assessment.responseSelection.values.toSet
      assertEquals(analysis.intersect(assessment), Set.empty[Int])

    val reverse = bound.keys.reverse.map(key => right(bound.at(key)).assessment.identity).reverse
    assertEquals(reverse, units.map(_.assessment.identity))
    assert(bound.receipt.seed.value != validation.receipt.seed.value)
    assert(bound.receipt.assignment.value.length > 0)

  test("repeated exact validation remains exact but cannot become exact-once cross-fit"):
    val samples = axis("a", "b", "c", "d", "e", "f")
    val repeated = right(KFold.ordered(3).repeat(2))
    val bound = right(ValidationDesign.bind(samples, repeated, ScientificSeed.fromLong(7L)))
    val assessed = bound.keys.flatMap(key => right(bound.at(key)).assessment.members.map(_.parentStableKey))

    assertEquals(bound.plan.shape.repeats, 2)
    assertEquals(assessed.groupMapReduce(identity)(_ => 1)(_ + _).values.toSet, Set(2))

    val errors = typeCheckErrors("""import scalafim.fmri.mvpa.*
import resample4s.core.*
def invalid[K](
  samples: AxisRef[K],
  repeated: Design[Split[Selection], Coverage.Exact],
  seed: ScientificSeed
)(using DigestAlgorithm) = CrossFitDesign.bind(samples, repeated, seed)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("Coverage.ExactOnce"))

  test("bootstrap analysis rows retain draw occurrence identity and ordinary coverage"):
    val samples = axis("a", "b", "c", "d", "e", "f")
    val design = Bootstrap.redrawing(times = 5, maxAttempts = 32)
    val root = ScientificSeed.fromLong(123456L)
    val first = right(BootstrapDesign.bind(samples, design, root))
    val repeated = right(BootstrapDesign.bind(samples, design, root))

    assertEquals(first.receipt, repeated.receipt)
    first.keys.foreach: key =>
      val unit = right(first.at(key))
      assertEquals(unit.analysis.kind, ReindexingKind.Draw)
      assertEquals(unit.assessment.kind, ReindexingKind.Selection)
      assertEquals(unit.analysis.members.length, samples.size)
      assertEquals(
        unit.analysis.members.map(_.occurrence.toVector),
        Vector.tabulate(samples.size)(position => Vector(position))
      )
      assertEquals(unit.analysis.members.distinct.length, samples.size)

    val wrongCoverage = typeCheckErrors("""import scalafim.fmri.mvpa.*
import resample4s.core.*
def invalid[S <: multivar.core.SemanticSpace,K](
  bootstrap: BootstrapDesign[S,K]
): Plan[Split[Draw], Coverage.Exact] = bootstrap.plan
""")
    assertEquals(wrongCoverage.length, 1)
    assert(wrongCoverage.head.message.contains("Coverage.Exact"))

  test("randomization receipts and assignments are axis-scoped and access-order independent"):
    val firstAxis = axis("a", "b", "c", "d", "e", "f")
    val reorderedAxis = axis("f", "e", "d", "c", "b", "a")
    val provider = ProviderPermutationDesign(times = 8)
    val root = ScientificSeed.fromLong(42L)
    val first = right(RandomizationDesign.bind(firstAxis, provider, root))
    val same = right(RandomizationDesign.bind(firstAxis, provider, root))
    val reordered = right(RandomizationDesign.bind(reorderedAxis, provider, root))

    assertEquals(first.receipt, same.receipt)
    assert(first.receipt != reordered.receipt)
    assert(first.receipt.seed.value != reordered.receipt.seed.value)

    val forward = first.keys.map(key => right(first.at(key)).permutation.identity)
    val backward = first.keys.reverse.map(key => right(first.at(key)).permutation.identity).reverse
    assertEquals(forward, backward)
    first.keys.foreach: key =>
      val unit = right(first.at(key))
      val expected = first.plan.at(key).toOption.toVector.flatMap(_.toVector).map(firstAxis.toRecord.stableKeys)
      assertEquals(unit.permutation.members.map(_.parentStableKey), expected)

  test("public plan mapping forgets coverage and design families cannot be interchanged"):
    val mappedCoverage = typeCheckErrors("""import resample4s.core.*
def invalid(
  exact: Plan[Split[Selection], Coverage.ExactOnce]
): Plan[Split[Selection], Coverage.ExactOnce] = exact.map(identity)
""")
    val wrongFamily = typeCheckErrors("""import scalafim.fmri.mvpa.*
import resample4s.core.*
def invalid[K](
  samples: AxisRef[K],
  validation: Design[Split[Selection], Coverage.ExactOnce],
  seed: ScientificSeed
)(using DigestAlgorithm) = BootstrapDesign.bind(samples, validation, seed)
""")
    assertEquals(mappedCoverage.length, 1)
    assert(mappedCoverage.head.message.contains("Coverage.ExactOnce"))
    assertEquals(wrongFamily.length, 1)
    assert(wrongFamily.head.message.contains("Split[resample4s.core.Draw]"))

  test("unknown plan units return a typed boundary error"):
    val samples = axis("a", "b", "c", "d")
    val bound = right(CrossFitDesign.bind(samples, KFold.ordered(2), ScientificSeed.fromLong(1L)))
    bound.at(UnitKey(0, 2)) match
      case Left(EvidenceError.UnknownResampleUnit(error)) =>
        assertEquals(error.key, UnitKey(0, 2))
      case other => fail(s"expected typed unknown-unit error, obtained $other")
