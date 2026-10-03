package scalafim.fmri.mvpa.analysis

import munit.FunSuite
import scalafim.fmri.mvpa.AxisSignature

class ResourceAdmissionSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val axis = AxisSignature.unsafe("a" * 64)
  private def plan(source: String) = PlanId.derived(EstimandId("resource-test"), Vector(axis), axis, axis,
    source, "fixed-splits", "fixed-frame", "fixed-question", Vector.empty, Vector.empty,
    "fixed-reduction", Vector("metric" -> "unchanged"), Set.empty)
  private val scientific = plan("source")
  private val noCopy = MaterializationPolicy.ForbidSourceCopy
  private val known = ResourceBound.Known(BigInt(40), "provider-v1")
  private def candidate(name: String, footprint: ResourceFootprint, copy: BigInt = 0, work: BigInt = 1) =
    ResourceCandidate(scientific, name, "same fixed operator and estimand", footprint, copy, work)

  test("live source plus all workers scratch and retained outputs is charged exactly"):
    val footprint = ResourceFootprint(known, ResourceBound.Known(7, "scratch-v1"), 3, 11, 13, 17, 3)
    val route = candidate("direct", footprint)
    val admitted = right(ResourceAdmission.evaluate(route, ResourceBudget(ResourceLimit.WholeNumeric(124), noCopy)))
    // Owned: 3*11 + 13 + 17 = 63; provider: 40 + 3*7 = 61.
    assertEquals(admitted.ownedNumericBytes, 63L)
    assertEquals(admitted.wholeNumericBytes, Some(124L))
    assertEquals(ResourceAdmission.evaluate(route, ResourceBudget(ResourceLimit.WholeNumeric(123), noCopy)), Left(ResourceError.MemoryExceeded(BigInt(124), 123L)))

  test("unknown strict scratch refuses while owned-only receipt preserves excluded costs"):
    val route = candidate("direct", ResourceFootprint(known, ResourceBound.Unknown("backend has no scratch certificate"), 1, 11, 13, 17))
    val strict = ResourceAdmission.evaluate(route, ResourceBudget(ResourceLimit.WholeNumeric(1000), noCopy))
    assert(strict.left.toOption.exists(_.isInstanceOf[ResourceError.UnknownStrictCost]))
    val own = right(ResourceAdmission.evaluate(route, ResourceBudget(ResourceLimit.OwnedNumeric(41), noCopy)))
    assertEquals(own.ownedNumericBytes, 41L)
    assertEquals(own.wholeNumericBytes, None)
    assertEquals(own.unknownCosts.size, 1)

  test("checked arithmetic refuses shape and live byte overflow before conversion"):
    assertEquals(ResourceAdmission.bytes(Int.MaxValue, Int.MaxValue), Left(ResourceError.Overflow("shape bytes", BigInt(Int.MaxValue) * Int.MaxValue * 8)))
    assertEquals(ResourceAdmission.bytes(-1, 1), Left(ResourceError.InvalidBound("shape")))
    val huge = BigInt(Long.MaxValue) + 1
    val ownOverflow = candidate("overflow", ResourceFootprint(known, known, 1, huge, 0, 0))
    assert(ResourceAdmission.evaluate(ownOverflow, ResourceBudget(ResourceLimit.OwnedNumeric(Long.MaxValue), noCopy)).isLeft)
    val excluded = candidate("excluded", ResourceFootprint(ResourceBound.Known(huge, "resident-v1"), ResourceBound.Unknown("unknown"), 1, 8, 0, 0))
    assertEquals(right(ResourceAdmission.evaluate(excluded, ResourceBudget(ResourceLimit.OwnedNumeric(8), noCopy))).ownedNumericBytes, 8L)

  test("uncertified concurrency is refused rather than inventing provider parallel safety"):
    val route = candidate("parallel", ResourceFootprint(known, known, 2, 8, 0, 0))
    assertEquals(ResourceAdmission.evaluate(route, ResourceBudget(ResourceLimit.WholeNumeric(1000), noCopy)), Left(ResourceError.ConcurrencyUnsupported(2, 1)))

  test("small dense route can win only with admitted copy policy and unchanged scientific plan"):
    val footprint = ResourceFootprint(known, known, 1, 16, 0, 0)
    val direct = candidate("operator", footprint, work = 100)
    val dense = candidate("dense", footprint.copy(sharedBytes = 32), copy = 32, work = 10)
    val budget = ResourceBudget(ResourceLimit.WholeNumeric(1000), MaterializationPolicy.AllowSourceCopy(32))
    assertEquals(right(ResourceAdmission.choose(scientific, Vector(direct, dense), budget)).candidate.route, "dense")
    assertEquals(right(ResourceAdmission.choose(scientific, Vector(dense, direct), budget.copy(materialization = noCopy))).candidate.route, "operator")
    assertEquals(ResourceAdmission.choose(scientific, Vector(dense.copy(plan = plan("different population"))), budget), Left(ResourceError.ScientificPlanMismatch))
    assert(ResourceAdmission.choose(scientific, Vector(dense), budget.copy(materialization = MaterializationPolicy.AllowSourceCopy(31))).isLeft)

  test("malformed provider declarations and negative copy costs refuse admission"):
    val bad = ResourceFootprint(ResourceBound.Known(-1, "negative"), known, 1, 1, 0, 0)
    assertEquals(ResourceAdmission.evaluate(candidate("bad", bad), ResourceBudget(ResourceLimit.WholeNumeric(1000), noCopy)), Left(ResourceError.InvalidBound("source")))
    assertEquals(ResourceAdmission.evaluateFootprint(bad.copy(source = known), -1, ResourceBudget(ResourceLimit.OwnedNumeric(1), noCopy)), Left(ResourceError.InvalidBound("source copy")))
