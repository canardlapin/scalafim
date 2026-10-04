package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import gale.optim.{FirstOrderConfig, FirstOrderStoppingStatus, FirstOrderTolerance}
import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef

class SpatialSupportSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(e => fail(e.toString), identity)
  private def axis(size: Int) = right(AxisRef.fromStableKeys("support", SpaceRole.Observed,
    Vector.tabulate(size)(i => s"v$i"), "vertex", "BOLD", "native"))
  private val strict = right(FirstOrderConfig.from(30000, right(FirstOrderTolerance.from(1e-10, 1e-10))))

  test("isolated row matches analytic group soft threshold through envelope cone"):
    val a = axis(1)
    val graph = right(SupportGraph(a.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    // With Z=(3,4), h=0, lambda_s=1, active cone has g=|A|.
    // Radial minimization 1/2(t-5)^2 + 1/2 t² + t gives t=2.
    val result = right(SpatialSupport.update(a, DMat.dense(1, 2, Vector(3.0, 4.0)), Vector(0.0), graph,
      right(SupportPenalty(1.0, 0.0)), 10000, strict))
    assert(result.converged)
    assertEqualsDouble(result.loadings(0, 0), 1.2, 1e-8)
    assertEqualsDouble(result.loadings(0, 1), 1.6, 1e-8)
    assertEqualsDouble(result.envelope.head, 2.0, 1e-8)
    assertEqualsDouble(result.objective, 8.5, 1e-8)
    assert(result.maximumConstraintViolation <= result.feasibilityTolerance)

  test("support TV preserves alternating signs; signed smoothing is independent"):
    val a = axis(4)
    val graph = right(SupportGraph(a.descriptor, Vector(SupportEdge(0, 1, 1), SupportEdge(1, 2, 1), SupportEdge(2, 3, 1)),
      SupportTopology.Declared("chain"), "one"))
    val z = DMat.dense(4, 1, Vector(2.0, -2.0, 2.0, -2.0))
    val result = right(SpatialSupport.update(a, z, Vector.fill(4)(2.0), graph, right(SupportPenalty(0.2, 5.0)), 10000, strict))
    assert(result.converged)
    (0 until 4).foreach: i =>
      assertEqualsDouble(result.envelope(i), 1.9, 1e-8)
      assertEqualsDouble(result.loadings(i, 0), if i % 2 == 0 then 1.9 else -1.9, 1e-8)
    val smooth = right(SpatialSupport.update(a, z, Vector.fill(4)(2.0), graph, right(SupportPenalty(0.2, 5.0, 2.0)), 10000, strict))
    assert(smooth.converged)
    assert(math.abs(smooth.loadings(1, 0)) < math.abs(result.loadings(1, 0)))

  test("weighted two-node TV agrees with independent fused-lasso solution"):
    val a = axis(2)
    val graph = right(SupportGraph(a.descriptor, Vector(SupportEdge(0, 1, 2.0)), SupportTopology.Declared("pair"), "one"))
    // Z=0 leaves the cone inactive; h=(1,5). TV weight .5*2=1
    // gives g=(2,4), objective=1+2=3 by the two scalar subgradients.
    val result = right(SpatialSupport.update(a, DMat.zeros(2, 1), Vector(1.0, 5.0), graph,
      right(SupportPenalty(0.0, 0.5)), 10000, strict))
    assert(result.converged)
    assertEqualsDouble(result.envelope(0), 2.0, 1e-8)
    assertEqualsDouble(result.envelope(1), 4.0, 1e-8)
    assertEqualsDouble(result.objective, 3.0, 1e-8)

  test("signed two-node quadratic agrees with independent linear solve"):
    val a = axis(2)
    val graph = right(SupportGraph(a.descriptor, Vector(SupportEdge(0, 1, 1.0)), SupportTopology.Declared("pair"), "one"))
    val result = right(SpatialSupport.update(a, DMat.dense(2, 1, Vector(3.0, -3.0)), Vector(5.0, 5.0), graph,
      right(SupportPenalty(0.0, 0.0, 1.0)), 10000, strict))
    assertEqualsDouble(result.loadings(0, 0), 1.0, 1e-8)
    assertEqualsDouble(result.loadings(1, 0), -1.0, 1e-8)
    assertEqualsDouble(result.objective, 6.0, 1e-8)

  test("zero group is sparse, exhausted iterations remain unqualified, budget refusal is typed"):
    val a = axis(1)
    val graph = right(SupportGraph(a.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    val penalty = right(SupportPenalty(10.0, 0.0))
    val result = right(SpatialSupport.update(a, DMat.dense(1, 1, Vector(1.0)), Vector(0.0), graph, penalty, 10000, strict))
    assertEqualsDouble(result.loadings(0, 0), 0.0, 1e-12)
    assertEqualsDouble(result.envelope.head, 0.0, 1e-12)
    val one = right(FirstOrderConfig.from(1, right(FirstOrderTolerance.from(1e-15, 0.0))))
    val exhausted = right(SpatialSupport.update(a, DMat.dense(1, 1, Vector(3.0)), Vector(0.0), graph,
      right(SupportPenalty(0.0, 0.0)), 10000, one))
    assertEquals(exhausted.solution.status, FirstOrderStoppingStatus.IterationLimit)
    SpatialSupport.update(a, DMat.zeros(1, 1), Vector(0.0), graph, penalty, 0) match
      case Left(SpatialSupportError.Budget(_, 0)) => ()
      case other => fail(s"expected budget refusal: $other")

  test("duplicate, self, nonfinite and foreign graph inputs are rejected"):
    val a = axis(2)
    val topology = SupportTopology.Declared("pair")
    assert(SupportGraph(a.descriptor, Vector(SupportEdge(0, 1, 1), SupportEdge(1, 0, 2)), topology, "one").isLeft)
    assert(SupportGraph(a.descriptor, Vector(SupportEdge(0, 0, 1)), topology, "one").isLeft)
    assert(SupportGraph(a.descriptor, Vector(SupportEdge(0, 1, Double.NaN)), topology, "one").isLeft)
    val foreign = right(SupportGraph(axis(1).descriptor, Vector.empty, topology, "one"))
    assertEquals(SpatialSupport.update(a, DMat.zeros(2, 1), Vector(0.0, 0.0), foreign, right(SupportPenalty(0, 0)), 1000),
      Left(SpatialSupportError.AxisMismatch))
