package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SemanticSpace, SpaceRole, Table, ValueId, ValueIdentity}
import scala.compiletime.testing.typeCheckErrors
import scalafim.fmri.mvpa.{AxisRef, EvidenceError, EvidenceIdentity, EvidenceOrigins}
import scalafim.response.SourceId

class QueryReuseSuite extends munit.FunSuite:
  private val budget = QueryReuseBudget(32, 128, 1000)
  private val contractionBudget = ContractionBudget(1000, 16)

  private def right[A](value: Either[EvidenceError, A]): A =
    value.fold(error => fail(error.toString), scala.Predef.identity)

  private def axis(name: String, role: SpaceRole, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(size)(i => s"$name-$i"), "fixture", "unit", "raw", Vector("query-reuse")))

  private def valueIdentity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def table[RK, CK](rows: AxisRef[RK], columns: AxisRef[CK], values: DMat, name: String): Table[rows.Id, columns.Id] =
    Lin.fromDenseMatrix(values, CoordinateEvidence.dual(columns.evidence), CoordinateEvidence.primal(rows.evidence), valueIdentity(name), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)

  private def operatorTable[RK, CK](rows: AxisRef[RK], columns: AxisRef[CK], values: DoubleLinearOperator, name: String): Table[rows.Id, columns.Id] =
    Lin.fromLinearMap(values, CoordinateEvidence.dual(columns.evidence), CoordinateEvidence.primal(rows.evidence), valueIdentity(name), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)

  private def closure[LK, RK](left: AxisRef[LK], rightAxis: AxisRef[RK], values: DMat, name: String): CrossClosure[left.Id, rightAxis.Id] =
    Lin.fromDenseMatrix(values, CoordinateEvidence.primal(rightAxis.evidence), CoordinateEvidence.dual(left.evidence), valueIdentity(name), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)

  private def operatorClosure[LK, RK](left: AxisRef[LK], rightAxis: AxisRef[RK], values: DoubleLinearOperator, name: String): CrossClosure[left.Id, rightAxis.Id] =
    Lin.fromLinearMap(values, CoordinateEvidence.primal(rightAxis.evidence), CoordinateEvidence.dual(left.evidence), valueIdentity(name), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)

  private val source = RelationSource("acq", "response", "readout", "prep", "noise")
  private val replay = RelationOrigins(source, RelationAccess.OwnedReplay("query-reuse"))

  private def relation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], values: DMat, name: String, origins: RelationOrigins = replay): Relation[effects.Id, neural.Id] =
    right(Relation(effects, neural, table(effects, neural, values, name), origins, Vector.fill(effects.size)(EffectEstimability.Estimable)))

  private def operatorRelation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], values: DoubleLinearOperator, name: String, origins: RelationOrigins): Relation[effects.Id, neural.Id] =
    right(Relation(effects, neural, operatorTable(effects, neural, values, name), origins, Vector.fill(effects.size)(EffectEstimability.Estimable)))

  private def targetIdentity(rows: AxisRef[String], columns: AxisRef[String], revision: String): EvidenceIdentity =
    EvidenceIdentity(rows.descriptor, columns.descriptor, SourceId.unsafe("targets"), Vector.empty, Vector.empty, valueIdentity(revision), EvidenceOrigins.Unknown)

  private def node(id: String, parents: Vector[String] = Vector.empty, dependencies: Vector[QueryDependency] = Vector.empty, fidelity: String = "f64", implementation: String = "cpu"): QueryNode =
    QueryNode(QueryProductId(id), parents.map(QueryProductId.apply), dependencies, fidelity, implementation)

  private def program(nodes: QueryNode*): QueryReuseProgram =
    right(QueryReuseProgram(nodes.toVector, budget))

  private def decision(current: QueryReuseProgram, previous: QueryReuseProgram, id: String): QueryReuseDecision =
    current.assess(previous).find(_._1 == QueryProductId(id)).fold(fail(s"missing $id"))(_._2)

  private def assertRejected(value: QueryReuseDecision, path: Vector[String]): Unit =
    value match
      case QueryReuseDecision.Rejected(paths, _) => assert(paths.contains(path.map(QueryProductId.apply)))
      case other => fail(s"expected rejection, got $other")

  test("dependency DAG admits exact matches, rejects changed ancestors, and propagates unknown inputs"):
    val previous = program(node("geometry"), node("comparison", Vector("geometry")), node("multiplicity", Vector("comparison")))
    assertEquals(decision(previous, previous, "multiplicity"), QueryReuseDecision.Admitted)

    val changed = program(node("geometry", fidelity = "f32"), node("comparison", Vector("geometry")), node("multiplicity", Vector("comparison")))
    assertRejected(decision(changed, previous, "comparison"), Vector("geometry", "comparison"))
    assertRejected(decision(changed, previous, "multiplicity"), Vector("geometry", "comparison", "multiplicity"))

    val unknown = program(node("geometry", dependencies = Vector(QueryDependency.Unknown("atlas", "revision unavailable"))), node("comparison", Vector("geometry")))
    decision(unknown, unknown, "geometry") match
      case QueryReuseDecision.Unknown(_, reasons) => assert(reasons.exists(_.contains("atlas")))
      case other => fail(s"expected unknown, got $other")
    decision(unknown, unknown, "comparison") match
      case QueryReuseDecision.Unknown(paths, _) => assert(paths.contains(Vector(QueryProductId("geometry"), QueryProductId("comparison"))))
      case other => fail(s"expected propagated unknown, got $other")

  test("dependency programs reject forward or cyclic parents, duplicate products, and budget excess"):
    assert(QueryReuseProgram(Vector(node("before", Vector("after")), node("after")), budget).isLeft)
    assert(QueryReuseProgram(Vector(node("self", Vector("self"))), budget).isLeft)
    assert(QueryReuseProgram(Vector(node("duplicate"), node("duplicate")), budget).isLeft)
    assert(QueryReuseProgram(Vector(node("a"), node("b")), QueryReuseBudget(1, 2, 1000)).isLeft)
    assert(QueryReuseProgram(Vector(node("a", dependencies = Vector(QueryDependency.Parameter("x", "1"), QueryDependency.Parameter("y", "2")))), QueryReuseBudget(2, 1, 1000)).isLeft)

  test("relation value identity changes at a fixed locator while display-only metadata is absent from reuse identity"):
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 2)
    val oldRelation = relation(effects, neural, DMat.dense(2, 2, Vector(1.0, 2.0, 3.0, 4.0)), "locator-revision-1")
    val newRelation = relation(effects, neural, DMat.dense(2, 2, Vector(9.0, 2.0, 3.0, 4.0)), "locator-revision-2")
    val oldProgram = program(node("geometry", dependencies = Vector(QueryDependency.relation(oldRelation))))
    val changed = program(node("geometry", dependencies = Vector(QueryDependency.relation(newRelation))))
    assertRejected(decision(changed, oldProgram, "geometry"), Vector("geometry"))

    val displayedAs = "Human-readable geometry title"
    val sameValuesDifferentDisplay = program(node("geometry", dependencies = Vector(QueryDependency.relation(oldRelation))))
    assert(displayedAs.nonEmpty)
    assertEquals(decision(sameValuesDifferentDisplay, oldProgram, "geometry"), QueryReuseDecision.Admitted)

  test("new RSA models invalidate comparison and multiplicity but retain geometry; targets and folds propagate only their descendants"):
    val geometry = node("geometry")
    val targetRows = axis("target-rows", SpaceRole.Samples, 2)
    val targetColumns = axis("target-columns", SpaceRole.Latent, 1)
    val old = program(geometry, node("comparison", Vector("geometry"), Vector(QueryDependency.Parameter("rsa-model", "v1"))), node("multiplicity", Vector("comparison")), node("target-score", Vector("geometry"), Vector(QueryDependency.Target(targetIdentity(targetRows, targetColumns, "v1")))), node("fold-score", Vector("geometry"), Vector(QueryDependency.Parameter("folds", "v1"))))
    val modelChanged = program(geometry, node("comparison", Vector("geometry"), Vector(QueryDependency.Parameter("rsa-model", "v2"))), node("multiplicity", Vector("comparison")), old.nodes(3), old.nodes(4))
    assertEquals(decision(modelChanged, old, "geometry"), QueryReuseDecision.Admitted)
    assertRejected(decision(modelChanged, old, "multiplicity"), Vector("comparison", "multiplicity"))

    val targetAndFoldChanged = program(geometry, old.nodes(1), old.nodes(2), node("target-score", Vector("geometry"), Vector(QueryDependency.Target(targetIdentity(targetRows, targetColumns, "v2")))), node("fold-score", Vector("geometry"), Vector(QueryDependency.Parameter("folds", "v2"))))
    assertEquals(decision(targetAndFoldChanged, old, "comparison"), QueryReuseDecision.Admitted)
    assertRejected(decision(targetAndFoldChanged, old, "target-score"), Vector("target-score"))
    assertRejected(decision(targetAndFoldChanged, old, "fold-score"), Vector("fold-score"))

  test("fidelity implementation scope and dense metric identity changes reject, including distinct ROI scopes"):
    val left = axis("left", SpaceRole.Observed, 2)
    val rightAxis = axis("right", SpaceRole.Observed, 2)
    val metric1 = closure(left, rightAxis, DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1.0)), "metric-v1")
    val metric2 = closure(left, rightAxis, DMat.dense(2, 2, Vector(2.0, 0.0, 0.0, 1.0)), "metric-v2")
    val old = program(node("geometry", dependencies = Vector(QueryDependency.metric(metric1, left.descriptor, rightAxis.descriptor), QueryDependency.Scope("roi", "A"))))
    val metricChanged = program(node("geometry", dependencies = Vector(QueryDependency.metric(metric2, left.descriptor, rightAxis.descriptor), QueryDependency.Scope("roi", "A"))))
    val scopeChanged = program(node("geometry", dependencies = Vector(QueryDependency.metric(metric1, left.descriptor, rightAxis.descriptor), QueryDependency.Scope("roi", "B"))))
    assertRejected(decision(metricChanged, old, "geometry"), Vector("geometry"))
    assertRejected(decision(scopeChanged, old, "geometry"), Vector("geometry"))
    assertRejected(decision(program(node("geometry", fidelity = "f32")), program(node("geometry")), "geometry"), Vector("geometry"))
    assertRejected(decision(program(node("geometry", implementation = "gpu")), program(node("geometry")), "geometry"), Vector("geometry"))

  test("retained budget refusal occurs before callback and separate dense metric ROI scopes cannot be additively reused"):
    val retained = right(RetainedQuery(QueryProductId("geometry"), program(node("geometry")), "payload", 4, budget))
    var callbacks = 0
    val tooSmall = QueryReuseBudget(32, 128, 3)
    val refusal = retained.use(program(node("geometry")), tooSmall): payload =>
      callbacks += 1
      payload
    assert(refusal match
      case Left(QueryReuseDecision.Rejected(_, _)) => true
      case _ => false)
    assertEquals(callbacks, 0)
    val successfulReuse = retained.use(program(node("geometry")), budget): payload =>
      callbacks += 1
      payload.reverse
    assertEquals(successfulReuse, Right("daolyap"))
    assertEquals(callbacks, 1)
    val separated = program(node("geometry", dependencies = Vector(QueryDependency.Scope("roi", "A"), QueryDependency.Parameter("dense-metric", "A"))))
    val requested = program(node("geometry", dependencies = Vector(QueryDependency.Scope("roi", "B"), QueryDependency.Parameter("dense-metric", "B"))))
    assertRejected(decision(requested, separated, "geometry"), Vector("geometry"))

  test("retained queries reject an over-budget stored program and a narrowed current budget before callbacks"):
    val threeNodes = program(node("geometry"), node("comparison", Vector("geometry")), node("multiplicity", Vector("comparison")))
    val narrowBudget = QueryReuseBudget(1, 8, 1000)
    assert(RetainedQuery(QueryProductId("geometry"), threeNodes, "payload", 1, narrowBudget).isLeft)

    val retained = right(RetainedQuery(QueryProductId("geometry"), threeNodes, "payload", 1, budget))
    var callbacks = 0
    val result = retained.use(program(node("geometry")), narrowBudget): payload =>
      callbacks += 1
      payload
    assert(result.isLeft)
    assertEquals(callbacks, 0)

  test("program construction refuses a deep explanation beyond its entry budget"):
    val explanationBudget = QueryReuseBudget(4, 8, 1000, maximumExplanationEntries = 1)
    assert(QueryReuseProgram(Vector(node("geometry"), node("comparison", Vector("geometry")), node("multiplicity", Vector("comparison"))), explanationBudget).isLeft)

  private def lowRankOracle(left: DMat, rightAxis: DMat, metric: DMat, u: DMat, v: DMat): Double =
    var total = 0.0
    var q = 0
    while q < u.rows do
      var el = 0
      while el < left.rows do
        var er = 0
        while er < rightAxis.rows do
          var nl = 0
          while nl < left.cols do
            var nr = 0
            while nr < rightAxis.cols do
              total += u(q, el) * left(el, nl) * metric(nl, nr) * rightAxis(er, nr) * v(q, er)
              nr += 1
            nl += 1
          er += 1
        el += 1
      q += 1
    total

  test("low-rank contraction matches a four-index oracle and rejects budget or one-shot evidence before reads"):
    val le = axis("le", SpaceRole.Latent, 2)
    val ln = axis("ln", SpaceRole.Observed, 3)
    val re = axis("re", SpaceRole.Latent, 3)
    val rn = axis("rn", SpaceRole.Observed, 2)
    val q = axis("q", SpaceRole.Latent, 2)
    val bLeft = DMat.dense(2, 3, Vector(1.0, -2.0, 3.0, 4.0, 0.0, -1.0))
    val bRight = DMat.dense(3, 2, Vector(2.0, 1.0, -1.0, 3.0, 0.5, -2.0))
    val k = DMat.dense(3, 2, Vector(2.0, 1.0, -1.0, 3.0, 0.5, -2.0))
    val u = DMat.dense(2, 2, Vector(1.0, -1.0, 2.0, 0.5))
    val v = DMat.dense(2, 3, Vector(2.0, -1.0, 0.5, 1.0, 3.0, -2.0))
    val pair = RelationPair(relation(le, ln, bLeft, "left"), relation(re, rn, bRight, "right"))
    val lf = Lin.fromDenseMatrix(u, CoordinateEvidence.primal(le.evidence), CoordinateEvidence.primal(q.evidence), valueIdentity("u"), SemanticProvenance.source("query-reuse")).fold(error => fail(error.toString), scala.Predef.identity)
    val rf = Lin.fromDenseMatrix(v, CoordinateEvidence.primal(re.evidence), CoordinateEvidence.primal(q.evidence), valueIdentity("v"), SemanticProvenance.source("query-reuse")).fold(error => fail(error.toString), scala.Predef.identity)
    assertEqualsDouble(right(LowRankRelationContraction.scalar(pair, lf, rf, closure(ln, rn, k, "k"), contractionBudget)).value, lowRankOracle(bLeft, bRight, k, u, v), 1e-12)
    assert(LowRankRelationContraction.scalar(pair, lf, rf, closure(ln, rn, k, "k"), ContractionBudget(0, 16)).isLeft)
    var metricReads = 0
    val poisonMetric = new DoubleLinearOperator:
      val rows = 3
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        metricReads += 1
        throw new IllegalStateException("budget-refused metric was read")
    assert(LowRankRelationContraction.scalar(pair, lf, rf, operatorClosure(ln, rn, poisonMetric, "poison-metric"), ContractionBudget(0, 16)).isLeft)
    assertEquals(metricReads, 0)
    var reads = 0
    val poisonLeft = new DoubleLinearOperator:
      val rows = 2
      val cols = 3
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("one-shot evidence was read")
    val poisonRight = new DoubleLinearOperator:
      val rows = 3
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("one-shot evidence was read")
    val oneShot = RelationOrigins(source, RelationAccess.OneShot)
    val oneShotPair = RelationPair(operatorRelation(le, ln, poisonLeft, "one-left", oneShot), operatorRelation(re, rn, poisonRight, "one-right", oneShot))
    assert(LowRankRelationContraction.scalar(oneShotPair, lf, rf, closure(ln, rn, k, "k"), contractionBudget).isLeft)
    assertEquals(reads, 0)

  test("low-rank contraction compensates trace cancellation and is invariant to endpoint reversal"):
    val leftEffects = axis("trace-left-effects", SpaceRole.Latent, 3)
    val leftNeural = axis("trace-left-neural", SpaceRole.Observed, 3)
    val rightEffects = axis("trace-right-effects", SpaceRole.Latent, 3)
    val rightNeural = axis("trace-right-neural", SpaceRole.Observed, 3)
    val components = axis("trace-components", SpaceRole.Latent, 3)
    val left = relation(leftEffects, leftNeural, DMat.eye(3), "trace-left")
    val rightRelation = relation(rightEffects, rightNeural, DMat.eye(3), "trace-right")
    val leftFactor = Lin.fromDenseMatrix(DMat.eye(3), CoordinateEvidence.primal(leftEffects.evidence), CoordinateEvidence.primal(components.evidence), valueIdentity("trace-u"), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)
    val rightFactor = Lin.fromDenseMatrix(DMat.dense(3, 3, Vector(1e16, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, -1e16)), CoordinateEvidence.primal(rightEffects.evidence), CoordinateEvidence.primal(components.evidence), valueIdentity("trace-v"), SemanticProvenance.source("query-reuse"))
      .fold(error => fail(error.toString), scala.Predef.identity)
    val metric = closure(leftNeural, rightNeural, DMat.eye(3), "trace-metric")
    val pair = RelationPair(left, rightRelation)

    val actual = right(LowRankRelationContraction.scalar(pair, leftFactor, rightFactor, metric, contractionBudget)).value
    assertEqualsDouble(actual, 1.0, 0.0)
    val reversed = right(LowRankRelationContraction.scalar(pair.reverse, rightFactor, leftFactor, metric.star, contractionBudget)).value
    assertEqualsDouble(reversed, actual, 0.0)

  test("low-rank factors reject a foreign component axis at compile time"):
    val errors = typeCheckErrors("""
import multivar.core.{Lin, Primal, SemanticSpace}
import scalafim.fmri.mvpa.relation.*
def invalid[EL <: SemanticSpace, NL <: SemanticSpace, ER <: SemanticSpace, NR <: SemanticSpace, Q <: SemanticSpace, Foreign <: SemanticSpace](pair: RelationPair[EL, NL, ER, NR], left: Lin[Primal[EL], Primal[Q]], right: Lin[Primal[ER], Primal[Foreign]], metric: CrossClosure[NL, NR], budget: ContractionBudget) = LowRankRelationContraction.scalar(pair, left, right, metric, budget)
""")
    assert(errors.nonEmpty)

  private def orderedOracle(left: Vector[Vector[Double]], rightAxis: Vector[Vector[Double]]): Double =
    left.indices.foldLeft(0.0): (total, i) =>
      rightAxis.indices.foldLeft(total): (acc, j) =>
        if i == j then acc else acc + left(i).zip(rightAxis(j)).map(_ * _).sum

  test("all-distinct products match explicit ordered pairs through cancellation, mixed scales, and overflow"):
    val cancellationLeft = Vector(Vector(1e16), Vector(1.0))
    val cancellationRight = Vector(Vector(1e16), Vector(1.0))
    assertEqualsDouble(right(AllDistinctProducts.sum(cancellationLeft, cancellationRight, contractionBudget)), orderedOracle(cancellationLeft, cancellationRight), 1e-12)
    val mixedLeft = Vector(Vector(1e12, 1e-9), Vector(-3.0, 2e8), Vector(7.0, -4e-8))
    val mixedRight = Vector(Vector(-2.0, 5e-8), Vector(1e-10, -9.0), Vector(4e11, 3.0))
    val mixedExpected = orderedOracle(mixedLeft, mixedRight)
    val mixedActual = right(AllDistinctProducts.sum(mixedLeft, mixedRight, contractionBudget))
    assert(math.abs(mixedActual - mixedExpected) <= 1e-12 * math.max(1.0, math.abs(mixedExpected)))
    assert(AllDistinctProducts.sum(Vector(Vector(Double.MaxValue), Vector(Double.MaxValue)), Vector(Vector(2.0), Vector(2.0)), contractionBudget).isLeft)

  test("all-distinct cancellation is stable under matched permutations and endpoint swapping"):
    val left = Vector(Vector(1e16), Vector(1.0), Vector(-1e16))
    val rightAxis = Vector(Vector(0.0), Vector(1.0), Vector(0.0))
    val expected = orderedOracle(left, rightAxis)
    assertEqualsDouble(expected, 0.0, 0.0)
    assertEqualsDouble(right(AllDistinctProducts.sum(left, rightAxis, contractionBudget)), expected, 0.0)

    val permutation = Vector(2, 1, 0)
    val permutedLeft = permutation.map(left)
    val permutedRight = permutation.map(rightAxis)
    assertEqualsDouble(right(AllDistinctProducts.sum(permutedLeft, permutedRight, contractionBudget)), orderedOracle(permutedLeft, permutedRight), 0.0)
    assertEqualsDouble(right(AllDistinctProducts.sum(rightAxis, left, contractionBudget)), orderedOracle(rightAxis, left), 0.0)

  test("explanation budget accounts for comparison reasons on input-free products"):
    assert(QueryReuseProgram(Vector(node("constant")), QueryReuseBudget(1, 1, 0, maximumExplanationEntries = 1)).isLeft)
    val before = program(node("constant"))
    val after = program(node("constant", fidelity = "f32", implementation = "gpu"))
    decision(after, before, "constant") match
      case QueryReuseDecision.Rejected(paths, reasons) =>
        assertEquals(paths, Vector(Vector(QueryProductId("constant"))))
        assertEquals(reasons.size, 2)
        assert(after.explanationUpperBound >= paths.flatten.size + reasons.size + 1)
      case other => fail(other.toString)

  test("global all-distinct fallback preserves small terms across cancelling coordinates"):
    val left = Vector(Vector(1e16, -1e16), Vector(1.0, 0.0))
    val rightValues = Vector(Vector(1.0, 0.0), Vector(1.0, 1.0))
    Vector(Vector(0, 1), Vector(1, 0)).foreach: order =>
      val a = order.map(left)
      val b = order.map(rightValues)
      assertEqualsDouble(right(AllDistinctProducts.sum(a, b, contractionBudget)), 1.0, 0.0)
      assertEqualsDouble(right(AllDistinctProducts.sum(b, a, contractionBudget)), 1.0, 0.0)
    assert(AllDistinctProducts.sum(left, rightValues, ContractionBudget(1000, 16, maximumScalarProducts = 1)).isLeft)
