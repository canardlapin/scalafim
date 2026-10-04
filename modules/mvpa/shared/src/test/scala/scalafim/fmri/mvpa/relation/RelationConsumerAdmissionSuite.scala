package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import munit.FunSuite
import multivar.core.{CoordinateEvidence, Lin, Primal, SemanticProvenance, SpaceRole, ValueId, ValueIdentity}
import scala.compiletime.testing.typeCheckErrors
import scalafim.fmri.mvpa.{AxisRef, EvidenceError}

/** Independent admission checks: every refusal below occurs before a retained
  * consumer can ask an operator for another value. */
class RelationConsumerAdmissionSuite extends FunSuite:
  private def right[A](value: Either[EvidenceError, A]): A = value.fold(error => fail(error.message), identity)
  private def axis(name: String, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Latent, Vector.tabulate(size)(i => s"$name-$i"), "admission", "unit", "raw"))
  private def id(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))
  private def relation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], values: DMat, source: RelationSource, access: RelationAccess = RelationAccess.OwnedReplay("admission"), estimability: Vector[EffectEstimability] = Vector.empty): Relation[effects.Id, neural.Id] =
    val table = Lin.fromDenseMatrix(values, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), id(source.readoutRevision), SemanticProvenance.source("admission")).fold(error => fail(error.message), identity)
    right(Relation.fromAcquired(effects, neural, table, RelationOrigins(source, access), if estimability.isEmpty then Vector.fill(effects.size)(EffectEstimability.Estimable) else estimability))
  private def operatorRelation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], operator: DoubleLinearOperator, source: RelationSource, access: RelationAccess): Relation[effects.Id, neural.Id] =
    val table = Lin.fromLinearMap(operator, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), id(source.readoutRevision), SemanticProvenance.source("admission")).fold(error => fail(error.message), identity)
    right(Relation.fromAcquired(effects, neural, table, RelationOrigins(source, access), Vector.fill(effects.size)(EffectEstimability.Estimable)))
  private def metric[SK](axis: AxisRef[SK]): CrossClosure[axis.Id, axis.Id] =
    Lin.fromDenseMatrix(DMat.eye(axis.size), CoordinateEvidence.primal(axis.evidence), CoordinateEvidence.dual(axis.evidence), id("metric"), SemanticProvenance.source("admission")).fold(error => fail(error.message), identity)
  private def contrast[EK, CK](effects: AxisRef[EK], contrasts: AxisRef[CK], values: DMat): Lin[Primal[effects.Id], Primal[contrasts.Id]] =
    Lin.fromDenseMatrix(values.t, CoordinateEvidence.primal(effects.evidence), CoordinateEvidence.primal(contrasts.evidence), id("contrast"), SemanticProvenance.source("admission")).fold(error => fail(error.message), identity)

  test("rectangular 2 by 3 forms preserve orientation and transpose explicitly"):
    val left = axis("left", 2); val rightAxis = axis("right", 3); val neural = axis("neural", 1)
    val leftRelation = relation(left, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), RelationSource("a", "r", "left", "prep", "noise"))
    val rightRelation = relation(rightAxis, neural, DMat.dense(3, 1, Vector(1.0, 3.0, 4.0)), RelationSource("b", "r", "right", "prep", "noise"))
    val form = right(SecondOrderQuery(RelationPair(leftRelation, rightRelation), metric = Some(metric(neural))).effectForm)
    val model = right(RectangularRelationModel("forward", left, rightAxis, DMat.dense(2, 3, Vector(1.0, 3.0, 4.0, 2.0, 6.0, 8.0)), id("forward")))
    right(RelationConsumers.rectangular(left, rightAxis, form, model, RelationConsumerBudget(1024, 1024))).outcome match
      case RectangularRelationOutcome.Defined(value) => assertEqualsDouble(value, 1.0, 1e-12)
      case other => fail(other.toString)
    val reversed = right(form.reverse)
    val transposed = right(RectangularRelationModel("reverse", rightAxis, left, DMat.dense(3, 2, Vector(1.0, 2.0, 3.0, 6.0, 4.0, 8.0)), id("reverse")))
    right(RelationConsumers.rectangular(rightAxis, left, reversed, transposed, RelationConsumerBudget(1024, 1024))).outcome match
      case RectangularRelationOutcome.Defined(value) => assertEqualsDouble(value, 1.0, 1e-12)
      case other => fail(other.toString)

  test("foreign rectangular endpoint axes do not typecheck"):
    val errors = typeCheckErrors("""
import multivar.core.SemanticSpace
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.relation.{EffectForm, RectangularRelationModel, RelationConsumerBudget, RelationConsumers}
def invalid[LK, RK, FK](left: AxisRef[LK], right: AxisRef[RK], foreign: AxisRef[FK], form: EffectForm[left.Id, right.Id], model: RectangularRelationModel[left.Id, foreign.Id]) =
  RelationConsumers.rectangular(left, right, form, model, RelationConsumerBudget(1024, 1024))
""")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("RectangularRelationModel")))
    val valid = typeCheckErrors("""
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.relation.{EffectForm, RectangularRelationModel, RelationConsumerBudget, RelationConsumers}
def compatible[LK, RK](left: AxisRef[LK], right: AxisRef[RK], form: EffectForm[left.Id, right.Id], model: RectangularRelationModel[left.Id, right.Id]) =
  RelationConsumers.rectangular(left, right, form, model, RelationConsumerBudget(1024, 1024))
""")
    assertEquals(valid, Nil)

  test("rectangular receipt frames source fields that collide under case-class display strings"):
    val left = axis("framed-left", 2)
    val rightAxis = axis("framed-right", 3)
    val neural = axis("framed-neural", 1)
    val first = RelationSource("a,b", "c", "d", "e", "f")
    val second = RelationSource("a", "b,c", "d", "e", "f")
    assertEquals(first.toString, second.toString)
    val model = right(RectangularRelationModel("framed", left, rightAxis, DMat.dense(2, 3, Vector(1.0, 3.0, 4.0, 2.0, 6.0, 8.0)), id("framed-model")))
    def evaluate(source: RelationSource) =
      val pair = RelationPair(relation(left, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), source),
        relation(rightAxis, neural, DMat.dense(3, 1, Vector(1.0, 3.0, 4.0)), RelationSource("b", "r", "right", "prep", "noise")))
      val form = right(SecondOrderQuery(pair, metric = Some(metric(neural))).effectForm)
      right(RelationConsumers.rectangular(left, rightAxis, form, model, RelationConsumerBudget(1024, 1024)))
    val a = evaluate(first)
    val b = evaluate(second)
    assert(a.identity != b.identity)
    assertEquals(a.outcome, b.outcome)

  test("first-order owned relation returns contrast and receipt; admission refusals occur early"):
    val partitions = axis("runs", 2); val effects = axis("effects", 3); val neural = axis("brain", 1); val contrasts = axis("contrasts", 1)
    val source = RelationSource("a", "r", "readout", "prep", "noise")
    val owned = relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), source)
    val query = FirstOrderQuery(effects, contrasts, contrast(effects, contrasts, DMat.dense(3, 1, Vector(1.0, -1.0, 0.0))))
    val set = right(RelationSet(partitions, effects, neural, Vector(owned, owned)))
    val coordinate = right(RelationRdm.allDistinctOrdered(partitions)).edges.head.left
    val result = right(RelationConsumers.firstOrderWithReceipt(set, coordinate, query, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)))
    assertEqualsDouble(result.pattern.values(DMat.dense(1, 1, Vector(1.0))).fold(error => fail(error.toString), _(0, 0)), -1.0, 1e-12)
    assert(result.receipt.program.nodes.head.dependencies.exists(_.isInstanceOf[QueryDependency.RelationValues]))
    assert(RelationConsumers.firstOrderWithReceipt(set, coordinate, query, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(0, 1024)).isLeft)
    val unavailable = relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), source, estimability = Vector(EffectEstimability.Estimable, EffectEstimability.NotEstimable("rank"), EffectEstimability.Estimable))
    assert(RelationConsumers.firstOrderWithReceipt(right(RelationSet(partitions, effects, neural, Vector(unavailable, unavailable))), coordinate, query, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)
    val oneShot = relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), source, RelationAccess.OneShot)
    assert(RelationConsumers.firstOrderWithReceipt(right(RelationSet(partitions, effects, neural, Vector(oneShot, oneShot))), coordinate, query, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)

  test("cache admission rejects retention, output and workspace budgets and reports structured refit"):
    val partitions = axis("runs", 2); val effects = axis("effects", 2); val neural = axis("brain", 1)
    def set(prep: String) = right(RelationSet(partitions, effects, neural, Vector(
      relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), RelationSource("a", "r", "one", prep, "noise")),
      relation(effects, neural, DMat.dense(2, 1, Vector(-1.0, -2.0)), RelationSource("b", "r", "two", prep, "noise"))
    )))
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    assert(RelationConsumers.cache(set("prep"), request, QueryReuseBudget(8, 64, 0), RelationConsumerBudget(1024, 1024)).isLeft)
    assert(RelationConsumers.cache(set("prep"), request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(0, 1024)).isLeft)
    assert(RelationConsumers.cache(set("prep"), request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 0)).isLeft)
    val fitRequest: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = request.copy(fit = FitRequirements(preparationRevision = Some("other")))
    RelationConsumers.cache(set("prep"), fitRequest, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)) match
      case Left(RelationCacheRefusal.RefitRequired(_, _, _)) => ()
      case other => fail(other.toString)

  test("rectangular missing effects retain endpoint identity and refuse before placeholder operator reads"):
    val left = axis("missing-left", 2)
    val rightAxis = axis("missing-right", 3)
    val neural = axis("missing-neural", 1)
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 1
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("missing effect placeholder was evaluated")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = applyTo(input, output)
    val table = Lin.fromLinearMap(poison, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(left.evidence), id("missing"), SemanticProvenance.source("missing")).fold(error => fail(error.message), identity)
    val leftRelation = right(Relation(left, neural, table, RelationOrigins(RelationSource("a", "r", "missing", "prep", "noise"), RelationAccess.OwnedReplay("fixture")), Vector(EffectEstimability.Estimable, EffectEstimability.NotEstimable("unsupported coefficient"))))
    val rightRelation = relation(rightAxis, neural, DMat.dense(3, 1, Vector(1.0, 3.0, 4.0)), RelationSource("b", "r", "right", "prep", "noise"))
    val form = right(SecondOrderQuery(RelationPair(leftRelation, rightRelation), metric = Some(metric(neural))).effectForm)
    val values = DMat.dense(2, 3, Vector(1.0, 3.0, 4.0, 2.0, 6.0, 8.0))
    val model = right(RectangularRelationModel("missing", left, rightAxis, values, id("missing-model")))
    val result = right(RelationConsumers.rectangular(left, rightAxis, form, model, RelationConsumerBudget(1024, 1024)))
    assertEquals(result.outcome, RectangularRelationOutcome.MissingEffects(Vector(1 -> "unsupported coefficient"), Vector.empty))
    val reversed = right(form.reverse)
    val reversedModel = right(RectangularRelationModel("missing-reverse", rightAxis, left, values.t, id("missing-reverse")))
    assertEquals(right(RelationConsumers.rectangular(rightAxis, left, reversed, reversedModel, RelationConsumerBudget(1024, 1024))).outcome, RectangularRelationOutcome.MissingEffects(Vector.empty, Vector(1 -> "unsupported coefficient")))
    assertEquals(reads, 0)

  test("budget, one-shot and reuse refusals never read poisoned sources; cached RSA adds no reads"):
    val partitions = axis("count-runs", 2); val effects = axis("count-effects", 2); val neural = axis("count-brain", 1)
    val source = RelationSource("a", "r", "count", "prep", "noise")
    var reads = 0
    def counted(values: Vector[Double], poison: Boolean): DoubleLinearOperator = new DoubleLinearOperator:
      val rows = 2
      val cols = 1
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        if poison then throw new IllegalStateException("admission read a poisoned source")
        output(0) = values(0) * input(0)
        output(1) = values(1) * input(0)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        if poison then throw new IllegalStateException("admission read a poisoned source")
        output(0) = values(0) * input(0) + values(1) * input(1)
    def relations(access: RelationAccess, poison: Boolean) = right(RelationSet(partitions, effects, neural, Vector(
      operatorRelation(effects, neural, counted(Vector(1.0, 2.0), poison), source, access),
      operatorRelation(effects, neural, counted(Vector(-1.0, -2.0), poison), source.copy(readoutRevision = "count-2"), access)
    )))
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    assert(RelationConsumers.cache(relations(RelationAccess.OwnedReplay("counter"), poison = true), request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(0, 1024)).isLeft)
    assertEquals(reads, 0)
    assert(RelationConsumers.cache(relations(RelationAccess.OneShot, poison = true), request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)
    assertEquals(reads, 0)
    val witness = new ScopeReplay("counter", "revision", "scoped-poison")
    val scoped = relations(RelationAccess.ScopedReplay(witness), poison = true)
    assert(RelationConsumers.cache(scoped, request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)
    val contrasts = axis("count-contrast", 1)
    val query = FirstOrderQuery(effects, contrasts, contrast(effects, contrasts, DMat.dense(2, 1, Vector(1.0, -1.0))))
    assert(RelationConsumers.firstOrderWithReceipt(scoped, request.pairing.edges.head.left, query, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)
    assertEquals(reads, 0)
    val current = relations(RelationAccess.OwnedReplay("counter"), poison = false)
    val cached = RelationConsumers.cache(current, request, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).fold(error => fail(error.toString), identity)
    assert(reads > 0)
    val beforeReuse = reads
    val model = right(SquareRelationModel("count-model", current.effectKeys, Map(("count-effects-0", "count-effects-1") -> 1.0), id("count-model"), "unit", "fixed", "none"))
    val poisoned = relations(RelationAccess.OneShot, poison = true)
    assert(RelationConsumers.rsaDirect(poisoned, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 0), RelationConsumerBudget(0, 1024)).isLeft)
    assert(RelationConsumers.rsaDirect(poisoned, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(1, 64, 0), RelationConsumerBudget(1024, 1024)).isLeft)
    assertEquals(reads, beforeReuse)
    RelationConsumers.rsa(current, request, cached, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).fold(error => fail(error.toString), identity)
    assertEquals(reads, beforeReuse)
