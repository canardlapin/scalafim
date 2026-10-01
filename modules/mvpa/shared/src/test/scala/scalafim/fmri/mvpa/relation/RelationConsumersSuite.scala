package scalafim.fmri.mvpa.relation

import gale.linalg.DMat
import munit.FunSuite
import multivar.core.{CoordinateEvidence, Lin, Primal, SemanticProvenance, SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, Column, EvidenceError}

class RelationConsumersSuite extends FunSuite:
  private def right[A](value: Either[EvidenceError, A]): A = value.fold(error => fail(error.message), scala.Predef.identity)
  private def rightCache[A](value: Either[RelationCacheRefusal, A]): A = value.fold(error => fail(error.toString), scala.Predef.identity)
  private def rightRsa[A](value: Either[RelationRsaRefusal, A]): A = value.fold(error => fail(error.toString), scala.Predef.identity)
  private def axis(name: String, role: SpaceRole, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(size)(i => s"$name-$i"), "fixture", "unit", "raw"))
  private def identity(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))
  private def relation[EK, NK](effects: AxisRef[EK], neural: AxisRef[NK], values: DMat, source: RelationSource, access: RelationAccess = RelationAccess.OwnedReplay("fixture")): Relation[effects.Id, neural.Id] =
    val table = Lin.fromDenseMatrix(values, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), identity(source.readoutRevision), SemanticProvenance.source("relation-consumer-fixture")).fold(error => fail(error.message), scala.Predef.identity)
    right(Relation.fromAcquired(effects, neural, table, RelationOrigins(source, access), Vector.fill(effects.size)(EffectEstimability.Estimable)))
  private def metric[EK](axis: AxisRef[EK], name: String): CrossClosure[axis.Id, axis.Id] =
    Lin.fromDenseMatrix(DMat.eye(axis.size), CoordinateEvidence.primal(axis.evidence), CoordinateEvidence.dual(axis.evidence), identity(name), SemanticProvenance.source("relation-consumer-fixture")).fold(error => fail(error.message), scala.Predef.identity)
  private def contrast[EK, CK](effects: AxisRef[EK], contrasts: AxisRef[CK], effectByContrast: DMat): Lin[Primal[effects.Id], Primal[contrasts.Id]] =
    Lin.fromDenseMatrix(effectByContrast.t, CoordinateEvidence.primal(effects.evidence), CoordinateEvidence.primal(contrasts.evidence), identity("contrast"), SemanticProvenance.source("relation-consumer-fixture")).fold(error => fail(error.message), scala.Predef.identity)

  test("samplewise block exclusion keeps undefined rows explicit"):
    val observed = Vector(Vector(0.0, 1.0, 2.0), Vector(1.0, 0.0, 3.0), Vector(2.0, 3.0, 0.0))
    val result = RelationConsumers.samplewise(observed, observed, Vector("i", "i", "j"), Vector("a", "a", "b"), RelationRsaMethod.Pearson, RelationConsumerBudget(16, 512)).fold(error => fail(error.message), scala.Predef.identity)
    assertEquals(result.rows.head, SamplewiseScore.Undefined(SamplewiseUndefined.InsufficientComparisons(1)))

  test("samplewise Pearson and Spearman use only cross-block entries with repeated items"):
    val observed = Vector(Vector(0.0, 9.0, 1.0, 2.0), Vector(8.0, 0.0, 3.0, 4.0), Vector(1.0, 3.0, 0.0, 7.0), Vector(2.0, 4.0, 6.0, 0.0))
    val model = Vector(Vector(0.0, -9.0, 1.0, 2.0), Vector(-8.0, 0.0, 3.0, 4.0), Vector(1.0, 3.0, 0.0, -7.0), Vector(2.0, 4.0, 6.0, 0.0))
    val common = (Vector("face", "face", "house", "house"), Vector("a", "a", "b", "b"), RelationConsumerBudget(32, 512))
    val pearson = RelationConsumers.samplewise(observed, model, common._1, common._2, RelationRsaMethod.Pearson, common._3).fold(error => fail(error.message), scala.Predef.identity)
    val spearman = RelationConsumers.samplewise(observed, model, common._1, common._2, RelationRsaMethod.Spearman, common._3).fold(error => fail(error.message), scala.Predef.identity)
    for result <- Vector(pearson, spearman) do result.rows(0) match
      case SamplewiseScore.Defined(value) => assertEqualsDouble(value, 1.0, 1e-12)
      case other => fail(other.toString)

  test("samplewise rejects a partial method without admitted row-local controls"):
    val matrix = Vector(Vector(0.0, 1.0), Vector(1.0, 0.0))
    val result = RelationConsumers.samplewise(matrix, matrix, Vector("a", "b"), Vector("one", "two"), RelationRsaMethod.PartialPearson(Vector.empty), RelationConsumerBudget(8, 512)).fold(error => fail(error.message), scala.Predef.identity)
    assert(result.rows.forall(_.isInstanceOf[SamplewiseScore.Undefined]))

  test("partial RSA uses row-major paired RHS and explicit Gale QR policy"):
    val c = Vector(-1.0, -1.0, 0.0, 0.0, 1.0, 1.0)
    val a = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val b = Vector(1.0, -1.0, -2.0, 2.0, 1.0, -1.0)
    val observed = c.zip(a).map((x, y) => 2.0 * x + y)
    val model = c.zip(a).zip(b).map((xy, z) => -3.0 * xy._1 + xy._2 + z)
    val control: Either[EvidenceError, Vector[Double]] = Right(c)
    RelationConsumers.partialGaleQR(observed, model, Vector(control), 1, PartialRsaPolicy()) match
      case RelationRsaOutcome.Defined(value) => assertEqualsDouble(value, 1.0 / math.sqrt(3.0), 1e-12)
      case other => fail(other.toString)

  test("partial RSA reports singular and zero residual outcomes and normalizes extreme scales"):
    val c = Vector(-1.0, -1.0, 0.0, 0.0, 1.0, 1.0)
    val a = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val b = Vector(1.0, -1.0, -2.0, 2.0, 1.0, -1.0)
    val observed = c.zip(a).map((x, y) => 2.0 * x + y)
    val model = c.zip(a).zip(b).map((xy, z) => -3.0 * xy._1 + xy._2 + z)
    val control: Either[EvidenceError, Vector[Double]] = Right(c)
    assert(RelationConsumers.partialGaleQR(observed, model, Vector(control, control), 2, PartialRsaPolicy()).isInstanceOf[RelationRsaOutcome.SingularControls])
    assertEquals(RelationConsumers.partialGaleQR(c, model, Vector(control), 1, PartialRsaPolicy()), RelationRsaOutcome.ZeroResidual(1e-12))
    RelationConsumers.partialGaleQR(observed.map(_ * 1e200), model.map(_ * 1e-200), Vector(control.map(_.map(_ * 1e100))), 1, PartialRsaPolicy()) match
      case RelationRsaOutcome.Defined(value) => assertEqualsDouble(value, 1.0 / math.sqrt(3.0), 1e-12)
      case other => fail(other.toString)

  test("partial RSA policy rejects malformed tolerances"):
    intercept[IllegalArgumentException](PartialRsaPolicy(rankTolerance = Double.NaN))
    intercept[IllegalArgumentException](PartialRsaPolicy(relativeResidualTolerance = -1.0))

  test("partial RSA centers large offsets and reports malformed vector shapes"):
    val c = Vector(-1.0, -1.0, 0.0, 0.0, 1.0, 1.0)
    val a = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val b = Vector(1.0, -1.0, -2.0, 2.0, 1.0, -1.0)
    val observed = c.zip(a).map((x, y) => 1e12 + 2.0 * x + y)
    val model = c.zip(a).zip(b).map((xy, z) => 1e12 - 3.0 * xy._1 + xy._2 + z)
    val control: Either[EvidenceError, Vector[Double]] = Right(c)
    RelationConsumers.partialGaleQR(observed, model, Vector(control), 1, PartialRsaPolicy()) match
      case RelationRsaOutcome.Defined(value) => assertEqualsDouble(value, 1.0 / math.sqrt(3.0), 1e-12)
      case other => fail(other.toString)
    RelationConsumers.partialGaleQR(Vector(1.0, 2.0), Vector(1.0), Vector.empty, 0, PartialRsaPolicy()) match
      case RelationRsaOutcome.Execution(EvidenceError.ShapeMismatch(_, 2, 1)) => ()
      case other => fail(other.toString)

  test("one relation set produces a retained RDM and an RSA receipt without another source read"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 3)
    val neural = axis("neural", SpaceRole.Observed, 1)
    val set = right(RelationSet(partitions, effects, neural, Vector(
      relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), RelationSource("a", "r", "readout-a", "prep", "noise")),
      relation(effects, neural, DMat.dense(3, 1, Vector(-1.0, -2.0, -4.0)), RelationSource("b", "r", "readout-b", "prep", "noise"))
    )))
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural, "metric"), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    val reuse = QueryReuseBudget(8, 64, 16)
    val cached = rightCache(RelationConsumers.cache(set, request, reuse, RelationConsumerBudget(16, 512)))
    // The owned-replay pair yields the signed RDM (-1, -9, -4), so this is
    // also an independent numerical fixture for the consumer scorer.
    val pairs = Map(("effects-0", "effects-1") -> 1.0, ("effects-0", "effects-2") -> 2.0, ("effects-1", "effects-2") -> 3.0)
    val model = right(SquareRelationModel("model", set.effectKeys, pairs, identity("model"), "unit", "fixed", "none"))
    val score = rightRsa(RelationConsumers.rsa(set, request, cached, model, RelationRsaMethod.Pearson, reuse, RelationConsumerBudget(16, 512)))
    assertEquals(score.receipt.product.text, "relation-rdm")
    assertEquals(score.receipt.comparison.nodes.last.id.text, "relation-rsa-comparison")
    score.outcome match
      case RelationRsaOutcome.Defined(value) => assertEqualsDouble(value, -3.0 * math.sqrt(3.0) / 14.0, 1e-12)
      case other => fail(other.toString)
    val contrasts = axis("shared-contrast", SpaceRole.Latent, 1)
    val query = FirstOrderQuery(effects, contrasts, contrast(effects, contrasts, DMat.dense(3, 1, Vector(1.0, -1.0, 0.0))))
    val first = right(RelationConsumers.firstOrderWithReceipt(set, request.pairing.edges.head.left, query, reuse, RelationConsumerBudget(16, 512)))
    val value = first.pattern.values(DMat.dense(1, 1, Vector(1.0))).fold(error => fail(error.toString), _(0, 0))
    assertEqualsDouble(value, -1.0, 1e-12)
    val selected = first.receipt.program.nodes.head.dependencies.collect:
      case dependency: QueryDependency.RelationValues => dependency
    assertEquals(selected, Vector(QueryDependency.relation(set.relations.head)))
    assert(score.receipt.program.nodes.head.dependencies.contains(selected.head))
    val witnesses = Vector(new ScopeReplay("fixture", "r", "a"), new ScopeReplay("fixture", "r", "b"))
    val scoped = right(RelationSet(partitions, effects, neural, Vector(
      relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), set.relations.head.origins.source, RelationAccess.ScopedReplay(witnesses.head)),
      relation(effects, neural, DMat.dense(3, 1, Vector(-1.0, -2.0, -4.0)), set.relations(1).origins.source, RelationAccess.ScopedReplay(witnesses(1))))))
    val detached = rightRsa(RelationConsumers.rsaDirect(scoped, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 0), RelationConsumerBudget(16, 512)))
    assertEquals(detached.outcome, score.outcome)
    assertEquals(detached.receipt.retainedGeometry, false)
    witnesses.foreach(_.expire())
    assertEquals(detached.outcome, score.outcome)
    assert(RelationConsumers.rsaDirect(scoped, request, model, RelationRsaMethod.Pearson, reuse, RelationConsumerBudget(16, 512)).isLeft)

  test("changed fitted relation provenance refuses cached geometry before it is read"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 1)
    def set(preparation: String) = right(RelationSet(partitions, effects, neural, Vector(
      relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), RelationSource("a", "r", "readout-a", preparation, "noise")),
      relation(effects, neural, DMat.dense(2, 1, Vector(-1.0, -2.0)), RelationSource("b", "r", "readout-b", preparation, "noise"))
    )))
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural, "metric"), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    val reuse = QueryReuseBudget(8, 64, 16)
    val cached = rightCache(RelationConsumers.cache(set("prep-a"), request, reuse, RelationConsumerBudget(16, 512)))
    val model = right(SquareRelationModel("model", Vector("effects-0", "effects-1"), Map(("effects-0", "effects-1") -> 1.0), identity("model"), "unit", "fixed", "none"))
    val result = RelationConsumers.rsa(set("prep-b"), request, cached, model, RelationRsaMethod.Pearson, reuse, RelationConsumerBudget(16, 512))
    assert(result.isLeft)

  test("one-shot relations refuse cache retention"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 1)
    val source = RelationSource("a", "r", "readout", "prep", "noise")
    val set = right(RelationSet(partitions, effects, neural, Vector(
      relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), source, RelationAccess.OneShot),
      relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), source, RelationAccess.OneShot)
    )))
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural, "metric"), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    assert(RelationConsumers.cache(set, request, QueryReuseBudget(8, 64, 16), RelationConsumerBudget(16, 512)).isLeft)
    val model = right(SquareRelationModel("one-shot model", set.effectKeys, Map(("effects-0", "effects-1") -> 1.0), identity("one-shot-model"), "unit", "fixed", "none"))
    assert(RelationConsumers.rsaDirect(set, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 0), RelationConsumerBudget(16, 512)).isLeft)

  test("typed samplewise geometry binds repeated items and blocks to its axis"):
    val samples = axis("samples", SpaceRole.Samples, 4)
    val geometry = right(SamplewiseGeometry(samples, DMat.dense(4, 4, Vector(
      0.0, 8.0, 1.0, 2.0,
      8.0, 0.0, 3.0, 4.0,
      1.0, 3.0, 0.0, 7.0,
      2.0, 4.0, 7.0, 0.0
    )), RelationOrigins(RelationSource("a", "r", "sample-geometry", "prep", "noise"), RelationAccess.OwnedReplay("fixture")), identity("geometry"), SamplewiseGeometryAdmission.OrdinaryDissimilarity("fixture squared distance", "raw")))
    val items = right(Column.fromValues(samples, Vector("face", "face", "house", "house"), identity("items")))
    val blocks = right(Column.fromValues(samples, Vector("a", "a", "b", "b"), identity("blocks")))
    val model = right(SquareRelationModel("model", Vector("face", "house"), Map(("face", "house") -> 1.0), identity("model"), "unit", "fixed", "none"))
    val result = right(RelationConsumers.samplewise(geometry, items, blocks, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 16), RelationConsumerBudget(16, 512)))
    assert(result.rows(0).score.isInstanceOf[SamplewiseScore.Undefined])
    assertEquals(result.rows.map(_.item), Vector("face", "face", "house", "house"))

  test("ordinary samplewise geometry preserves repeated-item block exclusion and consistent reordering"):
    val samples = axis("samplewise-six", SpaceRole.Samples, 6)
    val names = Vector("a", "b", "c", "a", "b", "c")
    val runs = Vector("run1", "run1", "run1", "run2", "run2", "run2")
    val model = right(SquareRelationModel("model", Vector("a", "b", "c"), Map(("a", "b") -> 1.0, ("a", "c") -> 2.0, ("b", "c") -> 4.0), identity("six-model"), "unit", "fixed", "none"))
    def distance(a: String, b: String): Double =
      if a == b then 0.0 else model.at(a, b).get
    val matrix = DMat.tabulate(6, 6)((row, col) => if row == col then 0.0 else if runs(row) == runs(col) then 999.0 else distance(names(row), names(col)))
    def evaluate(order: Vector[Int], preparation: String) =
      val ordered = right(AxisRef.fromStableKeys("samplewise-six", SpaceRole.Samples, order.map(samples.toRecord.stableKeys), "fixture", "unit", "raw"))
      val origins = RelationOrigins(RelationSource("a", "r", "six", preparation, "noise"), RelationAccess.OwnedReplay("fixture"))
      val geometry = right(SamplewiseGeometry(ordered, DMat.tabulate(6, 6)((row, col) => matrix(order(row), order(col))), origins, identity("six-geometry"), SamplewiseGeometryAdmission.OrdinaryDissimilarity("declared ordinary dissimilarity", "raw")))
      val items = right(Column.fromValues(ordered, order.map(names), identity("six-items")))
      val blocks = right(Column.fromValues(ordered, order.map(runs), identity("six-blocks")))
      right(RelationConsumers.samplewise(geometry, items, blocks, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)))
    val result = evaluate(Vector.range(0, 6), "prep")
    val reordered = evaluate(Vector(5, 2, 3, 0, 4, 1), "prep")
    for score <- result.rows ++ reordered.rows do score.score match
      case SamplewiseScore.Defined(value) => assertEqualsDouble(value, 1.0, 1e-12)
      case other => fail(other.toString)
    assertEquals(result.rows.map(row => row.item -> row.block).sorted, reordered.rows.map(row => row.item -> row.block).sorted)
    assert(result.program.nodes != evaluate(Vector.range(0, 6), "changed-preparation").program.nodes)
    val origins = RelationOrigins(RelationSource("a", "r", "six", "prep", "noise"), RelationAccess.OwnedReplay("fixture"))
    val geometry = right(SamplewiseGeometry(samples, matrix, origins, identity("six-geometry"), SamplewiseGeometryAdmission.OrdinaryDissimilarity("ordinary", "raw")))
    val unknown = right(Column.fromValues(samples, Vector.fill(6)("unknown-repeated-item"), identity("unknown")))
    val blocks = right(Column.fromValues(samples, runs, identity("blocks")))
    assert(RelationConsumers.samplewise(geometry, unknown, blocks, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 1024), RelationConsumerBudget(1024, 1024)).isLeft)
    assert(SamplewiseGeometry(samples, matrix, origins, identity("six-geometry"), SamplewiseGeometryAdmission.SignedCrossvalidated).isLeft)

  test("model, scorer and controls alter only the comparison child; fitting and geometry changes reject reuse"):
    val partitions = axis("reuse-runs", SpaceRole.Samples, 2)
    val effects = axis("reuse-effects", SpaceRole.Latent, 3)
    val neural = axis("reuse-neural", SpaceRole.Observed, 1)
    def source(preparation: String = "prep", noise: String = "noise", revision: String = "readout") =
      right(RelationSet(partitions, effects, neural, Vector(
        relation(effects, neural, DMat.dense(3, 1, Vector(1.0, 2.0, 4.0)), RelationSource("a", "r", revision + "a", preparation, noise)),
        relation(effects, neural, DMat.dense(3, 1, Vector(-1.0, -2.0, -4.0)), RelationSource("b", "r", revision + "b", preparation, noise)))))
    val set = source()
    val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(right(RelationRdm.allDistinctOrdered(partitions)), metric(neural, "metric"), MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
    val budget = RelationConsumerBudget(1024, 1024)
    val reuse = QueryReuseBudget(8, 64, 1024)
    val cached = rightCache(RelationConsumers.cache(set, request, reuse, budget))
    def model(values: Vector[Double], name: String) = right(SquareRelationModel(name, set.effectKeys,
      Map(("reuse-effects-0", "reuse-effects-1") -> values(0), ("reuse-effects-0", "reuse-effects-2") -> values(1), ("reuse-effects-1", "reuse-effects-2") -> values(2)), identity(name), "unit", "fixed", "none"))
    val baseModel = model(Vector(1.0, 2.0, 3.0), "base")
    val base = rightRsa(RelationConsumers.rsa(set, request, cached, baseModel, RelationRsaMethod.Pearson, reuse, budget))
    val changedModel = rightRsa(RelationConsumers.rsa(set, request, cached, model(Vector(3.0, 2.0, 1.0), "changed"), RelationRsaMethod.Pearson, reuse, budget))
    val spearman = rightRsa(RelationConsumers.rsa(set, request, cached, baseModel, RelationRsaMethod.Spearman, reuse, budget))
    val partial = rightRsa(RelationConsumers.rsa(set, request, cached, baseModel, RelationRsaMethod.PartialPearson(Vector(model(Vector(1.0, 0.0, -1.0), "control"))), reuse, budget))
    for next <- Vector(changedModel, spearman, partial) do
      assertEquals(next.receipt.program.nodes, base.receipt.program.nodes)
      assert(next.receipt.comparison.nodes.last != base.receipt.comparison.nodes.last)
    for changed <- Vector(source(preparation = "changed"), source(noise = "changed"), source(revision = "changed")) do
      assert(RelationConsumers.rsa(changed, request, cached, baseModel, RelationRsaMethod.Pearson, reuse, budget).isLeft)
    assert(RelationConsumers.rsa(set, request.copy(policy = IdentityRdmPolicy(normalizeByFeatures = !request.policy.normalizeByFeatures)), cached, baseModel, RelationRsaMethod.Pearson, reuse, budget).isLeft)
    assert(RelationConsumers.rsa(set, request.copy(metric = metric(neural, "new-metric")), cached, baseModel, RelationRsaMethod.Pearson, reuse, budget).isLeft)
    RelationConsumers.rsa(set, request.copy(fit = FitRequirements(noiseRevision = Some("refit-noise"))), cached, baseModel, RelationRsaMethod.Pearson, reuse, budget) match
      case Left(RelationRsaRefusal.RefitRequired(_, _, _)) => ()
      case other => fail(other.toString)

  test("first-order receipt names its selected relation and refuses non-estimable support"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 1)
    val contrasts = axis("contrasts", SpaceRole.Latent, 1)
    val source = RelationSource("a", "r", "readout", "prep", "noise")
    val raw = DMat.dense(2, 1, Vector(1.0, 2.0))
    val table = Lin.fromDenseMatrix(raw, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), identity("first-order"), SemanticProvenance.source("relation-consumer-fixture")).fold(error => fail(error.message), scala.Predef.identity)
    val unavailable = right(Relation(effects, neural, table, RelationOrigins(source, RelationAccess.OwnedReplay("fixture")), Vector(EffectEstimability.Estimable, EffectEstimability.NotEstimable("zero regressor"))))
    val available = relation(effects, neural, raw, RelationSource("b", "r", "readout-b", "prep", "noise"))
    val set = right(RelationSet(partitions, effects, neural, Vector(unavailable, available)))
    val query = FirstOrderQuery(effects, contrasts, contrast(effects, contrasts, DMat.dense(2, 1, Vector(1.0, 1.0))))
    val coordinate = right(RelationRdm.allDistinctOrdered(partitions)).edges.head.left
    assert(RelationConsumers.firstOrderWithReceipt(set, coordinate, query, QueryReuseBudget(8, 64, 32), RelationConsumerBudget(16, 512)).isLeft)

  test("rectangular consumer streams a typed form over explicit endpoints"):
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("neural", SpaceRole.Observed, 1)
    val left = relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0)), RelationSource("a", "r", "left", "prep", "noise"))
    val rightRelation = relation(effects, neural, DMat.dense(2, 1, Vector(1.0, 3.0)), RelationSource("b", "r", "right", "prep", "noise"))
    val form = right(SecondOrderQuery(RelationPair(left, rightRelation), metric = Some(metric(neural, "rect-metric"))).effectForm)
    val model = right(RectangularRelationModel("rectangular", effects, effects, DMat.dense(2, 2, Vector(1.0, 3.0, 2.0, 6.0)), identity("rect-model")))
    val score = right(RelationConsumers.rectangular(effects, effects, form, model, RelationConsumerBudget(8, 512)))
    score.outcome match
      case RectangularRelationOutcome.Defined(value) => assertEqualsDouble(value, 1.0, 1e-12)
      case other => fail(other.toString)
