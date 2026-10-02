package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AcquisitionCoordinates, AxisRef, Column, EvidenceError, EvidenceOrigins, EvidenceSource, Observations, PreparationSupport, ValueSupport}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ObservationMeanRelationsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), scala.Predef.identity)

  private def axis(name: String, role: SpaceRole, keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, keys, "fixture", "unit", "raw"))

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private def identity(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))

  private final class Resource(acquireResult: Either[String, Unit] = Right(()), closeResult: Either[String, Unit] = Right(())) extends ObservationMeanResource:
    var acquires = 0
    var closes = 0
    def acquire(): Either[String, Unit] =
      acquires += 1
      acquireResult
    def close(): Either[String, Unit] =
      closes += 1
      closeResult

  private final class Counted(values: DMat) extends DoubleLinearOperator:
    val rows = values.rows
    val cols = values.cols
    var calls = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      values.applyTo(input, output)
    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      calls += 1
      values.transposeApplyTo(input, output)

  private final class Fixture(operator: DoubleLinearOperator):
    val samples = axis("samples", SpaceRole.Samples, Vector("s0", "s1", "s2", "s3", "s4"))
    val partitions = axis("runs", SpaceRole.Samples, Vector("r1", "r2"))
    val effects = axis("effects", SpaceRole.Latent, Vector("a", "b", "c"))
    val neural = axis("neural", SpaceRole.Observed, Vector("n0", "n1"))
    val partition = right(Column.fromValues(samples, Vector("r1", "r1", "r1", "r2", "r2"), identity("partition")))
    val condition = right(Column.fromValues(samples, Vector("a", "a", "b", "a", "b"), identity("condition")))
    val plan = right(ObservationMeanPlan(samples, partitions, effects, partition, condition))
    val observations: Observations[samples.Id, neural.Id] =
      right(Observations.fromOperator(samples, neural, operator, identity("observation"), source("bold")))

  private def fixture(operator: DoubleLinearOperator = DMat.dense(5, 2, Vector(
      1.0, 10.0,
      3.0, 30.0,
      5.0, 50.0,
      7.0, 70.0,
      11.0, 110.0
  ))): Fixture =
    new Fixture(operator)

  private def origins(access: ObservationMeanAccess): ObservationMeanOrigins =
    ObservationMeanOrigins("acq", "response", "ordinary-mean", "fixed", "none", access)

  test("sparse mean legs average unequal cells, retain declared effect order, and implement the adjoint"):
    val f = fixture()
    val resource = new Resource
    right(ObservationMeanRelations.withSource(f.plan, f.neural, ObservationMeanSource.oneShot(f.observations, origins(ObservationMeanAccess.OneShot), resource), ObservationMeanBudget(512)): relations =>
      assert(relations.relations.forall(relation => RelationAccess.admitImmediate(relation.origins.access).isLeft))
      assert(relations.relations.map(_.origins.source).distinct.length == 1)
      val first = right(relations.relations.head.estimate(DMat.eye(f.neural.size)))
      assertEqualsDouble(first(0, 0), 2.0, 1e-12)
      assertEqualsDouble(first(0, 1), 20.0, 1e-12)
      assertEqualsDouble(first(1, 0), 5.0, 1e-12)
      assertEqualsDouble(first(1, 1), 50.0, 1e-12)
      assertEqualsDouble(first(2, 0), 0.0, 1e-12)
      assertEquals(relations.relations.head.estimability(2), EffectEstimability.NotEstimable("no observations for declared partition/effect cell"))
      val adjoint = right(relations.relations.head.estimate.star(DMat.dense(3, 1, Vector(2.0, 3.0, 7.0))))
      assertEqualsDouble(adjoint(0, 0), 19.0, 1e-12)
      assertEqualsDouble(adjoint(1, 0), 190.0, 1e-12)
      Right(())
    )
    assertEquals(resource.acquires, 1)
    assertEquals(resource.closes, 1)

  test("foreign axes and budgets refuse before acquisition or source reads"):
    val f = fixture()
    val counted = new Counted(DMat.dense(5, 2, Vector.fill(10)(1.0)))
    val observations = right(Observations.fromOperator(f.samples, f.neural, counted, identity("poison"), source("poison")))
    val resource = new Resource
    val foreign = axis("foreign", SpaceRole.Observed, Vector("n0", "n1"))
    // A decoded foreign descriptor is rejected at the runtime provider
    // boundary without touching its callable operator.
    val sourceHandle = ObservationMeanSource.oneShot(
      observations.asInstanceOf[Observations[f.samples.Id, foreign.Id]],
      origins(ObservationMeanAccess.OneShot),
      resource
    )
    assert(ObservationMeanRelations.withSource(f.plan, foreign, sourceHandle, ObservationMeanBudget(100))(_ => Right(())).isLeft)
    assertEquals(resource.acquires, 0)
    assertEquals(counted.calls, 0)
    val resource2 = new Resource
    assert(ObservationMeanRelations.withSource(f.plan, f.neural, ObservationMeanSource.oneShot(observations, origins(ObservationMeanAccess.OneShot), resource2), ObservationMeanBudget(0))(_ => Right(())).isLeft)
    assertEquals(resource2.acquires, 0)
    assertEquals(counted.calls, 0)

  test("scoped sources expire after the callback and cannot be retained"):
    val f = fixture()
    val resource = new Resource
    var escaped: RelationSet[f.partitions.Id, f.effects.Id, f.neural.Id] | Null = null
    right(ObservationMeanRelations.withSource(
      f.plan, f.neural,
      ObservationMeanSource.scopedReplay(f.observations, origins(ObservationMeanAccess.ScopedReplay("fixture", "v1")), resource),
      ObservationMeanBudget(512)
    ): relations =>
      assert(relations.relations.forall(_.origins.access.isInstanceOf[RelationAccess.ScopedReplay]))
      assert(relations.relations.forall(relation => !RelationAccess.admitsRetained(relation.origins.access)))
      escaped = relations
      Right(())
    )
    val relation = escaped.nn.relations.head
    assert(RelationAccess.admitImmediate(relation.origins.access).isLeft)
    intercept[IllegalStateException](relation.estimate(DMat.eye(f.neural.size)))
    intercept[IllegalStateException](relation.estimate.star(DMat.eye(f.effects.size)))

  test("acquisition and close failures preserve one acquire attempt and expire before close"):
    val f = fixture()
    val failedAcquire = new Resource(Left("cannot open"))
    val result = ObservationMeanRelations.withSource(
      f.plan, f.neural,
      ObservationMeanSource.oneShot(f.observations, origins(ObservationMeanAccess.OneShot), failedAcquire),
      ObservationMeanBudget(512)
    )(_ => Right(()))
    assert(result.isLeft)
    assertEquals(failedAcquire.acquires, 1)
    assertEquals(failedAcquire.closes, 0)
    val closeFailure = new Resource(closeResult = Left("cannot close"))
    val closeResult = ObservationMeanRelations.withSource(
      f.plan, f.neural,
      ObservationMeanSource.scopedReplay(f.observations, origins(ObservationMeanAccess.ScopedReplay("fixture", "v2")), closeFailure),
      ObservationMeanBudget(512)
    )(_ => Right(()))
    assertEquals(closeResult, Left(ObservationMeanRelationError.CloseFailure("cannot close")))
    assertEquals(closeFailure.closes, 1)
    val taskAndClose = new Resource(closeResult = Left("also cannot close"))
    val combined = ObservationMeanRelations.withSource(
      f.plan, f.neural,
      ObservationMeanSource.oneShot(f.observations, origins(ObservationMeanAccess.OneShot), taskAndClose),
      ObservationMeanBudget(512)
    )(_ => Left(ObservationMeanRelationError.Access("callback refused")))
    assertEquals(
      combined,
      Left(ObservationMeanRelationError.TaskAndCloseFailure(ObservationMeanRelationError.Access("callback refused"), "also cannot close"))
    )
    assertEquals(taskAndClose.closes, 1)
    val thrown = new Resource
    val thrownResult = ObservationMeanRelations.withSource(
      f.plan, f.neural,
      ObservationMeanSource.oneShot(f.observations, origins(ObservationMeanAccess.OneShot), thrown),
      ObservationMeanBudget(512)
    )(_ => throw IllegalStateException("callback threw"))
    assert(thrownResult.isLeft)
    assertEquals(thrown.closes, 1)

  test("owned dense snapshot copies fixture values and supplies retained access"):
    val f = fixture()
    val raw = DMat.dense(5, 2, Vector(1.0, 10.0, 3.0, 30.0, 5.0, 50.0, 7.0, 70.0, 11.0, 110.0))
    val result = right(ObservationMeanRelations.fromOwnedDense(
      f.plan, f.neural, raw, source("owned"), RelationSource("acq", "response", "ordinary-mean", "fixed", "none"), "fixture-owned", ObservationMeanBudget(512)
    ))
    assert(result.relations.forall(relation => RelationAccess.admitsRetained(relation.origins.access)))
    val mean = right(result.relations.head.estimate(DMat.eye(2)))
    assertEqualsDouble(mean(0, 0), 2.0, 1e-12)
    val changedCondition = right(Column.fromValues(f.samples, Vector("a", "b", "a", "a", "b"), identity("condition")))
    val changedPlan = right(ObservationMeanPlan(f.samples, f.partitions, f.effects, f.plan.partition, changedCondition))
    val changed = right(ObservationMeanRelations.fromOwnedDense(
      changedPlan, f.neural, raw, source("owned"), RelationSource("acq", "response", "ordinary-mean", "fixed", "none"), "fixture-owned", ObservationMeanBudget(512)
    ))
    val changedMean = right(changed.relations.head.estimate(DMat.eye(2)))
    assertEqualsDouble(changedMean(0, 0), 3.0, 1e-12)
    assertNotEquals(result.relations.head.estimate.valueIdentity, changed.relations.head.estimate.valueIdentity)
    assertNotEquals(QueryDependency.relation(result.relations.head), QueryDependency.relation(changed.relations.head))

  test("supported observation origins survive grouping and keep shared-acquisition pairing descriptive"):
    val f = fixture()
    val supportedSource = source("supported-bold")
    val supportedValues = identity("supported-values")
    val support = right(EvidenceOrigins.make(
      supportedSource,
      supportedValues,
      AcquisitionCoordinates.OriginalTemporalAxis(f.samples.descriptor),
      ValueSupport.Bounded(f.samples.descriptor, supportedValues, Vector(0, 1, 2, 3, 4)),
      PreparationSupport.FixedShared(ValueSupport.Bounded(f.samples.descriptor, supportedValues, Vector(0, 1, 2, 3, 4))),
      f.samples.descriptor
    ))
    val observations = right(Observations.fromDense(
      f.samples, f.neural,
      DMat.dense(5, 2, Vector(1.0, 10.0, 3.0, 30.0, 5.0, 50.0, 7.0, 70.0, 11.0, 110.0)),
      supportedValues,
      supportedSource,
      support
    ))
    val resource = new Resource
    right(ObservationMeanRelations.withSource(
      f.plan,
      f.neural,
      ObservationMeanSource.oneShot(observations, origins(ObservationMeanAccess.OneShot), resource),
      ObservationMeanBudget(512)
    ): relations =>
      assert(relations.relations.forall(_.origins.support.outputAssociation.contains(f.effects.descriptor)))
      val pairing = right(RelationRdm.allDistinctOrdered(f.partitions))
      right(pairing.assess(relations, MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty)).claim match
        case PairingClaim.Descriptive(_) => ()
        case other => fail(other.toString)
      Right(())
    )

  test("nested source refusals cannot close or expire the outer acquired scope"):
    val f = fixture()
    val resource = new Resource
    val input = ObservationMeanSource.scopedReplay(f.observations,origins(ObservationMeanAccess.ScopedReplay("nested","v1")),resource)
    right(ObservationMeanRelations.withSource(f.plan,f.neural,input,ObservationMeanBudget(512)): relations =>
      val before = right(relations.relations.head.estimate(DMat.eye(2)))
      assert(ObservationMeanRelations.withSource(f.plan,f.neural,input,ObservationMeanBudget(0))(_ => Right(())).isLeft)
      assert(ObservationMeanRelations.withSource(f.plan,f.neural,input,ObservationMeanBudget(512))(_ => Right(())).isLeft)
      assertEquals(resource.acquires,1)
      assertEquals(resource.closes,0)
      assert(RelationAccess.admitImmediate(relations.relations.head.origins.access).isRight)
      val after = right(relations.relations.head.estimate(DMat.eye(2)))
      assertEqualsDouble(after(0,0),before(0,0),1e-12)
      Right(())
    )
    assertEquals(resource.closes,1)

  test("finite convex means avoid predivision overflow and agree in forward and adjoint orientation"):
    val samples = axis("extreme-samples",SpaceRole.Samples,Vector("s0","s1"))
    val partitions = axis("extreme-run",SpaceRole.Samples,Vector("r"))
    val effects = axis("extreme-effect",SpaceRole.Latent,Vector("a"))
    val neural = axis("extreme-neural",SpaceRole.Observed,Vector("n"))
    val partition = right(Column.fromValues(samples,Vector("r","r"),identity("extreme-group")))
    val condition = right(Column.fromValues(samples,Vector("a","a"),identity("extreme-condition")))
    val plan = right(ObservationMeanPlan(samples,partitions,effects,partition,condition))
    val set = right(ObservationMeanRelations.fromOwnedDense(plan,neural,DMat.dense(2,1,Vector(1e308,1e308)),source("extreme"),RelationSource("a","r","mean","fixed","none"),"copied-extreme",ObservationMeanBudget(512)))
    val forward = right(set.relations.head.estimate(DMat.eye(1)))(0,0)
    val adjoint = right(set.relations.head.estimate.star(DMat.eye(1)))(0,0)
    assert(forward.isFinite && adjoint.isFinite)
    assertEqualsDouble(forward/1e308,1.0,1e-12)
    assertEqualsDouble(adjoint/1e308,1.0,1e-12)
