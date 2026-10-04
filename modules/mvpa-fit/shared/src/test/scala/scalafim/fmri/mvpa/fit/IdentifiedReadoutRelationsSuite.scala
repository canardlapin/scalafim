package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SpaceRole, ValueId, ValueIdentity}
import scalafim.dataset.RunId
import scalafim.fmri.fit.{LeastSquaresSeparate, LssTrialDesign, ResponseBlock}
import scalafim.fmri.mvpa.{
  AcquisitionCoordinates,
  AxisRef,
  EvidenceOrigins,
  EvidenceSource,
  PreparationSupport,
  ReindexingLeg,
  ValueSupport
}
import resample4s.core.{IndexSpace, Selection}
import scalafim.fmri.mvpa.relation.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class IdentifiedReadoutRelationsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("runwise B = A Y agrees with the dense TrialReadout oracle without eager adapter materialization"):
    val run = readoutRun("run-1")
    val partitions = axis("runs", Vector("run-1"))
    val effects = axis("effects", Vector("a", "b"))
    val neural = axis("voxels", Vector("0", "1"))
    val origins = ReadoutRelationOrigins("acquisition:r1", "response:r1", "readout:lss", "preparation:fixed", "noise:none", ReadoutRelationAccess.OneShot)
    val expected = right(run.explicitPatterns(scalafim.fmri.mvpa.PatternCopyBudget(100000L))).value
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.oneShot(run, origins, resource)))(relations =>
      val actual = right(relations.relations.head.estimate(DMat.eye(neural.size)))
      assertMatrixClose(actual, expected)
      Right(())
    ))

  test("effect and feature metadata are checked before response composition"):
    val run = readoutRun("run-1")
    val partitions = axis("runs", Vector("run-1"))
    val wrongEffects = axis("effects", Vector("a", "other"))
    val neural = axis("voxels", Vector("0", "1"))
    val origins = ReadoutRelationOrigins("acquisition:r1", "response:r1", "readout:lss", "preparation:fixed", "noise:none", ReadoutRelationAccess.OneShot)
    val result = IdentifiedReadoutRelations.withRuns(partitions, wrongEffects, neural, Vector(RunReadoutRelation.oneShot(run, origins, resource)))(_ => Right(()))
    assert(result.isLeft)

  test("explicit temporal support is retained without claiming it from readout receipts"):
    val partitions = axis("runs", Vector("run-1"))
    val effects = axis("effects", Vector("a", "b"))
    val neural = axis("voxels", Vector("0", "1"))
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val id = SourceId.unsafe("fixture-bold")
    val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("fixture-bold-root"), id)))
    val values = ValueIdentity.source(ValueId.unsafe("fixture-bold-v1"))
    val support = right(EvidenceOrigins.make(
      source,
      values,
      AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, values, Vector(0, 1, 2, 3)),
      PreparationSupport.JointlyLearned(ValueSupport.Unknown, ValueSupport.Unknown),
      effects.descriptor
    ))
    val supplied = ReadoutRelationOrigins(
      "acquisition:r1", "response:r1", "readout:lss", "preparation:joint", "noise:none", ReadoutRelationAccess.OneShot, support
    )
    var baseline = 0.0
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.oneShot(readoutRun("run-1"), supplied, resource))): relations =>
      assertEquals(relations.relations.head.origins.support, support)
      baseline = right(relations.relations.head.estimate(DMat.eye(2)))(0, 0)
      Right(())
    )
    val perturbed = right(RunTrialReadout.make(
      RunId("run-1"),
      ResponseBlock.unsafe(DMat.dense(4, 2, Vector(101.0, 10.0, 3.0, 30.0, 5.0, 50.0, 7.0, 70.0))),
      readoutRun("run-1").readout
    ))
    var changed = 0.0
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.oneShot(perturbed, supplied, resource))): relations =>
      changed = right(relations.relations.head.estimate(DMat.eye(2)))(0, 0)
      Right(())
    )
    assert(math.abs(changed - baseline) > 1e-12)

  private def axis(name: String, keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "fixture", "unit", "raw", Vector("fixture:v1")))

  private def resource: ReadoutResource = new ReadoutResource:
    def acquire(): Either[String, Unit] = Right(())
    def close(): Either[String, Unit] = Right(())

  private def readoutRun(name: String): RunTrialReadout =
    val design = Matrix.tabulate(4, 2)((row, column) =>
      if (row < 2 && column == 0) || (row >= 2 && column == 1) then 1.0 else 0.0
    )
    val readout = LeastSquaresSeparate.unsafePrepare(LssTrialDesign.unsafe(design, Vector("a", "b"))).trialReadout.toOption.get
    right(RunTrialReadout.make(RunId(name), ResponseBlock.unsafe(DMat.dense(4, 2, Vector(1.0, 10.0, 3.0, 30.0, 5.0, 50.0, 7.0, 70.0))), readout))

  private def assertMatrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; column <- 0 until actual.cols do
      assertEqualsDouble(actual(row, column), expected(row, column), 1e-12)

  private def origins = ReadoutRelationOrigins("acquisition:r1","response:r1","readout:lss","preparation:fixed","noise:none",ReadoutRelationAccess.OneShot)

  test("explicit scoped replay permits immediate scalar work and expires before escaped use"):
    val partitions = axis("runs", Vector("run-1"))
    val effects = axis("effects", Vector("a", "b"))
    val neural = axis("voxels", Vector("0", "1"))
    val scopedOrigins = ReadoutRelationOrigins("acquisition:r1", "response:r1", "readout:lss", "preparation:fixed", "noise:none", ReadoutRelationAccess.ScopedReplay("fixture", "v1"))
    val metric = Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.primal(neural.evidence), CoordinateEvidence.dual(neural.evidence), ValueIdentity.source(ValueId.unsafe("scoped-metric")), SemanticProvenance.source("fixture")).fold(error => fail(error.toString), identity)
    val experimental = Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.primal(effects.evidence), CoordinateEvidence.dual(effects.evidence), ValueIdentity.source(ValueId.unsafe("scoped-query")), SemanticProvenance.source("fixture")).fold(error => fail(error.toString), identity)
    var escaped: () => Either[scalafim.fmri.mvpa.EvidenceError, scalafim.fmri.mvpa.relation.ScalarRelationStatistic] = () => Left(scalafim.fmri.mvpa.EvidenceError.InvalidSource("unset"))
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.scopedReplay(readoutRun("run-1"), scopedOrigins, resource))): relations =>
      val relation = relations.relations.head
      assert(relation.origins.access.isInstanceOf[RelationAccess.ScopedReplay])
      val query = SecondOrderQuery(RelationPair(relation, relation), query = Some(experimental), metric = Some(metric))
      assert(right(query.scalar).value.isFinite)
      escaped = () => query.scalar
      Right(())
    )
    escaped() match
      case Left(scalafim.fmri.mvpa.EvidenceError.InvalidSource(message)) => assert(message.contains("expired"))
      case other => fail(other.toString)

  test("two scoped native readouts match independent signed RDM and detached RSA oracles"):
    val partitions = axis("oracle-runs", Vector("r1", "r2"))
    val effects = axis("oracle-effects", Vector("a", "b", "c"))
    val neural = axis("oracle-voxels", Vector("0", "1"))
    def run(name: String, sign: Double): RunTrialReadout =
      val design = Matrix.tabulate(6, 3)((row, col) => if row / 2 == col then 1.0 else 0.0)
      val readout = LeastSquaresSeparate.unsafePrepare(LssTrialDesign.unsafe(design, Vector("a", "b", "c"))).trialReadout.toOption.get
      val y = DMat.tabulate(6, 2): (row, col) =>
        sign * Vector(1.0, 2.0, 4.0)(row / 2) * (if col == 0 then 1.0 else 10.0)
      right(RunTrialReadout.make(RunId(name), ResponseBlock.unsafe(y), readout))
    val runs = Vector(run("r1", 1.0), run("r2", -1.0))
    // Independent readout means [1,10], [2,20], [4,40]; identity signed
    // products are negative squared differences / two features.
    val expected = Vector(-50.5, -454.5, -202.0)
    assertMatrixClose(right(runs.head.explicitPatterns(scalafim.fmri.mvpa.PatternCopyBudget(100000L))).value, DMat.dense(3, 2, Vector(1.0, 10.0, 2.0, 20.0, 4.0, 40.0)))
    val metric = right(Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.primal(neural.evidence), CoordinateEvidence.dual(neural.evidence), ValueIdentity.source(ValueId.unsafe("oracle-metric")), SemanticProvenance.source("fixture")))
    val model = right(SquareRelationModel("oracle", Vector("a", "b", "c"), Map(("a", "b") -> 1.0, ("a", "c") -> 2.0, ("b", "c") -> 3.0), ValueIdentity.source(ValueId.unsafe("oracle-model")), "unit", "fixed", "none"))
    var escaped = Vector.empty[() => Unit]
    val detached = right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, runs.map: run =>
      val declared = ReadoutRelationOrigins(run.runId.value, "response", "lss", "fixed", "none", ReadoutRelationAccess.ScopedReplay("matrix-fixture", "v1"))
      RunReadoutRelation.scopedReplay(run, declared, resource)
    ): relations =>
      val pairing = right(RelationRdm.allDistinctOrdered(partitions))
      val rdm = right(RelationRdm.identity(relations, pairing))
      assertEquals(rdm.cells.size, 3)
      rdm.cells.zip(expected).foreach:
        case (RelationRdmCell.Estimated(actual), target) => assertEqualsDouble(actual, target, 1e-10)
        case other => fail(other.toString)
      val request: RelationRdmRequest[partitions.Id, effects.Id, neural.Id, String] = RelationRdmRequest(pairing, metric, MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
      val score = right(RelationConsumers.rsaDirect(relations, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(8, 64, 0), RelationConsumerBudget(16, 1024)))
      assertEquals(score.receipt.retainedGeometry, false)
      val relation = relations.relations.head
      var foreignReads = 0
      val foreignOperator = new gale.linalg.DoubleLinearOperator:
        val rows = 3
        val cols = 2
        def applyTo(input: gale.linalg.DVec, output: gale.linalg.MutableDVec): Unit =
          foreignReads += 1
          throw IllegalStateException("foreign one-shot read")
        override def transposeApplyTo(input: gale.linalg.DVec, output: gale.linalg.MutableDVec): Unit =
          foreignReads += 1
          throw IllegalStateException("foreign one-shot adjoint read")
      val foreign = right(Lin.fromLinearMap(foreignOperator, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe("foreign-one-shot")), SemanticProvenance.source("fixture")))
      val borrowed = Relation(effects, neural, foreign, relation.origins.copy(source = relation.origins.source.copy(responseRevision = "foreign")), relation.estimability)
      assert(borrowed.isLeft, "public constructors cannot transplant a live witness")
      assertEquals(foreignReads, 0)
      val leg = right(ReindexingLeg.bind(effects, right(Selection.from(IArray(0), right(IndexSpace.of(3))))))
      val restricted = relation.restrict(leg)
      escaped = Vector(
        () => { relation.estimate(DMat.eye(2)); () },
        () => { relation.estimate.star(DMat.eye(3)); () },
        () => { restricted.estimate(DMat.eye(2)); () },
        () => { restricted.estimate.star(DMat.eye(1)); () })
      Right(score)
    )
    detached.outcome match
      case RelationRsaOutcome.Defined(value) => assertEqualsDouble(value, -3.0 * math.sqrt(3.0) / 14.0, 1e-12)
      case other => fail(other.toString)
    escaped.foreach(call => assertEquals(intercept[IllegalStateException](call()).getMessage, "relation source scope is closed"))

  test("same scoped declaration mints independent lifetimes and expires before close callbacks"):
    val partitions = axis("runs", Vector("run-1"))
    val effects = axis("effects", Vector("a", "b"))
    val neural = axis("voxels", Vector("0", "1"))
    val declared = ReadoutRelationOrigins("acquisition:r1", "response:r1", "readout:lss", "preparation:fixed", "noise:none", ReadoutRelationAccess.ScopedReplay("fixture", "same-revision"))
    val metric = Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.primal(neural.evidence), CoordinateEvidence.dual(neural.evidence), ValueIdentity.source(ValueId.unsafe("same-metric")), SemanticProvenance.source("fixture")).fold(error => fail(error.toString), identity)
    val experimental = Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.primal(effects.evidence), CoordinateEvidence.dual(effects.evidence), ValueIdentity.source(ValueId.unsafe("same-query")), SemanticProvenance.source("fixture")).fold(error => fail(error.toString), identity)
    var first: () => Either[scalafim.fmri.mvpa.EvidenceError, scalafim.fmri.mvpa.relation.ScalarRelationStatistic] = () => Left(scalafim.fmri.mvpa.EvidenceError.InvalidSource("unset"))
    var closeSawExpired = false
    val closing = new ReadoutResource:
      def acquire(): Either[String, Unit] = Right(())
      def close(): Either[String, Unit] =
        closeSawExpired = first().isLeft
        Right(())
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.scopedReplay(readoutRun("run-1"), declared, closing))): relations =>
      val relation = relations.relations.head
      val query = SecondOrderQuery(RelationPair(relation, relation), query = Some(experimental), metric = Some(metric))
      first = () => query.scalar
      assert(right(query.scalar).value.isFinite)
      Right(())
    )
    assert(closeSawExpired)
    right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, Vector(RunReadoutRelation.scopedReplay(readoutRun("run-1"), declared, resource))): relations =>
      val relation = relations.relations.head
      assert(right(SecondOrderQuery(RelationPair(relation, relation), query = Some(experimental), metric = Some(metric)).scalar).value.isFinite)
      Right(())
    )

  test("escaped direct and restricted forward and adjoint values refuse after closure"):
    val partitions = axis("runs",Vector("run-1"))
    val effects = axis("effects",Vector("a","b"))
    val neural = axis("voxels",Vector("0","1"))
    val input = RunReadoutRelation.oneShot(readoutRun("run-1"),origins,resource)
    var escaped = Vector.empty[() => Unit]
    right(IdentifiedReadoutRelations.withRuns(partitions,effects,neural,Vector(input)): relations =>
      val relation = relations.relations.head
      val leg = right(ReindexingLeg.bind(effects,right(Selection.from(IArray(0),right(IndexSpace.of(2))))))
      val restricted = relation.restrict(leg)
      assertEquals(restricted.origins,relation.origins)
      assertEquals(relation.origins,origins.relationOrigins)
      assertEquals(restricted.estimability,Vector(relation.estimability.head))
      escaped = Vector(
        () => { relation.estimate(DMat.eye(2)); () },
        () => { relation.estimate.star(DMat.eye(2)); () },
        () => { restricted.estimate(DMat.eye(2)); () },
        () => { restricted.estimate.star(DMat.eye(1)); () }
      )
      Right(())
    )
    escaped.foreach(call => assertEquals(intercept[IllegalStateException](call()).getMessage,"relation source scope is closed"))

  test("rejected nested acquisition does not close the outer scope"):
    val partitions = axis("runs",Vector("run-1"))
    val effects = axis("effects",Vector("a","b"))
    val neural = axis("voxels",Vector("0","1"))
    var closed = 0
    val owned = new ReadoutResource:
      def acquire(): Either[String,Unit] = Right(())
      def close(): Either[String,Unit] = { closed += 1; Right(()) }
    val input = RunReadoutRelation.scopedReplay(readoutRun("run-1"), origins.copy(access = ReadoutRelationAccess.ScopedReplay("fixture", "v1")), owned)
    right(IdentifiedReadoutRelations.withRuns(partitions,effects,neural,Vector(input)): relations =>
      assert(IdentifiedReadoutRelations.withRuns(partitions,effects,neural,Vector(input))(_ => Right(())).isLeft)
      assertEquals(closed,0)
      val witness = relationWitness(relations.relations.head.origins.access)
      assert(witness.isActive, "rejected nested acquisition preserves the outer witness")
      assertMatrixClose(right(relations.relations.head.estimate(DMat.eye(2))),right(readoutRun("run-1").explicitPatterns(scalafim.fmri.mvpa.PatternCopyBudget(100000L))).value)
      Right(())
    )
    assertEquals(closed,1)
    assert(!relationWitness(input.liveAccess).isActive)

  test("later acquisition failure closes only earlier owned resources"):
    var closed = Vector.empty[String]
    def owned(name: String, admitted: Boolean) = new ReadoutResource:
      def acquire(): Either[String,Unit] = if admitted then Right(()) else Left("acquire-refused")
      def close(): Either[String,Unit] = { closed :+= name; Right(()) }
    val first = RunReadoutRelation.scopedReplay(readoutRun("r1"), origins.copy(access = ReadoutRelationAccess.ScopedReplay("fixture", "v1")), owned("r1",true))
    val second = RunReadoutRelation.scopedReplay(readoutRun("r2"), origins.copy(access = ReadoutRelationAccess.ScopedReplay("fixture", "v1")), owned("r2",false))
    val result = IdentifiedReadoutRelations.withRuns(axis("runs",Vector("r1","r2")),axis("effects",Vector("a","b")),axis("voxels",Vector("0","1")),Vector(first, second))(_ => Right(()))
    assert(!relationWitness(first.liveAccess).isActive)
    assertEquals(closed,Vector("r1"))
    result match
      case Left(IdentifiedReadoutRelationError.Access("r2","acquire-refused")) => ()
      case other => fail(s"lost acquisition error: $other")

  test("task failure and multiple returned or thrown close failures are preserved"):
    var closed = Vector.empty[String]
    var witnesses = Vector.empty[scalafim.fmri.mvpa.relation.ScopeReplay]
    def owned(name: String, throws: Boolean) = new ReadoutResource:
      def acquire(): Either[String,Unit] = Right(())
      def close(): Either[String,Unit] =
        closed :+= name
        assert(!witnesses(if throws then 0 else 1).isActive, "this witness expires before its close callback")
        if throws then throw IllegalStateException(name) else Left(name)
    val result = IdentifiedReadoutRelations.withRuns(axis("runs",Vector("r1","r2")),axis("effects",Vector("a","b")),axis("voxels",Vector("0","1")),Vector(
      RunReadoutRelation.scopedReplay(readoutRun("r1"), origins.copy(access = ReadoutRelationAccess.ScopedReplay("fixture", "v1")), owned("close-throw",true)),
      RunReadoutRelation.scopedReplay(readoutRun("r2"), origins.copy(access = ReadoutRelationAccess.ScopedReplay("fixture", "v1")), owned("close-left",false))
    )): relations =>
      witnesses = relations.relations.map(relation => relationWitness(relation.origins.access))
      Left(IdentifiedReadoutRelationError.TaskFailure("task-fault"))
    assert(witnesses.forall(!_.isActive))
    assertEquals(closed,Vector("close-throw","close-left"))
    assertEquals(result,Left(IdentifiedReadoutRelationError.TaskAndCloseFailure(IdentifiedReadoutRelationError.TaskFailure("task-fault"),Vector("close-throw","close-left"))))

  test("foreign residual source is rejected before any capability callback"):
    import scalafim.fmri.mvpa.relation.*
    given Estimability[Int] with
      def effectEstimability(value: Int): Vector[EffectEstimability] = throw IllegalStateException("poison capability")
    val result = IdentifiedReadoutRelations.withRuns(axis("runs",Vector("run-1")),axis("effects",Vector("a","b")),axis("voxels",Vector("0","1")),Vector(RunReadoutRelation.oneShot(readoutRun("run-1"),origins,resource))): relations =>
      val relation = relations.relations.head
      val foreign = relation.origins.source.copy(responseRevision="foreign-response")
      assert(relation.bind(foreign,1).isLeft)
      Right(())
    right(result)

  private def relationWitness(access: RelationAccess): ScopeReplay = access match
    case RelationAccess.ScopedReplay(witness) => witness
    case other => fail(s"expected scoped access, got $other")
