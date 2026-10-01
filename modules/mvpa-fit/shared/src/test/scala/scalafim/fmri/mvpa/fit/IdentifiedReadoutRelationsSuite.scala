package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
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
import scalafim.fmri.mvpa.relation.RelationAccess
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class IdentifiedReadoutRelationsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("runwise B = A Y agrees with the dense TrialReadout oracle without eager adapter materialization"):
    val run = readoutRun("run-1")
    val partitions = axis("runs", Vector("run-1"))
    val effects = axis("effects", Vector("a", "b"))
    val neural = axis("voxels", Vector("0", "1"))
    val origins = ReadoutRelationOrigins("acquisition:r1", "response:r1", "readout:lss", "preparation:fixed", "noise:none", ReadoutRelationAccess.OneShot)
    val expected = right(run.explicitPatterns).value
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
    val input = RunReadoutRelation.oneShot(readoutRun("run-1"),origins,owned)
    right(IdentifiedReadoutRelations.withRuns(partitions,effects,neural,Vector(input)): relations =>
      assert(IdentifiedReadoutRelations.withRuns(partitions,effects,neural,Vector(input))(_ => Right(())).isLeft)
      assertEquals(closed,0)
      assertMatrixClose(right(relations.relations.head.estimate(DMat.eye(2))),right(readoutRun("run-1").explicitPatterns).value)
      Right(())
    )
    assertEquals(closed,1)

  test("later acquisition failure closes only earlier owned resources"):
    var closed = Vector.empty[String]
    def owned(name: String, admitted: Boolean) = new ReadoutResource:
      def acquire(): Either[String,Unit] = if admitted then Right(()) else Left("acquire-refused")
      def close(): Either[String,Unit] = { closed :+= name; Right(()) }
    val result = IdentifiedReadoutRelations.withRuns(axis("runs",Vector("r1","r2")),axis("effects",Vector("a","b")),axis("voxels",Vector("0","1")),Vector(
      RunReadoutRelation.oneShot(readoutRun("r1"),origins,owned("r1",true)),
      RunReadoutRelation.oneShot(readoutRun("r2"),origins,owned("r2",false))
    ))(_ => Right(()))
    assertEquals(closed,Vector("r1"))
    result match
      case Left(IdentifiedReadoutRelationError.Access("r2","acquire-refused")) => ()
      case other => fail(s"lost acquisition error: $other")

  test("task failure and multiple returned or thrown close failures are preserved"):
    var closed = Vector.empty[String]
    def owned(name: String, throws: Boolean) = new ReadoutResource:
      def acquire(): Either[String,Unit] = Right(())
      def close(): Either[String,Unit] =
        closed :+= name
        if throws then throw IllegalStateException(name) else Left(name)
    val result = IdentifiedReadoutRelations.withRuns(axis("runs",Vector("r1","r2")),axis("effects",Vector("a","b")),axis("voxels",Vector("0","1")),Vector(
      RunReadoutRelation.oneShot(readoutRun("r1"),origins,owned("close-throw",true)),
      RunReadoutRelation.oneShot(readoutRun("r2"),origins,owned("close-left",false))
    ))(_ => Left(IdentifiedReadoutRelationError.TaskFailure("task-fault")))
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
