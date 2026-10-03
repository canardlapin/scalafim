package scalafim.fmri.mvpa.fit

import multivar.family.canonical.{ResidualRegularization, TraceRidgeFraction}

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.Matrix
import scalafim.dataset.RunId
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegments, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.fit.{
  DesignMatrix,
  PreparedContrastGeometry,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank,
  TemporalPreparationScope,
  TrainingRunScope
}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}

class CanonicalEffectMvpaSuite extends munit.FunSuite:

  test("runwise streaming moments agree with batch products and explicit dense projectors"):
    val geometry = iidGeometry()
    val response = responseBlock(runResponses.head)
    val positions = Array(3, 0, 2)
    val streamed = CanonicalMoments.accumulate(response, geometry, positions).toOption.get
    val selected = selectColumns(response.value, positions)
    val design = geometry.preparedDesign.value
    val cross = selected.t * design
    val effectScore = cross * geometry.effectBasis
    val expectedEffect = effectScore * effectScore.t
    val expectedResidual = subtract(selected.t * selected, cross * geometry.inverseXtX * cross.t)
    val xProjector = design * geometry.inverseXtX * design.t
    val effectProjector = design * geometry.effectBasis * geometry.effectBasis.t * design.t
    val explicitEffect = selected.t * effectProjector * selected
    val explicitResidual = selected.t * subtract(DMat.eye(design.rows), xProjector) * selected

    assertMatrixClose(streamed.total, selected.t * selected, 1e-11)
    assertMatrixClose(streamed.responseDesign, cross, 1e-11)
    assertMatrixClose(streamed.effect, expectedEffect, 1e-11)
    assertMatrixClose(streamed.residual, expectedResidual, 1e-11)
    assertMatrixClose(streamed.effect, explicitEffect, 1e-11)
    assertMatrixClose(streamed.residual, explicitResidual, 1e-11)

  private val ridge = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
  private val budget = NativeCanonicalFixtures.budget
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("global canonical fits retain the neural domain and differ from fold assessments"):
    val packed = native(runResponses)
    val assessment = right(CanonicalGlobal.assess(packed.source, ridge, budget))
    val fit = right(CanonicalGlobal.fit(packed.source, NativeCanonicalFixtures.right(packed.source.selectRuns(Vector(RunId("run-0"), RunId("run-1"), RunId("run-2")))), ridge, budget))
    val sameNeural: multivar.core.SpaceEvidence[packed.N] = fit.neural
    assertEquals(sameNeural.descriptor, packed.source.neural.descriptor)
    assertEquals(fit.neuralAxis, packed.source.neuralAxis)
    assertEquals(fit.receipt.trainingRuns, Vector(RunId("run-0"), RunId("run-1"), RunId("run-2")))
    assertEquals(assessment.folds.length, 3)
    assert(assessment.meanHeldOutRoot >= 0.0)
    assert(assessment.folds.forall(_.receipt.temporalPreparation.length == 3))
    assertEquals(fit.fit.programFit.program.objective.label, "generalized-rayleigh")
    assert(fit.receipt.momentIdentity.matches("[0-9a-f]{64}"))

  test("held-out perturbations cannot alter the frozen training frame or training identity"):
    val original = native(runResponses)
    val changed = native(runResponses.updated(0, addHeldOutTaskSignal(runResponses.head, 25.0)))
    val before = right(CanonicalGlobal.assess(original.source, ridge, budget)).folds.head
    val after = right(CanonicalGlobal.assess(changed.source, ridge, budget)).folds.head
    assertMatrixClose(right(before.training.fit.functionalFrame.weights.toDense), right(after.training.fit.functionalFrame.weights.toDense), 0.0)
    assertEquals(before.training.receipt.momentIdentity, after.training.receipt.momentIdentity)
    assertEquals(before.training.receipt.evidenceIdentity, after.training.receipt.evidenceIdentity)
    assertNotEquals(before.heldOutRoot, after.heldOutRoot)

  test("response-learned temporal geometry resolves the exact training-run scope"):
    val geometries = trainingFoldGeometries()
    val complete = right(CanonicalGeometrySchedule.trainingFolds(geometries))
    val incomplete = right(CanonicalGeometrySchedule.trainingFolds(geometries.take(1)))
    val packed = native(runResponses, complete)
    val assessed = right(CanonicalGlobal.assess(packed.source, ridge, budget))
    assessed.folds.zipWithIndex.foreach: (fold, held) =>
      val expected = (0 until 3).filter(_ != held).toVector
      assert(fold.receipt.temporalPreparation.forall: (_, receipt) =>
        receipt.scope match
          case TemporalPreparationScope.TrainingFold(scope) => scope.runs.map(_.value) == expected
          case _ => false
      )
    val absent = poison(incomplete)
    CanonicalGlobal.assess(absent._1, ridge, budget) match
      case Left(CanonicalArtifactError.Temporal(_: OneShotMvpaError.MissingFoldGeometry)) => ()
      case other => fail(s"expected missing training scope, got $other")
    assertEquals(absent._2(), 0)

  test("dense capacity and strict unknown costs reject before provider access"):
    val p = poison(right(CanonicalGeometrySchedule.stable(iidGeometry())))
    val denied = budget.copy(memory = scalafim.fmri.mvpa.analysis.ResourceLimit.OwnedNumeric(0L))
    assert(CanonicalGlobal.assess(p._1, ridge, denied).isLeft)
    val strict = budget.copy(memory = scalafim.fmri.mvpa.analysis.ResourceLimit.WholeNumeric(1000000L))
    assert(CanonicalGlobal.assess(p._1, ridge, strict).isLeft)
    assertEquals(p._2(), 0)
    assert(p._1.selectRuns(Vector(RunId("run-0"), RunId("run-0"))).isLeft)
    assertEquals(p._2(), 0)

  test("canonical input domains require exact run ordering and unique run identities"):
    val n = native(runResponses)
    val partitions = right(scalafim.fmri.mvpa.AxisRef.fromStableKeys("bad-partitions", multivar.core.SpaceRole.Samples,
      Vector("run-1", "run-0", "run-2"), "run", "partition", "native"))
    assert(CanonicalRunSet.make(partitions)(n.source.runs)(RunId.apply).isLeft)

  test("one real run supports a global fit while assessment requires held-out contributors"):
    val n = native(runResponses)
    val partitions = right(scalafim.fmri.mvpa.AxisRef.fromStableKeys("one-canonical-run", multivar.core.SpaceRole.Samples,
      Vector("run-0"), "run", "partition", "native"))
    val one = right(CanonicalRunSet.make(partitions)(n.source.runs.take(1))(RunId.apply))
    val selection = right(one.selectRuns(Vector(RunId("run-0"))))
    val fit = right(CanonicalGlobal.fit(one, selection, ridge, budget))
    assertEquals(fit.receipt.trainingRuns, Vector(RunId("run-0")))
    assert(CanonicalGlobal.assess(one, ridge, budget).isLeft)

  test("run selections cannot cross nominal partition domains"):
    val errors = scala.compiletime.testing.typeCheckErrors("""
      import multivar.core.*
      import scalafim.fmri.mvpa.fit.*
      def wrong[P <: SemanticSpace, Other <: SemanticSpace](selection: CanonicalTrainingRuns[P]): CanonicalTrainingRuns[Other] = selection
    """)
    assert(errors.nonEmpty)

  test("failed MANOVA training does not acquire or read its held-out provider"):
    import multivar.core.{SpaceRole, ValueId, ValueIdentity}
    import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
    import scalafim.fmri.mvpa.analysis.*
    import scalafim.response.{Provenance, ProvenanceId, SourceId}
    val design = DMat.tabulate(8, 2)((r, c) => if c == 0 then 1.0 else if r % 2 == 0 then -1.0 else 1.0)
    val geometry = right(ResponsePreparationPlan.fromConfig(FitConfig()).prepareManova(DesignMatrix.unsafe(design), Vector("intercept", "task"),
      scalafim.fmri.fit.FContrast("task", Vector(Map("task" -> 1.0))), SelectedTimepointIndices.unsafe((0 until 8).toVector),
      Vector(RunPartition(0, (0 until 8).toVector, (0 until 8).toVector)), TemporalNuisanceRank.unsafe(1)))
    val schedule = right(ManovaGeometrySchedule.stable(geometry))
    val neural = right(AxisRef.fromStableKeys("manova-order-neural", SpaceRole.Observed, Vector("v0", "v1"), "native", "psc", "raw"))
    val partitions = right(AxisRef.fromStableKeys("manova-order-runs", SpaceRole.Samples, Vector("run-0", "run-1", "run-2"), "run", "partition", "native"))
    var heldAccesses = 0
    val runs = Vector.tabulate(3): index =>
      val time = right(AxisRef.fromStableKeys(s"manova-order-time-$index", SpaceRole.Samples, Vector.tabulate(8)(_.toString), "time", "TR", "native"))
      val id = SourceId.unsafe(s"manova-order-$index")
      val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"manova-order-root-$index"), id)))
      val values = if index == 0 then
        val poison = new gale.linalg.DoubleLinearOperator:
          val rows = 8
          val cols = 2
          def applyTo(in: gale.linalg.DVec, out: gale.linalg.MutableDVec): Unit =
            heldAccesses += 1
            throw IllegalStateException("held-out read before fit")
        right(Observations.fromOperator(time, neural, poison, ValueIdentity.source(ValueId.unsafe("manova-held-poison")), source))
      else right(Observations.fromDense(time, neural, design, ValueIdentity.source(ValueId.unsafe(s"manova-perfect-training-$index")), source))
      val resource = new ObservationProductResource:
        def acquire() =
          if index == 0 then heldAccesses += 1
          Right(())
        def close() = Right(())
      right(CanonicalRunEvidence.fromObservations(RunId(s"run-$index"), time, neural, values, schedule,
        ObservationReplay.Scoped("manova-order", "v1"), resource, ObservationProviderCosts()))
    val set = right(CanonicalRunSet.make(partitions)(runs)(RunId.apply))
    assert(CanonicalGlobal.assessManova(set, ResidualRegularization.Unregularized, budget).isLeft)
    assertEquals(heldAccesses, 0)

  private def native(rows: Vector[Vector[Vector[Double]]], schedule: CanonicalGeometrySchedule = right(CanonicalGeometrySchedule.stable(iidGeometry()))) =
    NativeCanonicalFixtures.contrast(rows.map(fromRows), Vector.fill(rows.length)(schedule))

  private def poison(schedule: CanonicalGeometrySchedule): (CanonicalRunSet[? <: multivar.core.SemanticSpace, ? <: multivar.core.SemanticSpace, CanonicalGeometrySchedule], () => Int) =
    import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
    import scalafim.fmri.mvpa.analysis.*
    import scalafim.response.{Provenance, ProvenanceId, SourceId}
    import multivar.core.{SpaceRole, ValueId, ValueIdentity}
    var reads = 0
    val neural = right(AxisRef.fromStableKeys("canonical-poison-neural", SpaceRole.Observed, Vector("v0", "v1"), "native", "psc", "raw"))
    val partitions = right(AxisRef.fromStableKeys("canonical-poison-runs", SpaceRole.Samples, Vector("run-0", "run-1", "run-2"), "run", "partition", "native"))
    val runs = Vector.tabulate(3): index =>
      val time = right(AxisRef.fromStableKeys(s"canonical-poison-time-$index", SpaceRole.Samples, Vector.tabulate(8)(_.toString), "time", "TR", "native"))
      val operator = new gale.linalg.DoubleLinearOperator:
        val rows = 8
        val cols = 2
        def applyTo(in: gale.linalg.DVec, out: gale.linalg.MutableDVec): Unit =
          reads += 1
          throw IllegalStateException("poison evidence")
      val id = SourceId.unsafe(s"canonical-poison-$index")
      val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"canonical-poison-root-$index"), id)))
      val values = right(Observations.fromOperator(time, neural, operator, ValueIdentity.source(ValueId.unsafe(s"canonical-poison-value-$index")), source))
      val resource = new ObservationProductResource:
        def acquire() =
          reads += 1
          Right(())
        def close() = Right(())
      right(CanonicalRunEvidence.fromObservations(RunId(s"run-$index"), time, neural, values, schedule,
        ObservationReplay.Scoped("poison", "v1"), resource, ObservationProviderCosts()))
    right(CanonicalRunSet.make(partitions)(runs)(RunId.apply)) -> (() => reads)

  private def iidGeometry(): PreparedContrastGeometry =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = design,
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> 1.0)),
        selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
        partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get

  private def trainingFoldGeometries(): Vector[PreparedContrastGeometry] =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    val partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector))
    val options = ArOptions(structure = ArStructure.Ar(1), global = true)
    val segments = TimeSegments.continuous(design.timepoints)
    val whitening = WhiteningPlan.global(
      ArmaCoefficients(Vector(0.2)),
      segments,
      exactFirstAr1 = true,
      method = WhiteningMethod.Estimated
    )
    Vector(Vector(1, 2), Vector(0, 2), Vector(0, 1)).map: training =>
      val scope = TrainingRunScope.fromInts(training).toOption.get
      ResponsePreparationPlan
        .fromConfig(FitConfig(autocorrelation = options))
        .prepareContrast(
          design = design,
          columnNames = Vector("intercept", "task", "drift"),
          contrast = TContrast("task", Map("task" -> 1.0)),
          selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
          partitions = partitions,
          nuisanceRank = TemporalNuisanceRank.unsafe(2),
          scope = TemporalPreparationScope.TrainingFold(scope),
          whitening = scalafim.fmri.fit.CanonicalTemporalWhitening.Shared(whitening)
        )
        .toOption
        .get

  private def addHeldOutTaskSignal(rows: Vector[Vector[Double]], amount: Double): Vector[Vector[Double]] =
    rows.zip(designRows).map: (response, design) =>
      response.updated(0, response.head + amount * design(1))

  private val designRows = Vector(
    Vector(1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -0.7),
    Vector(1.0, 1.0, -0.4),
    Vector(1.0, 1.0, -0.1),
    Vector(1.0, -1.0, 0.1),
    Vector(1.0, -1.0, 0.4),
    Vector(1.0, 1.0, 0.7),
    Vector(1.0, 1.0, 1.0)
  )

  private val baseResponse = Vector(
    Vector(-0.9, 0.1, -0.15, -0.2),
    Vector(-1.14, 0.5, -0.09, -0.5),
    Vector(0.91, -0.85, 0.21, 0.5),
    Vector(0.67, -0.55, 0.47, 0.2),
    Vector(-1.02, 0.75, -0.47, -0.1),
    Vector(-0.56, 0.65, -0.56, -0.4),
    Vector(0.99, -0.2, 0.49, 0.6),
    Vector(1.05, -0.4, 0.1, 0.3)
  )

  private val runResponses: Vector[Vector[Vector[Double]]] =
    Vector.tabulate(3): run =>
      baseResponse.zipWithIndex.map: (row, time) =>
        row.zipWithIndex.map: (value, feature) =>
          value + 0.03 * run.toDouble * ((time + feature) % 3 - 1).toDouble

  private def responseBlock(rows: Vector[Vector[Double]]): ResponseBlock =
    ResponseBlock.unsafe(fromRows(rows))

  private def selectColumns(matrix: DMat, positions: Array[Int]): DMat =
    val out = Matrix.newBuilder(matrix.rows, positions.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < positions.length do
        out(row, col) = matrix(row, positions(col))
        col += 1
      row += 1
    out.result()

  private def subtract(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
