package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ComponentCount
import multivar.core.OperatorRepresentation
import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection
import resample4s.core.Selection
import scalafim.fmri.mvpa.GlobalDecomposition.given

final class GlobalDecompositionSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("pca-samples"),
      AxisPurpose.Samples,
      Vector("s1", "s2", "s3", "s4").map(SampleId.unsafe),
      CoordinateBasis.unsafe("observation-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("pca-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("pca-neural"),
      AxisPurpose.NeuralFeatures,
      Vector("x1", "x2", "constant").map(FeatureId.unsafe),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("pca-suite", "v1")
    )
  )

  private val matrix = GaleTestMatrix.fromRows(
    Seq(
      Seq(-3.0, 1.0, 5.0),
      Seq(-1.0, -1.0, 5.0),
      Seq(1.0, -1.0, 5.0),
      Seq(3.0, 1.0, 5.0)
    )
  )

  private def observations(
      operatorBacked: Boolean,
      probe: Probe = Probe(),
      valueId: String = "pca-observations"
  ): Observations[samples.Id, neural.Id, FeatureId] =
    val evidence =
      if operatorBacked then
        right(
          EvidenceTable.operator(
            samples,
            neural,
            CountingOperator(matrix, probe),
            ValueId.unsafe(valueId)
          )
        )
      else
        right(
          EvidenceTable.dense(
            samples,
            neural,
            matrix,
            ValueId.unsafe(valueId)
          )
        )
    right(Observations(evidence))

  private val fitDesign = right(ObservationFitDesign.entireTable(samples))

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(1, 0)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("x2-then-x1"),
        injection
      )
    )

  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )

  private def strategy(policy: MaterializationPolicy) =
    right(
      ExecutionStrategy(
        BackendId.unsafe("multivar-portable"),
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.Selected(GlobalDecomposition.PcaSolver),
        Vector.empty,
        Scheduling.serial,
        policy,
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      )
    )

  private def onlySuccess(
      result: AnalysisResult[
        MeasuredPca[samples.Id],
        PcaBindRejection,
        GlobalDecompositionError,
        NoRendition.type
      ]
  ): MeasuredPca[samples.Id] =
    result.values match
      case Vector(MeasurementValue(_, _, MeasurementOutcome.Success(value, _))) => value
      case other => fail(s"expected one PCA success, obtained $other")

  test("observation-table PCA matches the independent base-R oracle"):
    val source = observations(operatorBacked = false)
    val result = right(
      Mvpa.run(source)(
        fitDesign,
        frame,
        GlobalDecomposition.pca(
          source,
          right(ComponentCount(2)),
          PcaPreparation.Centered
        ),
        strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)))
      )
    )
    val fit = onlySuccess(result)

    // Generated independently by tools/reference/mvpa-pca-oracle.R.
    assertEqualsDouble(fit.singularValues(0), math.sqrt(20.0), 1e-10, "first singular value")
    assertEqualsDouble(fit.singularValues(1), 2.0, 1e-10, "second singular value")
    assertEqualsDouble(math.abs(fit.loadings(0, 0)), 0.0, 1e-10, "x2 on PC1")
    assertEqualsDouble(math.abs(fit.loadings(1, 0)), 1.0, 1e-10, "x1 on PC1")
    assertEqualsDouble(math.abs(fit.loadings(0, 1)), 1.0, 1e-10, "x2 on PC2")
    assertEqualsDouble(math.abs(fit.loadings(1, 1)), 0.0, 1e-10, "x1 on PC2")
    assertEquals(fit.sourceSamples.identity, samples.identity)
    assert(fit.sourceSamples.evidence eq samples.evidence)
    assertEquals(fit.trainingRows, fitDesign.selection.child.identity)
    assertEquals(fit.local, measurement.local.identity)
    assertEquals(fit.components.keys.map(_.value), Vector(1, 2))
    assertEquals(fit.computation.preparation, PcaPreparation.Centered)
    assertEquals(fit.computation.materialization.elements, 8L)
    assertEquals(fit.computation.solver, GlobalDecomposition.PcaSolver)
    assertEquals(result.receipt.work.materializedCells, 8L)
    assertEquals(result.receipt.work.operatorApplications, 2L)

  test("direct PCA views reuse one exact Multivar fit without reopening evidence"):
    val probe = Probe()
    val source = observations(operatorBacked = true, probe = probe)
    val fit = onlySuccess(
      right(
        Mvpa.run(source)(
          fitDesign,
          frame,
          GlobalDecomposition.pca(source, right(ComponentCount(2))),
          strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)))
        )
      )
    )
    val applicationsAfterFit = probe.forwardCalls
    val loadings = fit.loadings
    val scores = fit.trainingScores
    val roots = fit.singularValues

    assertEquals(applicationsAfterFit, 2)
    assertEquals(probe.forwardCalls, applicationsAfterFit)
    assertMatrix(loadings, fit.multivar.result.v)
    assertMatrix(scores, fit.multivar.transform.trainingValues)
    assertVector(roots, fit.multivar.result.singularValues)
    assertEquals(
      fit.computation.materialization.sourceRepresentation,
      OperatorRepresentation.MatrixFree
    )

  test("fit scope, preparation, and component request force distinct plan and fit identities"):
    val source = observations(operatorBacked = false)
    val subsetSelection = right(
      ReindexingLeg.selection(
        samples,
        right(
          Selection.from(
            IArray.unsafeFromArray(Array(0, 1, 2)),
            right(IndexSpace.of(samples.size))
          )
        )
      )
    )
    val subset = right(ObservationFitDesign.trainingSubset(samples, subsetSelection))
    val centered = GlobalDecomposition.pca(
      source,
      right(ComponentCount(2)),
      PcaPreparation.Centered
    )
    val standardized = GlobalDecomposition.pca(
      source,
      right(ComponentCount(2)),
      PcaPreparation.Standardized
    )
    val oneComponent = GlobalDecomposition.pca(
      source,
      right(ComponentCount(1)),
      PcaPreparation.Centered
    )
    val fullSpec = right(Mvpa.specify(source)(fitDesign, frame, centered))
    val subsetSpec = right(Mvpa.specify(source)(subset, frame, centered))
    val standardizedSpec = right(Mvpa.specify(source)(fitDesign, frame, standardized))
    val oneComponentSpec = right(Mvpa.specify(source)(fitDesign, frame, oneComponent))

    assertNotEquals(fullSpec.identity, subsetSpec.identity)
    assertNotEquals(fullSpec.identity, standardizedSpec.identity)
    assertNotEquals(fullSpec.identity, oneComponentSpec.identity)
    assertEquals(fitDesign.scope, ObservationFitScope.EntireTable)
    assertEquals(subset.scope, ObservationFitScope.DeclaredTrainingSubset)

    val fullFit = onlySuccess(
      right(
        Mvpa.run(source)(
          fitDesign,
          frame,
          centered,
          strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)))
        )
      )
    )
    val subsetFit = onlySuccess(
      right(
        Mvpa.run(source)(
          subset,
          frame,
          centered,
          strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(6L)))
        )
      )
    )
    assertNotEquals(fullFit.computation.identity, subsetFit.computation.identity)

  test("foreign open objects and hidden or over-budget materialization fail closed"):
    val source = observations(operatorBacked = false)
    val foreign = observations(
      operatorBacked = false,
      valueId = "different-observation-revision"
    )
    val estimand = GlobalDecomposition.pca(source, right(ComponentCount(2)))

    Mvpa.run(foreign)(
      fitDesign,
      frame,
      estimand,
      strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(8L)))
    ) match
      case Left(
            MvpaRunError.Binding(
              BindError.EstimandRejected(
                _,
                PcaBindRejection.BoundaryMismatch(_, _),
                _
              )
            )
          ) =>
        ()
      case other => fail(s"expected foreign-boundary rejection, obtained $other")

    Mvpa.run(source)(
      fitDesign,
      frame,
      estimand,
      strategy(MaterializationPolicy.Reject)
    ) match
      case Left(MvpaRunError.Planning(ExecutionPlanError.MaterializationRequired(_))) => ()
      case other => fail(s"expected materialization planning rejection, obtained $other")

    val overBudget = right(
      Mvpa.run(source)(
        fitDesign,
        frame,
        estimand,
        strategy(MaterializationPolicy.Allow(MaterializationBudget.unsafe(7L)))
      )
    )
    overBudget.values.head.outcome match
      case MeasurementOutcome.Failed(
            GlobalDecompositionError.Evidence(
              EvidenceTableError.MaterializationBudgetExceeded(required, budget)
            ),
            _
          ) =>
        assertEquals(required, 8L)
        assertEquals(budget.maxElements, 7L)
      case other => fail(s"expected local budget failure, obtained $other")

  private def assertMatrix(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-12, s"($row,$column)")
        column += 1
      row += 1

  private def assertVector(actual: DVec, expected: DVec): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      assertEqualsDouble(actual(index), expected(index), 1e-12, s"index $index")
      index += 1

  private final class Probe:
    var forwardCalls: Int = 0

  private object Probe:
    def apply(): Probe = new Probe

  private final class CountingOperator(matrix: DMat, probe: Probe) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      probe.forwardCalls += 1
      var row = 0
      while row < rows do
        var sum = 0.0
        var column = 0
        while column < cols do
          sum += matrix(row, column) * input(column)
          column += 1
        output(row) = sum
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      var column = 0
      while column < cols do
        var sum = 0.0
        var row = 0
        while row < rows do
          sum += matrix(row, column) * input(row)
          row += 1
        output(column) = sum
        column += 1

  private object CountingOperator:
    def apply(matrix: DMat, probe: Probe): CountingOperator =
      new CountingOperator(matrix, probe)
