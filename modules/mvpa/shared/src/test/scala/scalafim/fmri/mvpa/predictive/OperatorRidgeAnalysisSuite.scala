package scalafim.fmri.mvpa.predictive

import gale.linalg.{DMat, Matrix}
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*

final class OperatorRidgeAnalysisSuite extends munit.FunSuite:
  import OperatorRidgeAnalysis.given

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("ridge-analysis-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(12)(index => SampleId.unsafe(s"run-${index / 3}-trial-${index % 3}")),
        CoordinateBasis.unsafe("run-trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("ridge-analysis-fixture", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("ridge-analysis-features"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(4)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("ridge-analysis-fixture", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("ridge-analysis-classes"),
        Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
        CoordinateProvenance.unsafe("ridge-analysis-fixture", "v1")
      )
      .toOption
      .get
  private val labels = Vector.tabulate(samples.size)(index => classes.keys(index % 3))
  private val categorical =
    CategoricalTarget(samples, classes, Column(samples, labels).toOption.get).toOption.get
  private val hardTarget = ClassMembershipTarget.hard(categorical).toOption.get
  private val values = fromRows(
    Vector(
      Vector(2.0, 0.8, 0.2, 1.0),
      Vector(-1.8, -0.9, 0.4, -0.5),
      Vector(0.1, 2.1, -1.0, 0.3),
      Vector(2.2, 1.1, 0.0, 0.8),
      Vector(-2.1, -1.2, 0.1, -0.7),
      Vector(-0.2, 1.8, -1.2, 0.5),
      Vector(1.9, 0.9, 0.3, 1.2),
      Vector(-1.9, -1.0, 0.2, -0.4),
      Vector(0.2, 2.2, -0.8, 0.4),
      Vector(2.1, 1.0, 0.1, 0.9),
      Vector(-2.2, -0.8, 0.3, -0.6),
      Vector(0.0, 1.9, -1.1, 0.2)
    )
  )
  private val evidence =
    EvidenceTable
      .dense(samples, features, values, ValueId.unsafe("ridge-analysis-values"))
      .toOption
      .get
  private val design =
    val labels = Labels
      .dense(IArray.unsafeFromArray(Array.tabulate(samples.size)(_ / 3)), samples.size)
      .toOption
      .get
    val ordinal = LeaveOneGroupOut(labels)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 311L)
    val compiled = ordinal
      .compile(IndexSpace.of(samples.size).toOption.get, authority.seed)
      .toOption
      .get
    val schedule = BoundSchedule(
      compiled,
      samples,
      AxisPopulationFingerprint.fromAxis(samples).toOption.get,
      ScheduleLabels.fromDesign(samples, ordinal).toOption.get,
      authority
    ).toOption.get
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(ScientificAxisName.unsafe("samples"), samples.identity)
    ).toOption.get
  private val measurement =
    Measurement
      .identity(features, MeasurementId.unsafe("ridge-analysis-global"))
      .toOption
      .get
  private val frame =
    MeasurementFrame(features)(Vector(MeasurementEntry(measurement, NoRendition))).toOption.get
  private val configuration = OperatorRidgeConfiguration.fixed(0.7).toOption.get
  private val solver = OperatorRidgeSolverSettings.unsafe(1e-12, 2000)
  private val strategy =
    ExecutionStrategy(
      BackendId.unsafe("gale-portable"),
      ExecutionRepresentation.Operator,
      NumericPrecision.Binary64,
      OperatorRidgeSolverSettings.choice(solver),
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("kernel" -> "operator-ridge")
    ).toOption.get

  test("typed operator-ridge estimand runs through bind, plan, execute, and receipt"):
    val result = run(hardTarget)
    val estimate = success(result)

    assertEquals(estimate.targetKind, MembershipTargetKind.HardIncidence)
    assertEquals(estimate.scoreKind, ClassificationScoreKind.UncalibratedDecision)
    assert(estimate.targetArgmaxAccuracy.value >= 0.9)
    assert(estimate.membershipMse.value >= 0.0)
    assertEquals(estimate.folds.length, 4)
    assertEquals(estimate.configuration.penalty, configuration.penalty)
    estimate.folds.foreach: fold =>
      assertEquals(
        fold.fit.trainingAxis,
        fold.analysisSamples
      )
      assertEquals(fold.fit.classAxis, classes.identity)
      assert(fold.fit.classFits.forall(_.iterations > 0))
    val receipt = result.values.head.outcome.receipt
    assert(receipt.operatorApplications > 0L)
    assertEquals(receipt.convergence.length, classes.size * 4)
    assert(receipt.convergence.forall(_.outcome == ConvergenceOutcome.Converged))
    assertEquals(result.receipt.work.materializedCells, 0L)
    assertEquals(result.receipt.solver, OperatorRidgeSolverSettings.choice(solver))

  test("soft target masses remain training targets rather than prediction outputs"):
    val softened = Matrix.tabulate(samples.size, classes.size): (row, klass) =>
      0.7 * hardTarget.values(row, klass) + 0.1
    val target = ClassMembershipTarget
      .simplex(samples, classes, softened)
      .toOption
      .get
    val hard = success(run(hardTarget))
    val soft = success(run(target))

    assertEquals(soft.targetKind, MembershipTargetKind.SoftSimplex)
    var row = 0
    while row < samples.size do
      var klass = 0
      while klass < classes.size do
        assertEqualsDouble(
          score(soft, row, klass),
          0.7 * score(hard, row, klass) + 0.1,
          1e-7
        )
        klass += 1
      row += 1
    assertEquals(
      soft.predictions.predicted.toVector,
      hard.predictions.predicted.toVector
    )

  test("held-out target perturbation cannot alter that fold's fitted scores"):
    val perturbed = Matrix.tabulate(samples.size, classes.size): (row, klass) =>
      if row < 3 then hardTarget.values(row, (klass + 1) % classes.size)
      else hardTarget.values(row, klass)
    val target = ClassMembershipTarget
      .simplex(samples, classes, perturbed)
      .toOption
      .get
    val baseline = success(run(hardTarget))
    val changed = success(run(target))

    var row = 0
    while row < 3 do
      var klass = 0
      while klass < classes.size do
        assertEqualsDouble(
          score(changed, row, klass),
          score(baseline, row, klass),
          1e-10
        )
        klass += 1
      row += 1

  private def run(target: ClassMembershipTarget[samples.Id]) =
    val source = MembershipObservationSource(evidence, target).toOption.get
    Mvpa
      .run(source)(
        design,
        frame,
        source.operatorRidge(configuration),
        strategy
      )
      .fold(error => fail(error.message), identity)

  private def success(
      result: AnalysisResult[
        OperatorRidgeValidationEstimate[samples.Id],
        OperatorRidgeBindRejection,
        OperatorRidgeCompileError,
        NoRendition.type
      ]
  ): OperatorRidgeValidationEstimate[samples.Id] =
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected operator-ridge success, obtained $other")

  private def score(
      value: OperatorRidgeValidationEstimate[samples.Id],
      row: Int,
      klass: Int
  ): Double =
    value.predictions.scores.get.scoreAt(row, klass).toOption.get.value

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))
