package scalafim.fmri.mvpa

import gale.linalg.{DMat, Matrix}
import multivar.core.ValueId
import multivar.family.canonical.{LdaObjective, TraceRidgeFraction, WithinScatterPolicy}
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.predictive.*

final class SoftLdaAnalysisSuite extends munit.FunSuite:
  import SoftLdaAnalysis.given

  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val samples =
    AxisRef
      .create(
        AxisId.unsafe("soft-lda-analysis-samples"),
        AxisPurpose.Samples,
        Vector.tabulate(12)(index => SampleId.unsafe(s"run-${index / 3}-trial-${index % 3}")),
        CoordinateBasis.unsafe("run-trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("soft-lda-analysis", "v1")
      )
      .toOption
      .get
  private val features =
    AxisRef
      .create(
        AxisId.unsafe("soft-lda-analysis-features"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(3)(index => FeatureId.unsafe(s"voxel-$index")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("soft-lda-analysis", "v1")
      )
      .toOption
      .get
  private val classes =
    ClassAxis
      .create(
        AxisId.unsafe("soft-lda-analysis-classes"),
        Vector(ClassId.unsafe("a"), ClassId.unsafe("b"), ClassId.unsafe("c")),
        CoordinateProvenance.unsafe("soft-lda-analysis", "v1")
      )
      .toOption
      .get
  private val classLabels = Vector.tabulate(samples.size)(index => classes.keys(index % 3))
  private val categorical =
    CategoricalTarget(
      samples,
      classes,
      Column(samples, classLabels).toOption.get
    ).toOption.get
  private val hardTarget = ClassMembershipTarget.hard(categorical).toOption.get
  private val patterns = fromRows(
    Seq(
      Seq(2.2, 0.1, 0.2),
      Seq(0.0, 2.0, -0.2),
      Seq(-2.0, -1.8, 0.1),
      Seq(2.0, -0.1, 0.0),
      Seq(0.2, 2.2, 0.1),
      Seq(-2.2, -2.0, -0.1),
      Seq(2.3, 0.2, -0.1),
      Seq(-0.1, 1.9, 0.2),
      Seq(-1.9, -2.1, 0.0),
      Seq(1.9, 0.0, 0.1),
      Seq(0.1, 2.1, -0.1),
      Seq(-2.1, -1.9, 0.2)
    )
  )
  private val evidence =
    EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("soft-lda-analysis-values"))
      .toOption
      .get
  private val design =
    val groups = Labels
      .dense(IArray.unsafeFromArray(Array.tabulate(samples.size)(_ / 3)), samples.size)
      .toOption
      .get
    val ordinal = LeaveOneGroupOut(groups)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 419L)
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
      .identity(features, MeasurementId.unsafe("soft-lda-analysis-global"))
      .toOption
      .get
  private val frame =
    MeasurementFrame(features)(Vector(MeasurementEntry(measurement, NoRendition))).toOption.get
  private val baseConfiguration: SoftLdaConfiguration[samples.Id] = SoftLdaConfiguration(
    WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05))
  ).toOption.get

  test("SoftLDA uses the ordinary typed analysis result and execution receipt"):
    val result = run(hardTarget, baseConfiguration)
    val estimate = success(result)

    assertEquals(estimate.targetKind, MembershipTargetKind.HardIncidence)
    assert(estimate.targetArgmaxAccuracy.value >= 0.9)
    assert(estimate.membershipMse.value >= 0.0)
    assertEquals(estimate.folds.length, 4)
    assert(estimate.folds.forall(_.fit.fit.diagnostics.residual >= 0.0))
    assert(estimate.folds.forall(_.fit.operatorApplications > 0L))
    val receipt = result.values.head.outcome.receipt
    assert(receipt.operatorApplications > 0L)
    assertEquals(receipt.convergence.length, 4)
    assert(receipt.convergence.forall(_.outcome == ConvergenceOutcome.Converged))
    assertEquals(result.receipt.work.materializedCells, 0L)

  test("normalized discriminant weights retain their decision-score semantics"):
    val estimate = success(run(hardTarget, baseConfiguration))

    var row = 0
    while row < samples.size do
      var total = 0.0
      var klass = 0
      while klass < classes.size do
        total += score(estimate, row, klass)
        klass += 1
      assertEqualsDouble(total, 1.0, 1e-10)
      row += 1

  test("held-out membership perturbation cannot alter that fold's scores"):
    val perturbed = Matrix.tabulate(samples.size, classes.size): (row, klass) =>
      if row < 3 then hardTarget.values(row, (klass + 1) % classes.size)
      else hardTarget.values(row, klass)
    val changedTarget = ClassMembershipTarget
      .simplex(samples, classes, perturbed)
      .toOption
      .get
    val baseline = success(run(hardTarget, baseConfiguration))
    val changed = success(run(changedTarget, baseConfiguration))

    var row = 0
    while row < 3 do
      var klass = 0
      while klass < classes.size do
        assertEqualsDouble(
          score(changed, row, klass),
          score(baseline, row, klass),
          1e-9
        )
        klass += 1
      row += 1

  test("trial nuisance is axis-bound, selected inside folds, and identity-bearing"):
    val nuisance = TrialNuisanceTable(
      samples,
      Matrix.tabulate(samples.size, 1)((row, _) => (row / 3).toDouble - 1.5)
    ).toOption.get
    val configuration: SoftLdaConfiguration[samples.Id] = SoftLdaConfiguration(
      WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05)),
      objective = LdaObjective.TraceRatio,
      trialNuisance = Some(nuisance)
    ).toOption.get
    val result = success(run(hardTarget, configuration))

    assert(result.folds.forall(_.fit.trialNuisanceColumns == 1))
    assert(result.folds.forall(_.fit.trainingAxis.size == 9))
    assert(result.folds.forall(_.fit.assessmentAxis.size == 3))
    assert(result.folds.forall(_.fit.fit.diagnostics.objective == LdaObjective.TraceRatio))
    assertNotEquals(configuration.identity, baseConfiguration.identity)

  private def run(
      target: ClassMembershipTarget[samples.Id],
      configuration: SoftLdaConfiguration[samples.Id]
  ) =
    val source = MembershipObservationSource(evidence, target).toOption.get
    val solver = SoftLdaSolverSettings(configuration.objective).toOption.get
    val strategy = ExecutionStrategy(
      BackendId.unsafe("multivar-portable"),
      ExecutionRepresentation.Operator,
      NumericPrecision.Binary64,
      SoftLdaSolverSettings.choice(solver),
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected,
      Vector("kernel" -> "soft-lda")
    ).toOption.get
    Mvpa
      .run(source)(
        design,
        frame,
        SoftLdaValidation(configuration),
        strategy
      )
      .fold(error => fail(error.message), identity)

  private def success(
      result: AnalysisResult[
        SoftLdaValidationEstimate[samples.Id],
        SoftLdaBindRejection,
        SoftLdaCompileError,
        NoRendition.type
      ]
  ): SoftLdaValidationEstimate[samples.Id] =
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected SoftLDA success, obtained $other")

  private def score(
      value: SoftLdaValidationEstimate[samples.Id],
      row: Int,
      klass: Int
  ): Double =
    value.predictions.scores.get.scoreAt(row, klass).toOption.get.value

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))
