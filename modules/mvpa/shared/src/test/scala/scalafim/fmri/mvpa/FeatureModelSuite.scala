package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import multivar.core.ValueId
import resample4s.core.*
import resample4s.designs.KFold
import scalafim.fmri.mvpa.FeatureModel.given

final class FeatureModelSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def bound[
      A,
      Cov <: Coverage,
      S <: SemanticSpace,
      FoldUnit
  ](
      design: Design[A, Cov],
      samples: AxisRef.Aux[SampleId, S],
      authority: SeedAuthority
  )(using
      binder: ScheduleUnitBinder.Aux[A, SampleId, S, FoldUnit]
  ): BoundSchedule[A, Cov, SampleId, S, FoldUnit] =
    val space = right(IndexSpace.of(samples.size))
    val compiled = right(design.compile(space, authority.seed))
    right(
      BoundSchedule(
        compiled,
        samples,
        right(AxisPopulationFingerprint.fromAxis(samples)),
        right(ScheduleLabels.fromDesign(samples, design)),
        authority
      )
    )

  private val samples = right(
    AxisRef.create(
      AxisId.unsafe("feature-model-samples"),
      AxisPurpose.Samples,
      Vector.tabulate(6)(position => SampleId.unsafe(s"trial-${position + 1}")),
      CoordinateBasis.unsafe("declared-trial-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("feature-model-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("feature-model-neural"),
      AxisPurpose.NeuralFeatures,
      Vector("voxel-left", "voxel-right", "voxel-mid").map(FeatureId.unsafe),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("feature-model-suite", "v1")
    )
  )

  private val features = right(
    AxisRef.create(
      AxisId.unsafe("feature-model-covariates"),
      AxisPurpose.Covariates,
      Vector(AxisKey.unsafe("semantic"), AxisKey.unsafe("visual")),
      CoordinateBasis.unsafe("declared-feature-order"),
      None,
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("feature-model-suite", "v1")
    )
  )

  private val featureRows = Vector(
    Vector(0.0, 1.0),
    Vector(1.0, 0.0),
    Vector(0.0, 2.0),
    Vector(2.0, 0.0),
    Vector(1.0, 3.0),
    Vector(3.0, 1.0)
  )

  private val patternRows = featureRows.map: row =>
    val semantic = row(0)
    val visual = row(1)
    Vector(
      1.0 + 2.0 * semantic + 0.5 * visual,
      -2.0 - semantic + 3.0 * visual,
      0.5 + semantic - visual
    )

  private val observations = right(
    Observations(
      right(
        EvidenceTable.dense(
          samples,
          neural,
          GaleTestMatrix.fromRows(patternRows),
          ValueId.unsafe("feature-model-patterns")
        )
      )
    )
  )

  private val modelFeatures = right(
    EvidenceTable.dense(
      samples,
      features,
      GaleTestMatrix.fromRows(featureRows),
      ValueId.unsafe("feature-model-features")
    )
  )

  private val source = right(FeatureModelSource(observations, modelFeatures))

  private val schedule = bound(
    KFold.ordered(3),
    samples,
    SeedAuthority.fromLong(SeedDomain.CrossFit, 8201L)
  )
  private val validation = right(
    ValidationDesign(
      schedule,
      ScientificAxisName.unsafe("samples"),
      GeneralizationAxis(ScientificAxisName.unsafe("samples"), samples.identity)
    )
  )
  private val crossFit = right(CrossFitDesign.targetBlind(validation))

  private val measurement = right(
    Measurement.identity(
      neural,
      MeasurementId.unsafe("feature-model-all-neural")
    )
  )

  private val frame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(measurement, NoRendition)))
  )

  private def strategy(materialization: MaterializationPolicy) = right(
    ExecutionStrategy(
      BackendId.unsafe("feature-model-portable"),
      ExecutionRepresentation.Dense,
      NumericPrecision.Binary64,
      SolverChoice.Selected(FeatureModel.RidgeSolver),
      Vector.empty,
      Scheduling.serial,
      materialization,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private type FeatureAnalysis = AnalysisResult[
    MeasuredFeatureModel[samples.Id],
    FeatureModelBindRejection,
    FeatureModelError,
    NoRendition.type
  ]

  private def estimate(direction: FeatureModelDirection): FeatureAnalysis =
    val penalty = FeatureModelPenalty.unsafe(1e-10)
    val estimand = direction match
      case FeatureModelDirection.Encoding => source.encode(penalty)
      case FeatureModelDirection.Decoding => source.decode(penalty)
    right(
      Mvpa.run(source)(
        crossFit,
        frame,
        estimand,
        strategy(
          MaterializationPolicy.Allow(MaterializationBudget.unsafe(100L))
        )
      )
    )

  private def onlySuccess(result: FeatureAnalysis): MeasuredFeatureModel[samples.Id] =
    val value = result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected feature-model success, obtained $other")
    value

  test("encoding carries exact sample and measured-neural target axes"):
    val analysis = estimate(FeatureModelDirection.Encoding)
    val result = onlySuccess(analysis)

    assertEquals(result.samples.identity, samples.identity)
    assert(result.samples.evidence eq samples.evidence)
    assertEquals(result.target, measurement.local.identity)
    assertEquals(result.predicted.rows, samples.size)
    assertEquals(result.predicted.cols, measurement.local.size)
    assertEquals(result.observed.rows, samples.size)
    assert(result.metrics.targetCorrelation.exists(_ > 0.999999))
    assert(result.metrics.rdmCorrelation.exists(_ > 0.999999))
    assert(result.metrics.meanSquaredError < 1e-8)
    assert(result.metrics.rSquared.exists(_ > 0.999999))
    assertEquals(analysis.values.head.measurement, measurement.identity)
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "direction").map(_.value),
      Some(FeatureModelDirection.Encoding.label)
    )
    assertEquals(
      analysis.plan.design.fields.find(_.name == "preparation-scope").map(_.value),
      Some(PreparationScope.TargetBlindAnalysis.label)
    )
    assertEquals(
      analysis.plan.design.fields.find(_.name == "fitting-scope").map(_.value),
      Some(FittingScope.OuterAnalysis.label)
    )
    assertEquals(result.computation.folds.length, 3)
    assertEquals(
      result.computation.folds.flatMap(_.assessmentSamples.orderedKeys).toSet,
      samples.identity.orderedKeys.toSet
    )
    assertEquals(analysis.values.head.outcome.receipt.materializations.length, 2)
    assertEquals(analysis.receipt.work.materializedCells, 30L)
    assertEquals(analysis.receipt.work.operatorApplications, 5L)

  test("decoding carries the declared covariate target axis"):
    val analysis = estimate(FeatureModelDirection.Decoding)
    val result = onlySuccess(analysis)

    assertEquals(result.target, features.identity)
    assertEquals(result.predicted.rows, samples.size)
    assertEquals(result.predicted.cols, features.size)
    assertEquals(result.observed.cols, features.size)
    assert(result.metrics.targetCorrelation.exists(_ > 0.999999))
    assert(result.metrics.meanSquaredError < 1e-8)
    assertEquals(
      analysis.plan.estimand.fields.find(_.name == "direction").map(_.value),
      Some(FeatureModelDirection.Decoding.label)
    )
    assertNotEquals(
      source.encode(FeatureModelPenalty.unsafe(1e-10)).identity,
      source.decode(FeatureModelPenalty.unsafe(1e-10)).identity
    )

  test("feature modelling refuses hidden materialization during planning"):
    val result = Mvpa.run(source)(
      crossFit,
      frame,
      source.encode(FeatureModelPenalty.unsafe(1e-6)),
      strategy(MaterializationPolicy.Reject)
    )

    assert(result.left.exists:
      case MvpaRunError.Planning(ExecutionPlanError.MaterializationRequired(reason)) =>
        reason.contains("feature modelling")
      case _ => false)

  test("feature source requires an explicitly declared covariate axis"):
    val wrongPurpose = right(
      AxisRef.create(
        AxisId.unsafe("feature-model-wrong-purpose"),
        AxisPurpose.NeuralFeatures,
        Vector(AxisKey.unsafe("semantic"), AxisKey.unsafe("visual")),
        CoordinateBasis.unsafe("declared-feature-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("feature-model-suite", "v1")
      )
    )
    val wrongTable = right(
      EvidenceTable.dense(
        samples,
        wrongPurpose,
        GaleTestMatrix.fromRows(featureRows),
        ValueId.unsafe("feature-model-wrong-purpose-values")
      )
    )

    assert(FeatureModelSource(observations, wrongTable).left.exists:
      case FeatureModelSourceError.InvalidFeaturePurpose(actual) =>
        actual == AxisPurpose.NeuralFeatures
      case _ => false)
