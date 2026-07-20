package scalafim.connectivity

import scalafim.linalg.DoubleMatrix

class PlansWorkflowSuite extends munit.FunSuite:

  test("filter and preprocessing specs validate inspectable plan structure") {
    val highPass = FilterSpec.highPassDct(128.0).toOption.get
    val bandPass = FilterSpec.bandPass(0.01, 0.08).toOption.get
    val whitening = Prewhitening.ar(2).toOption.get
    val delay = DelayAlignment.from(3, allowNegativeLags = true).toOption.get
    val plan = PreprocessPlan.from(Vector(highPass, bandPass), whitening, Some(delay), useFrameWeights = true).toOption.get

    assert(plan.requiresSamplePeriod)
    assert(plan.description.contains("high-pass-dct"))
    assert(FilterSpec.lowPass(0.0).isLeft)
    assert(FilterSpec.bandStop(0.1, 0.01).isLeft)
    assert(Prewhitening.ar(0).isLeft)
    assert(Prewhitening.external("bad method").isLeft)
    assert(DelayAlignment.from(-1).isLeft)
  }

  test("estimator specs declare topology output requirements and diagnostics") {
    val weighted = EstimatorSpec.weightedCorrelation()
    val shrinkage = EstimatorSpec.diagonalShrinkageCorrelation()
    val weightedShrinkage = EstimatorSpec.diagonalShrinkageCorrelation(useFrameWeights = true)
    val lagged = EstimatorSpec.laggedCorrelation(2).toOption.get
    val dynamic = EstimatorSpec.slidingWindowCorrelation

    assertEquals(weighted.output, EstimatorOutput.Static(EdgeTopology.Undirected))
    assert(!weighted.requirements.contains(EstimatorRequirement.FrameWeights))
    assert(!shrinkage.requirements.contains(EstimatorRequirement.FrameWeights))
    assert(weightedShrinkage.requirements.contains(EstimatorRequirement.FrameWeights))
    assertEquals(shrinkage.name, "diagonal-shrinkage-correlation")
    assertEquals(shrinkage.measure.scale, ConnectivityValueScale.CorrelationR)
    assert(lagged.output == EstimatorOutput.Static(EdgeTopology.Directed))
    assert(lagged.requiresSamplePeriod)
    assert(dynamic.output.isDynamic)
    assert(EstimatorSpec.laggedCorrelation(0).isLeft)
    assert(EstimatorSpec.external("bad name", EstimatorOutput.Static(EdgeTopology.Undirected), ConnectivityMeasure.correlation, Set.empty, Vector.empty).isLeft)
    assert(EstimatorSpec.external("rect-dyn", EstimatorOutput.Dynamic(EdgeTopology.Rectangular), ConnectivityMeasure.distance, Set.empty, Vector.empty).isLeft)
  }

  test("static and dynamic workflow bones reject illegal estimator mixes") {
    val plan = PreprocessPlan.from(Vector(FilterSpec.highPass(0.01).toOption.get), useFrameWeights = true).toOption.get
    val staticEstimator = EstimatorSpec.diagonalShrinkageCorrelation()
    val dynamicEstimator = EstimatorSpec.slidingWindowCorrelation
    val threshold = PostprocessStep.thresholdAbsolute(0.25).toOption.get

    assert(StaticConnectivityWorkflow.from(plan, staticEstimator, postprocess = Vector(threshold, PostprocessStep.FisherZ)).isRight)
    assert(StaticConnectivityWorkflow.from(plan, dynamicEstimator).isLeft)
    val dynamicWorkflow = DynamicConnectivityWorkflow.from(plan, dynamicEstimator, windowLength = 20, windowStep = 5).toOption.get

    assertEquals(dynamicWorkflow.window, WindowSpec.unsafe(20, 5))
    assertEquals(dynamicWorkflow.windowAxis(TimeAxis.unsafeSeconds(50, 1.0)).toOption.map(_.size), Some(7))
    assert(DynamicConnectivityWorkflow.from(plan, staticEstimator, windowLength = 20, windowStep = 5).isLeft)
    assert(DynamicConnectivityWorkflow.from(plan, dynamicEstimator, windowLength = 0, windowStep = 5).isLeft)
    assert(PostprocessStep.thresholdAbsolute(-0.1).isLeft)
    assert(PostprocessStep.thresholdAbsolute(Double.NaN).isLeft)
  }

  test("workflow construction checks estimator requirements against declared input policies") {
    val missingWeights = PreprocessPlan.empty
    val optionalWeights = PreprocessPlan.from(frameWeightPolicy = Some(InputPolicy.Optional)).toOption.get
    val withWeights = PreprocessPlan.from(frameWeightPolicy = Some(InputPolicy.Required)).toOption.get
    val estimator = EstimatorSpec.diagonalShrinkageCorrelation(useFrameWeights = true)
    val phaseEstimator =
      EstimatorSpec.external(
        "phase-estimator",
        EstimatorOutput.Static(EdgeTopology.Undirected),
        ConnectivityMeasure.correlation,
        Set(EstimatorRequirement.PhaseSeries),
        Vector.empty
      ).toOption.get

    assert(StaticConnectivityWorkflow.from(missingWeights, estimator).swap.toOption.exists(_.message.contains("frame-weights")))
    assert(StaticConnectivityWorkflow.from(optionalWeights, estimator).swap.toOption.exists(_.message.contains("frame-weights")))
    assert(StaticConnectivityWorkflow.from(PreprocessPlan.empty, phaseEstimator).swap.toOption.exists(_.message.contains("phase-series")))
    val workflow = StaticConnectivityWorkflow.from(withWeights, estimator).toOption.get
    assert(workflow.validateRun(runWith()).swap.toOption.exists(_.message.contains("required frame weights")))
    assert(workflow.validateRun(runWith(frameWeights = true)).isRight)
  }

  test("postprocess chain is typed by connectivity measure scale") {
    val correlation = EstimatorSpec.diagonalShrinkageCorrelation()
    val workflow = StaticConnectivityWorkflow.from(PreprocessPlan.empty, correlation, Vector(PostprocessStep.FisherZ, PostprocessStep.FisherR)).toOption.get
    val distanceEstimator =
      EstimatorSpec.external(
        "distance-estimator",
        EstimatorOutput.Static(EdgeTopology.Rectangular),
        ConnectivityMeasure.distance,
        Set.empty,
        Vector.empty
      ).toOption.get

    assertEquals(workflow.outputMeasure.scale, ConnectivityValueScale.CorrelationR)
    assert(StaticConnectivityWorkflow.from(PreprocessPlan.empty, distanceEstimator, Vector(PostprocessStep.FisherZ)).isLeft)
  }

  test("external measures are value-extensible through validated descriptors") {
    val scale = ConnectivityValueScale.external("wavelet-coherence").toOption.get
    val measure = ConnectivityMeasure.external("wavelet-coherence", scale, symmetric = false).toOption.get
    val estimator =
      EstimatorSpec.external(
        "wavelet-coherence",
        EstimatorOutput.Static(EdgeTopology.Directed),
        measure,
        Set(EstimatorRequirement.SamplePeriod),
        Vector(DiagnosticKind.NumericalWarnings)
      ).toOption.get

    assertEquals(estimator.measure.scale.label, "wavelet-coherence")
    assert(ConnectivityValueScale.external("bad scale").isLeft)
  }

  test("diagnostics and receipts have deterministic descriptions") {
    val plan = PreprocessPlan.from(Vector(FilterSpec.highPass(0.01).toOption.get), useFrameWeights = true).toOption.get
    val convergence = ConvergenceReport(converged = true, iterations = 4, tolerance = 1e-8, objective = Some(0.25))
    val diagnostics = ConnectivityDiagnostics(
      warnings = Vector(NumericalWarning.ClippedCorrelation(2, 1.01)),
      convergence = Some(convergence),
      metrics = Map("zeta" -> 2.0, "alpha" -> 1.0)
    )
    val preprocessReceipt = PreprocessReceipt(plan, inputSamples = 100, outputSamples = 96, notes = Vector("censored", "filtered"))
    val estimator = EstimatorSpec.weightedCorrelation()
    val receipt = EstimatorReceipt(estimator, estimator.output, "undirected:scala-native:4x4:6", diagnostics)

    assert(preprocessReceipt.description.contains("notes=[censored,filtered]"))
    assert(diagnostics.description.contains("metrics=[alpha=1.0,zeta=2.0]"))
    assert(receipt.description.contains("weighted-correlation"))
  }

  private def runWith(frameWeights: Boolean = false, nuisance: Boolean = false): RunTimeSeries =
    val axis = NodeAxis.generated(2).toOption.get
    val time = TimeAxis.unsafeSeconds(4, 1.0)
    val values = DoubleMatrix.fromRows(Vector(
      Vector(1.0, 2.0),
      Vector(2.0, 3.0),
      Vector(3.0, 4.0),
      Vector(4.0, 5.0)
    ))
    val series = ParcelTimeSeries.from(values, axis, time).toOption.get
    val weights =
      if frameWeights then Some(FrameWeights.from(Vector(1.0, 1.0, 1.0, 1.0), time).toOption.get) else None
    val nuisanceMatrix =
      if nuisance then Some(NuisanceMatrix.from(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0), Vector(0.0))), time, Vector("intercept")).toOption.get)
      else None
    RunTimeSeries(RunId.unsafe("run-1"), series, weights, nuisanceMatrix)
