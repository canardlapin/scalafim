package downstream.mvpa

import alder.kernel.{AuditValue, BackendFingerprint, ComponentId as AlderComponentId}
import alder.tune.PositiveInt
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.predictive.*

final class PublicConstructionSuite extends munit.FunSuite:
  test("an outside-package consumer constructs core scientific values through total factories"):
    val sample = right(SampleId("trial-1"))
    val feature = right(FeatureId("voxel-1"))
    val basis = right(CoordinateBasis("declared-order"))
    val provenance = right(CoordinateProvenance("downstream-suite", "v1"))
    val samples = right(
      AxisRef.create(
        right(AxisId("downstream-samples")),
        AxisPurpose.Samples,
        Vector(sample),
        basis,
        None,
        AxisScale.nominal,
        provenance
      )
    )
    val features = right(
      AxisRef.create(
        right(AxisId("downstream-features")),
        AxisPurpose.NeuralFeatures,
        Vector(feature),
        basis,
        Some(right(AxisUnits("arbitrary-signal"))),
        AxisScale.nominal,
        provenance
      )
    )
    val measurement = right(
      Measurement.identity(
        features,
        right(MeasurementId("all-features"))
      )
    )
    val source = right(
      ScientificSourceIdentity(
        right(ScientificSourceKind("downstream-observations")),
        Vector(
          ScientificSourceAxis(right(ScientificAxisName("samples")), samples.identity),
          ScientificSourceAxis(right(ScientificAxisName("neural")), features.identity)
        )
      )
    )
    val design = right(DesignIdentity(right(DesignKind("downstream-validation"))))
    val estimand = right(EstimandIdentity(right(EstimandKind("downstream-estimand"))))

    assertEquals(measurement.source.identity, features.identity)
    assertEquals(source.axes.length, 2)
    assertEquals(design.kind.value, "downstream-validation")
    assertEquals(estimand.kind.value, "downstream-estimand")

  test("an outside-package consumer constructs execution and predictive values through total factories"):
    val backend = right(BackendId("portable"))
    val solverId = right(SolverId("downstream-solver"))
    val solver = right(SolverIdentity(solverId))
    val stream = RandomStream(right(RandomStreamId("folds")), 42L)
    val scheduling = right(
      Scheduling.parallel(
        parallelism = 2,
        chunkSize = 1,
        completionOrder = CompletionOrder.MeasurementOrder
      )
    )
    val strategy = right(
      ExecutionStrategy(
        backend,
        ExecutionRepresentation.Operator,
        NumericPrecision.Binary64,
        SolverChoice.Selected(solver),
        Vector(stream),
        scheduling,
        MaterializationPolicy.Allow(right(MaterializationBudget(1024L))),
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      )
    )
    val classId = right(ClassId("face"))
    val score = right(DecisionScore(0.25))
    val penalty = right(FeatureModelPenalty(1e-6))
    val ridge = right(OperatorRidgeSolverSettings(tolerance = 1e-8, maxIterations = 100))
    val shrinkage = right(ScalingShrinkage(0.25))
    val classifierRidge = right(ClassifierRidge(0.01))
    val learnerId = right(CategoricalLearnerId("downstream-mean"))
    val learner = right(
      CategoricalLearnerDefinition(
        learnerId,
        AlderComponentId("downstream.mvpa.mean"),
        BackendFingerprint(
          "downstream-construction-court",
          "mean-v1",
          AuditValue.record()
        ),
        PositiveInt.one,
        "minimum-features:1",
        "negative-squared-distance",
        "none"
      )
    )
    val generalization = right(CrossDomainGeneralizationAxis("subject"))
    val component = right(ComponentId(1))
    val ridgePenalty = right(RidgePenalty(1e-4))
    val ridgeTolerance = right(OperatorRidgeTolerance(1e-8))
    val ridgeIterations = right(OperatorRidgeIterationLimit(100))
    val precisionTolerance = right(PrecisionTolerance(1e-10))
    val residualTolerance = right(ResidualMomentTolerance(1e-10))
    val residualDf = right(ResidualDegreesOfFreedom(12.0))
    val normalizationVariance = right(EffectNormalizationVariance(0.5))
    val signedStatistic = right(SignedCrossRunRayleigh(-0.25))

    assertEquals(strategy.backend, backend)
    assertEquals(classId.value, "face")
    assertEqualsDouble(score.value, 0.25, 0.0)
    assertEqualsDouble(penalty.value, 1e-6, 0.0)
    assertEquals(ridge.maxIterations.value, 100)
    assertEqualsDouble(shrinkage.toDouble, 0.25, 0.0)
    assertEqualsDouble(classifierRidge.toDouble, 0.01, 0.0)
    assertEquals(learner.id, learnerId)
    assertEquals(generalization.value, "subject")
    assertEquals(component.value, 1)
    assertEqualsDouble(ridgePenalty.value, 1e-4, 0.0)
    assertEqualsDouble(ridgeTolerance.value, 1e-8, 0.0)
    assertEquals(ridgeIterations.value, 100)
    assertEqualsDouble(precisionTolerance.toDouble, 1e-10, 0.0)
    assertEqualsDouble(residualTolerance.toDouble, 1e-10, 0.0)
    assertEqualsDouble(residualDf.toDouble, 12.0, 0.0)
    assertEqualsDouble(normalizationVariance.toDouble, 0.5, 0.0)
    assertEqualsDouble(signedStatistic.toDouble, -0.25, 0.0)

  test("hostile invalid values remain data in the public error channels"):
    val invalidValues = Vector(
      AxisId("").isLeft,
      AxisPurpose.named("Samples").isLeft,
      AxisKey(" ").isLeft,
      AxisUnits("\n").isLeft,
      AxisDescriptorField("bad field", "value").isLeft,
      CoordinateBasis("", Vector.empty).isLeft,
      CoordinateProvenance("", "v1").isLeft,
      SampleId("").isLeft,
      FeatureId(" ").isLeft,
      ScientificAxisName("Samples").isLeft,
      ScientificSourceKind("Bad Kind").isLeft,
      DesignKind("Validation").isLeft,
      NormalizationStepId("").isLeft,
      OutputBoundaryId(" ").isLeft,
      EstimandKind("RSA").isLeft,
      MeasurementId("bad id").isLeft,
      MeasurementFingerprint("not-a-fingerprint").isLeft,
      BackendId("Portable").isLeft,
      SolverId("").isLeft,
      RandomStreamId("bad id").isLeft,
      MaterializationBudget(0L).isLeft,
      Scheduling.parallel(0, 1, CompletionOrder.MeasurementOrder).isLeft,
      PartitionId("").isLeft,
      IndependenceDeclarationId("Pair Assumption").isLeft,
      PairingEdge(right(PartitionId("run-1")), right(PartitionId("run-2")), Double.NaN).isLeft,
      PairCoordinateId("").isLeft,
      SecondOrderModelName("Model Name").isLeft,
      FeatureModelPenalty(0.0).isLeft,
      OperatorRidgeSolverSettings(tolerance = Double.NaN, maxIterations = 0).isLeft,
      RidgePenalty(0.0).isLeft,
      OperatorRidgeTolerance(Double.NaN).isLeft,
      OperatorRidgeIterationLimit(0).isLeft,
      PrecisionTolerance(0.0).isLeft,
      ResidualMomentTolerance(Double.NaN).isLeft,
      ResidualDegreesOfFreedom(0.0).isLeft,
      EffectNormalizationVariance(Double.NegativeInfinity).isLeft,
      SignedCrossRunRayleigh(Double.PositiveInfinity).isLeft,
      ComponentId(0).isLeft,
      ClassId("").isLeft,
      DecisionScore(Double.PositiveInfinity).isLeft,
      ScalingShrinkage(Double.NaN).isLeft,
      ScalingShrinkage(-0.1).isLeft,
      ScalingShrinkage(1.1).isLeft,
      ClassifierRidge(0.0).isLeft,
      ClassifierRidge(Double.PositiveInfinity).isLeft,
      CategoricalLearnerId("Bad Learner").isLeft,
      CategoricalLearnerDefinition(
        right(CategoricalLearnerId("downstream-mean")),
        AlderComponentId("downstream.mvpa.mean"),
        BackendFingerprint(
          "downstream-construction-court",
          "mean-v1",
          AuditValue.record()
        ),
        PositiveInt.one,
        " invalid-parameter-field",
        "negative-squared-distance",
        "none"
      ).isLeft,
      CrossDomainGeneralizationAxis("Subject Axis").isLeft
    )

    assert(invalidValues.forall(identity))

  test("unsafe core construction is not callable from an outside package"):
    val axisErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val id = AxisId.unsafe("forged-axis")
      val key = AxisKey.unsafe("forged-key")
      val basis = CoordinateBasis.unsafe("forged-basis")
    """)
    val scientificErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val axis = ScientificAxisName.unsafe("samples")
      val kind = EstimandKind.unsafe("forged-estimand")
      val measurement = MeasurementId.unsafe("forged-measurement")
    """)
    val executionErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val backend = BackendId.unsafe("portable")
      val budget = MaterializationBudget.unsafe(1L)
      val settings = OperatorRidgeSolverSettings.unsafe()
      val component = ComponentId.unsafe(1)
    """)
    val trustedIdentityErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val estimand = EstimandIdentity.trusted(EstimandKind("forged").toOption.get)
      val boundary = OutputBoundaryIdentity.trusted(OutputBoundaryId("forged").toOption.get)
      val boundaries = RequestedBoundaries.trusted(Vector(boundary))
      val solver = SolverIdentity.trusted(SolverId("forged").toOption.get)
    """)

    assert(axisErrors.nonEmpty, clue(axisErrors))
    assert(axisErrors.contains("unsafe"), clue(axisErrors))
    assert(scientificErrors.nonEmpty, clue(scientificErrors))
    assert(scientificErrors.contains("unsafe"), clue(scientificErrors))
    assert(executionErrors.nonEmpty, clue(executionErrors))
    assert(executionErrors.contains("unsafe"), clue(executionErrors))
    assert(trustedIdentityErrors.nonEmpty, clue(trustedIdentityErrors))
    assert(trustedIdentityErrors.contains("trusted"), clue(trustedIdentityErrors))

  test("partial kernel receipts cannot be forged outside the executor boundary"):
    val ridgeReceiptErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val forged = OperatorRidgeKernelClassReceipt(???, -1, Double.NaN)
    """)
    val softLdaReceiptErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val forged = SoftLdaKernelFitReceipt(
        "",
        ???,
        ???,
        ???,
        ???,
        -1,
        -1,
        -1,
        ???
      )
    """)

    assert(ridgeReceiptErrors.nonEmpty, clue(ridgeReceiptErrors))
    assert(
      ridgeReceiptErrors.contains("OperatorRidgeKernelClassReceipt"),
      clue(ridgeReceiptErrors)
    )
    assert(softLdaReceiptErrors.nonEmpty, clue(softLdaReceiptErrors))
    assert(
      softLdaReceiptErrors.contains("SoftLdaKernelFitReceipt"),
      clue(softLdaReceiptErrors)
    )

  test("unsafe predictive and relational identifiers are not callable outside"):
    val predictiveErrors = compileErrors("""
      import scalafim.fmri.mvpa.predictive.*

      val label = ClassId.unsafe("face")
      val shrinkage = ScalingShrinkage.unsafe(0.25)
      val ridge = ClassifierRidge.unsafe(0.01)
      val learner = CategoricalLearnerId.unsafe("forged-learner")
      val definition = CategoricalLearnerDefinition.unsafe
      val axis = CrossDomainGeneralizationAxis.unsafe("subject")
    """)
    val relationalErrors = compileErrors("""
      import scalafim.fmri.mvpa.*

      val partition = PartitionId.unsafe("run-1")
      val pair = PairCoordinateId.unsafe("a::b")
      val model = SecondOrderModelName.unsafe("model")
      val precision = PrecisionTolerance.unsafe(1e-10)
      val residualTolerance = ResidualMomentTolerance.unsafe(1e-10)
      val residualDf = ResidualDegreesOfFreedom.unsafe(12.0)
      val variance = EffectNormalizationVariance.unsafe(0.5)
      val statistic = SignedCrossRunRayleigh.unsafe(0.0)
      val penalty = RidgePenalty.unsafe(1e-4)
      val solverTolerance = OperatorRidgeTolerance.unsafe(1e-8)
      val solverIterations = OperatorRidgeIterationLimit.unsafe(100)
    """)

    assert(predictiveErrors.nonEmpty, clue(predictiveErrors))
    assert(predictiveErrors.contains("unsafe"), clue(predictiveErrors))
    assert(relationalErrors.nonEmpty, clue(relationalErrors))
    assert(relationalErrors.contains("unsafe"), clue(relationalErrors))

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"unexpected construction failure: $error")
