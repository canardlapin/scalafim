package scalafim.fmri.mvpa

import gale.linalg.LinearOperator
import multivar.core.ValueId
import multivar.core.ValueIdentity
import multivar.family.canonical.ResidualRegularization
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.CanonicalAnalysis.given

final class CanonicalAnalysisSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("canonical-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("x"), FeatureId.unsafe("y")),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("canonical-analysis-suite", "v1")
    )
  )

  private val scalarEffects = right(
    AxisRef.create(
      AxisId.unsafe("canonical-scalar-effect"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("contrast")),
      CoordinateBasis.unsafe("normalized-contrast"),
      None,
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("canonical-analysis-suite", "v1")
    )
  )

  private val manovaEffects = right(
    AxisRef.create(
      AxisId.unsafe("canonical-manova-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("h1"), AxisKey.unsafe("h2")),
      CoordinateBasis.unsafe("orthonormal-hypothesis"),
      None,
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("canonical-analysis-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("canonical-runs"),
      AxisPurpose.Partitions,
      Vector(
        PartitionId.unsafe("run-1"),
        PartitionId.unsafe("run-2"),
        PartitionId.unsafe("run-3")
      ),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("canonical-analysis-suite", "v1")
    )
  )

  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val validation = right(
    RelationValidationDesign.leaveOnePartitionOut(
      partitionAxis,
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity)
    )
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("canonical-fit-samples"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2"), SampleId.unsafe("t3")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("canonical-analysis-suite", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(DesignKind.unsafe("canonical-relation-fit"))
  )

  private val selectedX =
    val injection = right(
      Injection.from(
        IArray(0),
        right(IndexSpace.of(neural.size))
      )
    )
    right(Measurement.hardSelection(neural, MeasurementId.unsafe("x-only"), injection))

  private val scalarFrame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(selectedX, NoRendition)))
  )

  private val identityMeasurement = right(
    Measurement.identity(neural, MeasurementId.unsafe("whole-space"))
  )

  private val identityFrame = right(
    MeasurementFrame(neural)(Vector(MeasurementEntry(identityMeasurement, NoRendition)))
  )

  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("canonical-portable"),
      ExecutionRepresentation.SufficientStatistics,
      NumericPrecision.Binary64,
      SolverChoice.NotApplicable,
      Vector.empty,
      Scheduling.serial,
      MaterializationPolicy.Reject,
      FallbackPolicy.forbidden,
      ResultDelivery.Collected
    )
  )

  private val scalarRows = Vector(
    Vector(2.0, 0.5),
    Vector(1.0, -0.25),
    Vector(3.0, 0.75)
  )

  private val scalarVariances = Vector(4.0, 1.0, 9.0)

  private def scalarSource(
      operator: Boolean,
      zero: Boolean = false,
      rows: Vector[Vector[Double]] = scalarRows,
      revision: String = "ordinary"
  ) =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val row = if zero then Vector(0.0, 0.0) else rows(position)
      val estimateMatrix = GaleTestMatrix.fromRows(Vector(row))
      val residualMatrix = GaleTestMatrix.fromRows(
        Vector(Vector(1.0, 0.0), Vector(0.0, 1.0))
      )
      val estimate =
        if operator then
          right(
            EvidenceTable.operator(
              scalarEffects,
              neural,
              LinearOperator.tabulate(1, 2)(estimateMatrix.apply),
              ValueId.unsafe(s"canonical-$revision-operator-estimate-${partition.value}")
            )
          )
        else
          right(
            EvidenceTable.dense(
              scalarEffects,
              neural,
              estimateMatrix,
              ValueId.unsafe(s"canonical-$revision-dense-estimate-${partition.value}")
            )
          )
      val residual =
        if operator then
          right(
            EvidenceTable.operator(
              neural,
              neural,
              LinearOperator.tabulate(2, 2)(residualMatrix.apply),
              ValueId.unsafe(s"canonical-$revision-operator-residual-${partition.value}")
            )
          )
        else
          right(
            EvidenceTable.dense(
              neural,
              neural,
              residualMatrix,
              ValueId.unsafe(s"canonical-$revision-dense-residual-${partition.value}")
            )
          )
      val certifiedResidual = right(CertifiedResidualMoments(residual))
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"canonical-$revision-response-${partition.value}")),
          relationDesign,
          right(Estimability(scalarEffects, Vector(true))),
          NormalizationIdentity.none,
          training
        )
      )
      val capabilities = right(
        ScalarEffectFitCapabilities(
          certifiedResidual,
          ResidualDegreesOfFreedom.unsafe(12.0),
          EffectNormalizationVariance.unsafe(scalarVariances(position))
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, capabilities))
      )
    right(PartitionedRelations(partitionAxis, scalarEffects, neural, entries))

  private def manovaSource =
    val matrices = Vector(
      Vector(Vector(2.0, 0.2), Vector(0.1, 1.5)),
      Vector(Vector(1.8, 0.3), Vector(0.2, 1.2)),
      Vector(Vector(2.1, 0.1), Vector(0.3, 1.7))
    )
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          manovaEffects,
          neural,
          GaleTestMatrix.fromRows(matrices(position)),
          ValueId.unsafe(s"manova-estimate-${partition.value}")
        )
      )
      val residual = right(
        EvidenceTable.dense(
          neural,
          neural,
          GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.1), Vector(0.1, 1.3))),
          ValueId.unsafe(s"manova-residual-${partition.value}")
        )
      )
      val certifiedResidual = right(CertifiedResidualMoments(residual))
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"manova-response-${partition.value}")),
          relationDesign,
          right(Estimability(manovaEffects, Vector(true, true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(
          Relation(
            estimate,
            receipt,
            right(
              ResidualFitCapabilities(
                certifiedResidual,
                ResidualDegreesOfFreedom.unsafe(12.0)
              )
            )
          )
        )
      )
    right(PartitionedRelations(partitionAxis, manovaEffects, neural, entries))

  private def scalarSchedule(operator: Boolean, zero: Boolean = false) =
    right(CrossValidatedRelations.stable(scalarSource(operator, zero)))

  private def manovaSchedule =
    right(CrossValidatedRelations.stable(manovaSource))

  private def success[A](
      result: AnalysisResult[A, CanonicalBindRejection, CanonicalTaskFailure, NoRendition.type]
  ): A =
    result.values.head.outcome match
      case MeasurementOutcome.Success(value, _) => value
      case other                                => fail(s"expected success, obtained $other")

  test("relation validation retains exact semantic keys over Resample4s selections"):
    assertEquals(validation.folds.map(_.heldOut), partitions.keys)
    assertEquals(
      validation.folds.map(_.training),
      Vector(
        Vector(partitions.keys(1), partitions.keys(2)),
        Vector(partitions.keys(0), partitions.keys(2)),
        Vector(partitions.keys(0), partitions.keys(1))
      )
    )
    assertEquals(validation.folds.map(_.analysis.domain), Vector(2, 2, 2))
    assertEquals(validation.folds.map(_.assessment.domain), Vector(1, 1, 1))

  test("canonical and nonnegative canonical agree with a one-dimensional oracle"):
    val source = scalarSchedule(operator = false)
    val ordinary = success(
      right(
        Mvpa.run(source)(
          validation,
          scalarFrame,
          CanonicalAnalysis.canonicalEffect(source, ResidualRegularization.Unregularized),
          strategy
        )
      )
    )
    val constrained = success(
      right(
        Mvpa.run(source)(
          validation,
          scalarFrame,
          CanonicalAnalysis.nonnegativeCanonical(source, ResidualRegularization.Unregularized),
          strategy
        )
      )
    )

    val expectedRoots = scalarRows.map(row => row.head * row.head)
    val expectedMean = expectedRoots.sum / expectedRoots.length.toDouble
    assertEquals(ordinary.folds.map(_.receipt.heldOut), partitions.keys)
    ordinary.folds
      .zip(expectedRoots)
      .foreach: (fold, expected) =>
        assertEqualsDouble(fold.heldOutRoot, expected, 1e-10)
    constrained.folds
      .zip(expectedRoots)
      .foreach: (fold, expected) =>
        assertEqualsDouble(fold.heldOutRoot, expected, 1e-8)
    assertEqualsDouble(ordinary.meanHeldOutRoot, expectedMean, 1e-10)
    assertEqualsDouble(constrained.meanHeldOutRoot, expectedMean, 1e-8)
    assertEquals(ordinary.measurement, selectedX.identity)
    assertEquals(constrained.measurement, selectedX.identity)
    assertEquals(ordinary.computation.operatorApplications, 12L)

  test("signed cross-run Rayleigh matches direct scalar arithmetic"):
    val source = scalarSchedule(operator = false)
    val estimate = success(
      right(
        Mvpa.run(source)(
          validation,
          scalarFrame,
          CanonicalAnalysis.signedCrossRunRayleigh(
            source,
            ResidualRegularization.Unregularized
          ),
          strategy
        )
      )
    )
    val expected = partitions.keys.indices.toVector.map: heldOut =>
      val training = partitions.keys.indices.filter(_ != heldOut).toVector
      val trainingProjection = training
        .map: index =>
          scalarRows(index).head * Math.sqrt(scalarVariances(index))
        .sum
      val heldOutProjection = scalarRows(heldOut).head * Math.sqrt(scalarVariances(heldOut))
      val numerator = trainingProjection * heldOutProjection / scalarVariances(heldOut)
      numerator / training.length.toDouble

    estimate.folds
      .zip(expected)
      .foreach: (fold, oracle) =>
        assertEqualsDouble(fold.statistic.toDouble, oracle, 1e-10)
    assertEqualsDouble(
      estimate.meanStatistic.toDouble,
      expected.sum / expected.length.toDouble,
      1e-10
    )
  test("MANOVA returns every named statistic through the ordinary typed result"):
    val source = manovaSchedule
    val estimate = success(
      right(
        Mvpa.run(source)(
          validation,
          identityFrame,
          CanonicalAnalysis.manova(source, ResidualRegularization.Unregularized),
          strategy
        )
      )
    )
    assertEquals(estimate.folds.length, partitions.size)
    assert(estimate.meanStatistics.royLargestRoot.isFinite)
    assert(estimate.meanStatistics.wilksLambda.isFinite)
    assert(estimate.meanStatistics.pillaiTrace.isFinite)
    assert(estimate.meanStatistics.hotellingLawleyTrace.isFinite)
    assertEquals(estimate.measurement, identityMeasurement.identity)

  test("dense and operator relation representations produce the same estimates"):
    val dense = scalarSchedule(operator = false)
    val operator = scalarSchedule(operator = true)
    val denseEstimate = success(
      right(
        Mvpa.run(dense)(
          validation,
          scalarFrame,
          CanonicalAnalysis.canonicalEffect(dense, ResidualRegularization.Unregularized),
          strategy
        )
      )
    )
    val operatorEstimate = success(
      right(
        Mvpa.run(operator)(
          validation,
          scalarFrame,
          CanonicalAnalysis.canonicalEffect(operator, ResidualRegularization.Unregularized),
          strategy
        )
      )
    )
    denseEstimate.folds
      .zip(operatorEstimate.folds)
      .foreach: (left, right) =>
        assertEqualsDouble(left.heldOutRoot, right.heldOutRoot, 1e-10)
    assertEqualsDouble(denseEstimate.meanHeldOutRoot, operatorEstimate.meanHeldOutRoot, 1e-10)
    assertNotEquals(dense.identity.fingerprint, operator.identity.fingerprint)

  test("fold-scoped relation preparation is selected by held-out semantic key"):
    val stableRelations = scalarSource(operator = false)
    val changedRows = scalarRows.updated(0, Vector(4.0, scalarRows.head(1)))
    val changedRelations = scalarSource(
      operator = false,
      rows = changedRows,
      revision = "held-out-run-1"
    )
    val scheduled = right(
      CrossValidatedRelations.scheduled(
        partitionAxis,
        scalarEffects,
        neural,
        Vector(
          partitions.keys(0) -> changedRelations,
          partitions.keys(1) -> stableRelations,
          partitions.keys(2) -> stableRelations
        )
      )
    )
    val estimate = success(
      right(
        Mvpa.run(scheduled)(
          validation,
          scalarFrame,
          CanonicalAnalysis.canonicalEffect(
            scheduled,
            ResidualRegularization.Unregularized
          ),
          strategy
        )
      )
    )

    assertEqualsDouble(estimate.folds(0).heldOutRoot, 16.0, 1e-10)
    assertEqualsDouble(estimate.folds(1).heldOutRoot, 1.0, 1e-10)
    assertEqualsDouble(estimate.folds(2).heldOutRoot, 9.0, 1e-10)
    assertEquals(estimate.computation.operatorApplications, 24L)
    assertNotEquals(scheduled.identity.fingerprint, scalarSchedule(operator = false).identity.fingerprint)

  test("non-identifiable leading directions remain typed local failures"):
    val source = scalarSchedule(operator = false, zero = true)
    val result = right(
      Mvpa.run(source)(
        validation,
        identityFrame,
        CanonicalAnalysis.canonicalEffect(source, ResidualRegularization.Unregularized),
        strategy
      )
    )
    assertEquals(result.counts.failed, 1)
    result.values.head.outcome match
      case MeasurementOutcome.Failed(
            CanonicalTaskFailure.NonIdentifiableDirection(_, multiplicity),
            _
          ) =>
        assertEquals(multiplicity, 2)
      case other => fail(s"expected non-identifiable-direction failure, obtained $other")

  test("scalar estimands reject a multi-effect source at bind time"):
    val source = manovaSchedule
    val specification = right(
      Mvpa.specify(source)(
        validation,
        identityFrame,
        CanonicalAnalysis.canonicalEffect(source, ResidualRegularization.Unregularized)
      )
    )
    Mvpa.bind(specification).left.toOption match
      case Some(BindError.EstimandRejected(_, CanonicalBindRejection.ScalarEffectRequired(2), _)) => ()
      case other => fail(s"expected scalar-effect bind rejection, obtained $other")
