package scalafim.fmri.mvpa.scenarios

import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.RelationalAnalysis.given

final class RsaToolboxRelationalConformanceSuite extends munit.FunSuite:
  private val fixture = MvpaExternalReferenceFixture

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("external-relation-effects"),
      AxisPurpose.Effects,
      fixture.conditionIds.map(AxisKey.unsafe),
      CoordinateBasis.unsafe("declared-condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("rsa-toolbox-fixture", "v1")
    )
  )
  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("external-relation-features"),
      AxisPurpose.NeuralFeatures,
      fixture.relationFeatureIds.map(FeatureId.unsafe),
      CoordinateBasis.unsafe("declared-feature-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("rsa-toolbox-fixture", "v1")
    )
  )
  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("external-relation-runs"),
      AxisPurpose.Partitions,
      fixture.relationRunIds.map(PartitionId.unsafe),
      CoordinateBasis.unsafe("declared-run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("rsa-toolbox-fixture", "v1")
    )
  )
  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )
  private val training = right(
    AxisRef.create(
      AxisId.unsafe("external-relation-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("fit-row-1"), SampleId.unsafe("fit-row-2")),
      CoordinateBasis.unsafe("declared-fit-row-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("rsa-toolbox-fixture", "v1")
    )
  )
  private val designIdentity = right(
    DesignIdentity(DesignKind.unsafe("external-fixed-relation"))
  )
  private val precisionMatrix = GaleTestMatrix.fromRows(fixture.precision)
  private val residualMatrix = GaleTestMatrix.fromRows(
    Vector.tabulate(neural.size): row =>
      Vector.tabulate(neural.size): column =>
        if row == column then 1.0 else 0.0
  )

  private val source =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          GaleTestMatrix.fromRows(fixture.relationPatternsByRun(position)),
          ValueId.unsafe(s"external-estimate-${partition.value}")
        )
      )
      val precision = right(
        EvidenceTable.dense(
          neural,
          neural,
          precisionMatrix,
          ValueId.unsafe(s"external-precision-${partition.value}")
        )
      )
      val residual = right(
        EvidenceTable.dense(
          neural,
          neural,
          residualMatrix,
          ValueId.unsafe(s"external-residual-${partition.value}")
        )
      )
      val capabilities = right(
        PrecisionFitCapabilities(
          right(CertifiedResidualMoments(residual)),
          right(CertifiedNoisePrecision(precision)),
          ResidualDegreesOfFreedom.unsafe(40.0)
        )
      )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"external-source-${partition.value}")),
          designIdentity,
          right(Estimability(effects, Vector.fill(effects.size)(true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, capabilities))
      )
    right(PartitionedRelations(partitionAxis, effects, neural, entries))

  private val pairing = right(
    PairingDesign.allOrdered(
      partitionAxis,
      PairingReducer.WeightedMean,
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
      right(
        PartitionIndependenceTestSupport.declareAllPairs(
          source.identity,
          partitionAxis,
          "external-independent-runs"
        )
      )
    )
  )

  private def selection(id: String, ordinals: Int*) =
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe(id),
        right(
          Injection.from(
            IArray.unsafeFromArray(ordinals.toArray),
            right(IndexSpace.of(neural.size))
          )
        )
      )
    )

  private val whole = right(
    Measurement.identity(neural, MeasurementId.unsafe("whole"))
  )
  private val anterior = selection("anterior", 0, 1, 2)
  private val posterior = selection("posterior", 3, 4, 5)
  private val frame = right(
    MeasurementFrame(neural)(
      Vector(
        MeasurementEntry(anterior, "anterior"),
        MeasurementEntry(posterior, "posterior"),
        MeasurementEntry(whole, "whole")
      )
    )
  )
  private val strategy = right(
    ExecutionStrategy(
      BackendId.unsafe("portable-external-conformance"),
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

  private def collect(
      result: AnalysisResult[
        MeasuredRelationalRdm[partitions.Id, effects.Id, AxisKey],
        RelationalBindRejection,
        RelationalTaskFailure,
        String
      ]
  ): Map[String, MeasuredRelationalRdm[partitions.Id, effects.Id, AxisKey]] =
    result.values
      .map: value =>
        val measured = value.outcome match
          case MeasurementOutcome.Success(observed, _) => observed
          case other                                   => fail(s"expected relational success, obtained $other")
        value.rendition -> measured
      .toMap

  private def assertValues(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual
      .zip(expected)
      .foreach: (left, right) =>
        assertEqualsDouble(left, right, 1e-12)

  // BEGIN SCENARIO relational-crossnobis
  test("fixed-precision crossnobis matches both RSA toolboxes across measurements"):
    val result = right(
      Mvpa.run(source)(
        pairing,
        frame,
        RelationalAnalysis.crossnobisRdm(
          source,
          RdmNormalization.DivideByNeuralDimension
        ),
        strategy
      )
    )
    val observed = collect(result)

    fixture.crossnobisFixedPrecision.foreach: (name, expected) =>
      val value = observed(name)
      val pairOrder = value.domain.pairs.map: pair =>
        pair.first.value -> pair.second.value
      assertEquals(pairOrder, fixture.relationPairOrder)
      assertValues(value.distances.toVector, expected)
    assert(observed("whole").distances.toVector.head < 0.0)
  // END SCENARIO relational-crossnobis

  test("identity geometry matches rsatoolbox crossvalidated squared Euclidean"):
    val result = right(
      Mvpa.run(source)(
        pairing,
        frame,
        RelationalAnalysis.identityRdm(
          source,
          RdmNormalization.DivideByNeuralDimension
        ),
        strategy
      )
    )
    val observed = collect(result)

    fixture.crossvalidatedSquaredEuclidean.foreach: (name, expected) =>
      assertValues(observed(name).distances.toVector, expected)

  // BEGIN SCENARIO relational-rsa
  test("Pearson, Spearman, and intercepted OLS match RSA Toolbox outputs"):
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.crossnobis(
        source,
        domain,
        RdmNormalization.DivideByNeuralDimension
      )
    )
    val fit = right(RelationalFit.query(query, pairing, whole))
    val category = right(
      SecondOrderModel.signal(
        domain,
        SecondOrderModelName.unsafe("two-category"),
        fixture.twoCategoryModel
      )
    )
    val graded = right(
      SecondOrderModel.signal(
        domain,
        SecondOrderModelName.unsafe("graded"),
        fixture.gradedModel
      )
    )

    assertEqualsDouble(
      right(RelationalRsa.pearson(fit, category)).correlation,
      fixture.rsaPearsonTwoCategory,
      1e-12
    )
    assertEqualsDouble(
      right(RelationalRsa.rank(fit, category)).correlation,
      fixture.rsaSpearmanTwoCategory,
      1e-12
    )
    assertEqualsDouble(
      right(RelationalRsa.pearson(fit, graded)).correlation,
      fixture.rsaPearsonGraded,
      1e-12
    )
    assertEqualsDouble(
      right(RelationalRsa.rank(fit, graded)).correlation,
      fixture.rsaSpearmanGraded,
      1e-12
    )

    val regression = right(
      RelationalRsa.regression(
        fit,
        Vector(category, graded),
        Vector.empty,
        intercept = true
      )
    )
    assertValues(
      regression.coefficients.map(_.value),
      fixture.rsaOlsCoefficients
    )
    assertEqualsDouble(
      regression.residualSumSquares,
      fixture.rsaOlsResidualSumSquares,
      1e-12
    )
  // END SCENARIO relational-rsa
