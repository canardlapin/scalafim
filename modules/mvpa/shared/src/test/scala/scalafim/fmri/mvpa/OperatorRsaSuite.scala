package scalafim.fmri.mvpa

import multivar.core.OperatorRepresentation
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

final class OperatorRsaSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("operator-rsa-effects"),
      AxisPurpose.Effects,
      Vector("a", "b", "c", "d").map(AxisKey.unsafe),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("operator-rsa-neural"),
      AxisPurpose.NeuralFeatures,
      Vector.tabulate(5)(position => FeatureId.unsafe(s"voxel-${position + 1}")),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("activation")),
      right(AxisScale.named("interval")),
      CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("operator-rsa-runs"),
      AxisPurpose.Partitions,
      Vector("run-1", "run-2", "run-3").map(PartitionId.unsafe),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
    )
  )

  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("operator-rsa-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("fit-row-1"), SampleId.unsafe("fit-row-2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(
      DesignKind.unsafe("operator-rsa-runwise-relations"),
      Vector("estimator" -> "declared-runwise-effect-patterns")
    )
  )

  // These are the three runwise condition-pattern tables used by the former
  // Matrix-free parity fixture, represented at its actual relation
  // boundary instead of being reconstructed from generic targets and folds.
  private val relationMatrices = Vector(
    GaleTestMatrix.fromRows(
      Seq(
        Seq(0.0, 0.2, 0.0, 0.1, -0.1),
        Seq(1.0, 0.1, 0.5, 0.0, 0.2),
        Seq(0.0, 2.0, 0.2, -0.1, 0.1),
        Seq(1.5, 1.0, -0.2, 0.3, 0.0)
      )
    ),
    GaleTestMatrix.fromRows(
      Seq(
        Seq(0.1, 0.0, 0.1, 0.2, -0.2),
        Seq(1.2, 0.2, 0.4, -0.1, 0.1),
        Seq(-0.1, 1.8, 0.3, 0.0, 0.2),
        Seq(1.4, 1.1, -0.1, 0.2, -0.1)
      )
    ),
    GaleTestMatrix.fromRows(
      Seq(
        Seq(-0.1, 0.1, -0.1, 0.0, 0.0),
        Seq(0.9, -0.1, 0.6, 0.1, 0.3),
        Seq(0.2, 2.1, 0.1, -0.2, 0.0),
        Seq(1.6, 0.9, -0.3, 0.4, 0.1)
      )
    )
  )

  private def pairingFor(
      evidence: ScientificSourceIdentity
  ): PairingDesign[partitions.Id, partitions.Id] = right(
    PairingDesign.allOrdered(
      partitionAxis,
      PairingReducer.WeightedMean,
      GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
      right(
        PartitionIndependenceTestSupport.declareAllPairs(
          evidence,
          partitionAxis,
          "operator-rsa-independent-runs"
        )
      )
    )
  )

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 1, 2)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("operator-rsa-first-three-voxels"),
        injection
      )
    )

  private def source(
      operatorBacked: Boolean,
      operators: Vector[MvpaLawFixtures.MatrixFree] = relationMatrices.map(MvpaLawFixtures.MatrixFree.apply)
  ): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neural.Id, FeatureId]
  ] =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate =
        if operatorBacked then
          right(
            EvidenceTable.operator(
              effects,
              neural,
              operators(position),
              ValueId.unsafe(s"operator-rsa-estimate-${partition.value}")
            )
          )
        else
          right(
            EvidenceTable.dense(
              effects,
              neural,
              relationMatrices(position),
              ValueId.unsafe(s"dense-rsa-estimate-${partition.value}")
            )
          )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(
            ValueId.unsafe(s"operator-rsa-source-${partition.value}")
          ),
          relationDesign,
          right(Estimability(effects, Vector.fill(effects.size)(true))),
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
            right(EstimateOnlyCapabilities(neural))
          )
        )
      )
    right(PartitionedRelations(partitionAxis, effects, neural, entries))

  private def fit(
      evidence: PartitionedRelations[
        partitions.Id,
        effects.Id,
        neural.Id,
        AxisKey,
        FeatureId,
        EstimateOnlyCapabilities[neural.Id, FeatureId]
      ]
  ): IdentityPrecisionRelationFit[
    partitions.Id,
    effects.Id,
    measurement.local.Id,
    AxisKey,
    FeatureId
  ] =
    val domain = right(WithinPairDomain(evidence.effects))
    val query = right(
      RelationalFitQuery.identityPrecision(
        evidence,
        domain,
        RdmNormalization.DivideByNeuralDimension
      )
    )
    right(RelationalFit.query(query, pairingFor(evidence.identity), measurement))

  test("dense and matrix-free relation evidence produce the same typed RDM"):
    val operators = relationMatrices.map(MvpaLawFixtures.MatrixFree.apply)
    val dense = fit(source(operatorBacked = false))
    val operator = fit(source(operatorBacked = true, operators))
    val expected = right(dense.rdm)
    val actual = right(operator.rdm)

    assertEquals(actual.definition.domain.items.identity, effects.identity)
    assert(actual.definition.domain.items.evidence eq effects.evidence)
    assertEquals(
      actual.definition.domain.pairs.map(pair => pair.first -> pair.second),
      Vector(
        effects.keys(0) -> effects.keys(1),
        effects.keys(0) -> effects.keys(2),
        effects.keys(0) -> effects.keys(3),
        effects.keys(1) -> effects.keys(2),
        effects.keys(1) -> effects.keys(3),
        effects.keys(2) -> effects.keys(3)
      )
    )
    actual.distances.toVector
      .zip(expected.distances.toVector)
      .foreach: (observed, reference) =>
        assertEqualsDouble(observed, reference, 1e-12)
    assertEquals(
      operator.computation.design,
      pairingFor(operator.sourceIdentity).identity
    )
    assertEquals(
      operator.computation.projectionMode,
      RelationalProjectionMode.OperatorProjection
    )
    assertEquals(operator.computation.materializedCells, 0L)
    assertEquals(operator.computation.operatorApplications, 3L)
    assertEquals(operator.computation.avoidedFullRelationCells, 24L)
    assert(operator.computation.measurementComposedBeforeProjection)
    assertEquals(
      operator.computation.projections.map(_.sourceRepresentation),
      Vector.fill(3)(OperatorRepresentation.MatrixFree)
    )
    assertEquals(operators.map(_.forwardCalls), Vector(0, 0, 0))
    assertEquals(operators.map(_.transposeCalls), Vector(4, 4, 4))

  test("operator RSA reuses one relation fit without reopening evidence"):
    val operators = relationMatrices.map(MvpaLawFixtures.MatrixFree.apply)
    val relationFit = fit(source(operatorBacked = true, operators))
    val observed = right(relationFit.rdm).distances.toVector
    val signal = right(
      SecondOrderModel.signal(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("observed-geometry"),
        observed
      )
    )
    val nuisance = right(
      SecondOrderModel.nuisance(
        relationFit.definition.domain,
        SecondOrderModelName.unsafe("graded-nuisance"),
        Vector.tabulate(observed.length)(_.toDouble)
      )
    )
    val callsAfterFit = operators.map(_.transposeCalls)
    val pearson = right(RelationalRsa.pearson(relationFit, signal))
    val rank = right(RelationalRsa.rank(relationFit, signal))
    val partial = right(RelationalRsa.partial(relationFit, signal, Vector(nuisance)))

    assertEqualsDouble(pearson.correlation, 1.0, 1e-12)
    assertEqualsDouble(rank.correlation, 1.0, 1e-12)
    assertEqualsDouble(partial.partialCorrelation, 1.0, 1e-12)
    assertEquals(pearson.fit, relationFit.identity)
    assertEquals(pearson.model, signal.identity)
    assertEquals(operators.map(_.transposeCalls), callsAfterFit)
    assertEquals(
      pearson.estimand.fields.find(_.name == "second-order-estimand").map(_.value),
      Some("pearson-correlation-of-signed-canonical-pair-distances")
    )

  test("crossvalidated geometry keeps negative null distances"):
    val nullEffects = right(
      AxisRef.create(
        AxisId.unsafe("operator-rsa-null-effects"),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b")),
        CoordinateBasis.unsafe("condition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
      )
    )
    val nullNeural = right(
      AxisRef.create(
        AxisId.unsafe("operator-rsa-null-neural"),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("x")),
        CoordinateBasis.unsafe("voxel-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("operator-rsa-suite", "v1")
      )
    )
    val nullMeasurement = right(
      Measurement.identity(
        nullNeural,
        MeasurementId.unsafe("operator-rsa-null-measurement")
      )
    )
    val nullMatrices = Vector(
      GaleTestMatrix.fromRows(Seq(Seq(1.0), Seq(-1.0))),
      GaleTestMatrix.fromRows(Seq(Seq(-1.0), Seq(1.0))),
      GaleTestMatrix.fromRows(Seq(Seq(1.0), Seq(-1.0)))
    )
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.operator(
          nullEffects,
          nullNeural,
          MvpaLawFixtures.MatrixFree(nullMatrices(position)),
          ValueId.unsafe(s"operator-rsa-null-${partition.value}")
        )
      )
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"null-source-${partition.value}")),
          relationDesign,
          right(Estimability(nullEffects, Vector(true, true))),
          NormalizationIdentity.none,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, right(EstimateOnlyCapabilities(nullNeural))))
      )
    val nullSource = right(
      PartitionedRelations(partitionAxis, nullEffects, nullNeural, entries)
    )
    val nullDomain = right(WithinPairDomain(nullSource.effects))
    val nullQuery = right(
      RelationalFitQuery.identityPrecision(
        nullSource,
        nullDomain,
        RdmNormalization.Raw
      )
    )
    val nullFit = right(
      RelationalFit.query(
        nullQuery,
        pairingFor(nullSource.identity),
        nullMeasurement
      )
    )

    assertEqualsDouble(right(nullFit.rdm).distances.toVector.head, -4.0 / 3.0, 1e-12)
    assertEquals(
      nullFit.definition.design.identity,
      pairingFor(nullSource.identity).identity
    )
