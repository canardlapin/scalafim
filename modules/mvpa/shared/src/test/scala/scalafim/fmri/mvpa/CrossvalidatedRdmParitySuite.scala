package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity

/** Independent execution court for the committed base-R RDM oracle. */
class CrossvalidatedRdmParitySuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("oracle-effects"),
      AxisPurpose.Effects,
      CrossvalidatedRdmReferenceFixtures.effectLabels.map(AxisKey.unsafe),
      CoordinateBasis.unsafe("declared-condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("base-r-crossvalidated-rdm", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("oracle-neural-features"),
      AxisPurpose.NeuralFeatures,
      CrossvalidatedRdmReferenceFixtures.featureLabels.map(FeatureId.unsafe),
      CoordinateBasis.unsafe("declared-feature-order"),
      Some(AxisUnits.unsafe("arbitrary-signal")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("base-r-crossvalidated-rdm", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("oracle-independent-runs"),
      AxisPurpose.Partitions,
      CrossvalidatedRdmReferenceFixtures.partitionLabels.map(PartitionId.unsafe),
      CoordinateBasis.unsafe("declared-run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("base-r-crossvalidated-rdm", "v1")
    )
  )

  private val partitionAxis = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("oracle-training-samples"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("sample-1"), SampleId.unsafe("sample-2")),
      CoordinateBasis.unsafe("declared-sample-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("base-r-crossvalidated-rdm", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(DesignKind.unsafe("base-r-crossvalidated-rdm"))
  )

  private val identityMatrix = GaleTestMatrix.fromRows(
    Vector(Vector(1.0, 0.0), Vector(0.0, 1.0))
  )

  private def receipt(
      partition: PartitionId
  ): RelationFitReceipt[effects.Id, AxisKey] =
    right(
      RelationFitReceipt(
        ValueIdentity.source(ValueId.unsafe(s"rdm-oracle-source-${partition.value}")),
        relationDesign,
        right(Estimability(effects, Vector.fill(effects.size)(true))),
        NormalizationIdentity.none,
        training
      )
    )

  private val source =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          CrossvalidatedRdmReferenceFixtures.partitionPatterns(position),
          ValueId.unsafe(s"rdm-oracle-estimate-${partition.value}")
        )
      )
      val residual = right(
        EvidenceTable.dense(
          neural,
          neural,
          identityMatrix,
          ValueId.unsafe(s"rdm-oracle-residual-${partition.value}")
        )
      )
      val precision = right(
        EvidenceTable.dense(
          neural,
          neural,
          identityMatrix,
          ValueId.unsafe(s"rdm-oracle-precision-${partition.value}")
        )
      )
      val capabilities = right(
        PrecisionFitCapabilities(
          right(CertifiedResidualMoments(residual)),
          right(CertifiedNoisePrecision(precision)),
          ResidualDegreesOfFreedom.unsafe(20.0)
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt(partition), capabilities))
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
          "base-r-oracle-independent-runs"
        )
      )
    )
  )

  private val measurement = right(
    Measurement.identity(
      neural,
      MeasurementId.unsafe("rdm-oracle-all-neural-features")
    )
  )

  private val domain = right(WithinPairDomain(source.effects))

  private def distance(normalization: RdmNormalization) =
    val query = right(RelationalFitQuery.crossnobis(source, domain, normalization))
    right(right(RelationalFit.query(query, pairing, measurement)).distance)

  test("crossnobis matches base-R values and declared upper-triangle pair order"):
    val observed = distance(RdmNormalization.Raw)
    val observedPairs = observed.rdm.definition.domain.pairs.map: pair =>
      pair.first.value -> pair.second.value

    assertEquals(observedPairs, CrossvalidatedRdmReferenceFixtures.pairOrder)
    observed.rdm.distances.toVector
      .zip(CrossvalidatedRdmReferenceFixtures.rawCrossvalidatedDistances)
      .foreach: (actual, expected) =>
        assertEqualsDouble(actual, expected, 1e-12)

  test("neural-dimension normalization matches base R without hiding signed estimates"):
    val observed = distance(RdmNormalization.DivideByNeuralDimension)

    observed.rdm.distances.toVector
      .zip(CrossvalidatedRdmReferenceFixtures.perFeatureCrossvalidatedDistances)
      .foreach: (actual, expected) =>
        assertEqualsDouble(actual, expected, 1e-12)
    assert(CrossvalidatedRdmReferenceFixtures.naiveSquaredDistances.head > 0.0)
    assert(observed.rdm.distances.values.head < 0.0)
    assertEquals(observed.units.label, "whitened-squared-per-neural-coordinate")
