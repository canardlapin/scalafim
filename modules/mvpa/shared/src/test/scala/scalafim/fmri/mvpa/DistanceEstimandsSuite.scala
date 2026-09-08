package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

class DistanceEstimandsSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("distance-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("distance-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("distance-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("v1"), FeatureId.unsafe("v2")),
      CoordinateBasis.unsafe("voxel-order"),
      Some(AxisUnits.unsafe("percent-signal-change")),
      AxisScale.nominal,
      CoordinateProvenance.unsafe("distance-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("distance-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("distance-suite", "v1")
    )
  )

  private val namedPartitions = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("distance-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("distance-suite", "v1")
    )
  )

  private val relationDesign = right(
    DesignIdentity(DesignKind.unsafe("distance-relation"))
  )

  private val selection =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 1)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("distance-all-features"),
        injection
      )
    )

  private val relationMatrices = Vector(
    GaleTestMatrix.fromRows(Seq(Seq(1.0, 0.0), Seq(0.0, 0.0))),
    GaleTestMatrix.fromRows(Seq(Seq(-1.0, 0.0), Seq(0.0, 0.0)))
  )

  private def receipt(
      partition: PartitionId
  ): RelationFitReceipt[effects.Id, AxisKey] =
    right(
      RelationFitReceipt(
        ValueIdentity.source(ValueId.unsafe(s"distance-source-${partition.value}")),
        relationDesign,
        right(Estimability(effects, Vector(true, true))),
        NormalizationIdentity.none,
        training
      )
    )

  private def estimateOnlySource(): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neural.Id, FeatureId]
  ] =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          relationMatrices(position),
          ValueId.unsafe(s"distance-estimate-${partition.value}")
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt(partition), right(EstimateOnlyCapabilities(neural))))
      )
    right(PartitionedRelations(namedPartitions, effects, neural, entries))

  private def precisionSource(token: String = "crossnobis"): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    PrecisionFitCapabilities[neural.Id, FeatureId]
  ] =
    val residualMatrix = GaleTestMatrix.fromRows(Seq(Seq(0.5, 0.0), Seq(0.0, 1.0)))
    val precisionMatrix = GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.0), Seq(0.0, 1.0)))
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val estimate = right(
        EvidenceTable.dense(
          effects,
          neural,
          relationMatrices(position),
          ValueId.unsafe(s"$token-estimate-${partition.value}")
        )
      )
      val residual = right(
        EvidenceTable.dense(
          neural,
          neural,
          residualMatrix,
          ValueId.unsafe(s"$token-residual-${partition.value}")
        )
      )
      val precision = right(
        EvidenceTable.dense(
          neural,
          neural,
          precisionMatrix,
          ValueId.unsafe(s"$token-precision-${partition.value}")
        )
      )
      val certifiedPrecision = right(CertifiedNoisePrecision(precision))
      val certifiedResidual = right(CertifiedResidualMoments(residual))
      val capabilities = right(
        PrecisionFitCapabilities(
          certifiedResidual,
          certifiedPrecision,
          ResidualDegreesOfFreedom.unsafe(20.0)
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt(partition), capabilities))
      )
    right(PartitionedRelations(namedPartitions, effects, neural, entries))

  private def pairingFor[C <: RelationCapabilities[neural.Id, FeatureId]](
      source: PartitionedRelations[
        partitions.Id,
        effects.Id,
        neural.Id,
        AxisKey,
        FeatureId,
        C
      ]
  ): PairingDesign[partitions.Id, partitions.Id] =
    val independence = right(
      PartitionIndependenceTestSupport.declareAllPairs(
        source.identity,
        namedPartitions,
        "distance-suite-independent-runs"
      )
    )
    right(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
        independence
      )
    )

  test("identity-precision distance is named directly and preserves negative unbiased estimates"):
    val source = estimateOnlySource()
    val pairing = pairingFor(source)
    val domain = right(WithinPairDomain(source.effects))
    val rawQuery = right(
      RelationalFitQuery.identityPrecision(
        source,
        domain,
        RdmNormalization.Raw
      )
    )
    val normalizedQuery = right(
      RelationalFitQuery.identityPrecision(
        source,
        domain,
        RdmNormalization.DivideByNeuralDimension
      )
    )
    val raw = right(right(RelationalFit.query(rawQuery, pairing, selection)).distance)
    val normalized = right(
      right(RelationalFit.query(normalizedQuery, pairing, selection)).distance
    )

    assertEqualsDouble(raw.rdm.distances.values(0), -1.0, 1e-12)
    assertEqualsDouble(normalized.rdm.distances.values(0), -0.5, 1e-12)
    assertEquals(raw.units.label, "percent-signal-change-squared")
    assertEquals(
      normalized.units.label,
      "percent-signal-change-squared-per-neural-coordinate"
    )
  test("crossnobis requires certified precision and retains its provenance"):
    val source = precisionSource()
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.crossnobis(
        source,
        domain,
        RdmNormalization.Raw
      )
    )
    val result = right(
      right(RelationalFit.query(query, pairingFor(source), selection)).distance
    )

    assertEqualsDouble(result.rdm.distances.values(0), -2.0, 1e-12)
    assertEquals(result.units.label, "whitened-squared")
    assertEquals(result.precision.partitions.map(_.partition), partitions.keys)
    assertEquals(result.precision.operatorApplications, 4L)
    assertEquals(result.precision.measurement, selection.identity)
    assertEquals(
      result.rdm.definition.identity.fields.find(_.name == "precision-policy").map(_.value),
      Some("fixed-precision")
    )

    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, LK](
          source: PartitionedRelations[P, E, N, EK, NK, EstimateOnlyCapabilities[N, NK]],
          domain: WithinPairDomain[E, EK]
      ) =
        RelationalFitQuery.crossnobis(
          source,
          domain,
          RdmNormalization.Raw
        )
    """)
    assert(errors.nonEmpty)

  test("crossnobis rejects a declaration from different exact evidence"):
    val declaredSource = precisionSource("declared-evidence")
    val substitutedSource = precisionSource("substituted-evidence")
    val design = pairingFor(declaredSource)
    val domain = right(WithinPairDomain(substitutedSource.effects))
    val query = right(
      RelationalFitQuery.crossnobis(
        substitutedSource,
        domain,
        RdmNormalization.Raw
      )
    )

    val result = RelationalFit.query(query, design, selection)

    assert(result.left.exists:
      case RelationalFitError.Distance(
            DistanceEstimandError.Independence(
              PairingDesignError.IndependenceEvidenceMismatch(
                "left",
                expected,
                actual
              )
            )
          ) =>
        expected == declaredSource.identity.fingerprint &&
        actual == substitutedSource.identity.fingerprint
      case _ => false)

  test("precision certification rejects asymmetric and indefinite matrices"):
    val asymmetric = GaleTestMatrix.fromRows(Seq(Seq(1.0, 0.5), Seq(0.0, 1.0)))
    val indefinite = GaleTestMatrix.fromRows(Seq(Seq(1.0, 2.0), Seq(2.0, 1.0)))
    val asymmetricEvidence = right(
      EvidenceTable.dense(
        neural,
        neural,
        asymmetric,
        ValueId.unsafe("asymmetric-precision")
      )
    )
    val indefiniteEvidence = right(
      EvidenceTable.dense(
        neural,
        neural,
        indefinite,
        ValueId.unsafe("indefinite-precision")
      )
    )

    assert(CertifiedNoisePrecision(asymmetricEvidence).left.exists:
      case DistanceEstimandError.AsymmetricPrecision(0, 1, 0.5, 0.0) => true
      case _                                                         => false)
    assert(CertifiedNoisePrecision(indefiniteEvidence).left.exists:
      case DistanceEstimandError.NonPositiveDefinitePrecision(_) => true
      case _                                                     => false)

  test("a reused ValueId cannot substitute a different matrix under a precision certificate"):
    val reused = ValueId.unsafe("reused-precision-value")
    val originalMatrix = GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.0), Seq(0.0, 1.0)))
    val substitutedMatrix = GaleTestMatrix.fromRows(Seq(Seq(1.0, 2.0), Seq(2.0, 1.0)))
    val alternateMatrix = GaleTestMatrix.fromRows(Seq(Seq(3.0, 0.0), Seq(0.0, 1.0)))
    val original = right(EvidenceTable.dense(neural, neural, originalMatrix, reused))
    val substituted = right(EvidenceTable.dense(neural, neural, substitutedMatrix, reused))
    val alternate = right(EvidenceTable.dense(neural, neural, alternateMatrix, reused))
    val certifiedOriginal = right(CertifiedNoisePrecision(original))

    assertEquals(
      certifiedOriginal.certificate.valueIdentity,
      substituted.table.valueIdentity
    )
    assert(CertifiedNoisePrecision(substituted).left.exists:
      case DistanceEstimandError.NonPositiveDefinitePrecision(_) => true
      case _                                                     => false)

    val certifiedAlternate = right(CertifiedNoisePrecision(alternate))
    assertNotEquals(
      certifiedOriginal.certificate.contentDigest,
      certifiedAlternate.certificate.contentDigest
    )
    assertNotEquals(
      certifiedOriginal.certificate.identity,
      certifiedAlternate.certificate.identity
    )

    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def substitute[N <: SemanticSpace, K](
          table: EvidenceTable[N, N, K, K],
          certificate: NoisePrecisionCertificate[N, K]
      ) =
        new CertifiedNoisePrecision(table, certificate)
    """)
    assert(errors.nonEmpty)

  test("dense and operator precision evidence receive the same numeric certificate"):
    val matrix = GaleTestMatrix.fromRows(Seq(Seq(2.0, 0.25), Seq(0.25, 1.0)))
    val dense = right(
      EvidenceTable.dense(
        neural,
        neural,
        matrix,
        ValueId.unsafe("dense-certified-precision")
      )
    )
    val operator = right(
      EvidenceTable.operator(
        neural,
        neural,
        MvpaLawFixtures.MatrixFree(matrix),
        ValueId.unsafe("operator-certified-precision")
      )
    )

    val denseCertificate = right(CertifiedNoisePrecision(dense)).certificate
    val operatorCertificate = right(CertifiedNoisePrecision(operator)).certificate
    assertEquals(denseCertificate.contentDigest, operatorCertificate.contentDigest)

  test("relation precision is not silently transported through a weighted measurement"):
    val weighted = right(
      Measurement.weightedRegion(
        neural,
        MeasurementId.unsafe("weighted-distance"),
        Vector(neural.keys(0) -> 0.5, neural.keys(1) -> 0.5)
      )
    )
    val source = precisionSource()
    val domain = right(WithinPairDomain(source.effects))
    val query = right(
      RelationalFitQuery.crossnobis(source, domain, RdmNormalization.Raw)
    )
    val result = RelationalFit.query(query, pairingFor(source), weighted)
    assert(result.left.exists:
      case RelationalFitError.Distance(
            DistanceEstimandError.UnsupportedPrecisionMeasurement(
              MeasurementKind.WeightedRegion
            )
          ) =>
        true
      case _ => false)

  test("one partition cannot establish a lawful crossvalidated distance design"):
    val one = right(
      AxisRef.create(
        AxisId.unsafe("one-distance-run"),
        AxisPurpose.Partitions,
        Vector(PartitionId.unsafe("only-run")),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("distance-suite", "v1")
      )
    )
    val named = right(PartitionAxis(ScientificAxisName.unsafe("runs"), one))
    val result = PartitionIndependenceTestSupport.declareAllPairsForDesign(
      named,
      "singleton-distance"
    )
    assert(result.left.exists(_ == PairingDesignError.EmptyIndependenceDeclaration))

  test("normalization is a required scientific argument"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[P <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK, LK, C <: RelationCapabilities[N, NK]](
          source: PartitionedRelations[P, E, N, EK, NK, C],
          domain: WithinPairDomain[E, EK]
      ) =
        RelationalFitQuery.identityPrecision(source, domain)
    """)
    assert(errors.nonEmpty)
