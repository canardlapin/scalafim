package scalafim.fmri.mvpa

import multivar.core.ValueId
import multivar.core.ValueIdentity

class RelationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def effects(id: String = "relation-effects"): AxisRef[AxisKey] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.Effects,
        Vector(AxisKey.unsafe("condition-a"), AxisKey.unsafe("condition-b")),
        CoordinateBasis.unsafe("condition-order"),
        None,
        right(AxisScale.named("interval")),
        CoordinateProvenance.unsafe("relation-suite", "v1")
      )
    )

  private def neural(id: String = "relation-neural"): AxisRef[FeatureId] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.NeuralFeatures,
        Vector(FeatureId.unsafe("v1"), FeatureId.unsafe("v2"), FeatureId.unsafe("v3")),
        CoordinateBasis.unsafe("voxel-order"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        right(AxisScale.named("ratio")),
        CoordinateProvenance.unsafe("relation-suite", "v1")
      )
    )

  private def samples(id: String = "relation-training"): AxisRef[SampleId] =
    right(
      AxisRef.create(
        AxisId.unsafe(id),
        AxisPurpose.Samples,
        Vector(SampleId.unsafe("trial-1"), SampleId.unsafe("trial-2")),
        CoordinateBasis.unsafe("trial-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relation-suite", "v1")
      )
    )

  private def partitions(): AxisRef[PartitionId] =
    right(
      AxisRef.create(
        AxisId.unsafe("relation-runs"),
        AxisPurpose.Partitions,
        Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
        CoordinateBasis.unsafe("run-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("relation-suite", "v1")
      )
    )

  private def design(label: String = "runwise-glm"): DesignIdentity =
    right(
      DesignIdentity(
        DesignKind.unsafe("relation-fit"),
        Vector("model" -> label)
      )
    )

  private def relation[
      E <: multivar.core.SemanticSpace,
      N <: multivar.core.SemanticSpace
  ](
      effectAxis: AxisRef.Aux[AxisKey, E],
      neuralAxis: AxisRef.Aux[FeatureId, N],
      training: AxisRef[SampleId],
      id: String
  ): Relation[E, N, AxisKey, FeatureId, EstimateOnlyCapabilities[N, FeatureId]] =
    val estimate = right(
      EvidenceTable.dense(
        effectAxis,
        neuralAxis,
        GaleTestMatrix.fromRows(
          Seq(
            Seq(1.0, 2.0, 3.0),
            Seq(0.5, 1.5, 2.5)
          )
        ),
        ValueId.unsafe(s"estimate-$id")
      )
    )
    val estimability = right(Estimability(effectAxis, Vector(true, true)))
    val receipt = right(
      RelationFitReceipt(
        ValueIdentity.source(ValueId.unsafe(s"source-$id")),
        design(),
        estimability,
        NormalizationIdentity.none,
        training
      )
    )
    right(
      Relation(
        estimate,
        receipt,
        right(EstimateOnlyCapabilities(neuralAxis))
      )
    )

  test("estimability and fit provenance are exact owner-bound evidence"):
    val effectAxis = effects()
    val training = samples()
    val estimability = right(Estimability(effectAxis, Vector(true, false)))
    val receipt = right(
      RelationFitReceipt(
        ValueIdentity.source(ValueId.unsafe("run-1-revision")),
        design(),
        estimability,
        NormalizationIdentity.none,
        training
      )
    )

    assertEquals(estimability.rank, 1)
    assert(estimability.isEstimable(effectAxis.keys.head))
    assert(!estimability.isEstimable(effectAxis.keys.last))
    assertEquals(estimability.estimableKeys, Vector(effectAxis.keys.head))
    assertEquals(receipt.trainingSamples, training.identity)
    assertEquals(receipt.estimability.identity, estimability.identity)
    assertEquals(receipt.sourceRevision.stableKey, "run-1-revision")

    val revised = right(
      RelationFitReceipt(
        ValueIdentity.source(ValueId.unsafe("run-2-revision")),
        design(),
        estimability,
        NormalizationIdentity.none,
        training
      )
    )
    assertNotEquals(receipt.identity, revised.identity)

  test("relation rejects mismatched effect and neural owners"):
    val effectErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[E1 <: SemanticSpace, E2 <: SemanticSpace, N <: SemanticSpace, EK, NK](
          estimate: EvidenceTable[E1, N, EK, NK],
          receipt: RelationFitReceipt[E2, EK],
          capabilities: EstimateOnlyCapabilities[N, NK]
      ) = Relation(estimate, receipt, capabilities)
    """)
    assert(effectErrors.nonEmpty)

    val neuralErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[E <: SemanticSpace, N1 <: SemanticSpace, N2 <: SemanticSpace, EK, NK](
          estimate: EvidenceTable[E, N1, EK, NK],
          receipt: RelationFitReceipt[E, EK],
          capabilities: EstimateOnlyCapabilities[N2, NK]
      ) = Relation(estimate, receipt, capabilities)
    """)
    assert(neuralErrors.nonEmpty)

  test("partition binder canonicalizes keyed evidence and requires exact coverage"):
    val effectAxis = effects()
    val neuralAxis = neural()
    val training = samples()
    val runAxis = partitions()
    val namedRuns = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), runAxis)
    )
    val run1 = relation(effectAxis, neuralAxis, training, "run-1")
    val run2 = relation(effectAxis, neuralAxis, training, "run-2")
    val bound = right(
      PartitionedRelations(
        namedRuns,
        effectAxis,
        neuralAxis,
        Vector(
          PartitionRelation(runAxis.keys(1), run2),
          PartitionRelation(runAxis.keys(0), run1)
        )
      )
    )

    assertEquals(bound.partitionKeys, runAxis.keys)
    assertEquals(bound.size, 2)
    assertEquals(
      right(bound.relation(runAxis.keys(0))).estimate.table.valueIdentity.stableKey,
      "estimate-run-1"
    )
    val visited = Vector.newBuilder[String]
    bound.foreachRelation: (partition, value) =>
      visited += s"${partition.value}:${value.estimate.table.valueIdentity.stableKey}"
    assertEquals(
      visited.result(),
      Vector("run-1:estimate-run-1", "run-2:estimate-run-2")
    )
    assertEquals(
      bound.identity.axis(ScientificAxisName.unsafe("runs")),
      Some(runAxis.identity)
    )

    assert(
      PartitionedRelations(
        namedRuns,
        effectAxis,
        neuralAxis,
        Vector(PartitionRelation(runAxis.keys(0), run1))
      ).left.exists:
        case RelationError.MissingPartition(partition) => partition == runAxis.keys(1)
        case _                                         => false
    )
    assert(
      PartitionedRelations(
        namedRuns,
        effectAxis,
        neuralAxis,
        Vector(
          PartitionRelation(runAxis.keys(0), run1),
          PartitionRelation(runAxis.keys(0), run2)
        )
      ).left.exists:
        case RelationError.DuplicatePartition(partition) => partition == runAxis.keys(0)
        case _                                           => false
    )

  test("residual and precision evidence are static capabilities, not optional fields"):
    val neuralAxis = neural()
    val residual = right(
      EvidenceTable.dense(
        neuralAxis,
        neuralAxis,
        GaleTestMatrix.fromRows(
          Seq(
            Seq(2.0, 0.0, 0.0),
            Seq(0.0, 3.0, 0.0),
            Seq(0.0, 0.0, 4.0)
          )
        ),
        ValueId.unsafe("residual-moments")
      )
    )
    val precisionMatrix = GaleTestMatrix.fromRows(
      Seq(
        Seq(0.5, 0.0, 0.0),
        Seq(0.0, 1.0 / 3.0, 0.0),
        Seq(0.0, 0.0, 0.25)
      )
    )
    val precision = right(
      EvidenceTable.dense(
        neuralAxis,
        neuralAxis,
        precisionMatrix,
        ValueId.unsafe("noise-precision")
      )
    )
    val certifiedPrecision = right(CertifiedNoisePrecision(precision))
    val certifiedResidual = right(CertifiedResidualMoments(residual))
    val capability = right(
      PrecisionFitCapabilities(
        certifiedResidual,
        certifiedPrecision,
        ResidualDegreesOfFreedom.unsafe(18.0)
      )
    )

    def precisionIdentity[
        N <: multivar.core.SemanticSpace,
        K,
        C <: HasNoisePrecision[N, K]
    ](value: C): String =
      value.noisePrecision.table.table.valueIdentity.stableKey

    assertEquals(precisionIdentity(capability), "noise-precision")
    assertEquals(capability.residualDegreesOfFreedom.toDouble, 18.0)

    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[N <: SemanticSpace, K](
          capability: EstimateOnlyCapabilities[N, K]
      ): HasNoisePrecision[N, K] =
        capability
    """)
    assert(errors.nonEmpty)

    val splitCertificateErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[N <: SemanticSpace, K](
          neural: AxisRef.Aux[K, N],
          residual: EvidenceTable[N, N, K, K],
          precision: EvidenceTable[N, N, K, K],
          certificate: NoisePrecisionCertificate[N, K],
          degreesOfFreedom: ResidualDegreesOfFreedom
      ) =
        PrecisionFitCapabilities(
          neural,
          residual,
          precision,
          certificate,
          degreesOfFreedom
        )
    """)
    assert(splitCertificateErrors.nonEmpty)

  test("residual-moment certification enforces symmetry and positive semidefiniteness"):
    val neuralAxis = neural("residual-law-neural")
    def table(id: String, rows: Seq[Seq[Double]]) =
      right(
        EvidenceTable.dense(
          neuralAxis,
          neuralAxis,
          GaleTestMatrix.fromRows(rows),
          ValueId.unsafe(id)
        )
      )

    val singularPsd = table(
      "singular-psd-residual",
      Seq(
        Seq(1.0, 1.0, 0.0),
        Seq(1.0, 1.0, 0.0),
        Seq(0.0, 0.0, 0.0)
      )
    )
    val certified = right(CertifiedResidualMoments(singularPsd))
    assert(certified.certificate.smallestEigenvalue >= -1e-10)
    assertEquals(
      certified.certificate.valueIdentity,
      singularPsd.table.valueIdentity
    )

    val asymmetric = table(
      "asymmetric-residual",
      Seq(
        Seq(1.0, 0.2, 0.0),
        Seq(0.1, 1.0, 0.0),
        Seq(0.0, 0.0, 1.0)
      )
    )
    assert(CertifiedResidualMoments(asymmetric).left.exists:
      case RelationError.AsymmetricResidualMoments(0, 1, _, _) => true
      case _                                                   => false)

    val indefinite = table(
      "indefinite-residual",
      Seq(
        Seq(1.0, 2.0, 0.0),
        Seq(2.0, 1.0, 0.0),
        Seq(0.0, 0.0, 1.0)
      )
    )
    assert(CertifiedResidualMoments(indefinite).left.exists:
      case RelationError.NonPositiveSemidefiniteResidualMoments(value, _) =>
        value < 0.0
      case _ => false)

  test("dense and operator residual certification attest the same contents"):
    val neuralAxis = neural("residual-parity-neural")
    val matrix = GaleTestMatrix.fromRows(
      Seq(
        Seq(2.0, 0.25, 0.0),
        Seq(0.25, 1.5, 0.1),
        Seq(0.0, 0.1, 0.75)
      )
    )
    val dense = right(
      EvidenceTable.dense(
        neuralAxis,
        neuralAxis,
        matrix,
        ValueId.unsafe("dense-residual-certification")
      )
    )
    val operator = right(
      EvidenceTable.operator(
        neuralAxis,
        neuralAxis,
        gale.linalg.LinearOperator.tabulate(3, 3)(matrix.apply),
        ValueId.unsafe("operator-residual-certification")
      )
    )
    val denseCertificate = right(CertifiedResidualMoments(dense)).certificate
    val operatorCertificate = right(CertifiedResidualMoments(operator)).certificate

    assertEquals(denseCertificate.contentDigest, operatorCertificate.contentDigest)
    assertEqualsDouble(
      denseCertificate.smallestEigenvalue,
      operatorCertificate.smallestEigenvalue,
      1e-12
    )
    assertNotEquals(denseCertificate.identity, operatorCertificate.identity)

  test("equal-shape foreign neural evidence cannot be certified or attached"):
    val rectangularOwnerErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[N1 <: SemanticSpace, N2 <: SemanticSpace, K](
          evidence: EvidenceTable[N1, N2, K, K]
      ) = CertifiedResidualMoments(evidence)
    """)
    assert(rectangularOwnerErrors.nonEmpty)

    val foreignPrecisionErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[N1 <: SemanticSpace, N2 <: SemanticSpace, K](
          residual: CertifiedResidualMoments[N1, K],
          precision: CertifiedNoisePrecision[N2, K],
          degreesOfFreedom: ResidualDegreesOfFreedom
      ) = PrecisionFitCapabilities(residual, precision, degreesOfFreedom)
    """)
    assert(foreignPrecisionErrors.nonEmpty)

    val forgedPairErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[N <: SemanticSpace, K](
          table: EvidenceTable[N, N, K, K],
          certificate: ResidualMomentCertificate[N, K]
      ) = new CertifiedResidualMoments(table, certificate)
    """)
    assert(forgedPairErrors.nonEmpty)

  test("fit provenance remains descriptive and cannot stand in for a certified claim"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace

      def invalid[E <: SemanticSpace, K](
          receipt: RelationFitReceipt[E, K]
      ): ResidualMomentCertificate[?, ?] =
        receipt.residualMomentCertificate
    """)
    assert(errors.nonEmpty)

  test("fit receipt identity covers design, estimability, preparation, and training samples"):
    val effectAxis = effects()
    val baseEstimability = right(Estimability(effectAxis, Vector(true, true)))
    val reducedEstimability = right(Estimability(effectAxis, Vector(true, false)))
    val training = samples()
    val otherTraining = samples("other-training")
    val step = right(
      NormalizationStepIdentity(
        NormalizationStepId.unsafe("demean"),
        Vector("scope" -> "within-run")
      )
    )
    val prepared = NormalizationIdentity(Vector(step))

    def receipt(
        fitDesign: DesignIdentity = design(),
        fitEstimability: Estimability[effectAxis.Id, AxisKey] = baseEstimability,
        fitPreparation: NormalizationIdentity = NormalizationIdentity.none,
        fitSamples: AxisRef[SampleId] = training
    ): RelationFitReceipt[effectAxis.Id, AxisKey] =
      right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe("fixed-source-revision")),
          fitDesign,
          fitEstimability,
          fitPreparation,
          fitSamples
        )
      )

    val baseline = receipt()
    val alternatives = Vector(
      receipt(fitDesign = design("alternative-glm")),
      receipt(fitEstimability = reducedEstimability),
      receipt(fitPreparation = prepared),
      receipt(fitSamples = otherTraining)
    )
    assert(alternatives.forall(_.identity != baseline.identity))
