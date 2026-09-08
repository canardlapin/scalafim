package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.DoubleLinearOperator
import gale.linalg.MutableDVec
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.IndexSpace
import resample4s.core.Injection

class RelationalFitSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private val effects = right(
    AxisRef.create(
      AxisId.unsafe("reuse-effects"),
      AxisPurpose.Effects,
      Vector(AxisKey.unsafe("a"), AxisKey.unsafe("b"), AxisKey.unsafe("c")),
      CoordinateBasis.unsafe("condition-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-fit-suite", "v1")
    )
  )

  private val neural = right(
    AxisRef.create(
      AxisId.unsafe("reuse-neural"),
      AxisPurpose.NeuralFeatures,
      Vector(FeatureId.unsafe("v1"), FeatureId.unsafe("v2")),
      CoordinateBasis.unsafe("voxel-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-fit-suite", "v1")
    )
  )

  private val partitions = right(
    AxisRef.create(
      AxisId.unsafe("reuse-runs"),
      AxisPurpose.Partitions,
      Vector(PartitionId.unsafe("run-1"), PartitionId.unsafe("run-2")),
      CoordinateBasis.unsafe("run-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-fit-suite", "v1")
    )
  )

  private val namedPartitions = right(
    PartitionAxis(ScientificAxisName.unsafe("runs"), partitions)
  )

  private val training = right(
    AxisRef.create(
      AxisId.unsafe("reuse-training"),
      AxisPurpose.Samples,
      Vector(SampleId.unsafe("t1"), SampleId.unsafe("t2")),
      CoordinateBasis.unsafe("time-order"),
      None,
      AxisScale.nominal,
      CoordinateProvenance.unsafe("relational-fit-suite", "v1")
    )
  )

  private val measurement =
    val injection = right(
      Injection.from(
        IArray.unsafeFromArray(Array(0, 1)),
        right(IndexSpace.of(neural.size))
      )
    )
    right(
      Measurement.hardSelection(
        neural,
        MeasurementId.unsafe("reuse-measurement"),
        injection
      )
    )

  private val matrices = Vector(
    GaleTestMatrix.fromRows(Seq(Seq(0.0, 0.0), Seq(1.0, 0.0), Seq(0.0, 1.0))),
    GaleTestMatrix.fromRows(Seq(Seq(0.1, -0.1), Seq(1.1, 0.1), Seq(-0.1, 0.9)))
  )

  private val fitDesign = right(DesignIdentity(DesignKind.unsafe("reuse-relation")))

  private def source(
      operatorBacked: Boolean,
      preparation: NormalizationIdentity = NormalizationIdentity.none,
      probes: Vector[Probe] = Vector(Probe(), Probe())
  ): PartitionedRelations[
    partitions.Id,
    effects.Id,
    neural.Id,
    AxisKey,
    FeatureId,
    EstimateOnlyCapabilities[neural.Id, FeatureId]
  ] =
    val entries = partitions.keys.zipWithIndex.map: (partition, position) =>
      val id = ValueId.unsafe(s"reuse-estimate-${partition.value}")
      val estimate =
        if operatorBacked then
          right(
            EvidenceTable.operator(
              effects,
              neural,
              CountingOperator(matrices(position), probes(position)),
              id
            )
          )
        else right(EvidenceTable.dense(effects, neural, matrices(position), id))
      val receipt = right(
        RelationFitReceipt(
          ValueIdentity.source(ValueId.unsafe(s"reuse-source-${partition.value}")),
          fitDesign,
          right(Estimability(effects, Vector(true, true, true))),
          preparation,
          training
        )
      )
      PartitionRelation(
        partition,
        right(Relation(estimate, receipt, right(EstimateOnlyCapabilities(neural))))
      )
    right(PartitionedRelations(namedPartitions, effects, neural, entries))

  private def pairingFor(
      evidence: PartitionedRelations[
        partitions.Id,
        effects.Id,
        neural.Id,
        AxisKey,
        FeatureId,
        EstimateOnlyCapabilities[neural.Id, FeatureId]
      ]
  ): PairingDesign[partitions.Id, partitions.Id] =
    right(
      PairingDesign.allOrdered(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(ScientificAxisName.unsafe("runs"), partitions.identity),
        right(
          PartitionIndependenceTestSupport.declareAllPairs(
            evidence.identity,
            namedPartitions,
            "reuse-independent-runs"
          )
        )
      )
    )

  private def fitFor(
      evidence: PartitionedRelations[
        partitions.Id,
        effects.Id,
        neural.Id,
        AxisKey,
        FeatureId,
        EstimateOnlyCapabilities[neural.Id, FeatureId]
      ],
      normalization: RdmNormalization
  ): IdentityPrecisionRelationFit[
    partitions.Id,
    effects.Id,
    measurement.local.Id,
    AxisKey,
    FeatureId
  ] =
    val domain = right(WithinPairDomain(evidence.effects))
    val query = right(
      RelationalFitQuery.identityPrecision(evidence, domain, normalization)
    )
    right(RelationalFit.query(query, pairingFor(evidence), measurement))

  test("one reduced relation fit answers RDM, distance, and RSA without reopening evidence"):
    val probes = Vector(Probe(), Probe())
    val evidence = source(operatorBacked = true, probes = probes)
    val fit = fitFor(evidence, RdmNormalization.Raw)
    val applicationsAfterFit = probes.map(_.transposeApplications)
    val rdm = right(fit.rdm)
    val distance = right(fit.distance)
    val model = right(
      SecondOrderModel.signal(
        fit.definition.domain,
        SecondOrderModelName.unsafe("geometry"),
        rdm.distances.toVector
      )
    )
    val rsa = right(RelationalRsa.rank(fit, model))

    assertEquals(applicationsAfterFit, Vector(3, 3))
    assertEquals(probes.map(_.transposeApplications), applicationsAfterFit)
    assertEquals(rdm.distances.toVector, distance.rdm.distances.toVector)
    assertEqualsDouble(rsa.correlation, 1.0, 1e-12)
    assertEquals(rsa.fit, fit.identity)
    assertEquals(fit.computation.reducer, PairingReducer.WeightedMean)

  test("representation changes receipts but not compatible fit identity"):
    val operatorSource = source(operatorBacked = true)
    val denseSource = source(operatorBacked = false)
    val operatorFit = fitFor(operatorSource, RdmNormalization.Raw)
    val denseFit = fitFor(denseSource, RdmNormalization.Raw)

    assertEquals(operatorFit.sourceIdentity, denseFit.sourceIdentity)
    assertEquals(operatorFit.identity, denseFit.identity)
    assertEquals(right(operatorFit.rdm).distances.toVector, right(denseFit.rdm).distances.toVector)
    assertNotEquals(
      operatorFit.computation.projections.map(_.sourceRepresentation),
      denseFit.computation.projections.map(_.sourceRepresentation)
    )

  test("changed preparation or normalization forces a distinct fit identity"):
    val step = right(
      NormalizationStepIdentity(
        NormalizationStepId.unsafe("run-demean"),
        Vector("scope" -> "training-run")
      )
    )
    val baselineSource = source(operatorBacked = false)
    val preparedSource = source(
      operatorBacked = false,
      preparation = NormalizationIdentity(Vector(step))
    )
    val baseline = fitFor(baselineSource, RdmNormalization.Raw)
    val prepared = fitFor(preparedSource, RdmNormalization.Raw)
    val normalized = fitFor(
      baselineSource,
      RdmNormalization.DivideByNeuralDimension
    )

    assertNotEquals(baseline.sourceIdentity, prepared.sourceIdentity)
    assertNotEquals(baseline.identity, prepared.identity)
    assertNotEquals(baseline.identity, normalized.identity)

  private final class Probe:
    var forwardApplications: Int = 0
    var transposeApplications: Int = 0

  private object Probe:
    def apply(): Probe = new Probe

  private final class CountingOperator(matrix: DMat, probe: Probe) extends DoubleLinearOperator:
    override def rows: Int = matrix.rows
    override def cols: Int = matrix.cols

    override def applyTo(input: DVec, output: MutableDVec): Unit =
      probe.forwardApplications += 1
      var row = 0
      while row < rows do
        var sum = 0.0
        var column = 0
        while column < cols do
          sum += matrix(row, column) * input(column)
          column += 1
        output(row) = sum
        row += 1

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      probe.transposeApplications += 1
      var column = 0
      while column < cols do
        var sum = 0.0
        var row = 0
        while row < rows do
          sum += matrix(row, column) * input(row)
          row += 1
        output(column) = sum
        column += 1

  private object CountingOperator:
    def apply(matrix: DMat, probe: Probe): CountingOperator =
      new CountingOperator(matrix, probe)
