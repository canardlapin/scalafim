package downstream.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.RelationalAnalysis.given
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given

/** Example downstream renderer for a measurement-local relational failure. The caller supplies a typed method renderer;
  * every other field is derived from admitted source, outcome, and receipt values.
  */
object DownstreamFailureRenderer:
  def relational[
      P <: SemanticSpace,
      E <: SemanticSpace,
      N <: SemanticSpace,
      EK,
      NK,
      C <: RelationCapabilities[N, NK],
      A,
      Rejection,
      Failure,
      Rendition
  ](
      source: PartitionedRelations[P, E, N, EK, NK, C],
      partition: PartitionId,
      renderFailure: Failure => String,
      value: MeasurementValue[A, Rejection, Failure, Rendition]
  ): Either[RelationError, Option[String]] =
    source
      .relation(partition)
      .map: relation =>
        value.outcome match
          case MeasurementOutcome.Failed(failure, receipt) =>
            val axis = source.neuralAxis.identity
            val provenance = axis.coordinateProvenance
            Some(
              s"execution stage; axis '${source.neuralAxisName.value}' role '${axis.purpose.value}'; " +
                s"capability '${relation.capabilities.identity.value}'; provenance '${provenance.source}@${provenance.revision}'; " +
                s"measurement '${value.measurement.id.value}'; target '${receipt.actual.label}': ${renderFailure(failure)}"
            )
          case _ => None

private object PublicWorkflowFixtures:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64

  def admitted[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.toString), identity)

  val samples =
    admitted(
      AxisRef.create(
        admitted(AxisId("downstream-workflow-samples")),
        AxisPurpose.Samples,
        Vector.tabulate(4)(position => admitted(SampleId(s"trial-$position"))),
        admitted(CoordinateBasis("trial-order")),
        None,
        AxisScale.nominal,
        admitted(CoordinateProvenance("downstream-workflow", "v1"))
      )
    )
  val neural =
    admitted(
      AxisRef.create(
        admitted(AxisId("downstream-workflow-neural")),
        AxisPurpose.NeuralFeatures,
        Vector(admitted(FeatureId("voxel-x")), admitted(FeatureId("voxel-y"))),
        admitted(CoordinateBasis("voxel-order")),
        Some(admitted(AxisUnits("percent-signal-change"))),
        AxisScale.nominal,
        admitted(CoordinateProvenance("downstream-workflow", "v1"))
      )
    )
  val measurement =
    admitted(
      Measurement.identity(
        neural,
        admitted(MeasurementId("whole-pattern"))
      )
    )
  val frame =
    admitted(
      MeasurementFrame(neural)(
        Vector(MeasurementEntry(measurement, NoRendition))
      )
    )

  val predictiveSource =
    val face = admitted(ClassId("face"))
    val scene = admitted(ClassId("scene"))
    val classes =
      admitted(
        ClassAxis.create(
          admitted(AxisId("downstream-workflow-classes")),
          Vector(face, scene),
          admitted(CoordinateProvenance("downstream-workflow", "v1"))
        )
      )
    val evidence =
      admitted(
        EvidenceTable.dense(
          samples,
          neural,
          matrix(
            Vector(
              Vector(2.0, 1.0),
              Vector(-2.0, -1.0),
              Vector(1.8, 1.1),
              Vector(-1.8, -1.1)
            )
          ),
          admitted(ValueId("downstream-workflow-patterns"))
        )
      )
    val target =
      admitted(
        CategoricalTarget(
          samples,
          classes,
          admitted(Column(samples, Vector(face, scene, face, scene)))
        )
      )
    admitted(CategoricalObservationSource(evidence, target))

  val validation =
    val ordinal =
      LeaveOneGroupOut(
        admitted(
          Labels.dense(
            IArray.unsafeFromArray(Array(1, 1, 2, 2)),
            samples.size
          )
        )
      )
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 20260825L)
    val compiled =
      admitted(
        ordinal.compile(
          admitted(IndexSpace.of(samples.size)),
          authority.seed
        )
      )
    val schedule =
      admitted(
        BoundSchedule(
          compiled,
          samples,
          admitted(AxisPopulationFingerprint.fromAxis(samples)),
          admitted(ScheduleLabels.fromDesign(samples, ordinal)),
          authority
        )
      )
    val sampleAxisName = admitted(ScientificAxisName("samples"))
    admitted(
      ValidationDesign(
        schedule,
        sampleAxisName,
        GeneralizationAxis(sampleAxisName, samples.identity)
      )
    )

  val predictiveStrategy =
    admitted(
      ExecutionStrategy(
        admitted(BackendId("downstream-portable")),
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        MaterializationPolicy.Allow(admitted(MaterializationBudget(32L))),
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      )
    )

  object Relational:
    val face = admitted(AxisKey("face"))
    val scene = admitted(AxisKey("scene"))
    val effects =
      admitted(
        AxisRef.create(
          admitted(AxisId("downstream-workflow-effects")),
          AxisPurpose.Effects,
          Vector(face, scene),
          admitted(CoordinateBasis("condition-order")),
          None,
          AxisScale.nominal,
          admitted(
            CoordinateProvenance(
              "downstream-workflow",
              "v1",
              Vector("runwise-condition-means")
            )
          )
        )
      )
    val runKeys = Vector(admitted(PartitionId("run-1")), admitted(PartitionId("run-2")))
    val runAxis =
      admitted(
        AxisRef.create(
          admitted(AxisId("downstream-workflow-runs")),
          AxisPurpose.Partitions,
          runKeys,
          admitted(CoordinateBasis("run-order")),
          None,
          AxisScale.nominal,
          admitted(CoordinateProvenance("downstream-workflow", "v1"))
        )
      )
    val runAxisName = admitted(ScientificAxisName("runs"))
    val partitions = admitted(PartitionAxis(runAxisName, runAxis))
    private val relationDesign =
      admitted(
        DesignIdentity(
          admitted(DesignKind("downstream-condition-means"))
        )
      )
    private val capabilities = admitted(EstimateOnlyCapabilities(neural))

    def source(
        estimable: Vector[Boolean] = Vector(true, true),
        revision: String = "complete"
    ) =
      val estimates = Vector(
        Vector(Vector(1.0, 0.5), Vector(-1.0, -0.5)),
        Vector(Vector(1.2, 0.4), Vector(-0.8, -0.6))
      )
      val relations = runKeys.zipWithIndex.map: (partition, position) =>
        val estimate =
          admitted(
            EvidenceTable.dense(
              effects,
              neural,
              matrix(estimates(position)),
              admitted(
                ValueId(s"downstream-relation-${partition.value}-$revision")
              )
            )
          )
        val training =
          admitted(
            AxisRef.create(
              admitted(AxisId(s"downstream-${partition.value}-training")),
              AxisPurpose.Samples,
              Vector(
                admitted(SampleId(s"${partition.value}-face")),
                admitted(SampleId(s"${partition.value}-scene"))
              ),
              admitted(CoordinateBasis("condition-order")),
              None,
              AxisScale.nominal,
              admitted(CoordinateProvenance("downstream-workflow", "v1"))
            )
          )
        val receipt =
          admitted(
            RelationFitReceipt(
              ValueIdentity.source(
                admitted(
                  ValueId(s"downstream-source-${partition.value}-$revision")
                )
              ),
              relationDesign,
              admitted(Estimability(effects, estimable)),
              NormalizationIdentity.none,
              training
            )
          )
        PartitionRelation(
          partition,
          admitted(Relation(estimate, receipt, capabilities))
        )
      admitted(
        PartitionedRelations(
          partitions,
          effects,
          neural,
          relations
        )
      )

    def pairing(declaredSource: ScientificSourceIdentity) =
      val evidence =
        admitted(PartitionEvidenceIdentity(declaredSource, partitions))
      val independence =
        admitted(
          PartitionIndependenceDeclaration(
            admitted(IndependenceDeclarationId("downstream-independent-runs")),
            evidence,
            evidence,
            Vector(DeclaredIndependentPair(runKeys(0), runKeys(1)))
          )
        )
      admitted(
        PairingDesign.allOrdered(
          partitions,
          PairingReducer.WeightedMean,
          GeneralizationAxis(runAxisName, runAxis.identity),
          independence
        )
      )

    def provenanceVariant(source: ScientificSourceIdentity) =
      admitted(
        ScientificSourceIdentity(
          admitted(ScientificSourceKind("downstream-relations-provenance-variant")),
          source.axes,
          Vector("revision" -> "foreign-v2")
        )
      )

    val strategy =
      admitted(
        ExecutionStrategy(
          admitted(BackendId("downstream-portable")),
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

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    rows match
      case first +: _ =>
        Matrix.tabulate(rows.length, first.length)((row, column) => rows(row)(column))
      case _ => throw new IllegalArgumentException("fixture matrix rows must be non-empty")

final class PublicWorkflowSuite extends munit.FunSuite:
  import PublicWorkflowFixtures.*

  test("a downstream package runs a real predictive workflow through Mvpa.run"):
    val analysis = predictiveSource.classify(
      admitted(
        ClassificationConfiguration(
          SwiftCentroid(PredictorScaling.ZScore)
        )
      )
    )
    val result = admitted(
      Mvpa.run(predictiveSource)(
        validation,
        frame,
        analysis,
        predictiveStrategy
      )
    )

    assertEquals(result.counts.succeeded, 1)
    val measured = result.values match
      case Vector(value) => value
      case values        => fail(s"expected one predictive result, obtained ${values.length}")
    measured.outcome match
      case MeasurementOutcome.Success(estimate, receipt) =>
        assertEqualsDouble(estimate.accuracy.value, 1.0, 1e-12)
        assertEquals(estimate.predictions.samples.keys, samples.keys)
        assertEquals(estimate.predictions.classes.keys.map(_.value), Vector("face", "scene"))
        assertEquals(receipt.status, MeasurementStatus.Succeeded)
      case other => fail(s"expected predictive success, obtained $other")

  test("the same downstream package runs a real relational workflow through Mvpa.run"):
    val source = Relational.source()
    val analysis =
      RelationalAnalysis.identityRdm(
        source,
        RdmNormalization.DivideByNeuralDimension
      )
    val result = admitted(
      Mvpa.run(source)(
        Relational.pairing(source.identity),
        frame,
        analysis,
        Relational.strategy
      )
    )

    assertEquals(result.counts.succeeded, 1)
    val measured = result.values match
      case Vector(value) => value
      case values        => fail(s"expected one relational result, obtained ${values.length}")
    measured.outcome match
      case MeasurementOutcome.Success(estimate, receipt) =>
        assertEqualsDouble(
          admitted(estimate.distance(Relational.face, Relational.scene)),
          2.5,
          1e-12
        )
        assertEquals(estimate.measurement, measurement.identity)
        assertEquals(receipt.status, MeasurementStatus.Succeeded)
      case other => fail(s"expected relational success, obtained $other")

  test("a real bind rejection retains its stage, partition role, and effect"):
    val source = Relational.source(Vector(true, false), revision = "nonestimable")
    val analysis = RelationalAnalysis.identityRdm(source, RdmNormalization.Raw)
    val result = Mvpa.run(source)(
      Relational.pairing(source.identity),
      frame,
      analysis,
      Relational.strategy
    )

    result match
      case Left(
            error @ MvpaRunError.Binding(
              BindError.EstimandRejected(
                _,
                RelationalBindRejection.NonEstimableEffect(partition, effect),
                explanation
              )
            )
          ) =>
        assertEquals(partition, Relational.runKeys(0))
        assertEquals(effect, "scene")
        assert(explanation.contains("partition 'run-1'"), clue(explanation))
        assert(explanation.contains("estimable 'scene' relation"), clue(explanation))
        assert(error.message.contains("binding stage"), clue(error.message))
      case other => fail(s"expected typed binding rejection, obtained $other")

  test("a local execution failure renders axis, role, capability, and provenance context"):
    val source = Relational.source()
    val foreign = Relational.provenanceVariant(source.identity)
    val analysis = RelationalAnalysis.identityRdm(source, RdmNormalization.Raw)
    val result = admitted(
      Mvpa.run(source)(
        Relational.pairing(foreign),
        frame,
        analysis,
        Relational.strategy
      )
    )

    assertEquals(result.counts.failed, 1)
    val measured = result.values match
      case Vector(value) => value
      case values        => fail(s"expected one failed result, obtained ${values.length}")
    measured.outcome match
      case MeasurementOutcome.Failed(
            RelationalTaskFailure.Fit(
              RelationalFitError.Distance(
                DistanceEstimandError.Independence(
                  PairingDesignError.IndependenceEvidenceMismatch(
                    boundary,
                    expected,
                    actual
                  )
                )
              )
            ),
            receipt
          ) =>
        val rendered = admitted(
          DownstreamFailureRenderer.relational(
            source,
            Relational.runKeys(0),
            analysis.failureMessage,
            measured
          )
        ).getOrElse(fail("expected the downstream renderer to accept a failed outcome"))

        assertEquals(boundary, "left")
        assertEquals(expected, foreign.fingerprint)
        assertEquals(actual, source.identity.fingerprint)
        assertEquals(receipt.status, MeasurementStatus.Failed)
        assert(rendered.contains("execution stage"), clue(rendered))
        assert(rendered.contains("axis 'neural' role 'neural-features'"), clue(rendered))
        assert(rendered.contains("capability '"), clue(rendered))
        assert(rendered.contains("provenance 'downstream-workflow@v1'"), clue(rendered))
        assert(rendered.contains("target 'downstream-portable/sufficient-statistics'"), clue(rendered))
        assert(rendered.contains("independence declaration left evidence"), clue(rendered))
        assert(rendered.contains(expected.value), clue(rendered))
        assert(rendered.contains(actual.value), clue(rendered))
      case other => fail(s"expected typed local execution failure, obtained $other")

  test("a downstream estimate-only relation cannot request a precision capability"):
    val errors = compileErrors("""
      import multivar.core.SemanticSpace
      import scalafim.fmri.mvpa.*

      def incompatible[
          P <: SemanticSpace,
          E <: SemanticSpace,
          N <: SemanticSpace,
          EK,
          NK
      ](
          source: PartitionedRelations[
            P,
            E,
            N,
            EK,
            NK,
            EstimateOnlyCapabilities[N, NK]
          ]
      ) = RelationalAnalysis.crossnobisRdm(source, RdmNormalization.Raw)
    """)

    assert(errors.nonEmpty, clue(errors))
    assert(errors.contains("HasNoisePrecision"), clue(errors))
