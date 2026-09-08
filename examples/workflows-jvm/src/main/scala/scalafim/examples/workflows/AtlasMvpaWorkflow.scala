package scalafim.examples.workflows

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.*
import resample4s.designs.LeaveOneGroupOut
import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.RelationalAnalysis.given
import scalafim.fmri.mvpa.predictive.*
import scalafim.fmri.mvpa.predictive.PredictiveAnalysis.given
import scalafim.image.*
import scalafim.locus.SpaceKey

final case class AtlasMvpaRegionResult(
    regionId: Int,
    label: String,
    nFeatures: Int,
    accuracy: Double,
    testedSamples: Int
)

final case class AtlasMvpaRelationResult(
    regionId: Int,
    label: String,
    nFeatures: Int,
    faceSceneDistance: Double,
    measurement: MeasurementIdentity,
    fit: ScientificComponentFingerprint
)

object AtlasMvpaWorkflows:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def admitted[E, A](value: Either[E, A]): A =
    value.fold(
      error => throw new IllegalArgumentException(error.toString),
      identity
    )

  private val dims: Vector[Int] =
    Vector(4, 4, 2)

  private val sampleLabels: Vector[String] =
    Vector("face", "scene", "face", "scene", "face", "scene", "face", "scene")

  private val blocks: Vector[Int] =
    Vector(1, 1, 2, 2, 3, 3, 4, 4)

  private[workflows] val packedDomain =
    VolumeDomain.semantic(
      admitted(SpaceKey.make("atlas-mvpa-workflow-volume")),
      VolumeSpace(space)
    )
  private[workflows] type Voxel = packedDomain.S
  private[workflows] val domain: VolumeDomain[Voxel] = packedDomain.value
  private[workflows] val neural =
    SpatialAxes
      .volume(domain)
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )
  private val samples =
    admitted(
      AxisRef.create(
        admitted(AxisId("atlas-workflow-samples")),
        AxisPurpose.Samples,
        Vector.tabulate(sampleLabels.length)(index => admitted(SampleId(s"run-${blocks(index)}-trial-$index"))),
        admitted(CoordinateBasis("run-trial-order")),
        None,
        AxisScale.nominal,
        admitted(CoordinateProvenance("atlas-workflow", "v1"))
      )
    )
  private val face = admitted(ClassId("face"))
  private val scene = admitted(ClassId("scene"))
  private val classes =
    admitted(
      ClassAxis.create(
        admitted(AxisId("atlas-workflow-classes")),
        Vector(face, scene),
        admitted(CoordinateProvenance("atlas-workflow", "v1"))
      )
    )

  private object RelationalInput:
    val faceEffect = admitted(AxisKey("face"))
    val sceneEffect = admitted(AxisKey("scene"))
    val effects =
      admitted(
        AxisRef.create(
          admitted(AxisId("atlas-workflow-effects")),
          AxisPurpose.Effects,
          Vector(faceEffect, sceneEffect),
          admitted(CoordinateBasis("condition-order")),
          None,
          AxisScale.nominal,
          admitted(CoordinateProvenance("atlas-workflow", "v1", Vector("runwise-condition-means")))
        )
      )
    val runKeys =
      blocks.distinct.map(run => admitted(PartitionId(s"run-$run")))
    val runAxis =
      admitted(
        AxisRef.create(
          admitted(AxisId("atlas-workflow-runs")),
          AxisPurpose.Partitions,
          runKeys,
          admitted(CoordinateBasis("run-order")),
          None,
          AxisScale.nominal,
          admitted(CoordinateProvenance("atlas-workflow", "v1"))
        )
      )
    val runAxisName = admitted(ScientificAxisName("runs"))
    val partitions = admitted(PartitionAxis(runAxisName, runAxis))
    val relationDesign =
      admitted(
        DesignIdentity(
          admitted(DesignKind("runwise-condition-means"))
        )
      )
    val capabilities = admitted(EstimateOnlyCapabilities(neural.features))
    val source =
      val labels = labelData()
      val relations = runKeys.zipWithIndex.map: (partition, runPosition) =>
        val run = blocks.distinct(runPosition)
        val samplePositions = blocks.zipWithIndex.collect:
          case (`run`, samplePosition) => samplePosition
        val faceSample =
          samplePositions
            .find(sampleLabels(_) == "face")
            .getOrElse(
              throw new IllegalStateException(s"run $run has no face example")
            )
        val sceneSample =
          samplePositions
            .find(sampleLabels(_) == "scene")
            .getOrElse(
              throw new IllegalStateException(s"run $run has no scene example")
            )
        val estimate =
          admitted(
            EvidenceTable.dense(
              effects,
              neural.features,
              matrix(
                Vector(
                  pattern(faceSample, labels),
                  pattern(sceneSample, labels)
                )
              ),
              admitted(ValueId(s"atlas-workflow-relation-run-$run"))
            )
          )
        val runSamples =
          admitted(
            AxisRef.create(
              admitted(AxisId(s"atlas-workflow-run-$run-samples")),
              AxisPurpose.Samples,
              samplePositions.map(samples.keys),
              admitted(CoordinateBasis("run-trial-order")),
              None,
              AxisScale.nominal,
              admitted(CoordinateProvenance("atlas-workflow", "v1"))
            )
          )
        val receipt =
          admitted(
            RelationFitReceipt(
              ValueIdentity.source(
                admitted(ValueId(s"atlas-workflow-source-run-$run"))
              ),
              relationDesign,
              admitted(Estimability(effects, Vector(true, true))),
              NormalizationIdentity.none,
              runSamples
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
          neural.features,
          relations
        )
      )
    val pairing =
      val partitionEvidence =
        admitted(PartitionEvidenceIdentity(source.identity, partitions))
      val independentPairs =
        for
          left <- runKeys.indices.toVector
          right <- (left + 1 until runKeys.length).toVector
        yield DeclaredIndependentPair(runKeys(left), runKeys(right))
      val independence =
        admitted(
          PartitionIndependenceDeclaration(
            admitted(IndependenceDeclarationId("independent-runs")),
            partitionEvidence,
            partitionEvidence,
            independentPairs
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

  def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, labelVolume(), label = "workflow-atlas")

  private[workflows] val frame =
    VolumeFrames
      .atlas(neural, domain, atlas())
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def evidence =
    val labels = labelData()
    val rows = sampleLabels.indices.map(sample => pattern(sample, labels)).toVector
    admitted(
      EvidenceTable.dense(
        samples,
        neural.features,
        matrix(rows),
        admitted(ValueId("atlas-workflow-patterns"))
      )
    )

  private def target =
    admitted(
      CategoricalTarget(
        samples,
        classes,
        admitted(
          Column(
            samples,
            sampleLabels.map:
              case "face"  => face
              case "scene" => scene
              case other   => throw new IllegalStateException(s"unexpected example class $other")
          )
        )
      )
    )

  private def validation =
    val ordinal =
      LeaveOneGroupOut(
        admitted(
          Labels.dense(IArray.unsafeFromArray(blocks.toArray), samples.size)
        )
      )
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 20260825L)
    val compiled = admitted(
      ordinal.compile(admitted(IndexSpace.of(samples.size)), authority.seed)
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
    val sampleAxis = admitted(ScientificAxisName("samples"))
    admitted(
      ValidationDesign(
        schedule,
        sampleAxis,
        GeneralizationAxis(sampleAxis, samples.identity)
      )
    )

  private def predictiveStrategy =
    admitted(
      ExecutionStrategy(
        admitted(BackendId("atlas-workflow-portable")),
        ExecutionRepresentation.Dense,
        NumericPrecision.Binary64,
        SolverChoice.NotApplicable,
        Vector.empty,
        Scheduling.serial,
        MaterializationPolicy.Allow(admitted(MaterializationBudget(64L))),
        FallbackPolicy.forbidden,
        ResultDelivery.Collected
      )
    )

  private def relationalStrategy =
    admitted(
      ExecutionStrategy(
        admitted(BackendId("atlas-workflow-portable")),
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

  def runClassification(): Vector[AtlasMvpaRegionResult] =
    val source = admitted(CategoricalObservationSource(evidence, target))
    val requested =
      source.classify(
        admitted(
          ClassificationConfiguration(
            SwiftCentroid(PredictorScaling.ZScore)
          )
        )
      )
    val result =
      Mvpa
        .run(source)(validation, frame, requested, predictiveStrategy)
        .fold(error => throw new IllegalStateException(error.message), identity)

    result.values
      .zip(frame.entries)
      .map: (measured, frameEntry) =>
        require(
          measured.measurement == frameEntry.measurement.identity,
          "analysis results must retain measurement-frame order"
        )
        measured.outcome match
          case MeasurementOutcome.Success(estimate, _) =>
            AtlasMvpaRegionResult(
              regionId = measured.rendition.region.id.value,
              label = measured.rendition.region.label,
              nFeatures = frameEntry.measurement.local.size,
              accuracy = estimate.accuracy.value,
              testedSamples = estimate.predictions.samples.size
            )
          case MeasurementOutcome.Rejected(error, _) =>
            throw new IllegalStateException(
              s"measurement '${measured.measurement.id.value}' was rejected: ${error.message}"
            )
          case MeasurementOutcome.Failed(error, _) =>
            throw new IllegalStateException(
              s"measurement '${measured.measurement.id.value}' failed: ${error.message}"
            )

  def runRelationalRdm(): Vector[AtlasMvpaRelationResult] =
    val requested =
      RelationalAnalysis.identityRdm(
        RelationalInput.source,
        RdmNormalization.DivideByNeuralDimension
      )
    val result =
      Mvpa
        .run(RelationalInput.source)(
          RelationalInput.pairing,
          frame,
          requested,
          relationalStrategy
        )
        .fold(error => throw new IllegalStateException(error.message), identity)

    result.values
      .zip(frame.entries)
      .map: (measured, frameEntry) =>
        measured.outcome match
          case MeasurementOutcome.Success(estimate, _) =>
            AtlasMvpaRelationResult(
              regionId = measured.rendition.region.id.value,
              label = measured.rendition.region.label,
              nFeatures = frameEntry.measurement.local.size,
              faceSceneDistance = admitted(
                estimate.distance(
                  RelationalInput.faceEffect,
                  RelationalInput.sceneEffect
                )
              ),
              measurement = estimate.measurement,
              fit = estimate.fit
            )
          case MeasurementOutcome.Rejected(error, _) =>
            throw new IllegalStateException(
              s"measurement '${measured.measurement.id.value}' was rejected: ${error.message}"
            )
          case MeasurementOutcome.Failed(error, _) =>
            throw new IllegalStateException(
              s"measurement '${measured.measurement.id.value}' failed: ${error.message}"
            )

  private def ref: AtlasRef =
    AtlasRef(
      family = "workflow",
      model = "AtlasMvpaToy",
      representation = AtlasRepresentation.Volume,
      templateSpace = SpaceId.MNI152,
      coordSpace = SpaceId.MNI152,
      resolution = Some("2mm"),
      provenance = Some("scalafim workflow examples"),
      source = Some("synthetic"),
      lineage = Some("Generated in memory to demonstrate atlas-to-MVPA wiring."),
      confidence = Confidence.Exact,
      notes = Some("Three parcels, each with four voxels.")
    )

  private def regions: RegionIndex =
    RegionIndex(
      Vector(
        Region(RegionId(1), "Visual", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("Visual"))),
        Region(
          RegionId(2),
          "Somatomotor",
          hemisphere = Some(Hemisphere.Right),
          network = Some(NetworkId("Somatomotor"))
        ),
        Region(RegionId(3), "Default", hemisphere = Some(Hemisphere.Bilateral), network = Some(NetworkId("Default")))
      )
    )

  private def space: NeuroSpace =
    NeuroSpace(
      dims = dims,
      spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )

  private def labelVolume(): NeuroVol[Int] =
    NeuroVol.fromLinear(labelData(), space, label = "workflow-labels")

  private def labelData(): Array[Int] =
    val out = PrimitiveBuffers.fillConst[Int](dims.product, 0)
    fillBlock(out, 0 to 1, 0 to 1, 0 to 0, 1)
    fillBlock(out, 2 to 3, 0 to 1, 0 to 0, 2)
    fillBlock(out, 1 to 2, 2 to 3, 1 to 1, 3)
    out

  private def fillBlock(out: Array[Int], xs: Range, ys: Range, zs: Range, id: Int): Unit =
    for
      x <- xs
      y <- ys
      z <- zs
    do out(Indexing.gridToIndex3D(dims, x, y, z)) = id

  private def pattern(sample: Int, labels: Array[Int]): Vector[Double] =
    val sign = if sampleLabels(sample) == "face" then 1.0 else -1.0
    Vector.tabulate(labels.length): feature =>
      labels(feature) match
        case 1 => sign * (2.0 + sample.toDouble * 0.01)
        case 2 => sign * (1.5 + sample.toDouble * 0.01)
        case 3 => sign * (1.0 + sample.toDouble * 0.01)
        case _ => 0.0

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    rows match
      case first +: _ =>
        Matrix.tabulate(rows.length, first.length)((row, column) => rows(row)(column))
      case _ => throw new IllegalArgumentException("workflow matrix rows must be non-empty")

@main def runAtlasMvpaWorkflow(): Unit =
  println("classification")
  println("regionId\tlabel\tnFeatures\taccuracy\ttestedSamples")
  AtlasMvpaWorkflows.runClassification().foreach { row =>
    println(s"${row.regionId}\t${row.label}\t${row.nFeatures}\t${row.accuracy}\t${row.testedSamples}")
  }
  println("relational-rdm")
  println("regionId\tlabel\tnFeatures\tfaceSceneDistance")
  AtlasMvpaWorkflows.runRelationalRdm().foreach { row =>
    println(s"${row.regionId}\t${row.label}\t${row.nFeatures}\t${row.faceSceneDistance}")
  }
