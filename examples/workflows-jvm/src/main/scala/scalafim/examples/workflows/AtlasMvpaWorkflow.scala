package scalafim.examples.workflows

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.DigestAlgorithm
import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.fmri.mvpa.dataset.predictive.*
import scalafim.fmri.mvpa.spatial.*
import scalafim.image.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}
import ravel.DType.given

final case class AtlasMvpaRegionResult(
    regionId: Int,
    label: String,
    nFeatures: Int,
    accuracy: Double,
    testedSamples: Int,
    featureOrdinals: Vector[Int]
)

object AtlasMvpaWorkflows:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private val dims = Vector(4, 4, 2)
  private val sampleLabels = Vector("face", "scene", "face", "scene", "face", "scene", "face", "scene")
  private val runKeys = Vector("run-1", "run-1", "run-2", "run-2", "run-3", "run-3", "run-4", "run-4")
  def atlas(): VolumeAtlas = VolumeAtlas.fromLabelVolume(ref, regions, labelVolume())

  private lazy val samples = orThrow(AxisRef.fromStableKeys("workflow-samples", SpaceRole.Samples, Vector.tabulate(8)(i => s"trial-$i"), "trial", "none", "one"))
  lazy val neural = orThrow(AxisRef.fromStableKeys("workflow-neural", SpaceRole.Observed, Vector.tabulate(dims.product)(i => s"voxel-$i"), "voxel", "psc", "raw"))
  private lazy val responseAxis = orThrow(AxisRef.fromStableKeys("workflow-response", SpaceRole.Observed, Vector("class"), "class", "none", "code"))
  private lazy val source = orThrow(EvidenceSource(SourceId.unsafe("workflow-source"), Provenance.source(ProvenanceId.unsafe("workflow-root"), SourceId.unsafe("workflow-source"))))
  private lazy val observations = orThrow(Observations.fromDense(samples, neural, DMat.dense(8, dims.product, patternValues), ValueIdentity.source(ValueId.unsafe("workflow-patterns")), source))
  private lazy val targets = orThrow(MultiResponse.fromDense(samples, responseAxis, DMat.dense(8, 1, sampleLabels.map(label => if label == "face" then 0.0 else 1.0)), ValueIdentity.source(ValueId.unsafe("workflow-targets")), source))
  private lazy val mapping = orThrow(NativeAxisMapping.fromAxis(samples, Vector.tabulate(8)(100L + _), DataFingerprint.external("workflow-v1")))
  private lazy val groups = orThrow(Column.fromValues(samples, runKeys, ValueIdentity.source(ValueId.unsafe("workflow-run-groups"))))
  private lazy val validation = orThrow(LeaveOneGroupOutDesign.bind(samples, groups, ScientificSeed.fromLong(23))(identity)).validation
  private lazy val coding = orThrow(SwiftTargetCoding(Vector(0.0 -> "face", 1.0 -> "scene")))

  def measurementFrame(): MeasurementFrame[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] =
    val sites = atlas().regions.regions.map: metadata =>
      val support = atlas().realization.region(metadata.id).getOrElse(throw new IllegalStateException(s"missing ${metadata.id.value}"))
      val region = orThrow(locus4s.Region.fromOrdinals(neural.locus, support.ordinalsInDomainOrder.toVector))
      SpatialMeasurementSite(MeasurementId.unsafe(s"region-${metadata.id.value}"), SpatialMeasurementSupport.Regional(region), label = Some(metadata.label))
    orThrow(SpatialMeasurementFrames.regions(neural, MeasurementFrameDeclaration("workflow-atlas", "v1", Vector.empty), sites))

  def runClassification(): Vector[AtlasMvpaRegionResult] =
    val frame = measurementFrame()
    val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, SpatialMeasurementRendition[neural.Locus], AtlasMvpaRegionResult]:
      def visit[L <: multivar.core.SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] { type Local = L }, measured: MeasuredObservations[samples.Id, L]) =
        val rows = orThrow(AlderPredictiveAdmission.nativeMeasurement(observations, entry.measurement, targets, samples.toRecord.stableKeys, DataFingerprint.external("workflow-metadata"), mapping, orThrow(NativeReadPolicy(4, orThrow(MaterializationBudget(10000))))))
        AlderSwiftCentroid.crossValidate(rows, validation, coding)
          .left.map(error => MeasurementFailure.Task(error.toString))
          .map: result =>
            val metadata = atlas().regions.find(entry.rendition.label.get).head
            AtlasMvpaRegionResult(metadata.id.value, metadata.label, entry.rendition.support.ordinalsInDomainOrder.length, result.assessment.accuracy, result.assessment.samples.toInt, entry.rendition.support.ordinalsInDomainOrder.toVector)
    val traversed = frame.traverse(1)(Right(MeasurementResource(observations) {}))(visitor)
    traversed.error.foreach(error => throw new IllegalStateException(error.toString))
    traversed.value.map(_.value.fold(error => throw new IllegalStateException(error.toString), identity)).sortBy(_.regionId)

  private def patternValues =
    val labels = labelData()
    sampleLabels.zipWithIndex.flatMap: (label, sample) =>
      val sign = if label == "face" then 1.0 else -1.0
      Vector.tabulate(labels.length)(feature => labels(feature) match
        case 1 => sign * (2.0 + sample * 0.01)
        case 2 => sign * (1.5 + sample * 0.01)
        case 3 => sign * (1.0 + sample * 0.01)
        case _ => 0.0)
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
  private def regions =
    RegionIndex(Vector(
      AtlasRegionMetadata(RegionId(1), "Visual", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("Visual"))),
      AtlasRegionMetadata(RegionId(2), "Somatomotor", hemisphere = Some(Hemisphere.Right), network = Some(NetworkId("Somatomotor"))),
      AtlasRegionMetadata(RegionId(3), "Default", hemisphere = Some(Hemisphere.Bilateral), network = Some(NetworkId("Default")))
    ))
  private def space = SampleSpaces.regular(dims, VoxelSpacing.unsafe(2.0, 2.0, 2.0), WorldPoint.Origin).fold(error => throw new IllegalArgumentException(error.message), identity)
  private def labelVolume() = SomeLabelVolume.unsafeCopyFromCanonicalArray(labelData(), space, "workflow-labels")
  private def labelData() =
    val out = PrimitiveBuffers.fillConst[Int](dims.product, 0)
    def fill(xs: Range, ys: Range, zs: Range, id: Int): Unit =
      for
        x <- xs
        y <- ys
        z <- zs
      do out(Indexing.gridToIndex3D(dims, x, y, z)) = id
    fill(0 to 1, 0 to 1, 0 to 0, 1)
    fill(2 to 3, 0 to 1, 0 to 0, 2)
    fill(1 to 2, 2 to 3, 1 to 1, 3)
    out
  private def orThrow[A](value: Either[?, A]): A = value.fold(error => throw new IllegalArgumentException(error.toString), identity)

@main def runAtlasMvpaWorkflow(): Unit =
  println("regionId\tlabel\tnFeatures\taccuracy\ttestedSamples")
  AtlasMvpaWorkflows.runClassification().foreach: row =>
    println(s"${row.regionId}\t${row.label}\t${row.nFeatures}\t${row.accuracy}\t${row.testedSamples}")
