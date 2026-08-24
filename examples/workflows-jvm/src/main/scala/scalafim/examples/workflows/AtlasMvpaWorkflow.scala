package scalafim.examples.workflows

import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.spatial.SpatialFeatureSetPlans
import scalafim.image.*
import ravel.DType.given

final case class AtlasMvpaRegionResult(
  regionId: Int,
  label: String,
  nFeatures: Int,
  accuracy: Double,
  testedSamples: Int
)

object AtlasMvpaWorkflows:
  private val dims: Vector[Int] =
    Vector(4, 4, 2)

  private val sampleLabels: Vector[String] =
    Vector("face", "scene", "face", "scene", "face", "scene", "face", "scene")

  private val blocks: Vector[Int] =
    Vector(1, 1, 2, 2, 3, 3, 4, 4)

  def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(ref, regions, labelVolume())

  def featurePlan(): FeatureSetPlan =
    orThrow(SpatialFeatureSetPlans.fromVolumeAtlas("workflow-atlas-regions", atlas()))

  def response(): Response =
    orThrow(Response.categorical(sampleLabels))

  def folds(): FoldPlan =
    orThrow(FoldPlan.leaveOneBlockOut(blocks))

  def patterns(): PatternMatrix =
    val labels = labelData()
    val rows =
      sampleLabels.zipWithIndex.map { case (label, sample) =>
        val sign = if label == "face" then 1.0 else -1.0
        Vector.tabulate(labels.length) { feature =>
          labels(feature) match
            case 1 => sign * (2.0 + sample.toDouble * 0.01)
            case 2 => sign * (1.5 + sample.toDouble * 0.01)
            case 3 => sign * (1.0 + sample.toDouble * 0.01)
            case _ => 0.0
        }
      }
    PatternMatrix.fromRows(rows)

  def runClassification(): Vector[AtlasMvpaRegionResult] =
    val plan = featurePlan()
    val byId = plan.featureSets.map(featureSet => featureSet.id.value -> featureSet).toMap
    val result =
      orThrow(
        MvpaEngine.run(
          patterns(),
          plan,
          response(),
          CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()),
          Some(folds())
        )
      )

    if result.failures.nonEmpty then
      val messages = result.failures.map(failure => s"${failure.id.value}: ${failure.error.message}").mkString("; ")
      throw new IllegalStateException(s"workflow produced failed ROIs: $messages")

    result.successes.sortBy(_.roiId.value).map { success =>
      val featureSet = byId(success.roiId.value)
      AtlasMvpaRegionResult(
        regionId = success.roiId.value,
        label = featureSet.label.getOrElse(success.roiId.value.toString),
        nFeatures = featureSet.size,
        accuracy = success.metrics("Accuracy").getOrElse(Double.NaN),
        testedSamples = success.metrics("TestedSamples").getOrElse(Double.NaN).toInt
      )
    }

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
        AtlasRegionMetadata(RegionId(1), "Visual", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("Visual"))),
        AtlasRegionMetadata(RegionId(2), "Somatomotor", hemisphere = Some(Hemisphere.Right), network = Some(NetworkId("Somatomotor"))),
        AtlasRegionMetadata(RegionId(3), "Default", hemisphere = Some(Hemisphere.Bilateral), network = Some(NetworkId("Default")))
      )
    )

  private def space: SomeSampleSpace =
    SampleSpaces(
      dims = dims,
      spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )

  private def labelVolume(): SomeLabelVolume[Int] =
    SomeLabelVolume.unsafeCopyFromCanonicalArray(labelData(), space, "workflow-labels")

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

  private def orThrow[A](result: Either[MvpaError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

@main def runAtlasMvpaWorkflow(): Unit =
  println("regionId\tlabel\tnFeatures\taccuracy\ttestedSamples")
  AtlasMvpaWorkflows.runClassification().foreach { row =>
    println(s"${row.regionId}\t${row.label}\t${row.nFeatures}\t${row.accuracy}\t${row.testedSamples}")
  }
