package scalafim.fmri.mvpa.spatial

import scalafim.atlas.*
import scalafim.fmri.mvpa.*
import scalafim.image.*
import scalafim.surface.{
  Hemisphere as SurfaceHemisphere,
  LabelInfo,
  LabeledSurface,
  MeshTopology,
  SurfaceGeometry,
  SurfaceKind,
  TriangleMesh,
  VertexId
}

class SpatialFeatureSetPlansSuite extends munit.FunSuite:

  private val meanAnalysis: RoiAnalysis =
    new RoiAnalysis:
      override val name: String = "mean-signal"

      override def evaluate(roi: PatternMatrix, context: RoiContext): Either[MvpaError, RoiAnalysisResult] =
        var sum = 0.0
        var row = 0
        while row < roi.value.rows do
          var col = 0
          while col < roi.value.cols do
            sum += roi.value(row, col)
            col += 1
          row += 1
        Right(RoiAnalysisResult(MetricVector("mean" -> (sum / (roi.value.rows * roi.value.cols)))))

  private def volumeSpace: NeuroSpace =
    NeuroSpace(
      dims = Vector(3, 2, 1),
      spacing = Some(Vector(1.0, 1.0, 1.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )

  private def labelVolume: NeuroVol[Int] =
    NeuroVol.fromLinear(
      NArrayUtil.fromArray(Array(0, 2, 1, 2, 1, 0)),
      volumeSpace
    )

  private def toyVolumeAtlas: VolumeAtlas =
    val regions =
      RegionIndex(
        Vector(
          Region(RegionId(1), "semantic"),
          Region(RegionId(2), "visual")
        )
      )
    val ref =
      AtlasRef(
        family = "toy",
        model = "toy",
        representation = AtlasRepresentation.Volume,
        templateSpace = SpaceId.Custom,
        coordSpace = SpaceId.Custom,
        confidence = Confidence.Exact
      )
    VolumeAtlas.fromLabelVolume(ref, regions, labelVolume, label = "toy")

  private def fourFeaturePatterns: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
        Vector(2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
        Vector(3.0, 4.0, 5.0, 6.0, 7.0, 8.0),
        Vector(4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
      )
    )

  private def response: Response =
    Response.categorical(Vector("a", "a", "b", "b")).toOption.get

  test("volume label maps become regional feature plans with linear voxel ordering") {
    val plan = SpatialFeatureSetPlans.fromVolumeLabels("volume-labels", labelVolume).toOption.get

    assertEquals(plan.kind, FeatureSetKind.Region)
    assertEquals(plan.featureSets.map(_.id.value), Vector(1, 2))
    assertEquals(plan.featureSets.map(_.label), Vector(Some("1"), Some("2")))
    assertEquals(plan.featureSets.head.featureIndices.map(_.value), Vector(2, 4))
    assertEquals(plan.featureSets(1).featureIndices.map(_.value), Vector(1, 3))

    val result = MvpaEngine.run(fourFeaturePatterns, plan, response, meanAnalysis).toOption.get
    assertEquals(result.successes.length, 2)
    assertEquals(result.failures.length, 0)
  }

  test("volume atlas plans preserve atlas region ids and labels") {
    val plan = SpatialFeatureSetPlans.fromVolumeAtlas("toy-atlas", toyVolumeAtlas).toOption.get

    assertEquals(plan.kind, FeatureSetKind.Region)
    assertEquals(plan.featureSets.map(_.id.value), Vector(1, 2))
    assertEquals(plan.featureSets.map(_.label), Vector(Some("semantic"), Some("visual")))
    assertEquals(plan.featureSets.map(_.featureIndices.map(_.value)), Vector(Vector(2, 4), Vector(1, 3)))
  }

  test("ROI windows become searchlight plans with deterministic coordinate ordering") {
    val space =
      NeuroSpace(
        dims = Vector(3, 3, 1),
        spacing = Some(Vector(1.0, 1.0, 1.0)),
        origin = Some(Vector(0.0, 0.0, 0.0))
      )
    val window = Searchlight.sphericalRoi(space, Vector(1, 1, 0), radius = 1.0, fill = 1, mask = None, label = "center")
    val plan = SpatialFeatureSetPlans.fromRoiWindows("window", Vector(window)).toOption.get
    val featureSet = plan.featureSets.head

    assertEquals(plan.kind, FeatureSetKind.Searchlight)
    assertEquals(featureSet.id.value, 4)
    assertEquals(featureSet.center.map(_.value), Some(4))
    assertEquals(featureSet.label, Some("center"))
    assertEquals(featureSet.featureIndices.map(_.value), Vector(3, 1, 4, 7, 5))
  }

  test("searchlight mask plans run through the MVPA engine") {
    val space =
      NeuroSpace(
        dims = Vector(3, 3, 1),
        spacing = Some(Vector(1.0, 1.0, 1.0)),
        origin = Some(Vector(0.0, 0.0, 0.0))
      )
    val plan = SpatialFeatureSetPlans.fromSearchlightMask("mask-search", Mask.all(space), radius = 1.0).toOption.get
    val patterns =
      PatternMatrix.fromRows(
        Vector.tabulate(4) { row =>
          Vector.tabulate(9)(col => row.toDouble + col.toDouble)
        }
      )

    assertEquals(plan.kind, FeatureSetKind.Searchlight)
    assertEquals(plan.featureSets.length, 9)
    assertEquals(plan.featureSets.head.center.map(_.value), Some(0))

    val result = MvpaEngine.run(patterns, plan, response, meanAnalysis).toOption.get
    assertEquals(result.successes.length, 9)
    assertEquals(result.failures.length, 0)
  }

  test("labeled surface parcels become deterministic regional plans") {
    val geometry =
      SurfaceGeometry(
        TriangleMesh.fromRows(
          Vector(
            Vector(0.0, 0.0, 0.0),
            Vector(1.0, 0.0, 0.0),
            Vector(0.0, 1.0, 0.0),
            Vector(1.0, 1.0, 0.0)
          ),
          Vector(
            (0, 1, 2),
            (1, 3, 2)
          )
        ),
        SurfaceHemisphere.Left,
        SurfaceKind.Pial
      )
    val topology = MeshTopology.from(geometry.mesh)
    val labels =
      LabeledSurface.fromIndexed(
        geometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(2, 2, 1, 1),
        Vector(LabelInfo(1, "posterior"), LabelInfo(2, "anterior"))
      )

    val plan = SpatialFeatureSetPlans.fromLabeledSurface("surface-parcels", labels, topology).toOption.get

    assertEquals(plan.kind, FeatureSetKind.Region)
    assertEquals(plan.featureSets.map(_.id.value), Vector(0, 1))
    assertEquals(plan.featureSets.map(_.label), Vector(Some("posterior"), Some("anterior")))
    assertEquals(plan.featureSets.map(_.featureIndices.map(_.value)), Vector(Vector(2, 3), Vector(0, 1)))
  }

  test("adapter validation reports typed MVPA plan errors") {
    val badLabels =
      NeuroVol.fromLinear(
        NArrayUtil.fromArray(Array(0, -1, 1, 1, 0, 0)),
        volumeSpace
      )
    val error = SpatialFeatureSetPlans.fromVolumeLabels("bad", badLabels).swap.toOption.get

    assert(error.message.contains("non-negative"))
  }
