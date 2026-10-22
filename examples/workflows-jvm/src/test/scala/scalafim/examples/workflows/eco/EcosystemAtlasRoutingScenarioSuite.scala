package scalafim.examples.workflows.eco

import image4s.{ImageMetadata, NonSpatialAxes, SampleSpace}
import image4s.geometry.{Affine, D3, Grid, GridId}
import neuroatlas4s.*
import neuroim4s.image.*
import scalafim.scenarios.ScenarioHarness

/** Synthetic catalog and label data exercise the real MNI305-to-MNI152 route. */
class EcosystemAtlasRoutingScenarioSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  test("catalog, transformed query, realization and parcel reduction compose"):
    val spec = AtlasSpec("synthetic-routes", "Synthetic routes", "synthetic", SpaceId.MNI152,
      AtlasRepresentation.Volume, aliases = Vector("Synthetic Route Atlas"))
    val registry = AtlasRegistry.empty.register(spec)
    val found = right(registry.find("  SYNTHETIC-route atlas  "))
    val volumeSpace = right(SpaceId.asVolume(found.defaultSpace))
    val ref = AtlasRef.volume(found.family, found.id, volumeSpace, volumeSpace, confidence = Confidence.Exact)
    val regions = RegionIndex(Vector(AtlasRegionMetadata(RegionId(50), "second"), AtlasRegionMetadata(RegionId(10), "first")))
    val provenance = AtlasProvenance.fromRef(ref, regions)
    val frame = right(TemplateCatalog.standard.frame(found.defaultSpace))
    val affine = right(Affine.fromRowMajor[D3](Vector(
      1.0, 0.0, 0.0, 10.6941,
      0.0, 1.0, 0.0, -18.4064,
      0.0, 0.0, 1.0, 36.1385,
      0.0, 0.0, 0.0, 1.0)))
    // Atlas realization persists its ambient domain; admit an explicit grid ID
    // in the catalog frame rather than supplying an ephemeral GridSpec.
    val gridId = right(GridId.parse("eco-synthetic-route-grid"))
    val grid = right(Grid.createPersistent(gridId, frame)(Vector(3, 1, 1), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val labels = right(NeuroVolume.copyCategoricalFromCanonicalArray(space, Array(10, 0, 50), ImageMetadata.named("synthetic labels")))
    val atlas = right(VolumeAtlas.fromLabelVolumeEither(ref, regions, SomeNeuroVolume.eraseSpace(labels), provenance))
    val dataSpace = SampleSpace.create(atlas.realization.domain.grid, NonSpatialAxes.empty)
    val data = right(NeuroVolume.copyContinuousFromCanonicalArray(dataSpace, Array(5.0, 999.0, 9.0), ImageMetadata.named("synthetic values")))
    val reduced = right(AtlasReduce.summarizeVolumeEither(atlas, SomeNeuroVolume.eraseSpace(data)))
    val input = Point3D(10.0, -20.0, 35.0)
    val hit = right(AtlasQuery.exactEither(atlas, input, SpaceId.MNI305))
    val background = right(AtlasQuery.exactEither(atlas, Point3D(11.6941, -18.4064, 36.1385), SpaceId.MNI152))
    val expected = Vector(10.6941, -18.4064, 36.1385)
    val mapped = hit.atlasPoint.toVector
    def close(actual: Double, expected: Double): Boolean =
      actual.isFinite && math.abs(actual - expected) <= 1e-12
    val mappedCorrectly = mapped.size == expected.size && mapped.zip(expected).forall(close)
    val moved = mapped.zip(input.toVector).exists((actual, original) => !close(actual, original))
    val parcelValues = reduced.values.map(_.value)
    val parcelValuesCorrect = parcelValues.size == 2 && parcelValues.zip(Vector(9.0, 5.0)).forall(close)
    val foreignGrid = right(GridSpec.in(frame)(SpatialDims(3, 1, 1), affine))
    val foreignSpace = SampleSpace.create(foreignGrid.grid, NonSpatialAxes.empty)
    val foreignData = right(NeuroVolume.copyContinuousFromCanonicalArray(foreignSpace, Array(5.0, 999.0, 9.0), ImageMetadata.named("foreign grid")))
    val foreignRef = ref.copy(family = "foreign")
    val foreignAtlas = right(VolumeAtlas.fromLabelVolumeEither(foreignRef, regions, SomeNeuroVolume.eraseSpace(labels), AtlasProvenance.fromRef(foreignRef, regions)))
    val planned = right(VolumeTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym))
    val result = ScenarioHarness.result("ecosystem-atlas-routing", Vector(
      ScenarioHarness.fact("catalog", found == spec && registry.find("missing atlas").isLeft && atlas.ref == ref, found.toString),
      ScenarioHarness.fact("routed-query", mappedCorrectly && moved && hit.id.contains(RegionId(10)), s"mapped=$mapped hit=${hit.id}"),
      ScenarioHarness.fact("background", background.id.isEmpty, background.toString),
      ScenarioHarness.fact("parcel-reduction", reduced.values.map(_.region.id) == Vector(RegionId(50), RegionId(10)) && parcelValuesCorrect, reduced.values.toString),
      ScenarioHarness.fact("alignment-provenance", atlas.realization.assignmentAlignedTo(atlas.realization.parcelDomain).isRight && atlas.provenance == provenance && provenance.identity.family == found.family, provenance.identity.toString),
      ScenarioHarness.fact("refusals", AtlasReduce.summarizeVolumeEither(atlas, SomeNeuroVolume.eraseSpace(foreignData)).isLeft && atlas.realization.assignmentAlignedTo(foreignAtlas.realization.parcelDomain).isLeft && planned.transform(Vector.empty).isLeft, "foreign live grid, foreign parcel identity, planned route")))
    println(result.render)
    assert(result.ciPass, result.render)
