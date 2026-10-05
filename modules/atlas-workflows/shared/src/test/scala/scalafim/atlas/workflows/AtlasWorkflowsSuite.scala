package scalafim.atlas.workflows

import image4s.{ImageMetadata, NonSpatialAxes, SampleSpace}
import image4s.geometry.{Affine, D3, Grid, GridId}
import locus4s.data.{Field, VectorField}
import scalafim.atlas.*
import scalafim.connectivity.TimeAxis
import scalafim.image.*
import scalafim.image.world.{FrameCatalog, WorldSpace}

class AtlasWorkflowsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private def grid(space: SpaceId): GridSpec[?] =
    val world = right(TemplateCatalog.standard.world(space))
    val frame = FrameCatalog.frame(world)
    GridSpec.fromGrid(right(Grid.createPersistent(right(GridId.parse(s"workflow-grid-${space.value}")), frame)(
      Vector(4, 1, 1), Affine.identity[D3])))

  private def atlas(): VolumeAtlas =
    val geometry = grid(SpaceId.MNI305)
    val space = SampleSpace.create(geometry.grid, NonSpatialAxes.empty)
    val labels = right(NeuroVolume.copyCategoricalFromCanonicalArray(space, Array(7, 19, 19, 7), ImageMetadata.empty))
    val regions = RegionIndex(Vector(AtlasRegionMetadata.fromStrings(RegionId(19), "B"),
      AtlasRegionMetadata.fromStrings(RegionId(7), "A")))
    VolumeAtlas.fromLabelVolume(AtlasRef.volume("workflow", "fixture", SpaceId.MNI305, SpaceId.MNI305),
      regions, SomeNeuroVolume.eraseSpace(labels))

  test("named masked extraction reaches canonical connectivity nodes with explicit timing"):
    val realization = atlas().realization
    def sample(scale: Double): Field[realization.X, Double] =
      VectorField.tabulate(realization.parcelAssignment.from)(voxel =>
        if voxel.ordinal == 0 then scale else if voxel.ordinal == 3 then 100.0 * scale else -scale)
    val mask = VectorField.tabulate(realization.parcelAssignment.from)(_.ordinal != 3)
    val samples = Vector(1.0, 2.0, 4.0, 8.0).zipWithIndex.map((value, index) => s"time-$index" -> sample(value))
    val batch = right(ParcelWorkflows.batch(realization)(samples, mask = Some(mask)))
    assertEqualsDouble(batch.head._2(realization.parcelPoint(RegionId(7)).get), 1.0, 0.0)
    val timing = right(TimeAxis.fromSeconds(4, 0.8))
    val input = right(ParcelWorkflows.connectivity(realization)(batch.map(_._2), timing))
    assertEquals(input.source, realization.identity)
    assertEquals(input.parcelKeys, realization.parcelDomain.indices.map(realization.parcelKeys.apply).toVector)
    assert(input.series.timeAxis eq timing)
    assertEquals(input.series.nodeAxis.ids.distinct.length, 2)
    val correlation = right(input.correlation)
    assertEqualsDouble(correlation.matrix.values(0, 1), -1.0, 1e-12)
    val single = right(AtlasReduce.reduceField(realization)(samples.head._2, mask = Some(mask)))
    realization.parcelDomain.indices.foreach(point => assertEqualsDouble(batch.head._2(point), single(point), 0.0))

  test("same-sized foreign owners, duplicate batches and timing mismatches are refused"):
    val first = atlas().realization
    val second = atlas().realization
    val foreign = VectorField.tabulate(second.parcelDomain)(_ => 1.0)
      .asInstanceOf[Field[first.P, Double]]
    assertEquals(ParcelWorkflows.connectivity(first)(Vector(foreign), right(TimeAxis.fromSeconds(1, 1.0))),
      Left(ParcelWorkflowError.WrongParcelOwner))
    val own = VectorField.tabulate(first.parcelDomain)(_ => 1.0)
    assert(ParcelWorkflows.connectivity(first)(Vector(own), right(TimeAxis.fromSeconds(2, 1.0))).isLeft)
    val spatial = VectorField.tabulate(first.parcelAssignment.from)(_ => Double.NaN)
    assert(ParcelWorkflows.batch(first)(Vector("x" -> spatial, "x" -> spatial)).isLeft)
    assert(ParcelWorkflows.batch(first)(Vector(" " -> spatial)).isLeft)
    assert(ParcelWorkflows.batch(first)(Vector("x" -> spatial), policy =
      ParcelReductionPolicy(missing = MissingValuePolicy.RejectNaN)).isLeft)
    val wrongSpatial = VectorField.tabulate(second.parcelAssignment.from)(_ => 1.0)
      .asInstanceOf[Field[first.X, Double]]
    assert(ParcelWorkflows.batch(first)(Vector("foreign" -> wrongSpatial)).isLeft)

  test("affine nearest-label transport checks pull direction and returns completed evidence"):
    val source = atlas()
    val target = grid(SpaceId.MNI152)
    val affine = right(Affine.fromRowMajor[D3](Vector(1.0, 0.0, 0.0, 1.0,
      0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)))
    val step = TransformStep(SpaceId.MNI152, SpaceId.MNI305, TransformKind.Affine,
      TransformBackend.InternalAffine, Confidence.Exact, true, Vector.empty, TransformStatus.Available,
      affine = Some(affine))
    val route = right(SpaceTransforms.plan(step.from, step.to, registry = Vector(step)))
    val plan = right(AtlasTransport.prepare(source, target, SpaceId.MNI152, route))
    assertEquals(plan.source, source.realization.identity)
    val completed = right(plan.execute())
    assertEquals(completed.receipt.result, completed.atlas.realization.identity)
    assertEquals(completed.receipt.source, source.realization.identity)
    val labels = completed.atlas.labelVolume
    assertEquals(Vector.tabulate(4)(x => labels.data(x, 0, 0)), Vector(19, 19, 7, 0))
    assertEquals(completed.atlas.ref.coordSpace, SpaceId.MNI152)
    assert(completed.atlas.provenance.derivation.exists:
      case DerivationStep.Resampled(SpaceId.MNI305, SpaceId.MNI152, _, TransformStatus.Available, _) => true
      case _ => false)
    right(completed.atlas.realization.validateNeuropublishProjection(completed.atlas.realization.neuropublishProjection))
    val reverse = right(SpaceTransforms.plan(step.to, step.from, registry = Vector(step)))
    assert(AtlasTransport.prepare(source, target, SpaceId.MNI152, reverse).isLeft)
    assert(AtlasTransport.prepare(source, grid(SpaceId.MNI305), SpaceId.MNI152, route).isLeft)
    val unavailable = right(SpaceTransforms.plan(step.from, step.to,
      registry = Vector(step.copy(status = TransformStatus.Planned, affine = None))))
    assert(AtlasTransport.prepare(source, target, SpaceId.MNI152, unavailable).isLeft)
