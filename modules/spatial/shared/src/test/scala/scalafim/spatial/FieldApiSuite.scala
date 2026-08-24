package scalafim.spatial

import scalafim.image.{SampleSpaces, SomeSampleSpace, SpatialAxis, VoxelCoord}
import scalafim.image.SampleSpaces.*
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, TriangleMesh}

class FieldApiSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(name: String, dims: Vector[Int] = Vector(4, 1, 1)): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(SampleSpaces(dims, affine = Some(ProviderAffines.identity))))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(name: String): Domain =
    val mesh =
      TriangleMesh.fromRows(
        vertices = Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0)
        ),
        faces = Vector((0, 1, 2))
      )
    val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Midthickness)
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val sampling = spatialValue(SamplingGeometry.surface(geometry))
    spatialValue(Domain.build(id, SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.Midthickness), sampling))

  private def affine(name: String, source: Domain, target: Domain): Morphism =
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine(source, target, ProviderAffines.identity))
      )
    )

  private def volumeToSurface(name: String, source: Domain, target: Domain): Morphism =
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.VolumeToSurface,
        routeTag = RouteTag.Anatomical
      )
    )

  test("volume pipelines read as lazy descriptions and retain one root"):
    val root = volumeDomain("root")
    val mid = volumeDomain("mid")
    val target = volumeDomain("target")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, mid, target),
        Vector(affine("root-mid", root, mid), affine("mid-target", mid, target))
      )
    )
    val field = Field.fromMatrix(root.id, DoubleMatrix.zeros(root.nElements, 5), "bold")
    val runtime = RecordingRuntime()
    given FieldRuntime = runtime

    val view = apiValue(
      field
        .to(mid, sampling = SamplingPolicy.Nearest)
        .flatMap(_.in(target))
        .flatMap(_.slice(SpatialAxis.X, 1))
        .flatMap(_.timeBlock(start = 1, length = 2))
    )

    assertEquals(runtime.viewCalls, 0)
    assertEquals(runtime.dataCalls, 0)
    assertEquals(view.rootId, field.rootId)
    assertEquals(view.root, root.id)
    assertEquals(view.domain, target.id)
    assertEquals(view.sampleCount, 1)
    assertEquals(view.observations, 2)
    assertEquals(view.pending, Vector.empty)
    assertEquals(view.plan.steps.length, 4)
    assertEquals(view.plan.intent.rowSelection, RowSelection.Rows(Vector(1)))
    assertEquals(view.plan.intent.observationSelection, ObservationSelection.Indices(Vector(1, 2)))

    val values = apiValue(view.value)
    val materialized = apiValue(view.materialize)

    assertEquals(values.rows, 1)
    assertEquals(values.cols, 2)
    assertNotEquals(materialized.rootId, field.rootId)
    assertEquals(materialized.root, target.id)
    assertEquals(materialized.domain, target.id)
    assert(materialized.isMaterializedRoot)
    assertEquals(materialized.plan.steps, Vector.empty)
    assertEquals(runtime.viewCalls, 0)
    assertEquals(runtime.dataCalls, 2)

  test("volume-to-surface pipelines need no manual compiler or runtime view call"):
    val volume = volumeDomain("native-volume")
    val surface = surfaceDomain("left-surface")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(volume, surface),
        Vector(volumeToSurface("native-to-left", volume, surface))
      )
    )
    val field = Field.fromMatrix(volume.id, DoubleMatrix.zeros(volume.nElements, 3), "bold")

    val cortical = apiValue(field.to(surface).flatMap(_.vertices(2, 0)))

    assertEquals(cortical.rootId, field.rootId)
    assertEquals(cortical.domain, surface.id)
    assertEquals(cortical.sampleCount, 2)
    assertEquals(cortical.plan.intent.rowSelection, RowSelection.Rows(Vector(2, 0)))
    assertEquals(cortical.pending, Vector.empty)

  test("typed selectors report domain, bound, route, and terminal-order failures"):
    val root = volumeDomain("root")
    val target = volumeDomain("target")
    val disconnected = surfaceDomain("disconnected")
    given SpatialGraph = spatialValue(
      SpatialGraph.build(Vector(root, target, disconnected), Vector(affine("root-target", root, target)))
    )
    val field = Field.fromMatrix(root.id, DoubleMatrix.zeros(root.nElements, 2), "bold")
    val targetView = apiValue(field.to(target))
    val selected = apiValue(targetView.rows(3, 1))

    assertEquals(
      targetView.vertices(0).left.toOption,
      Some(
        FieldApiError.Plan(
          ViewPlanError.InvalidDemand(
            DemandError.DomainKindMismatch("vertex", DomainKind.Surface, DomainKind.Volume)
          )
        )
      )
    )
    assertEquals(
      targetView.voxels(VoxelCoord(4, 0, 0)).left.toOption,
      Some(
        FieldApiError.Plan(
          ViewPlanError.InvalidDemand(DemandError.IndexOutOfBounds("voxel", 0, target.nElements))
        )
      )
    )
    assertEquals(
      selected.to(root).left.toOption,
      Some(FieldApiError.Plan(ViewPlanError.SelectionMustBeTerminal(target.id)))
    )
    assertEquals(
      field.to(disconnected).left.toOption,
      Some(FieldApiError.Spatial(SpatialError.NoPath(root.id, disconnected.id)))
    )

  test("materialization validates the terminal runtime result shape"):
    val root = volumeDomain("root")
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root), Vector.empty))
    val field = Field.fromMatrix(root.id, DoubleMatrix.zeros(root.nElements, 2), "bold")
    given FieldRuntime = FixedRuntime(DoubleMatrix.zeros(1, 1))

    assertEquals(
      field.materialize.left.toOption,
      Some(FieldApiError.MaterializedShapeMismatch(root.nElements, 1, 2, 1))
    )

private final class RecordingRuntime extends FieldRuntime:
  var viewCalls: Int = 0
  var dataCalls: Int = 0

  override def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field] =
    viewCalls += 1
    Left(SpatialError.OperatorAssemblyFailed("descriptive API must not call FieldRuntime.view"))

  override def data(field: Field): Either[SpatialError, DoubleMatrix] =
    dataCalls += 1
    Right(DoubleMatrix.zeros(field.sampleCount, field.observations))

private final class FixedRuntime(result: DoubleMatrix) extends FieldRuntime:
  override def view(field: Field, operator: SpatialOperator): Either[SpatialError, Field] =
    Left(SpatialError.OperatorAssemblyFailed("unused"))

  override def data(field: Field): Either[SpatialError, DoubleMatrix] =
    Right(result)
