package scalafim.spatial

import ravel.NDArray as RavelArray
import scalafim.image.{SampleSpaces, DMat, DenseFieldMorphism, GridSpec, SomeSampleSpace, Resample, SpatialDomainId}
import scalafim.image.SampleSpaces.*
import scalafim.surface.*

class MixedPullbackSuite extends munit.FunSuite:

  private val volumeSpace = SampleSpaces(Vector(6, 1, 1), trans = Some(DMat.eye(4)))

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def imageValue[A](result: Either[scalafim.image.MorphismError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volumeDomain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(volumeSpace))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(name: String, geometry: SurfaceGeometry): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val sampled = spatialValue(SamplingGeometry.surface(geometry))
    spatialValue(Domain.build(id, SpaceRef.Surface(subject, geometry.hemisphere, geometry.kind), sampled))

  private def surfaceAt(xs: Vector[Double], kind: SurfaceKind): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(xs(0), 0.0, 0.0),
          Vector(xs(1), 0.0, 0.0),
          Vector(xs(2), 0.0, 0.0)
        ),
        Vector((0, 1, 2))
      ),
      Hemisphere.Left,
      kind
    )

  private def affine(name: String, source: Domain, target: Domain, x: Double): Morphism =
    val matrix =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, x),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.Affine3D,
        RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(matrix))
      )
    )

  private def warp(name: String, source: Domain, target: Domain, sourceX: Vector[Double]): Morphism =
    val grid = GridSpec.identity(Vector(6, 1, 1))
    val data =
      RavelArray.tabulate[Double](
        grid.shape.x,
        grid.shape.y,
        grid.shape.z,
        3
      ) { (x, y, z, component) =>
        component match
          case 0 => sourceX(x)
          case 1 => y.toDouble
          case _ => z.toDouble
      }
    val dense =
      imageValue(
        DenseFieldMorphism.coordinates(
          SpatialDomainId(source.id.value),
          SpatialDomainId(target.id.value),
          grid,
          data,
          interpolation = Resample.Method.Linear
        )
      )
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.Warp3D,
        RouteTag.Anatomical,
        coordinateMap = spatialValue(CoordinateMap.dense3D(dense))
      )
    )

  private def volumeToSurface(
    name: String,
    source: Domain,
    target: Domain,
    plan: VolumeSurfaceSamplingPlan
  ): Morphism =
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.VolumeToSurface,
        RouteTag.Anatomical,
        inverse = Inverse.AdjointOnly,
        coordinateMap = CoordinateMap.volumeSamples(plan)
      )
    )

  private def surfaceToSurface(
    name: String,
    source: Domain,
    target: Domain,
    mapping: SurfaceVertexMapping
  ): Morphism =
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        MorphismKind.SurfaceToSurface,
        RouteTag.Anatomical,
        inverse = Inverse.AdjointOnly,
        coordinateMap = CoordinateMap.surfaceVertices(mapping)
      )
    )

  private def rootValues: DoubleMatrix =
    DoubleMatrix.fromRows(Vector.tabulate(6)(index => Vector(index.toDouble * 10.0)))

  test("affine-to-ribbon-to-surface routes compile selected vertices into one root operator"):
    val root = volumeDomain("root")
    val anatomical = volumeDomain("anatomical")
    val white = surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.White)
    val pial = surfaceAt(Vector(2.0, 3.0, 4.0), SurfaceKind.Pial)
    val anchor = surfaceDomain("anchor", white)
    val finalGeometry = surfaceAt(Vector(10.0, 11.0, 12.0), SurfaceKind.Inflated)
    val target = surfaceDomain("target", finalGeometry)
    val plan =
      VolumeSurfaceSamplingPlan(
        SurfaceGeometryPair(white, pial),
        SurfaceSamplingPath.FractionalThickness(Vector(0.0, 1.0))
      )
    val mapping =
      SurfaceVertexMapping.nearestIndex(
        white,
        finalGeometry,
        Vector(VertexId(2), VertexId(0), VertexId(1))
      )
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(root, anatomical, anchor, target),
        Vector(
          affine("root-anatomical", root, anatomical, 0.5),
          volumeToSurface("anatomical-anchor", anatomical, anchor, plan),
          surfaceToSurface("anchor-target", anchor, target, mapping)
        )
      )
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val view = apiValue(
      Field
        .fromMatrix(root.id, rootValues, "root-values")
        .to(anatomical)
        .flatMap(_.to(anchor))
        .flatMap(_.to(target))
        .flatMap(_.vertices(2, 0))
    )

    val result = apiValue(view.value)

    assertEquals(result.toRows, Vector(Vector(25.0), Vector(35.0)))
    assertEquals(view.plan.steps.length, 4)
    val trace = runtime.lastTrace.getOrElse(fail("expected mixed evaluation trace"))
    assertEquals(trace.operator.recipe.path.length, 3)
    assertEquals(trace.operator.recipe.rowSelection.roi, Some(Vector(2, 0)))
    assertEquals(trace.operator.recipe.compiler, "mixed-pullback-fused-v1")
    assertEquals(trace.valueResamplingPasses, 1)
    assertEquals(trace.support.sourceRows, Vector(1, 2, 3, 4, 5))
    val triplets = trace.operator.shape
    assertEquals(triplets.rows, 2)
    assertEquals(triplets.cols, 6)

  test("nonlinear-to-surface chains pull sample points through the warp before root sampling"):
    val root = volumeDomain("root")
    val warped = volumeDomain("warped")
    val white = surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.White)
    val pair = SurfaceGeometryPair(white, surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.Pial))
    val target = surfaceDomain("surface", white)
    val transform = warp("root-warped", root, warped, Vector(0.0, 0.5, 1.5, 3.0, 4.0, 5.0))
    val bridge =
      volumeToSurface(
        "warped-surface",
        warped,
        target,
        VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.White)
      )
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root, warped, target), Vector(transform, bridge)))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val view = apiValue(Field.fromMatrix(root.id, rootValues, "root-values").to(warped).flatMap(_.to(target)))

    val result = apiValue(view.value)

    assertEquals(result.toRows, Vector(Vector(0.0), Vector(5.0), Vector(15.0)))
    val trace = runtime.lastTrace.getOrElse(fail("expected nonlinear mixed trace"))
    assertEquals(trace.operator.recipe.compiler, "mixed-pullback-fused-v1")
    assertEquals(trace.valueResamplingPasses, 1)
    assertEquals(trace.support.sourceRows, Vector(0, 1, 2))

  test("surface-root routes compose vertex maps and target selection into one operator"):
    val sourceGeometry = surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.White)
    val targetGeometry = surfaceAt(Vector(10.0, 11.0, 12.0), SurfaceKind.Inflated)
    val source = surfaceDomain("source-surface", sourceGeometry)
    val target = surfaceDomain("target-surface", targetGeometry)
    val mapping =
      SurfaceVertexMapping.nearestIndex(
        sourceGeometry,
        targetGeometry,
        Vector(VertexId(2), VertexId(0), VertexId(1))
      )
    given SpatialGraph = spatialValue(
      SpatialGraph.build(
        Vector(source, target),
        Vector(surfaceToSurface("surface-map", source, target, mapping))
      )
    )
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val view = apiValue(
      Field
        .fromMatrix(source.id, DoubleMatrix.fromRows(Vector(Vector(10.0), Vector(20.0), Vector(30.0))))
        .to(target)
        .flatMap(_.vertices(1, 0))
    )

    assertEquals(apiValue(view.value).toRows, Vector(Vector(10.0), Vector(30.0)))
    assertEquals(runtime.lastTrace.map(_.support.sourceRows), Some(Vector(0, 2)))
    assertEquals(runtime.lastTrace.map(_.operator.recipe.compiler), Some("mixed-pullback-fused-v1"))

  test("surface payloads reject mesh mismatches before compilation"):
    val volume = volumeDomain("volume")
    val white = surfaceAt(Vector(0.0, 1.0, 2.0), SurfaceKind.White)
    val target = surfaceDomain("surface", white)
    val otherWhite = surfaceAt(Vector(1.0, 2.0, 3.0), SurfaceKind.White)
    val badPlan =
      VolumeSurfaceSamplingPlan(
        SurfaceGeometryPair(otherWhite, surfaceAt(Vector(1.0, 2.0, 3.0), SurfaceKind.Pial)),
        SurfaceSamplingPath.White
      )
    val result =
      Morphism.between(
        spatialValue(MorphismId("bad-bridge")),
        volume,
        target,
        MorphismKind.VolumeToSurface,
        RouteTag.Anatomical,
        coordinateMap = CoordinateMap.volumeSamples(badPlan)
      )

    assertEquals(
      result.left.toOption,
      Some(SpatialError.SurfaceSamplingGeometryMismatch(spatialValue(MorphismId("bad-bridge"))))
    )

    val otherTargetGeometry = surfaceAt(Vector(2.0, 3.0, 4.0), SurfaceKind.Inflated)
    val surfaceSource = surfaceDomain("surface-source", white)
    val surfaceTarget = surfaceDomain("surface-target", white)
    val badMapping =
      SurfaceVertexMapping.nearestIndex(
        white,
        otherTargetGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2))
      )
    val badSurface =
      Morphism.between(
        spatialValue(MorphismId("bad-surface")),
        surfaceSource,
        surfaceTarget,
        MorphismKind.SurfaceToSurface,
        RouteTag.Anatomical,
        coordinateMap = CoordinateMap.surfaceVertices(badMapping)
      )
    assertEquals(
      badSurface.left.toOption,
      Some(SpatialError.SurfaceMappingGeometryMismatch(spatialValue(MorphismId("bad-surface"))))
    )
