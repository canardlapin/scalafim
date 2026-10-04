package scalafim.atlas

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.{GridSpec, SpatialPullbacks}
import scalafim.image.world.{FrameCatalog, TemplateName, WorldSpace}
import scalafim.spatial.{CoordinateMap, DomainKind, MorphismKind, SamplingGeometry}
import scalafim.transform.{TransformProvenance, WorldTransform}

class SpaceTransformGraphSuite extends munit.FunSuite:

  private def value[A](result: Either[AtlasError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    Affine
      .fromRowMajor[D3](Vector(1.0, 0.0, 0.0, x, 0.0, 1.0, 0.0, y, 0.0, 0.0, 1.0, z, 0.0, 0.0, 0.0, 1.0))
      .fold(error => fail(error.message), identity)

  /** A world transform from `from` to `to` whose forward map translates by `shift`, on the catalog's frames. */
  private def linearAsset(from: SpaceId, to: SpaceId, shift: (Double, Double, Double)): TransformAsset =
    val catalog = TemplateCatalog.standard
    val sourceFrame = value(catalog.frame(from))
    val targetFrame = value(catalog.frame(to))
    val (x, y, z) = shift
    val pullback = FramedAffine.betweenFrames[Frame[D3], Frame[D3], D3](targetFrame, sourceFrame)(translation(-x, -y, -z))
    TransformAsset(WorldTransform.Linear(pullback, TransformProvenance.constructed("test translation")), s"test:${from.value}->${to.value}")

  /** A small grid in the world a catalogued space's coordinates live in, on a fresh frame of that world. */
  private def gridIn(space: SpaceId): GridSpec[?] =
    val world = value(TemplateCatalog.standard.world(space))
    GridSpec.in(FrameCatalog.frame(world))(scalafim.image.SpatialDims(2, 2, 2), Affine.identity[D3]).fold(error => fail(error.message), identity)

  private def affineStep(from: SpaceId, to: SpaceId, forward: Affine[D3], reversible: Boolean = true): TransformStep =
    TransformStep(
      from,
      to,
      TransformKind.Affine,
      TransformBackend.InternalAffine,
      Confidence.Exact,
      reversible = reversible,
      dataFiles = Vector.empty,
      TransformStatus.Available,
      affine = Some(forward)
    )

  test("the standard manifest populates one spatial graph over unsampled template domains"):
    val manifestGraph = value(SpaceTransforms.standardGraph)
    assertEquals(manifestGraph.graph.morphisms.length, SpaceTransforms.manifest.length)
    val mni = manifestGraph.graph.domains(TemplateCatalog.domainId(SpaceId.MNI152NLin2009cAsym))
    mni.geometry match
      case SamplingGeometry.Unsampled(kind, _) => assertEquals(kind, DomainKind.Volume)
      case other => fail(s"expected an unsampled template domain, got $other")
    assertEquals(manifestGraph.graph.domains(TemplateCatalog.domainId(SpaceId.FsLR32k)).kind, DomainKind.Surface)

    val kinds = manifestGraph.graph.morphisms.map(_.kind).toSet
    assert(kinds.contains(MorphismKind.VolumeToSurface))
    assert(kinds.contains(MorphismKind.SurfaceToVolume))
    val geometric = manifestGraph.graph.morphisms.count(_.coordinateMap.isInstanceOf[CoordinateMap.Geometric])
    assertEquals(geometric, 2, clue = "only the internal MNI305/MNI152 affines are executable out of the box")

  test("the internal affine route matches FreeSurfer mni152.register.dat"):
    val plan = value(SpaceTransforms.plan(SpaceId.MNI305, SpaceId.MNI152))
    assert(plan.isExecutable)
    assert(!plan.usedInverses)
    val mapped = value(plan.transform(Vector(Point3D(10.0, -20.0, 35.0)))).head
    assertEqualsDouble(mapped.x, 10.694, 0.01)
    assertEqualsDouble(mapped.y, -18.406, 0.01)
    assertEqualsDouble(mapped.z, 36.139, 0.01)
    val back = value(SpaceTransforms.transformCoords(Vector(mapped), SpaceId.MNI152, SpaceId.MNI305)).head
    assertEqualsDouble(back.x, 10.0, 1e-10)
    assertEqualsDouble(back.y, -20.0, 1e-10)
    assertEqualsDouble(back.z, 35.0, 1e-10)

  test("TemplateFlow warps stay planned and typed non-executable until an asset implements them"):
    val planned = value(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym))
    assertEquals(planned.status, TransformStatus.Planned)
    assert(!planned.isExecutable)
    SpaceTransforms.transformCoords(Vector(Point3D.Origin), SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym) match
      case Left(AtlasError.TransformNotExecutable(from, to, reason)) =>
        assertEquals(from, SpaceId.MNI152NLin6Asym)
        assertEquals(to, SpaceId.MNI152NLin2009cAsym)
        assert(reason.contains("unavailable steps"), clue = reason)
      case other => fail(s"expected a non-executable route, got $other")

    val implemented =
      SpaceTransforms.manifest.map { step =>
        if step.from == SpaceId.MNI152NLin6Asym && step.to == SpaceId.MNI152NLin2009cAsym then
          step.withAsset(linearAsset(step.from, step.to, (1.0, -2.0, 0.5)))
        else step
      }
    val executable = value(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, registry = implemented))
    assertEquals(executable.status, TransformStatus.Available)
    assert(executable.isExecutable, clue = executable.executability.toString)
    val mapped = value(executable.transform(Vector(Point3D(3.0, 4.0, 5.0)))).head
    assertEqualsDouble(mapped.x, 4.0, 1e-12)
    assertEqualsDouble(mapped.y, 2.0, 1e-12)
    assertEqualsDouble(mapped.z, 5.5, 1e-12)
    // the reverse TemplateFlow file is still unloaded, so the reverse route stays non-executable
    assert(!value(SpaceTransforms.plan(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym, registry = implemented)).isExecutable)

  test("assets on frames outside the catalog are rejected when the graph is built"):
    def foreign(name: String): Frame[D3] = FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe(name)))
    val pullback = FramedAffine.betweenFrames[Frame[D3], Frame[D3], D3](foreign("MNI152NLin2009cAsym"), foreign("MNI152NLin6Asym"))(translation(0.0, 0.0, 0.0))
    val asset = TransformAsset(WorldTransform.Linear(pullback, TransformProvenance.empty), "foreign")
    val registry =
      Vector(
        TransformStep(
          SpaceId.MNI152NLin6Asym,
          SpaceId.MNI152NLin2009cAsym,
          TransformKind.NonlinearWarp,
          TransformBackend.TemplateFlowAnts,
          Confidence.High,
          reversible = true,
          dataFiles = Vector.empty,
          TransformStatus.Planned
        ).withAsset(asset)
      )
    SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, registry = registry) match
      case Left(AtlasError.TransformGraph(_)) => ()
      case other => fail(s"expected a graph error for foreign frames, got $other")

  test("routes fall back to exact inverses only when no forward route exists"):
    val registry = Vector(affineStep(SpaceId.MNI305, SpaceId.MNI152, SpaceTransforms.mni305ToMni152))
    val reverse = value(SpaceTransforms.plan(SpaceId.MNI152, SpaceId.MNI305, registry = registry))
    assert(reverse.usedInverses)
    assert(reverse.warnings.exists(_.contains("inverse")), clue = reverse.warnings.mkString(";"))
    assertEquals(reverse.steps.map(step => (step.from, step.to)), Vector((SpaceId.MNI152, SpaceId.MNI305)))
    val mapped = value(reverse.transform(Vector(Point3D(10.694, -18.406, 36.139)))).head
    assertEqualsDouble(mapped.x, 10.0, 0.01)
    assertEqualsDouble(mapped.y, -20.0, 0.01)

    val irreversible = Vector(affineStep(SpaceId.MNI305, SpaceId.MNI152, SpaceTransforms.mni305ToMni152, reversible = false))
    assertEquals(
      SpaceTransforms.plan(SpaceId.MNI152, SpaceId.MNI305, registry = irreversible),
      Left(AtlasError.NoTransformRoute(SpaceId.MNI152, SpaceId.MNI305))
    )

  test("available steps without provider maps are planned but not executable"):
    val plan = value(SpaceTransforms.plan(SpaceId.FsAverage, SpaceId.FsAverage5))
    assertEquals(plan.status, TransformStatus.Available)
    plan.executability match
      case Left(AtlasError.TransformNotExecutable(_, _, reason)) =>
        assert(reason.contains("without a coordinate map"), clue = reason)
      case other => fail(s"expected a map-less route, got $other")

  test("grid pullbacks come from fused affine routes, including inverse steps"):
    val registry = Vector(affineStep(SpaceId.MNI305, SpaceId.MNI152, translation(1.0, 2.0, 3.0)))
    val grid = GridSpec.identity(Vector(2, 2, 2))
    val pullback = value(SpaceTransforms.spatialPullback(SpaceId.MNI152, SpaceId.MNI305, gridIn(SpaceId.MNI305), gridIn(SpaceId.MNI152), registry))
    val pulled = SpatialPullbacks.transform(pullback, Point3D(0.0, 0.0, 0.0)).fold(error => fail(error.message), identity)
    assertEqualsDouble(pulled.x, -1.0, 1e-12)
    assertEqualsDouble(pulled.y, -2.0, 1e-12)
    assertEqualsDouble(pulled.z, -3.0, 1e-12)
    val route = value(SpaceTransforms.plan(SpaceId.MNI152, SpaceId.MNI305, registry = registry))
    val carried = value(route.transform(Vector(Point3D.Origin))).head
    assertEqualsDouble(pulled.x, carried.x, 1e-12)
    assertEqualsDouble(pulled.y, carried.y, 1e-12)
    assertEqualsDouble(pulled.z, carried.z, 1e-12)

    val warp = value(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym))
    assert(warp.pullback(grid, grid).isLeft)

  test("grid pullbacks reject grids outside the route's endpoint worlds"):
    val registry = Vector(affineStep(SpaceId.MNI305, SpaceId.MNI152, translation(1.0, 2.0, 3.0)))
    val plan = value(SpaceTransforms.plan(SpaceId.MNI152, SpaceId.MNI305, DataKind.Voxel, registry))
    val mni152 = gridIn(SpaceId.MNI152)
    val mni305 = gridIn(SpaceId.MNI305)
    assert(plan.pullback(mni305, mni152).isRight)
    // Swapped endpoints: the source must be in MNI305 (`to`) and the target in MNI152 (`from`).
    plan.pullback(mni152, mni305) match
      case Left(AtlasError.GridWorldMismatch("target", space, _)) => assertEquals(space, SpaceId.MNI152)
      case other                                                  => fail(s"expected a target world mismatch, got $other")
    // Unresolved grids carry no evidence of being in either template.
    val unresolved = GridSpec.identity(Vector(2, 2, 2))
    plan.pullback(unresolved, mni152) match
      case Left(AtlasError.GridWorldMismatch("source", space, _)) => assertEquals(space, SpaceId.MNI305)
      case other                                                  => fail(s"expected a source world mismatch, got $other")
    assert(plan.pullback(mni305, unresolved).isLeft)
    // An identity route still requires both grids in its one world.
    val identityRoute = value(SpaceTransforms.plan(SpaceId.MNI305, SpaceId.MNI305, DataKind.Voxel, registry))
    assert(identityRoute.pullback(mni305, mni305).isRight)
    assert(identityRoute.pullback(mni152, mni305).isLeft)

  test("an identity route over an uncatalogued space plans, but has no world for grid pullbacks"):
    val unknown = SpaceId.normalize("T1w")
    val route = value(SpaceTransforms.plan(unknown, unknown))
    assert(route.isExecutable)
    val grid = GridSpec.identity(Vector(2, 2, 2))
    assertEquals(route.pullback(grid, grid).left.toOption, Some(AtlasError.UnknownSpace(unknown)))
