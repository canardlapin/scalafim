package scalafim.spatial

import scalafim.image.{SampleSpaces, DMat, SomeSampleSpace, SpatialPoint}
import scalafim.image.SampleSpaces.*

class PullbackCompilerSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def domain(name: String, voxels: Int = 3): Domain =
    val id = value(DomainId(name))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality(name))
    val geometry =
      value(SamplingGeometry.volume(SampleSpaces(Vector(voxels, 1, 1), trans = Some(DMat.eye(4)))))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def affine(
    name: String,
    source: Domain,
    target: Domain,
    matrix: DMat
  ): Morphism =
    value(
      Morphism.build(
        id = value(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = value(CoordinateMap.affine3D(matrix))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    value(SpatialGraph.build(domains, morphisms))

  test("identity routes lower to a zero-work pullback program"):
    val root = domain("root")
    val program =
      value(PullbackProgram.compile(graph(Vector(root), Vector.empty), CompileRequest(root.id, root.id)))

    assert(program.isIdentity)
    assert(!program.requiresSpatialSampling)
    assertEquals(program.steps, Vector.empty)
    assertEquals(program.targetRows, Vector(0, 1, 2))
    assertEquals(value(program.pullback(SpatialPoint(1.0, 2.0, 3.0))), SpatialPoint(1.0, 2.0, 3.0))
    assert(program.fingerprint.value.contains("identity-pullback-v1"))

  test("complete affine routes lower and pull target coordinates back to the root"):
    val root = domain("root")
    val mid = domain("mid")
    val target = domain("target")
    val first = affine("root-mid", root, mid, translation(1.0, 0.0, 0.0))
    val second = affine("mid-target", mid, target, translation(0.0, 2.0, 0.0))
    val g = graph(Vector(root, mid, target), Vector(first, second))

    val program = value(PullbackProgram.compile(g, CompileRequest(root.id, target.id)))
    val repeated = value(PullbackProgram.compile(g, CompileRequest(root.id, target.id)))

    assertEquals(program.steps.map(_.morphism.id.value), Vector("root-mid", "mid-target"))
    assertEquals(program.compilerTrace.map(_.value), Vector("affine-pullback-v1", "affine-pullback-v1"))
    assertEquals(value(program.pullback(SpatialPoint.Origin)), SpatialPoint(1.0, 2.0, 0.0))
    assertEquals(program.fingerprint, repeated.fingerprint)

  test("program fingerprints include row demand and sampling policy"):
    val root = domain("root")
    val target = domain("target")
    val transform = affine("root-target", root, target, DMat.eye(4))
    val g = graph(Vector(root, target), Vector(transform))
    val full = value(PullbackProgram.compile(g, CompileRequest(root.id, target.id)))
    val selected =
      value(
        PullbackProgram.compile(
          g,
          CompileRequest(root.id, target.id, roi = Some(Vector(2, 0)))
        )
      )
    val nearest =
      value(
        PullbackProgram.compile(
          g,
          CompileRequest(root.id, target.id, sampling = SamplingPolicy.Nearest)
        )
      )

    assertNotEquals(full.fingerprint, selected.fingerprint)
    assertNotEquals(full.fingerprint, nearest.fingerprint)
    assertEquals(selected.targetRows, Vector(2, 0))

  test("missing and duplicate morphism compilers are explicit typed failures"):
    val root = domain("root")
    val target = domain("target")
    val filter =
      value(
        Morphism.build(
          id = value(MorphismId("root-target-filter")),
          source = root.id,
          target = target.id,
          kind = MorphismKind.Filter,
          routeTag = RouteTag.Anatomical
        )
      )
    val g = graph(Vector(root, target), Vector(filter))

    assertEquals(
      PullbackProgram
        .compile(g, CompileRequest(root.id, target.id), MorphismCompilerRegistry.empty)
        .left
        .toOption,
      Some(SpatialError.MorphismCompilerNotFound(MorphismKind.Filter))
    )
    assertEquals(
      MorphismCompilerRegistry
        .build(Vector(MorphismPullbackCompiler.affine3D, MorphismPullbackCompiler.affine3D))
        .left
        .toOption,
      Some(SpatialError.DuplicateMorphismCompiler(MorphismKind.Affine3D))
    )

  test("a new morphism compiler registers without changing central lowering logic"):
    val root = domain("root")
    val target = domain("target")
    val functional =
      value(
        Morphism.build(
          id = value(MorphismId("root-target-functional")),
          source = root.id,
          target = target.id,
          kind = MorphismKind.Functional,
          routeTag = RouteTag.Functional
        )
      )
    val compiler = IdentityLikeFunctionalCompiler
    val registry =
      value(
        MorphismCompilerRegistry.build(
          Vector(MorphismPullbackCompiler.identity, compiler)
        )
      )
    val program =
      value(
        PullbackProgram.compile(
          graph(Vector(root, target), Vector(functional)),
          CompileRequest(root.id, target.id, routing = RoutingPolicy.Functional),
          registry
        )
      )

    assertEquals(program.compilerTrace.map(_.value), Vector("functional-test-v1"))
    assertEquals(value(program.pullback(SpatialPoint(2.0, 0.0, 0.0))), SpatialPoint(2.0, 0.0, 0.0))

  test("compiler inputs retain graph domain compatibility checks"):
    val root = domain("root")
    val target = domain("target")
    val bad =
      value(
        Morphism.build(
          id = value(MorphismId("bad-volume-surface")),
          source = root.id,
          target = target.id,
          kind = MorphismKind.VolumeToSurface,
          routeTag = RouteTag.Anatomical
        )
      )

    assertEquals(
      SpatialGraph.build(Vector(root, target), Vector(bad)).left.toOption,
      Some(
        SpatialError.IncompatibleMorphismKind(
          MorphismKind.VolumeToSurface,
          root.id,
          DomainKind.Volume,
          target.id,
          DomainKind.Volume
        )
      )
    )

private object IdentityLikeFunctionalCompiler extends MorphismPullbackCompiler:
  override val id: MorphismCompilerId =
    MorphismCompilerId.unsafe("functional-test-v1")

  override val kind: MorphismKind =
    MorphismKind.Functional

  override def lower(
    morphism: Morphism,
    source: Domain,
    target: Domain
  ): Either[SpatialError, Vector[PullbackStep]] =
    if morphism.kind != kind then
      Left(SpatialError.MorphismCompilerKindMismatch(id.value, kind, morphism.kind))
    else
      Morphism
        .validateDomains(morphism, source, target)
        .flatMap(_ => PullbackStep.build(morphism, id, CoordinateMap.Identity))
        .map(Vector(_))
