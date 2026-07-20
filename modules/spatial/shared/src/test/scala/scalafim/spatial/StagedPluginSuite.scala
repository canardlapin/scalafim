package scalafim.spatial

import gale.linalg.DMat as GaleDMat
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.DoubleMatrix

class StagedPluginSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def volume(name: String, voxels: Int): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(NeuroSpace(Vector(voxels, 1, 1), trans = Some(DMat.eye(4)))))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def hybrid(name: String, parts: Vector[(String, Domain)]): Domain =
    val geometry = spatialValue(
      SamplingGeometry.hybrid(parts.map { case (partName, domain) => spatialValue(PartName(partName)) -> domain })
    )
    spatialValue(
      Domain.build(
        spatialValue(DomainId(name)),
        SpaceRef.Template(spatialValue(TemplateName(name)), None, TemplateKind.Hybrid),
        geometry
      )
    )

  private def pluginId(value: String): MorphismPluginId =
    spatialValue(MorphismPluginId(value))

  private def affine(source: Domain, target: Domain, x: Double): Morphism =
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
        spatialValue(MorphismId(s"${source.id.value}-to-${target.id.value}-affine")),
        source,
        target,
        MorphismKind.Affine3D,
        RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(matrix))
      )
    )

  private def valueMorphism(
    name: String,
    source: Domain,
    target: Domain,
    kind: MorphismKind,
    plugin: MorphismPlugin
  ): Morphism =
    spatialValue(
      Morphism.between(
        spatialValue(MorphismId(name)),
        source,
        target,
        kind,
        if kind == MorphismKind.Functional then RouteTag.Functional else RouteTag.Anatomical,
        plugin = Some(plugin)
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    spatialValue(SpatialGraph.build(domains, morphisms))

  test("functional observation stages commute across one fused spatial pullback"):
    val root = volume("root", 4)
    val mid = volume("mid", 4)
    val target = volume("target", 4)
    val spatial = affine(root, mid, 1.0)
    val observationMatrix = GaleDMat.dense(2, 2, Vector(2.0, 0.0, 0.0, 3.0))
    val functional = valueMorphism(
      "mid-to-target-functional",
      mid,
      target,
      MorphismKind.Functional,
      spatialValue(MorphismPlugin.functional(pluginId("contrast-v1"), observationMatrix))
    )
    given SpatialGraph = graph(Vector(root, mid, target), Vector(spatial, functional))
    val field =
      Field.fromMatrix(
        root.id,
        DoubleMatrix.fromRows(
          Vector(
            Vector(0.0, 1.0),
            Vector(8.0, 2.0),
            Vector(0.0, 3.0),
            Vector(4.0, 4.0)
          )
        ),
        "root"
      )
    val view = apiValue(field.to(target))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val result = spatialValue(runtime.data(view))
    val trace = runtime.lastTrace.getOrElse(fail("expected an evaluation trace"))
    val program = trace.pullback
    val compiled = spatialValue(PullbackProgram.compile(summon[SpatialGraph], CompileRequest(root.id, target.id)))

    assertEquals(result.toRows, Vector(Vector(16.0, 6.0), Vector(0.0, 9.0), Vector(8.0, 12.0), Vector(0.0, 0.0)))
    assertEquals(compiled.stageTrace.map(_.semantics), Vector(StageSemantics.CoordinatePullback, StageSemantics.ValueTransform))
    assertEquals(compiled.stageTrace.map(_.morphism.id.value), Vector(spatial.id.value, functional.id.value))
    assert(compiled.fingerprint.value.contains("contrast-v1"))
    assert(program.value.contains("functional-value-plugin-v1"))
    assertEquals(trace.valueResamplingPasses, 1)
    assertEquals(runtime.stats.executions, 1L)

    val selectedRuntime = LazyFieldRuntime(summon[SpatialGraph])
    val selected = spatialValue(selectedRuntime.data(apiValue(view.timeBlock(1, 1))))
    assertEquals(selected.toRows, Vector(Vector(6.0), Vector(9.0), Vector(12.0), Vector(0.0)))

  test("filter row plugins execute through Gale and honor final row demand"):
    val root = volume("filter-root", 3)
    val target = volume("filter-target", 2)
    val matrix = GaleDMat.dense(2, 3, Vector(1.0, 1.0, 0.0, 0.0, 0.5, 0.5))
    val filter = valueMorphism(
      "row-filter",
      root,
      target,
      MorphismKind.Filter,
      spatialValue(MorphismPlugin.filter(pluginId("row-filter-v1"), matrix))
    )
    given SpatialGraph = graph(Vector(root, target), Vector(filter))
    val field = Field.fromMatrix(root.id, DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))
    val view = apiValue(apiValue(field.to(target)).rows(1))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val result = spatialValue(runtime.data(view))
    val trace = runtime.lastTrace.getOrElse(fail("expected an evaluation trace"))

    assertEquals(result.toRows, Vector(Vector(2.5)))
    assertEquals(trace.valueResamplingPasses, 0)
    assertEquals(trace.support.sourceRows, Vector(0, 1, 2))
    assert(trace.key.label.contains("filter-value-plugin-v1"))

  test("hybrid plugins execute typed row projections between hybrid and volume domains"):
    val left = volume("left-part", 2)
    val right = volume("right-part", 1)
    val root = hybrid("hybrid-root", Vector("left" -> left, "right" -> right))
    val target = volume("hybrid-target", 2)
    val projection = GaleDMat.dense(2, 3, Vector(1.0, 0.0, 1.0, 0.0, 1.0, -1.0))
    val morphism = valueMorphism(
      "hybrid-project",
      root,
      target,
      MorphismKind.Hybrid,
      spatialValue(MorphismPlugin.hybrid(pluginId("hybrid-project-v1"), projection))
    )
    given SpatialGraph = graph(Vector(root, target), Vector(morphism))
    val field = Field.fromMatrix(root.id, DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(5.0), Vector(1.0))))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val result = spatialValue(runtime.data(apiValue(field.to(target))))

    assertEquals(result.toRows, Vector(Vector(3.0), Vector(4.0)))
    assertEquals(runtime.lastTrace.map(_.valueResamplingPasses), Some(0))
    assertEquals(runtime.lastTrace.map(_.operator.recipe.compiler), Some("staged-gale-pullback-v1"))

  test("terminal pointwise barriers stay ordered and pre-spatial barriers fail explicitly"):
    val root = volume("barrier-root", 3)
    val mid = volume("barrier-mid", 3)
    val target = volume("barrier-target", 3)
    val spatial = affine(root, mid, 1.0)
    val barrier = valueMorphism(
      "terminal-barrier",
      mid,
      target,
      MorphismKind.Filter,
      spatialValue(MorphismPlugin.pointwiseBarrier(pluginId("offset-v1"), 2.0, 1.0))
    )
    given SpatialGraph = graph(Vector(root, mid, target), Vector(spatial, barrier))
    val field = Field.fromMatrix(root.id, DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    val result = spatialValue(runtime.data(apiValue(field.to(target))))

    assertEquals(result.toRows, Vector(Vector(5.0), Vector(7.0), Vector(1.0)))
    assertEquals(runtime.lastTrace.map(_.valueResamplingPasses), Some(1))

    val earlyBarrier = valueMorphism(
      "early-barrier",
      root,
      mid,
      MorphismKind.Filter,
      spatialValue(MorphismPlugin.pointwiseBarrier(pluginId("early-offset-v1"), 1.0, 1.0))
    )
    val laterSpatial = affine(mid, target, 1.0)
    val badGraph = graph(Vector(root, mid, target), Vector(earlyBarrier, laterSpatial))
    val program = spatialValue(PullbackProgram.compile(badGraph, CompileRequest(root.id, target.id)))
    VolumePullbackOperatorCompiler.compile(program).left.toOption match
      case Some(SpatialError.UnsupportedPluginComposition(detail)) =>
        assert(detail.contains("precedes a later coordinate"))
      case other => fail(s"expected fusion-barrier ordering failure, got $other")

  test("standard registry dispatch is exhaustive and plugin payload failures are typed"):
    assertEquals(
      MorphismCompilerRegistry.standard.ids.map(_.value),
      Vector(
        "identity-pullback-v1",
        "affine-pullback-v1",
        "dense-warp-pullback-v1",
        "volume-to-surface-pullback-v1",
        "surface-to-surface-pullback-v1",
        "functional-value-plugin-v1",
        "filter-value-plugin-v1",
        "hybrid-value-plugin-v1"
      )
    )
    val root = volume("missing-root", 2)
    val target = volume("missing-target", 2)
    val missing = spatialValue(
      Morphism.build(
        spatialValue(MorphismId("missing-plugin")),
        root.id,
        target.id,
        MorphismKind.Functional,
        RouteTag.Functional
      )
    )
    val g = graph(Vector(root, target), Vector(missing))
    PullbackProgram.compile(g, CompileRequest(root.id, target.id)).left.toOption match
      case Some(SpatialError.InvalidMorphismPlugin(id, detail)) =>
        assertEquals(id, missing.id)
        assert(detail.contains("no executable plugin payload"))
      case other => fail(s"expected missing plugin payload, got $other")

    val wrong = GaleDMat.zeros(3, 2)
    val malformed = spatialValue(
      Morphism.build(
        spatialValue(MorphismId("wrong-filter")),
        root.id,
        target.id,
        MorphismKind.Filter,
        RouteTag.Anatomical,
        plugin = Some(spatialValue(MorphismPlugin.filter(pluginId("wrong-shape"), wrong)))
      )
    )
    SpatialGraph.build(Vector(root, target), Vector(malformed)).left.toOption match
      case Some(SpatialError.InvalidMorphismPlugin(id, detail)) =>
        assertEquals(id, malformed.id)
        assert(detail.contains("expected 2x2"))
      case other => fail(s"expected row plugin shape error, got $other")
