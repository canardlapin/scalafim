package scalafim.atlas

import gale.linalg.DMat
import scalafim.spatial.{DomainKind, Inverse, SamplingGeometry}
import scalafim.surface.*
import scalafim.surface.fixtures.SphereMeshes

/** Template surface domains with vertex geometry in the transform graph, and sphere-resampling steps compiled to vertex
  * operators (STP P7.04). Synthetic spheres stand in for the TemplateFlow assets: an order-5 icosphere has fsaverage5's
  * 10242 vertices, a 171 x 190 latitude-longitude sphere fsLR 32k's 32492. Workbench parity through the graph, on the
  * real spheres, is `io.TemplateSurfaceGraphParitySuite` (JVM, asset-gated).
  */
class TemplateSurfaceGraphSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private type OnFsAverage = SphereRegistration.FsAverage.type

  private def sphere(mesh: TemplateMesh, hemisphere: CorticalHemisphere, geometry: TriangleMesh, identity: String): TemplateSphere[OnFsAverage] =
    ok(TemplateSphere.admit(TemplateSurface(mesh, hemisphere), SphereRegistration.FsAverage, geometry, identity))

  private val fsAverage5 = sphere(TemplateMesh.FsAverage5, CorticalHemisphere.Left, SphereMeshes.icosphere(5, 100.0), "synthetic:icosphere-5")
  private val fsLR32k = sphere(TemplateMesh.FsLR32k, CorticalHemisphere.Left, SphereMeshes.uvSphere(171, 190, 100.0, tilt = 0.3), "synthetic:uv-171x190")
  private val sampling = ok(TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, Vector(fsAverage5, fsLR32k)))

  /** A manifest joining the two synthetic spaces directly: Workbench steps both ways, and a nearest-vertex step. */
  private def step(from: AnySpaceId, to: AnySpaceId, backend: TransformBackend): TransformStep =
    TransformStep(from, to, TransformKind.SphereResample, backend, Confidence.High, reversible = true, dataFiles = Vector.empty, TransformStatus.Planned, notes = Some("synthetic"))

  private val direct = Vector(
    step(SpaceId.FsLR32k, SpaceId.FsAverage5, TransformBackend.Workbench),
    step(SpaceId.FsAverage5, SpaceId.FsLR32k, TransformBackend.Workbench)
  )

  private def column(values: Array[Double]): DMat =
    val builder = DMat.newBuilder(values.length, 1)
    values.indices.foreach(i => builder.writeLinear(i, values(i)))
    builder.result()

  private def forward(operator: scalafim.spatial.SpatialOperator, values: Array[Double]): Array[Double] =
    val out = ok(operator.forward(column(values)))
    Array.tabulate(out.rows)(out(_, 0))

  private def adjoint(operator: scalafim.spatial.SpatialOperator, values: Array[Double]): Array[Double] =
    val out = ok(operator.map.transposeApplyTo(column(values)))
    Array.tabulate(out.rows)(out(_, 0))

  private def field(of: TemplateSphere[?]): Array[Double] =
    val c = of.sphere.coordinates
    Array.tabulate(of.sphere.vertexCount)(i => 1.0 + c(3 * i) / 100.0 + 0.5 * c(3 * i + 1) * c(3 * i + 2) / 10000.0)

  private def maxAbs(a: Array[Double], b: Array[Double]): Double =
    assertEquals(a.length, b.length)
    a.indices.map(i => math.abs(a(i) - b(i))).max

  test("without loaded spheres, fsaverage <-> fsLR stays a typed non-executable edge"):
    val graph = ok(SpaceTransformGraph.build(SpaceTransforms.manifest))
    assert(graph.sampling.isEmpty)
    val domain = graph.graph.domains(TemplateCatalog.domainId(SpaceId.FsLR32k))
    assertEquals(domain.kind, DomainKind.Surface)
    assert(domain.geometry.isInstanceOf[SamplingGeometry.Unsampled], domain.geometry.toString)
    assertEquals(ok(graph.plan(SpaceId.FsAverage, SpaceId.FsLR32k, DataKind.Vertex)).status, TransformStatus.Planned)
    graph.vertexOperator(SpaceId.FsAverage, SpaceId.FsLR32k) match
      case Left(AtlasError.TransformNotExecutable(_, _, reason)) => assert(reason.contains("no template sphere loaded"), reason)
      case other                                                 => fail(s"expected a typed refusal, got $other")

  test("loaded spheres give their spaces vertex geometry; an unloaded space in the route keeps it non-executable"):
    val graph = ok(SpaceTransforms.graph(SpaceTransforms.manifest, sampling = sampling))
    assertEquals(graph.graph.domains(TemplateCatalog.domainId(SpaceId.FsLR32k)).nElements, 32492)
    assertEquals(graph.graph.domains(TemplateCatalog.domainId(SpaceId.FsAverage5)).nElements, 10242)
    assertEquals(graph.graph.domains(TemplateCatalog.domainId(SpaceId.FsAverage5)).geometry, SamplingGeometry.Surface(fsAverage5.geometry, None))
    // fsaverage (fsaverage7) has no sphere here: the fsLR 32k -> fsaverage5 route runs through it and cannot execute
    assert(graph.graph.domains(TemplateCatalog.domainId(SpaceId.FsAverage)).geometry.isInstanceOf[SamplingGeometry.Unsampled])
    val plan = ok(graph.plan(SpaceId.FsLR32k, SpaceId.FsAverage5, DataKind.Vertex))
    assertEquals(plan.steps.map(_.to), Vector(SpaceId.FsAverage, SpaceId.FsAverage5))
    assert(plan.steps.forall(_.resampling.isEmpty))
    graph.vertexOperator(SpaceId.FsLR32k, SpaceId.FsAverage5) match
      case Left(AtlasError.TransformNotExecutable(_, _, reason)) =>
        assert(reason.contains("unavailable steps=fsLR_32k->fsaverage"), reason)
        assert(reason.contains("steps without a vertex resampling plan"), reason)
      case other => fail(s"expected a typed refusal, got $other")

  test("a sphere-resampling step between sampled spaces carries its plan, routes and compiles both ways"):
    val graph = ok(SpaceTransforms.graph(direct, sampling = sampling))
    val there = ok(graph.plan(SpaceId.FsLR32k, SpaceId.FsAverage5, DataKind.Vertex))
    assertEquals(there.status, TransformStatus.Available)
    assert(there.steps.head.notes.exists(_.contains("synthetic:uv-171x190 -> synthetic:icosphere-5")), there.steps.head.notes.toString)
    assertEquals(there.path.morphisms.head.inverse, Inverse.AdjointOnly)
    // vertex data, not points: the point route stays refused
    assert(there.transform(Vector(scalafim.atlas.Point3D.Origin)).isLeft)
    val expected = ok(TemplateResampling.plan(fsLR32k, fsAverage5))
    val operator = ok(graph.vertexOperator(SpaceId.FsLR32k, SpaceId.FsAverage5))
    assertEquals((operator.rows, operator.cols), (10242, 32492))
    val values = field(fsLR32k)
    assert(maxAbs(forward(operator, values), ok(expected.resample(values))) <= 1e-12)
    val back = field(fsAverage5)
    assert(maxAbs(adjoint(operator, back), ok(expected.adjoint(back, SurfaceResampling.Normalization.None))) <= 1e-12)
    // the reverse direction is its own step with its own plan, not the adjoint
    val reverse = ok(graph.vertexOperator(SpaceId.FsAverage5, SpaceId.FsLR32k))
    assert(!reverse.path.usedInverses)
    assert(maxAbs(forward(reverse, back), ok(ok(TemplateResampling.plan(fsAverage5, fsLR32k)).resample(back))) <= 1e-12)
    // an identity route is the identity operator on a sampled space
    val same = ok(graph.vertexOperator(SpaceId.FsAverage5, SpaceId.FsAverage5))
    assert(maxAbs(forward(same, back), back) == 0.0)

  test("SphereNearest steps resample by nearest vertex, Workbench steps barycentrically"):
    val graph = ok(SpaceTransforms.graph(Vector(step(SpaceId.FsLR32k, SpaceId.FsAverage5, TransformBackend.SphereNearest)), sampling = sampling))
    val plan = ok(graph.plan(SpaceId.FsLR32k, SpaceId.FsAverage5, DataKind.Vertex)).steps.head.resampling.getOrElse(fail("no plan"))
    assertEquals(plan.plan.method, SurfaceResampling.Method.Nearest)
    assertEquals(plan.plan.nonZeros, 10242)
    val barycentric = ok(SpaceTransforms.graph(direct, sampling = sampling))
    assertEquals(ok(barycentric.plan(SpaceId.FsLR32k, SpaceId.FsAverage5, DataKind.Vertex)).steps.head.resampling.map(_.plan.method), Some(SurfaceResampling.Method.Barycentric))

  test("a sampling holds one hemisphere of manifest spaces, one sphere per space"):
    val right = sphere(TemplateMesh.FsAverage5, CorticalHemisphere.Right, SphereMeshes.icosphere(5, 100.0), "synthetic:right")
    def refused(result: Either[AtlasError, TemplateSurfaceSampling], fragment: String): Unit =
      result match
        case Left(AtlasError.InvalidSurfaceSampling(detail)) => assert(detail.contains(fragment), detail)
        case other                                           => fail(s"expected a refusal naming '$fragment', got $other")
    refused(TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, Vector(fsAverage5, right)), "is not hemisphere lh")
    refused(TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, Vector(fsAverage5, fsAverage5)), "more than one sphere samples fsaverage5")
    val fsLR59k = sphere(TemplateMesh.FsLR59k, CorticalHemisphere.Left, SphereMeshes.uvSphere(242, 245, 100.0), "synthetic:59k")
    refused(TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, Vector(fsLR59k)), "fsLR_59k is not a transform-manifest space")
    val empty = ok(TemplateSurfaceSampling.on(SphereRegistration.FsAverage, CorticalHemisphere.Left, Vector.empty))
    assert(empty.isEmpty)
    assertEquals(sampling.spaces, Vector(SpaceId.FsLR32k, SpaceId.FsAverage5).sortBy(_.value))
    assertEquals(sampling.identity(SpaceId.FsAverage5), Some("synthetic:icosphere-5"))

  test("a transform step carries at most one implementation, and only a sphere-resampling step a plan"):
    val plan = ok(TemplateResampling.plan(fsLR32k, fsAverage5))
    intercept[IllegalArgumentException](step(SpaceId.FsLR32k, SpaceId.FsAverage5, TransformBackend.Workbench).copy(kind = TransformKind.Affine, resampling = Some(plan)))
    val implemented = step(SpaceId.FsLR32k, SpaceId.FsAverage5, TransformBackend.Workbench).withResampling(plan)
    assertEquals(implemented.status, TransformStatus.Available)
    intercept[IllegalArgumentException](implemented.copy(affine = Some(image4s.geometry.Affine.identity[image4s.geometry.D3])))
    // a plan belongs to its step's spaces: fsLR 32k -> fsaverage5 cannot implement the reverse step
    intercept[IllegalArgumentException](step(SpaceId.FsAverage5, SpaceId.FsLR32k, TransformBackend.Workbench).withResampling(plan))
