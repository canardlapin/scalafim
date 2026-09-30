package scalafim.spatial

import scalafim.image.{Mask, PrimitiveBuffers, SampleSpaces}
import scalafim.image.SampleSpaces.*
import scalafim.image.world.{SubjectId, TemplateName}
import scalafim.surface.*

class TemplateSurfaceOperatorSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(error.toString), identity)

  private def icosphere(levels: Int, radius: Double, rotation: Double = 0.0): TriangleMesh =
    val t = (1.0 + math.sqrt(5.0)) / 2.0
    val vertices = scala.collection.mutable.ArrayBuffer(
      (-1.0, t, 0.0), (1.0, t, 0.0), (-1.0, -t, 0.0), (1.0, -t, 0.0),
      (0.0, -1.0, t), (0.0, 1.0, t), (0.0, -1.0, -t), (0.0, 1.0, -t),
      (t, 0.0, -1.0), (t, 0.0, 1.0), (-t, 0.0, -1.0), (-t, 0.0, 1.0)
    )
    var faces = Vector(
      (0, 11, 5), (0, 5, 1), (0, 1, 7), (0, 7, 10), (0, 10, 11),
      (1, 5, 9), (5, 11, 4), (11, 10, 2), (10, 7, 6), (7, 1, 8),
      (3, 9, 4), (3, 4, 2), (3, 2, 6), (3, 6, 8), (3, 8, 9),
      (4, 9, 5), (2, 4, 11), (6, 2, 10), (8, 6, 7), (9, 8, 1)
    )
    (0 until levels).foreach: _ =>
      val midpoints = scala.collection.mutable.HashMap.empty[Long, Int]
      def midpoint(a: Int, b: Int): Int =
        val key = math.min(a, b).toLong * 1000000L + math.max(a, b)
        midpoints.get(key) match
          case Some(index) => index
          case None =>
            val (p, q) = (vertices(a), vertices(b))
            vertices.addOne(((p._1 + q._1) / 2, (p._2 + q._2) / 2, (p._3 + q._3) / 2))
            val index = vertices.size - 1
            midpoints(key) = index
            index
      faces = faces.flatMap: (a, b, c) =>
        val (ab, bc, ca) = (midpoint(a, b), midpoint(b, c), midpoint(c, a))
        Vector((a, ab, ca), (b, bc, ab), (c, ca, bc), (ab, bc, ca))
    val (cosine, sine) = (math.cos(rotation), math.sin(rotation))
    val coordinates = new Array[Double](3 * vertices.size)
    vertices.indices.foreach: i =>
      val (x, y, z) = vertices(i)
      val norm = math.sqrt(x * x + y * y + z * z)
      coordinates(3 * i) = radius * (cosine * x - sine * y) / norm
      coordinates(3 * i + 1) = radius * (sine * x + cosine * y) / norm
      coordinates(3 * i + 2) = radius * z / norm
    TriangleMesh.fromArrays(coordinates, faces.flatMap((a, b, c) => Vector(a, b, c)).toArray)

  private val template = TemplateSurface(TemplateMesh.FsAverage5, CorticalHemisphere.Left)
  private lazy val sourceMesh = icosphere(5, 97.0)
  private lazy val targetMesh = icosphere(5, 103.0, 0.013)
  private lazy val plan =
    ok(
      TemplateResampling.plan(
        ok(TemplateSphere.admit(template, SphereRegistration.FsAverage, sourceMesh, "source")),
        ok(TemplateSphere.admit(template, SphereRegistration.FsAverage, targetMesh, "target"))
      )
    )

  private def domain(id: String, label: String, geometry: SurfaceGeometry): Domain =
    ok(
      Domain.build(
        ok(DomainId(id)),
        SpaceRef.Template(ok(TemplateName(label).asSpatial), None, TemplateKind.Surface),
        ok(SamplingGeometry.surface(geometry))
      )
    )

  private def surfaceEdge(id: String, source: Domain, target: Domain, binding: SurfaceResamplingBinding): Morphism =
    ok(
      Morphism.between(
        ok(MorphismId(id)), source, target, MorphismKind.SurfaceToSurface, RouteTag.Anatomical,
        inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.surfaceResampling(binding)
      )
    )

  test("template graph compiles selected reordered rows, matches native resampling, and obeys its adjoint"):
    val sourceGeometry = SurfaceGeometry(sourceMesh, Hemisphere.Left, SurfaceKind.White)
    val targetGeometry = SurfaceGeometry(targetMesh, Hemisphere.Left, SurfaceKind.Inflated)
    val source = domain("source", "fsaverage5", sourceGeometry)
    val target = domain("target", "fsaverage5", targetGeometry)
    val binding = ok(SurfaceResamplingBinding.checked(plan, sourceGeometry, targetGeometry))
    val graph = ok(SpatialGraph.build(Vector(source, target), Vector(surfaceEdge("resample", source, target, binding))))
    val rows = Vector(19, 3, 9977)
    val duplicate = OperatorCompiler.compile(graph, CompileRequest(source.id, target.id, roi = Some(Vector(19, 3, 19))))
    assertEquals(duplicate.left.toOption, Some(SpatialError.DuplicateRoiRow(19)))
    val operator = ok(OperatorCompiler.compile(graph, CompileRequest(source.id, target.id, roi = Some(rows))))
    val input = DoubleMatrix.fromRows(Vector.tabulate(10242)(i => Vector(math.sin(i * 0.013), 2.0 + math.cos(i * 0.071))))
    val first = ok(plan.resample(Array.tabulate(10242)(i => math.sin(i * 0.013))) )
    val second = ok(plan.resample(Array.tabulate(10242)(i => 2.0 + math.cos(i * 0.071))) )
    val runtime = CachedFieldRuntime(InMemoryOperatorCache.empty)
    val observed = ok(runtime.data(ok(runtime.view(Field.fromMatrix(source.id, input, "two columns"), operator))))
    rows.indices.foreach: outputRow =>
      assertEqualsDouble(observed(outputRow, 0), first(rows(outputRow)), 1e-10)
      assertEqualsDouble(observed(outputRow, 1), second(rows(outputRow)), 1e-10)
    assert(ok(SpatialQc.adjointLaw(operator, input, DoubleMatrix.fromRows(Vector.fill(rows.length)(Vector(0.3, -0.7)))).map(_.passed)))
    assertEquals(operator.qc.coverage.targetRows, rows)
    assertEquals(operator.qc.coverage.rowCoverage, Vector.fill(rows.length)(1.0))

  test("template resampling refuses typed reverse routing and wrong endpoint template label"):
    val sourceGeometry = SurfaceGeometry(sourceMesh, Hemisphere.Left, SurfaceKind.White)
    val targetGeometry = SurfaceGeometry(targetMesh, Hemisphere.Left, SurfaceKind.Inflated)
    val source = domain("source2", "fsaverage5", sourceGeometry)
    val target = domain("target2", "fsaverage5", targetGeometry)
    val binding = ok(SurfaceResamplingBinding.checked(plan, sourceGeometry, targetGeometry))
    val graph = ok(SpatialGraph.build(Vector(source, target), Vector(surfaceEdge("forward", source, target, binding))))
    assert(OperatorCompiler.compile(graph, CompileRequest(target.id, source.id, allowInverses = true)).isLeft)
    val wrong = domain("wrong", "not-fsaverage5", targetGeometry)
    val id = ok(MorphismId("wrong-label"))
    val refused = Morphism.between(id, source, wrong, MorphismKind.SurfaceToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.surfaceResampling(binding))
    assertEquals(refused.left.toOption, Some(SpatialError.SurfaceMappingGeometryMismatch(id)))

  test("repeated binding admission retains frozen weights and rejects later endpoint topology mutation"):
    val mesh = TriangleMesh.fromArrays(sourceMesh.coordinates.clone(), sourceMesh.faceIndices.clone())
    val sourceGeometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White)
    val targetGeometry = SurfaceGeometry(targetMesh, Hemisphere.Left, SurfaceKind.Inflated)
    val source = domain("mutable-source", "fsaverage5", sourceGeometry)
    val target = domain("mutation-target", "fsaverage5", targetGeometry)
    val binding = ok(SurfaceResamplingBinding.checked(plan, sourceGeometry, targetGeometry))
    val frozen = binding.normalizedCsr
    val edge = surfaceEdge("mutation-edge", source, target, binding)
    val graph = ok(SpatialGraph.build(Vector(source, target), Vector(edge)))
    ok(OperatorCompiler.compile(graph, CompileRequest(source.id, target.id, roi = Some(Vector(19, 3)))))
    assert(binding.normalizedCsr eq frozen)
    val first = mesh.faceIndices(0)
    mesh.faceIndices(0) = mesh.faceIndices(1)
    mesh.faceIndices(1) = first
    binding.validateEndpoints match
      case Left(SpatialError.InvalidMixedPullback(reason)) => assert(reason.contains("ordered template topology"), reason)
      case other => fail(s"expected typed endpoint topology rejection, got $other")
    assert(SpatialGraph.build(Vector(source, target), Vector(edge)).isLeft)

  test("masked volume ribbon followed by weighted template resampling matches staged native resampling and reports bounded coverage"):
    val volumeSpace = SampleSpaces(Vector(6, 6, 6))
    val mask = Mask.fromIndices(volumeSpace, PrimitiveBuffers.fromArray(Array.tabulate(216)(identity).filter(_ % 2 == 0)), "checkerboard")
    val root = ok(Domain.build(ok(DomainId("volume-root")), SpaceRef.Volume(ok(SubjectId("sub-template").asSpatial), None, ok(Modality("bold"))), ok(SamplingGeometry.volume(volumeSpace, Some(mask)))))
    def display(mesh: TriangleMesh, kind: SurfaceKind, isPial: Boolean = false): SurfaceGeometry =
      val coordinates = mesh.coordinates.clone()
      var index = 0
      while index < coordinates.length do
        coordinates(index) = 3.0 + coordinates(index) / 100.0
        if isPial && index % 3 == 2 then coordinates(index) += 0.20
        index += 1
      SurfaceGeometry(TriangleMesh.fromArrays(coordinates, mesh.faceIndices.clone()), Hemisphere.Left, kind)
    val white = display(sourceMesh, SurfaceKind.White)
    val pial = display(sourceMesh, SurfaceKind.Pial, isPial = true)
    val inflated = display(targetMesh, SurfaceKind.Inflated)
    val whiteDomain = domain("template-white", "fsaverage5", white)
    val inflatedDomain = domain("template-inflated", "fsaverage5", inflated)
    val pair = SurfaceGeometryPair(white, pial)
    val ribbon = VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.5, 1.0)))
    val bridge = ok(Morphism.between(ok(MorphismId("volume-white")), root, whiteDomain, MorphismKind.VolumeToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.volumeSamples(ribbon)))
    val binding = ok(SurfaceResamplingBinding.checked(plan, white, inflated))
    val resample = surfaceEdge("white-inflated", whiteDomain, inflatedDomain, binding)
    val stagedGraph = ok(SpatialGraph.build(Vector(root, whiteDomain), Vector(bridge)))
    val graph = ok(SpatialGraph.build(Vector(root, whiteDomain, inflatedDomain), Vector(bridge, resample)))
    val contributorCount = Array.fill(10242)(0)
    var entry = 0
    while entry < plan.plan.rows.length do
      contributorCount(plan.plan.rows(entry)) += 1
      entry += 1
    val rows = (0 until 10242).filter(row => contributorCount(row) >= 3).take(3).toVector
    assertEquals(rows.length, 3)
    val source = DoubleMatrix.fromRows(Vector.tabulate(216)(i => Vector(i.toDouble + 0.25)))
    val staged = ok(VolumeToSurfaceOperatorCompiler.compile(stagedGraph, VolumeToSurfaceRequest.ribbon(root.id, whiteDomain.id, pair, Vector(0.0, 0.5, 1.0), sampling = SamplingPolicy.Trilinear)))
    val stagedValues = ok(staged.forward(source))
    val expected = ok(plan.resample(Array.tabulate(10242)(i => stagedValues(i, 0))))
    val compiled = ok(OperatorCompiler.compile(graph, CompileRequest(root.id, inflatedDomain.id, sampling = SamplingPolicy.Trilinear, roi = Some(rows))))
    val observed = ok(compiled.forward(source))
    rows.indices.foreach: outputRow =>
      assertEqualsDouble(observed(outputRow, 0), expected(rows(outputRow)), 1e-10)
    assertEquals(compiled.qc.coverage.targetRows, rows)
    assertEquals(compiled.qc.coverage.rowCoverage.length, rows.length)
    rows.indices.foreach: outputRow =>
      val targetRow = rows(outputRow)
      var expectedCoverage = 0.0
      var sourceRow = 0
      while sourceRow < staged.qc.coverage.rowCoverage.length do
        expectedCoverage += binding.normalizedCsr(targetRow, sourceRow) * staged.qc.coverage.rowCoverage(sourceRow)
        sourceRow += 1
      assertEqualsDouble(compiled.qc.coverage.rowCoverage(outputRow), math.max(0.0, math.min(1.0, expectedCoverage)), 1e-10)
      assert(compiled.qc.coverage.rowCoverage(outputRow) >= 0.0 && compiled.qc.coverage.rowCoverage(outputRow) <= 1.0)
