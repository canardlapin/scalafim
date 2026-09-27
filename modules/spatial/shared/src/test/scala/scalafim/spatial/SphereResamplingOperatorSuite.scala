package scalafim.spatial

import gale.linalg.DMat
import scalafim.image.SampleSpaces
import scalafim.image.world.{Spaces as WorldFrames, SubjectId, TemplateName}
import scalafim.surface.*
import scalafim.surface.fixtures.SphereMeshes

/** Template sphere resampling plans as `SurfaceToSurface` graph edges, compiled by the mixed pullback compiler.
  *
  * Synthetic spheres stand in for the TemplateFlow assets: an order-5 icosphere has fsaverage5's 10242 vertices and a
  * 171 x 190 latitude-longitude sphere fsLR 32k's 32492, so both are admitted as those template surfaces on the
  * fsaverage sphere. The compiled operator must be the plan itself (rows normalised to one), its transpose the plan's
  * adjoint, and composition with other surface edges must multiply the weights.
  */
class SphereResamplingOperatorSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val fsAverage5 =
    ok(TemplateSphere.admit(TemplateSurface(TemplateMesh.FsAverage5, CorticalHemisphere.Left), SphereRegistration.FsAverage, SphereMeshes.icosphere(5, 100.0), "synthetic:icosphere-5"))
  private val fsLR32k =
    ok(TemplateSphere.admit(TemplateSurface(TemplateMesh.FsLR32k, CorticalHemisphere.Left), SphereRegistration.FsAverage, SphereMeshes.uvSphere(171, 190, 100.0, tilt = 0.3), "synthetic:uv-171x190"))

  /** fsLR 32k data onto fsaverage5 vertices, and back. */
  private val toFsAverage5 = ok(TemplateResampling.plan(fsLR32k, fsAverage5))
  private val toFsLR32k = ok(TemplateResampling.plan(fsAverage5, fsLR32k))

  private def templateDomain(name: String, geometry: SurfaceGeometry, mask: Option[SurfaceRoi[Boolean]] = None): Domain =
    ok(Domain.build(ok(DomainId(name)), SpaceRef.Template(TemplateName.unsafe(name), None, TemplateKind.Surface), ok(SamplingGeometry.surface(geometry, mask))))

  private def resampling(name: String, source: Domain, target: Domain, plan: TemplateResamplingPlan): Either[SpatialError, Morphism] =
    Morphism.between(ok(MorphismId(name)), source, target, MorphismKind.SurfaceToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.sphereResampling(plan))

  private def column(values: Array[Double]): DMat = DoubleMatrix.unsafe(values.length, 1, values.clone())

  private def columnValues(matrix: DMat): Array[Double] = Array.tabulate(matrix.rows)(matrix(_, 0))

  private def maxAbs(a: Array[Double], b: Array[Double]): Double =
    assertEquals(a.length, b.length)
    a.indices.map(i => math.abs(a(i) - b(i))).max

  /** A smooth field of each sphere vertex's position. */
  private def field(sphere: TemplateSphere[?]): Array[Double] =
    val c = sphere.sphere.coordinates
    Array.tabulate(sphere.sphere.vertexCount)(i => 1.0 + c(3 * i) / 100.0 + 0.5 * c(3 * i + 1) * c(3 * i + 2) / 10000.0)

  private val fsLRDomain = templateDomain("fsLR_32k", fsLR32k.geometry)
  private val fsAverage5Domain = templateDomain("fsaverage5", fsAverage5.geometry)

  private def compile(domains: Vector[Domain], edges: Vector[Morphism], from: Domain, to: Domain): SpatialOperator =
    ok(OperatorCompiler.compile(ok(SpatialGraph.build(domains, edges)), CompileRequest(from.id, to.id)))

  test("a sphere resampling edge compiles to the plan's interpolation, and its transpose is the plan's adjoint"):
    val operator = compile(Vector(fsLRDomain, fsAverage5Domain), Vector(ok(resampling("fslr-to-fsaverage5", fsLRDomain, fsAverage5Domain, toFsAverage5))), fsLRDomain, fsAverage5Domain)
    assertEquals((operator.rows, operator.cols), (10242, 32492))
    assertEquals(operator.provenance.compiler, "mixed-pullback-fused-v1")
    assert(operator.qc.coverage.rowCoverage.forall(c => math.abs(c - 1.0) <= 1e-12), "every fsaverage5 vertex is covered")
    val values = field(fsLR32k)
    val compiled = columnValues(ok(operator.forward(column(values))))
    assert(maxAbs(compiled, ok(toFsAverage5.resample(values))) <= 1e-12)
    // a smooth field is interpolated, not merely reproduced: the result is close to the field on the target sphere
    assert(maxAbs(compiled, field(fsAverage5)) <= 0.02, s"interpolation error ${maxAbs(compiled, field(fsAverage5))}")
    val back = field(fsAverage5)
    val adjoint = columnValues(ok(operator.map.adjoint.forward(column(back))))
    assert(maxAbs(adjoint, ok(toFsAverage5.adjoint(back, SurfaceResampling.Normalization.None))) <= 1e-12)
    val law = ok(SpatialQc.adjointLaw(operator, column(values), column(back)))
    assert(law.passed, law.toString)

  test("the edge is a value map: no point transform, no inverse, and no reverse route"):
    val edge = ok(resampling("fslr-to-fsaverage5", fsLRDomain, fsAverage5Domain, toFsAverage5))
    assert(edge.coordinateMap.transform(scalafim.image.SpatialPoint(1.0, 2.0, 3.0)).isLeft)
    assert(edge.coordinateMap.inverted.isLeft)
    assertEquals(edge.reversed, Left(SpatialError.NonInvertibleMorphism(edge.id)))
    val graph = ok(SpatialGraph.build(Vector(fsLRDomain, fsAverage5Domain), Vector(edge)))
    assertEquals(graph.path(fsAverage5Domain.id, fsLRDomain.id, allowInverses = true), Left(SpatialError.NoPath(fsAverage5Domain.id, fsLRDomain.id)))
    assert(edge.coordinateMap.fingerprint.startsWith("sphere-resampling-v1:"))
    assertNotEquals(edge.coordinateMap.fingerprint, CoordinateMap.sphereResampling(toFsLR32k).fingerprint)

  test("the edge's domains must be sampled on the plan's own sphere geometries"):
    // the same vertices admitted again are another geometry value: the plan does not describe them
    val again = ok(TemplateSphere.admit(fsLR32k.surface, SphereRegistration.FsAverage, fsLR32k.sphere, "synthetic:again"))
    val stranger = templateDomain("fsLR_32k-other", again.geometry)
    assertEquals(resampling("stranger", stranger, fsAverage5Domain, toFsAverage5).left.map(_.getClass), Left(classOf[SpatialError.SurfaceMappingGeometryMismatch]))
    // swapped endpoints
    assertEquals(resampling("swapped", fsAverage5Domain, fsLRDomain, toFsAverage5).left.map(_.getClass), Left(classOf[SpatialError.SurfaceMappingGeometryMismatch]))
    // an unsampled surface domain has no vertices to resample
    val unsampled = ok(Domain.build(ok(DomainId("fsLR_32k-unsampled")), SpaceRef.Template(TemplateName.unsafe("fsLR_32k"), None, TemplateKind.Surface), ok(SamplingGeometry.unsampled(DomainKind.Surface, WorldFrames.fsLR))))
    assert(resampling("unsampled", unsampled, fsAverage5Domain, toFsAverage5).isLeft)

  test("routes compose resampling weights: fsaverage5 -> fsLR 32k -> fsaverage5, then a vertex permutation"):
    val permuted = SurfaceGeometry(fsAverage5.sphere, Hemisphere.Left, SurfaceKind.Inflated)
    val permutedDomain = templateDomain("fsaverage5-permuted", permuted)
    val order = Vector.tabulate(10242)(i => VertexId((i * 7919) % 10242))
    val permutation =
      ok(
        Morphism.between(ok(MorphismId("permute")), fsAverage5Domain, permutedDomain, MorphismKind.SurfaceToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly,
          coordinateMap = CoordinateMap.surfaceVertices(SurfaceVertexMapping.nearestIndex(fsAverage5.geometry, permuted, order)))
      )
    // fsaverage5 -> fsLR 32k and back, on distinct domain ids so the route has two resampling steps
    val roundTripDomain = templateDomain("fsaverage5-roundtrip", fsAverage5.geometry)
    val there = ok(resampling("fsaverage5-to-fslr", roundTripDomain, fsLRDomain, toFsLR32k))
    val back = ok(resampling("fslr-to-fsaverage5", fsLRDomain, fsAverage5Domain, toFsAverage5))
    val operator = compile(Vector(roundTripDomain, fsLRDomain, fsAverage5Domain, permutedDomain), Vector(there, back, permutation), roundTripDomain, permutedDomain)
    assertEquals(operator.path.ids.map(_.value), Vector("fsaverage5-to-fslr", "fslr-to-fsaverage5", "permute"))
    val values = field(fsAverage5)
    val stepwise = ok(toFsLR32k.resample(values).flatMap(toFsAverage5.resample(_)))
    val expected = order.map(v => stepwise(v.index)).toArray
    assert(maxAbs(columnValues(ok(operator.forward(column(values)))), expected) <= 1e-12)

  test("surface masks: masked target rows are uncovered, masked source vertices drop out and the rest renormalise"):
    val targetMask = SurfaceRoi(fsAverage5.geometry, Array.range(0, 10242), Array.tabulate(10242)(i => i % 5 != 0), "target-mask")
    val maskedTarget = templateDomain("fsaverage5-masked", fsAverage5.geometry, Some(targetMask))
    // exclude a polar cap of the source sphere
    val c = fsLR32k.sphere.coordinates
    val sourceMask = SurfaceRoi(fsLR32k.geometry, Array.range(0, 32492), Array.tabulate(32492)(i => c(3 * i + 2) < 80.0), "source-mask")
    val maskedSource = templateDomain("fsLR_32k-masked", fsLR32k.geometry, Some(sourceMask))
    val operator = compile(Vector(maskedSource, maskedTarget), Vector(ok(resampling("masked", maskedSource, maskedTarget, toFsAverage5))), maskedSource, maskedTarget)
    val coverage = operator.qc.coverage.rowCoverage
    assert((0 until 10242).filter(_ % 5 == 0).forall(coverage(_) == 0.0), "masked target rows carry no weight")
    val t = fsAverage5.sphere.coordinates
    val far = (0 until 10242).filter(i => i % 5 != 0 && t(3 * i + 2) < 70.0)
    assert(far.forall(i => math.abs(coverage(i) - 1.0) <= 1e-12), "rows away from the masked cap are fully covered")
    val cap = (0 until 10242).filter(i => i % 5 != 0 && t(3 * i + 2) > 85.0)
    assert(cap.nonEmpty && cap.forall(coverage(_) == 0.0), "rows inside the masked cap are uncovered")
    // a constant field stays constant wherever a row has any weight: the kept weights are renormalised
    val ones = columnValues(ok(operator.forward(column(Array.fill(32492)(1.0)))))
    assert((0 until 10242).forall(i => if coverage(i) > 0.0 then math.abs(ones(i) - 1.0) <= 1e-12 else ones(i) == 0.0))

  test("a volume sampled onto one template sphere and resampled onto another compiles into one root operator"):
    // a linear field on a 3 x 3 x 3 grid spanning the sphere: trilinear sampling at the fsaverage5 vertices is exact
    val grid = ProviderAffines.fromRows(Vector(Vector(100.0, 0.0, 0.0, -100.0), Vector(0.0, 100.0, 0.0, -100.0), Vector(0.0, 0.0, 100.0, -100.0), Vector(0.0, 0.0, 0.0, 1.0)))
    val volume =
      ok(Domain.build(ok(DomainId("volume")), SpaceRef.Volume(ok(SubjectId("sub-01").asSpatial), None, ok(Modality("bold"))), ok(SamplingGeometry.volume(SampleSpaces(Vector(3, 3, 3), affine = Some(grid))))))
    def linear(x: Double, y: Double, z: Double): Double = 2.0 + 0.01 * x - 0.02 * y + 0.03 * z
    val voxels = Array.tabulate(27)(k => linear(-100.0 + 100.0 * (k / 9), -100.0 + 100.0 * ((k / 3) % 3), -100.0 + 100.0 * (k % 3)))
    val plan = VolumeSurfaceSamplingPlan(SurfaceGeometryPair(fsAverage5.geometry, fsAverage5.geometry), SurfaceSamplingPath.White)
    val bridge =
      ok(Morphism.between(ok(MorphismId("sample")), volume, fsAverage5Domain, MorphismKind.VolumeToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.volumeSamples(plan)))
    val onward = ok(resampling("to-fslr", fsAverage5Domain, fsLRDomain, toFsLR32k))
    val operator = compile(Vector(volume, fsAverage5Domain, fsLRDomain), Vector(bridge, onward), volume, fsLRDomain)
    assertEquals((operator.rows, operator.cols), (32492, 27))
    assert(operator.qc.coverage.rowCoverage.forall(c => math.abs(c - 1.0) <= 1e-12))
    val c = fsAverage5.sphere.coordinates
    val onSphere = Array.tabulate(10242)(i => linear(c(3 * i), c(3 * i + 1), c(3 * i + 2)))
    val expected = ok(toFsLR32k.resample(onSphere))
    assert(maxAbs(columnValues(ok(operator.forward(column(voxels)))), expected) <= 1e-10)

  test("a volume covering part of the sphere: rows renormalise over the bridge vertices that hit it"):
    // a 3 x 3 x 2 grid starting at z = 50: trilinear corners reach down to z = -50, and vertices below sample nothing
    val grid = ProviderAffines.fromRows(Vector(Vector(100.0, 0.0, 0.0, -100.0), Vector(0.0, 100.0, 0.0, -100.0), Vector(0.0, 0.0, 100.0, 50.0), Vector(0.0, 0.0, 0.0, 1.0)))
    val volume =
      ok(Domain.build(ok(DomainId("half-volume")), SpaceRef.Volume(ok(SubjectId("sub-01").asSpatial), None, ok(Modality("bold"))), ok(SamplingGeometry.volume(SampleSpaces(Vector(3, 3, 2), affine = Some(grid))))))
    val plan = VolumeSurfaceSamplingPlan(SurfaceGeometryPair(fsAverage5.geometry, fsAverage5.geometry), SurfaceSamplingPath.White)
    val bridge =
      ok(Morphism.between(ok(MorphismId("sample-half")), volume, fsAverage5Domain, MorphismKind.VolumeToSurface, RouteTag.Anatomical, inverse = Inverse.AdjointOnly, coordinateMap = CoordinateMap.volumeSamples(plan)))
    val onward = ok(resampling("to-fslr", fsAverage5Domain, fsLRDomain, toFsLR32k))
    val operator = compile(Vector(volume, fsAverage5Domain, fsLRDomain), Vector(bridge, onward), volume, fsLRDomain)
    val coverage = operator.qc.coverage.rowCoverage
    assert(coverage.exists(c => c > 0.0 && c < 1.0 - 1e-9), "some fsLR rows straddle the volume's edge")
    assert(coverage.contains(0.0) && coverage.exists(c => math.abs(c - 1.0) <= 1e-12))
    // a constant volume stays constant wherever a row has any weight: partial rows are not pulled towards zero
    val ones = columnValues(ok(operator.forward(column(Array.fill(18)(1.0)))))
    assert(coverage.indices.forall(i => if coverage(i) > 0.0 then math.abs(ones(i) - 1.0) <= 1e-12 else ones(i) == 0.0))
