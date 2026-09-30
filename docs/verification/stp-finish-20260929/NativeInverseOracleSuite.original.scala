package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Grid, GridId, LatticeIndex}
import scalafim.image.world.{FrameCatalog, ToolCoordinates, WorldSpace}
import scalafim.transform.field.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.oracle.OracleFixtures

/** Successful estimates against native inverse assets on explicitly bounded interior domains.
  * This does not qualify the large affine registrations that require an upstream start guess.
  */
class NativeInverseOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("native inverse source")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("native inverse target")))
  private val frames = Frames[source.type, target.type](source, target)
  private val inverseFrames = Frames[target.type, source.type](target, source)
  private val residualLimit = 0.01 // millimetres, declared before comparing native assets

  private def field(path: String): VectorFieldNifti =
    ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))))

  /** Supremum infinity-norm derivative bound for the trilinear displacement.
    * Each voxel-axis derivative is a convex combination of edge differences;
    * multiplication by the inverse lattice affine converts it to physical units.
    */
  private def contractionBound(
      shape: Vector[Int],
      affine: Affine[D3],
      displacement: (Int, Int, Int) => Vector[Double]
  ): Double =
    val edges = Array.fill(3, 3)(0.0)
    for x <- 0 until shape(0); y <- 0 until shape(1); z <- 0 until shape(2) do
      val index = Vector(x, y, z)
      val here = displacement(x, y, z)
      for axis <- 0 until 3 if index(axis) + 1 < shape(axis) do
        val next = index.updated(axis, index(axis) + 1)
        val there = displacement(next(0), next(1), next(2))
        for component <- 0 until 3 do
          edges(component)(axis) = math.max(edges(component)(axis), math.abs(there(component) - here(component)))
    val inverse = affine.inverse.rowMajor
    (0 until 3).map: component =>
      (0 until 3).map(axis => edges(component)(axis) * (0 until 3).map(column => math.abs(inverse(4 * axis + column))).sum).sum
    .max

  private def qualifies(
      name: String,
      shape: Vector[Int],
      affine: Affine[D3],
      forward: WorldTransform.Mapped[source.type, target.type],
      nativeInverse: WorldTransform.Mapped[target.type, source.type],
      displacement: (Int, Int, Int) => Vector[Double]
  )(using munit.Location): Unit =
    val k = contractionBound(shape, affine, displacement)
    assert(k < 1.0, s"$name is not a certified contraction: $k")
    // Two voxel layers are excluded on every side before inversion. No failed
    // inverse samples are filtered out of this declared evaluation domain.
    val matrix = affine.rowMajor
    val cropped = ok(Affine.fromRowMajor[D3](matrix.updated(3, matrix(3) + 2 * (matrix(0) + matrix(1) + matrix(2)))
      .updated(7, matrix(7) + 2 * (matrix(4) + matrix(5) + matrix(6)))
      .updated(11, matrix(11) + 2 * (matrix(8) + matrix(9) + matrix(10)))))
    val lattice = ok(GridId.parse(s"native-inverse-$name").flatMap(id => Grid.createPersistent[D3, source.type](id, source)(shape.map(_ - 4), cropped)))
    val policy = ok(InversionPolicy.create(minimumCoverage = 1.0, maximumResidual = residualLimit, p99Residual = residualLimit, tolerance = 1e-8))
    val estimated = ok(forward.invertNumerically(lattice, policy))
    val qualified = estimated.availability match
      case PushAvailability.Estimated(value) => value
      case other => fail(s"expected qualified estimate, got $other")
    assertEqualsDouble(qualified.evidence.coveredFraction, 1.0, 0.0)
    assertEquals(qualified.evidence.interiorStatusCounts.diverged, 0L)
    assert(qualified.evidence.forwardResidual.exists(r => r.maximum <= residualLimit && r.p99 <= residualLimit))
    assert(qualified.evidence.reverseResidual.exists(r => r.maximum <= residualLimit && r.p99 <= residualLimit))
    var count = 0
    for x <- 0 until lattice.shape(0); y <- 0 until lattice.shape(1); z <- 0 until lattice.shape(2) do
      val point = ok(LatticeIndex.fromVector[D3](Vector(x, y, z)).flatMap(lattice.pointAt))
      val native = ok(nativeInverse.pullPoint(point))
      val actual = ok(estimated.mapPoint(point))
      val nativeResidual = ok(forward.pullPoint(native)).coordinates.zip(point.coordinates).map((a, b) => math.abs(a - b)).max
      val estimatedResidual = ok(forward.pullPoint(actual)).coordinates.zip(point.coordinates).map((a, b) => math.abs(a - b)).max
      assert(nativeResidual <= residualLimit, s"$name native inverse residual $nativeResidual at ${point.coordinates}")
      // F = identity + u, Lip_infinity(u) <= k < 1, hence
      // |a-b|_infinity <= (|F(a)-x| + |F(b)-x|)/(1-k).
      // The native inverse is an approximation too; never assert exact equality.
      val bound = (nativeResidual + estimatedResidual) / (1.0 - k) + 1e-10
      actual.coordinates.zip(native.coordinates).foreach((a, b) => assertEqualsDouble(a, b, bound, s"$name at ${point.coordinates}"))
      count += 1
    assert(count >= 100, s"$name compared only $count native inverse nodes")

  test("a successful gated estimate agrees with ANTs native InverseWarp on its declared interior"):
    val forwardFile = field("ants_native/syn_0Warp.nii.gz")
    val inverseFile = field("ants_native/syn_0InverseWarp.nii.gz")
    val affine = ok(LatticeAffine.of(forwardFile.raw, LatticeAffine.Itk))
    val forward = ok(LpsDisplacementInterpretation.Ants.interpret(forwardFile, DenseContext(frames)))
    val inverse = ok(LpsDisplacementInterpretation.Ants.interpret(inverseFile, DenseContext(inverseFrames)))
    qualifies("ants", forwardFile.spatialDims, affine, forward, inverse, (x, y, z) => Vector(-forwardFile.component(x, y, z, 0), -forwardFile.component(x, y, z, 1), forwardFile.component(x, y, z, 2)))

  test("a successful gated estimate agrees with FSL6 invwarp for the separately declared coincident-geometry control"):
    val forwardFile = field("neurotransform/fsl_dense_oracle/left_left_relative/warp.nii.gz")
    val inverseFile = field("fsl6_native/invwarp_dense_matched_geometry.nii.gz")
    // This control intentionally uses the original TARGET geometry at both
    // endpoints. It is not the original source-to-reference registration.
    val geometry = ok(FslHeaderGeometry(forwardFile.raw))
    val forward = ok(FnirtFieldInterpretation.interpret(forwardFile, FnirtContext(frames, geometry, Some(FnirtDefinition.Relative))))
    val inverse = ok(FnirtFieldInterpretation.interpret(inverseFile, FnirtContext(inverseFrames, geometry, Some(FnirtDefinition.Relative))))
    val toWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(geometry)).rowMajor
    qualifies("fsl", forwardFile.spatialDims, geometry.voxelToWorld, forward, inverse, (x, y, z) =>
      val vector = Vector.tabulate(3)(c => forwardFile.component(x, y, z, c))
      Vector.tabulate(3)(r => (0 until 3).map(c => toWorld(4 * r + c) * vector(c)).sum)
    )
