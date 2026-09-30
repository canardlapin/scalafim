package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Grid, GridId, LatticeIndex, Point}
import reframe4s.core.MapError
import scalafim.image.world.{FrameCatalog, ToolCoordinates, WorldSpace}
import scalafim.transform.field.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

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
  private val nativeCoordinateTolerance = 1e-6
  private val oracleRoot = "native_inverse_coordinates"

  private def field(path: String): VectorFieldNifti =
    ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))))

  private def point(frame: Frame[D3], coordinates: Vector[Double]) =
    ok(Point.in(frame)(coordinates(0), coordinates(1), coordinates(2)))

  private def affineAt(affine: Affine[D3], index: Vector[Double]): Vector[Double] =
    val m = affine.rowMajor
    Vector.tabulate(3)(row => m(4 * row) * index(0) + m(4 * row + 1) * index(1) + m(4 * row + 2) * index(2) + m(4 * row + 3))

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

  private def croppedAffine(affine: Affine[D3]): Affine[D3] =
    val matrix = affine.rowMajor
    ok(Affine.fromRowMajor[D3](matrix.updated(3, matrix(3) + 2 * (matrix(0) + matrix(1) + matrix(2)))
      .updated(7, matrix(7) + 2 * (matrix(4) + matrix(5) + matrix(6)))
      .updated(11, matrix(11) + 2 * (matrix(8) + matrix(9) + matrix(10)))))

  private def refinedAffine(coarse: Affine[D3], factor: Int): Affine[D3] =
    ok(Affine.fromRowMajor[D3](coarse.rowMajor.zipWithIndex.map: (value, index) =>
      if index / 4 < 3 && index % 4 < 3 then value / factor.toDouble else value
    ))

  private def table(path: String): OracleTable = OracleTable.load(s"$oracleRoot/$path")

  private def assertNativeCoordinates(
      name: String,
      nativeInverse: WorldTransform.Mapped[target.type, source.type],
      nodes: OracleTable,
      offNodes: OracleTable
  )(using munit.Location): Unit =
    val expectedNodes = if name == "ants" then 1287 else 693
    assertEquals(nodes.rows.size, expectedNodes, s"$name original cropped-node table count")
    assertEquals(offNodes.rows.size, 12, s"$name fixed off-node table count")
    for row <- nodes.rows ++ offNodes.rows do
      val query = row.slice(3, 6)
      val expected = row.slice(6, 9)
      val actual = ok(nativeInverse.pullPoint(point(source, query))).coordinates
      query.zip(actual).foreach: (q, a) =>
        assert(q.isFinite && a.isFinite, s"$name non-finite native coordinate")
      actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, nativeCoordinateTolerance, s"$name native table query $query"))

  private def assertOutsideRejected(
      name: String,
      nativeInverse: WorldTransform.Mapped[target.type, source.type],
      affine: Affine[D3]
  )(using munit.Location): Unit =
    val outside = point(source, affineAt(affine, Vector(-1e-6, 2.0, 2.0)))
    nativeInverse.pullPoint(outside) match
      case Left(TransformError.Map(MapError.OutsideDomain(_))) => ()
      case other => fail(s"$name native inverse must reject a point outside its field, got $other")

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
    // The native table and coarse probes retain the original two-voxel crop.
    // Independent knot-crossing diagnostics justify fixed quarter spacing for
    // ANTs. FSL retains half spacing. Physical crop and coarse probes are fixed.
    val cropped = croppedAffine(affine)
    val coarseShape = shape.map(_ - 4)
    val coarse = ok(Grid.createPersistent[D3, source.type](ok(GridId.parse(s"native-inverse-$name-coarse")), source)(coarseShape, cropped))
    val factor = if name == "ants" then 4 else 2
    val refinedShape = coarseShape.map(size => factor * (size - 1) + 1)
    val refined = ok(Grid.createPersistent[D3, source.type](ok(GridId.parse(s"native-inverse-$name-refined")), source)(refinedShape, refinedAffine(cropped, factor)))
    assertEquals(refined.shape, coarse.shape.map(size => factor * (size - 1) + 1))
    // Establish native-decoder agreement independently before evaluating the
    // estimated inverse. This evidence remains available if a residual gate
    // correctly rejects the estimate.
    assertNativeCoordinates(name, nativeInverse, table(s"${name}_inverse_nodes.tsv"), table(s"${name}_inverse_off_nodes.tsv"))
    assertOutsideRejected(name, nativeInverse, affine)
    val policy = ok(InversionPolicy.create(minimumCoverage = 1.0, maximumResidual = residualLimit, p99Residual = residualLimit, tolerance = 1e-8))
    val estimated = ok(forward.invertNumerically(refined, policy))
    val qualified = estimated.availability match
      case PushAvailability.Estimated(value) => value
      case other => fail(s"expected qualified estimate, got $other")
    assertEqualsDouble(qualified.evidence.coveredFraction, 1.0, 0.0)
    assertEquals(qualified.evidence.interiorStatusCounts.diverged, 0L)
    val estimatedForward = qualified.evidence.forwardResidual.getOrElse(fail(s"$name missing forward residual"))
    val estimatedReverse = qualified.evidence.reverseResidual.getOrElse(fail(s"$name missing reverse residual"))
    assert(estimatedForward.maximum <= residualLimit && estimatedForward.p99 <= residualLimit)
    assert(estimatedReverse.maximum <= residualLimit && estimatedReverse.p99 <= residualLimit)
    if name == "ants" then assertEquals(estimatedReverse.sampleCount, 962L, "original reverse-probe count must not shrink")

    // Recheck every original reverse probe admitted by the fixed physical
    // crop. This makes a silently skipped provider query a test failure.
    var reverseCount = 0L
    for x <- 1 until shape(0) - 1; y <- 1 until shape(1) - 1; z <- 1 until shape(2) - 1 do
      val original = point(target, affineAt(affine, Vector(x.toDouble, y.toDouble, z.toDouble)))
      val pulled = ok(forward.pullPoint(original))
      val coarseIndex = affineAt(cropped.inverse, pulled.coordinates)
      if coarseIndex.zip(coarseShape).forall((index, size) => index >= -1e-9 && index <= size - 1.0 + 1e-9) then
        val recovered = ok(estimated.mapPoint(pulled))
        val error = math.sqrt(recovered.coordinates.zip(original.coordinates).map((a, b) => (a - b) * (a - b)).sum)
        assert(error <= residualLimit, s"$name original reverse probe ($x,$y,$z): $error mm")
        reverseCount += 1
    assertEquals(reverseCount, estimatedReverse.sampleCount, s"$name every original reverse probe must be exercised")

    // Native decoder tables deliberately cover the whole field, including
    // points outside this solve crop. Estimate holdouts instead use fixed
    // fractions inside the already declared cropped physical domain.
    val holdouts = Vector(
      Vector(.17, .31, .42), Vector(.62, .24, .57), Vector(.38, .71, .23),
      Vector(.83, .44, .66), Vector(.29, .53, .81), Vector(.47, .19, .35),
      Vector(.71, .82, .46), Vector(.23, .63, .59), Vector(.54, .37, .74),
      Vector(.36, .48, .28), Vector(.68, .56, .39), Vector(.41, .76, .62)
    )
    assertEquals(holdouts.size, 12)
    holdouts.foreach: fractions =>
      val index = fractions.zip(coarseShape).map((fraction, size) => 1.0 + fraction * (size - 3.0))
      val probe = point(source, affineAt(cropped, index))
      val mapped = ok(estimated.mapPoint(probe))
      val recovered = ok(forward.pullPoint(mapped))
      val error = math.sqrt(recovered.coordinates.zip(probe.coordinates).map((a, b) => (a - b) * (a - b)).sum)
      assert(error <= residualLimit, s"$name fixed off-node estimated closure: $error mm")
    estimated.mapPoint(point(source, affineAt(cropped, Vector(-1e-6, 2.0, 2.0)))) match
      case Left(TransformError.Map(MapError.OutsideDomain(_))) => ()
      case other => fail(s"$name estimate must reject a point outside its declared crop, got $other")

    var count = 0
    var nativeClosureMaximum = 0.0
    var estimatedClosureMaximum = 0.0
    var estimateNativeMaximum = 0.0
    val estimateNativeErrors = scala.collection.mutable.ArrayBuffer.empty[Double]
    // Every original coarse node, including all faces, is queried. No failures
    // are filtered: coverage is already a hard gate on the refined solve.
    for x <- 0 until coarse.shape(0); y <- 0 until coarse.shape(1); z <- 0 until coarse.shape(2) do
      val probe = ok(LatticeIndex.fromVector[D3](Vector(x, y, z)).flatMap(coarse.pointAt))
      val native = ok(nativeInverse.pullPoint(probe))
      val actual = ok(estimated.mapPoint(probe))
      val nativeResidual = ok(forward.pullPoint(native)).coordinates.zip(probe.coordinates).map((a, b) => math.abs(a - b)).max
      val estimatedResidual = ok(forward.pullPoint(actual)).coordinates.zip(probe.coordinates).map((a, b) => math.abs(a - b)).max
      nativeClosureMaximum = math.max(nativeClosureMaximum, nativeResidual)
      estimatedClosureMaximum = math.max(estimatedClosureMaximum, estimatedResidual)
      val coordinateError = actual.coordinates.zip(native.coordinates).map((a, b) => math.abs(a - b)).max
      estimateNativeMaximum = math.max(estimateNativeMaximum, coordinateError)
      estimateNativeErrors += coordinateError
      // F = identity + u, Lip_infinity(u) <= k < 1, hence native inverse
      // assets are compared through their measured composition closure, never
      // assumed exact. The estimate's own qualification remains the 0.01 mm
      // residual gate above.
      val bound = (nativeResidual + estimatedResidual) / (1.0 - k) + 1e-10
      assert(coordinateError <= bound, s"$name estimate/native error $coordinateError exceeds residual certificate $bound at ${probe.coordinates}")
      count += 1
    val sorted = estimateNativeErrors.sorted
    val p99 = sorted(math.ceil(sorted.size * .99).toInt - 1)
    println(f"$name native inverse evidence: coarseNodes=$count reverseProbes=$reverseCount offNodes=12 k=$k%.9g estimatedForwardMax=${estimatedForward.maximum}%.9g estimatedReverseMax=${estimatedReverse.maximum}%.9g nativeClosureMax=$nativeClosureMaximum%.9g estimatedClosureMax=$estimatedClosureMaximum%.9g estimateNativeMax=$estimateNativeMaximum%.9g estimateNativeP99=$p99%.9g")
    assertEquals(count, coarse.shape.product, s"$name must query every original coarse node")

  test("a successful gated estimate agrees with independent ANTs native InverseWarp decoding on its declared interior"):
    val forwardFile = field("ants_native/syn_0Warp.nii.gz")
    val inverseFile = field("ants_native/syn_0InverseWarp.nii.gz")
    val affine = ok(LatticeAffine.of(forwardFile.raw, LatticeAffine.Itk))
    val forward = ok(LpsDisplacementInterpretation.Ants.interpret(forwardFile, DenseContext(frames)))
    val inverse = ok(LpsDisplacementInterpretation.Ants.interpret(inverseFile, DenseContext(inverseFrames)))
    qualifies("ants", forwardFile.spatialDims, affine, forward, inverse, (x, y, z) => Vector(-forwardFile.component(x, y, z, 0), -forwardFile.component(x, y, z, 1), forwardFile.component(x, y, z, 2)))

  test("a successful gated estimate agrees with independent FSL invwarp decoding for the coincident-geometry control"):
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
