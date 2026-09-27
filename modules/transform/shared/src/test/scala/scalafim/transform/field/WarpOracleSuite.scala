package scalafim.transform.field

import image4s.geometry.{Affine, D3, Frame, Grid, GridId, LatticeIndex, Point}
import reframe4s.field.{CoverageCounts, DeterminantValue, InversionError, InversionGateFailure}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.itk.{ItkHdf5Dumps, ItkHdf5Interpretation}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** STP Phase 6 warp algebra against native tool output: FSL 5.0.9 `convertwarp` (materialization), ITK
  * `TransformPoint` on composites (materialized composition), FSL `applywarp` coordinate ramps (numerical inversion),
  * and FSL `fnirtfileutils --jac` (Jacobian determinants).
  */
class WarpOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private def field(path: String): VectorFieldNifti =
    ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("oracle warp source")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("oracle warp target")))
  private val frames = Frames[source.type, target.type](source, target)

  private def fieldLattice(warpField: VectorFieldNifti): Grid[target.type, D3] =
    ok(Grid.forFrame[D3, target.type](target)(warpField.spatialDims, ok(FslHeaderGeometry(warpField.raw)).voxelToWorld))

  private def indices(shape: Vector[Int]): Vector[Vector[Int]] =
    for i <- (0 until shape(0)).toVector; j <- 0 until shape(1); k <- 0 until shape(2) yield Vector(i, j, k)

  private def pointAt[F <: Frame[D3]](lattice: Grid[F, D3], index: Vector[Int]): Point[F, D3] =
    ok(LatticeIndex.fromVector[D3](index).flatMap(lattice.pointAt))

  private val handedness = Vector("left_left", "left_right", "right_left", "right_right")

  test("materializing a relative FNIRT field on its lattice reproduces FSL convertwarp --absout in every handedness case"):
    handedness.foreach: pair =>
      // Reject: on these oblique lattices a face point's continuous index can round to just outside [0, n - 1], and
      // reframe4s drops the resulting outside stencil corner (weight ~1e-16) rather than rejecting the point.
      def fnirt(kind: String, definition: FnirtDefinition) =
        val dir = s"neurotransform/fsl_dense_oracle/${pair}_$kind"
        val sourceGeometry = ok(FslHeaderGeometry(raw(s"$dir/source.nii.gz")))
        val warpField = field(s"$dir/warp.nii.gz")
        (warpField, ok(FnirtFieldInterpretation.interpret(warpField, FnirtContext(frames, sourceGeometry, Some(definition)))))
      val (_, relative) = fnirt("relative", FnirtDefinition.Relative)
      val (absoluteField, absolute) = fnirt("absolute", FnirtDefinition.Absolute)
      val lattice = fieldLattice(absoluteField)
      val materialized = ok(relative.materialize(lattice))
      // every lattice point, faces included, is covered by the field itself
      assertEquals(materialized.coverage.counts, CoverageCounts(lattice.shape.product.toLong, 0L, 0L, 0L, 0L), pair)
      // convertwarp's absolute output, read through the FNIRT absolute interpretation, sampled at its own lattice points
      indices(lattice.shape).foreach: index =>
        val expected = ok(absolute.pullPoint(pointAt(lattice, index))).coordinates
        val actual = Vector.tabulate(3)(c => ok(materialized.field.coordinates.valueAt(index, Vector(c))))
        actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, 1e-4, s"$pair at $index")) // float32 storage

  test("materialized ITK composites reproduce ITK TransformPoint at the lattice points"):
    val context = DenseContext.itk(frames)
    val points = OracleTable.load("itk_hdf5/points.tsv")
    def lps(v: Vector[Double]) = Vector(-v(0), -v(1), v(2))
    Vector("affine_warp.h5", "warp_affine.h5").foreach: name =>
      val chain = ok(ItkHdf5Interpretation.interpret(ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5/${name.stripSuffix(".h5")}.components.txt")), context)).composed
      points.keyed.filter(_._1 == name).foreach: (_, row) =>
        // a small lattice whose first point is the oracle point
        val origin = lps(row.take(3))
        val lattice = ok(Grid.forFrame[D3, target.type](target)(Vector(2, 2, 2), ok(Affine.fromRowMajor[D3](Vector(0.5, 0, 0, origin(0), 0, 0.5, 0, origin(1), 0, 0, 0.5, origin(2), 0, 0, 0, 1)))))
        val materialized = ok(chain.materialize(lattice))
        val sampled = Vector.tabulate(3)(c => ok(materialized.field.coordinates.valueAt(Vector(0, 0, 0), Vector(c))))
        lps(sampled).zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-9, s"$name at ${row.take(3)}"))

  test("a native FSL dense field far from the identity is refused by the inversion gates, with its evidence"):
    // FNIRT maps between FSL scaled-voxel frames, so these fields send reference points to source points up to ~20 mm
    // away. The default identity start lies outside the pull's support here: the estimate must fail its coverage gate as
    // a typed failure, never return a partial success. (NumericalInverseParitySuite qualifies the same fields from an
    // affine-guess start against derived coverage bounds.)
    handedness.foreach: pair =>
      val dir = s"neurotransform/fsl_dense_oracle/${pair}_relative"
      val sourceRaw = raw(s"$dir/source.nii.gz")
      val sourceGeometry = ok(FslHeaderGeometry(sourceRaw))
      val warp = ok(FnirtFieldInterpretation.interpret(field(s"$dir/warp.nii.gz"), FnirtContext(frames, sourceGeometry, Some(FnirtDefinition.Relative))))
      val inverseLattice = ok(GridId.parse(s"fsl-dense-inverse-$pair").flatMap(id => Grid.createPersistent[D3, source.type](id, source)(sourceRaw.spatialShape, sourceGeometry.voxelToWorld)))
      val policy = ok(InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 0.5, p99Residual = 0.25))
      warp.invertNumerically(inverseLattice, policy) match
        case Left(TransformError.Inversion(InversionError.GatesFailed(failures, evidence))) =>
          assert(failures.exists(_.isInstanceOf[InversionGateFailure.CoverageBelowMinimum]), s"$pair: $failures")
          assert(evidence.coveredFraction < 0.5, pair)
          assertEquals(evidence.statusCounts.total, sourceRaw.spatialShape.product.toLong, pair)
        case other => fail(s"$pair: expected a coverage gate failure, got $other")
      assert(warp.mapPoint(ok(Point.fromVector(source, Vector(0.0, 0.0, 0.0)))).left.exists(_.isInstanceOf[TransformError.NoForwardMap]))

  test("Jacobian determinants of a real FNIRT registration match FSL fnirtfileutils --jac"):
    val dir = "fsl_jacobian"
    val warpField = field(s"$dir/field_abs.nii.gz")
    val sourceGeometry = ok(FslHeaderGeometry(raw(s"$dir/source_header.nii.gz")))
    val warp = ok(FnirtFieldInterpretation.interpret(warpField, FnirtContext(frames, sourceGeometry, Some(FnirtDefinition.Absolute))))
    val lattice = fieldLattice(warpField)
    val determinant = ok(warp.jacobianDeterminant(lattice))
    assertEquals(determinant.foldCount, 0L)
    val fsl = raw(s"$dir/jac.nii.gz")
    val support = raw(s"$dir/support.nii.gz")
    val shape = lattice.shape
    // FSL differentiates the B-spline analytically; the dense field is differenced centrally, so compare the interior
    val errors =
      for
        index <- indices(shape)
        if index.zip(shape).forall((v, n) => v > 0 && v < n - 1) && support.value(index(0), index(1), index(2)) > 0.5
      yield
        val expected = fsl.value(index(0), index(1), index(2))
        determinant.at(index) match
          case Some(DeterminantValue.Regular(value)) =>
            assert(math.signum(value) == math.signum(expected), s"sign at $index")
            math.abs(value - expected) / math.abs(expected)
          case other => fail(s"expected a regular determinant at $index, got $other")
    val sorted = errors.sorted
    val (median, p99, max) = (sorted(sorted.size / 2), sorted(math.ceil(0.99 * sorted.size).toInt - 1), sorted.last)
    // measured on this block: median 0.60%, p99 3.3%, max 5.2% (neurotransform, whole brain: median 0.36%, p99 2.3%)
    assertEquals(sorted.size, 18 * 18 * 18)
    assert(median < 0.01, s"median relative error $median")
    assert(p99 < 0.05, s"p99 relative error $p99")
    assert(max < 0.08, s"max interior relative error $max")
