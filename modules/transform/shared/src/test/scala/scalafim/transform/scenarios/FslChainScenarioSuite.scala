package scalafim.transform.scenarios

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, D3, Frame, Grid, GridId, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.field.{DeterminantValue, InversionError, InversionGateFailure}
import reframe4s.lie.FramedAffine
import reframe4s.resample.Interpolation
import scalafim.image.world.{FrameCatalog, FslVolumeGeometry, WorldSpace}
import scalafim.scenarios.{CaveatKind, CaveatSeverity, ScenarioCaveat, ScenarioHarness, ScenarioObservation, ScenarioPolicy, ScenarioResult, ScenarioStatus, ScenarioTolerance}
import scalafim.transform.*
import scalafim.transform.field.{
  FnirtCoefficientContext,
  FnirtCoefficientInterpretation,
  FnirtCoefficientsCodec,
  FnirtContext,
  FnirtDefinition,
  FnirtFieldInterpretation,
  VectorFieldNiftiCodec
}
import scalafim.transform.fsl.{FlirtCodec, FlirtInterpretation, FlirtMatrix, FslHeaderGeometry}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.OracleFixtures
import ChainScenarioSupport.*

/** Scenario `transform.fsl-chain.v1`: FSL's standard registration chain, example_func -> highres (a FLIRT matrix)
  * followed by highres -> standard (FNIRT `--cout` coefficients fitted with `--aff`), composed into one field on the
  * standard lattice as `convertwarp --premat=example_func2highres.mat --warp1=highres2standard_warp` would.
  *
  * Workflow risk protected: FSL's matrices live in each volume's scaled-voxel coordinates, mirrored along x for a
  * neurological volume, and a FLIRT matrix maps input to reference while resampling needs the pullback. The scenario
  * fails if the FLIRT direction is swapped or the x mirror is dropped (both mutations are run below); a composite in the
  * wrong order does not compile.
  *
  * References:
  *   - the FNIRT leg: native FSL 5.0.9 `applywarp --warp=coef` coordinate ramps and phantom image
  *     (`neurotransform/fsl_coef_oracle/srcleft_refright_aff`, fitted with `--aff`);
  *   - the FLIRT leg: a mathematical oracle. The matrix is synthesized from a declared world-space registration `W`
  *     through FSL's documented scaled-voxel convention, written inline here (not through the module), so the
  *     interpretation must recover `W` itself;
  *   - the Jacobian: native FSL 5.0.9 `fnirtfileutils --jac --withaff` of a real FLIRT+FNIRT registration to MNI152
  *     (`fsl_jacobian`, in its own declared frames), compared through statistical gates (caveat JacobianGates).
  *
  * A law check, not an oracle: the chain rule `det D(W^-1 . pull) = det(W^-1) det D pull` on the composed field.
  *
  * No native FLIRT output or `convertwarp --premat` output exists for this pair, so the composed field is checked as the
  * FLIRT oracle applied to the native FNIRT output (caveat NoNativeComposition).
  */
class FslChainScenarioSuite extends munit.FunSuite:
  import FslChainScenarioSuite.{Caveats, Mutation, Policy}

  private val Id = "transform.fsl-chain.v1"
  private val Case = "neurotransform/fsl_coef_oracle/srcleft_refright_aff"
  private val CoefPath = s"$Case/coef.nii.gz"

  private val func: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fsl chain example_func (synthesized)")))
  private val highres: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fsl_coef_oracle source (highres)")))
  private val standard: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fsl_coef_oracle reference (standard)")))
  // the fsl_jacobian registration is a different subject and template: its own frames
  private val jacobianSource: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fsl_jacobian highres (FLIRT+FNIRT source)")))
  private val jacobianTemplate: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fsl_jacobian MNI152 2 mm block")))

  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private def affine(rowMajor: Vector[Double]): Affine[D3] = ok(Affine.fromRowMajor[D3](rowMajor))
  private def compose(first: Affine[D3], second: Affine[D3]): Affine[D3] = ok(first.andThen(second))

  // ------------------------------------------------------------------------------------ the declared registration

  private val highresGeometry: FslVolumeGeometry = ok(FslHeaderGeometry(raw(s"$Case/source.nii.gz")))
  private val standardGeometry: FslVolumeGeometry = ok(FslHeaderGeometry(raw(s"$Case/target.nii.gz")))

  /** `W`, example_func world -> highres world: a 9-DOF FLIRT-like registration (rotation, anisotropic scale, shift)
    * about the highres centre.
    */
  private val registration: Affine[D3] =
    val centre = affineAt(highresGeometry.voxelToWorld, highresGeometry.dims.map(n => (n - 1) / 2.0))
    val (cz, sz, cx, sx) = (math.cos(0.06), math.sin(0.06), math.cos(-0.04), math.sin(-0.04))
    val rotation = Vector(Vector(cz, -sz * cx, sz * sx), Vector(sz, cz * cx, -cz * sx), Vector(0.0, sx, cx))
    val scale = Vector(1.02, 0.99, 1.01)
    val shift = Vector(1.2, -0.8, 2.1)
    val linear = Vector.tabulate(3, 3)((r, c) => rotation(r)(c) * scale(c))
    // x -> centre + shift + L (x - centre)
    val offset = Vector.tabulate(3)(r => centre(r) + shift(r) - (0 until 3).map(c => linear(r)(c) * centre(c)).sum)
    affine(Vector.tabulate(3)(r => linear(r) :+ offset(r)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0))

  /** An oblique, anisotropic, neurological (positive-determinant) functional grid covering the highres volume. */
  private val funcGeometry: FslVolumeGeometry =
    val pixdim = Vector(2.2, 2.0, 2.6)
    val (cy, sy) = (math.cos(0.1), math.sin(0.1))
    val rotation = Vector(Vector(cy, 0.0, sy), Vector(0.0, 1.0, 0.0), Vector(-sy, 0.0, cy))
    val toFunc = registration.inverse
    val corners =
      for i <- Vector(-1.0, highresGeometry.dims(0).toDouble); j <- Vector(-1.0, highresGeometry.dims(1).toDouble); k <- Vector(-1.0, highresGeometry.dims(2).toDouble)
      yield affineAt(toFunc, affineAt(highresGeometry.voxelToWorld, Vector(i, j, k)))
    // grid coordinates u = R^T x / pixdim, padded by three voxels
    val u = corners.map(x => Vector.tabulate(3)(a => (0 until 3).map(r => rotation(r)(a) * x(r)).sum / pixdim(a)))
    val lo = Vector.tabulate(3)(a => math.floor(u.map(_(a)).min) - 3.0)
    val dims = Vector.tabulate(3)(a => (math.ceil(u.map(_(a)).max) + 3.0 - lo(a)).toInt + 1)
    val origin = Vector.tabulate(3)(r => (0 until 3).map(a => rotation(r)(a) * lo(a) * pixdim(a)).sum)
    val sform = affine(Vector.tabulate(3)(r => Vector.tabulate(3)(a => rotation(r)(a) * pixdim(a)) :+ origin(r)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0))
    ok(FslVolumeGeometry.fromHeader(dims, pixdim, 0, None, 1, Some(sform)))

  /** FSL's scaled-voxel convention, stated independently of the module: voxel * pixdim, x mirrored for a volume whose
    * voxel-to-world affine has a positive determinant (FSL's "neurological" storage).
    */
  private def fslVoxelToScaled(dims: Vector[Int], pixdim: Vector[Double], voxelToWorld: Affine[D3], mirror: Boolean): Affine[D3] =
    val m = voxelToWorld.rowMajor
    val det = m(0) * (m(5) * m(10) - m(6) * m(9)) - m(1) * (m(4) * m(10) - m(6) * m(8)) + m(2) * (m(4) * m(9) - m(5) * m(8))
    val x = if mirror && det > 0.0 then Vector(-pixdim(0), 0.0, 0.0, (dims(0) - 1) * pixdim(0)) else Vector(pixdim(0), 0.0, 0.0, 0.0)
    affine(x ++ Vector(0.0, pixdim(1), 0.0, 0.0, 0.0, 0.0, pixdim(2), 0.0, 0.0, 0.0, 0.0, 1.0))

  private def scaled(g: FslVolumeGeometry, mirror: Boolean): Affine[D3] = fslVoxelToScaled(g.dims, g.pixdim, g.voxelToWorld, mirror)

  /** example_func2highres.mat: input (func) scaled-voxel mm -> reference (highres) scaled-voxel mm, written as FLIRT
    * writes it (ten decimals) and read back through the codec.
    */
  private val flirtMatrix: FlirtMatrix =
    val funcScaledToWorld = compose(scaled(funcGeometry, mirror = true).inverse, funcGeometry.voxelToWorld)
    val highresWorldToScaled = compose(highresGeometry.voxelToWorld.inverse, scaled(highresGeometry, mirror = true))
    val m = compose(compose(funcScaledToWorld, registration), highresWorldToScaled)
    val text = m.rowMajor.grouped(4).map(_.map(v => f"$v%.10f").mkString("  ")).mkString("", "\n", "\n")
    ok(FlirtCodec.decode(TransformSource.Text(text)))

  // --------------------------------------------------------------------------------------------- the candidate chain

  private def flirtLeg(mutation: Mutation): WorldTransform.Linear[func.type, highres.type] =
    val asset = AssetRef("example_func2highres.mat", None)
    mutation match
      case Mutation.FlirtDirectionSwapped =>
        // the matrix read as highres -> func, then inverted: FLIRT's input/reference roles swapped
        ok(FlirtInterpretation.interpretWith(flirtMatrix, FslGrids[highres.type, func.type](highres, highresGeometry, func, funcGeometry), asset)).inverse
      case Mutation.FslXFlipDropped =>
        // the pullback srcScaledToWorld . M^-1 . refWorldToScaled with unmirrored scaled-voxel coordinates
        val flirt = affine(flirtMatrix.rowMajor)
        val pull = compose(compose(compose(highresGeometry.voxelToWorld.inverse, scaled(highresGeometry, mirror = false)), flirt.inverse), compose(scaled(funcGeometry, mirror = false).inverse, funcGeometry.voxelToWorld))
        WorldTransform.Linear(FramedAffine.betweenFrames[highres.type, func.type, D3](highres, func)(pull), TransformProvenance.read(TransformFormat.FslFlirt, asset))
      case Mutation.Faithful =>
        ok(FlirtInterpretation.interpretWith(flirtMatrix, FslGrids[func.type, highres.type](func, funcGeometry, highres, highresGeometry), asset))

  private val fnirtLeg: WorldTransform.Mapped[highres.type, standard.type] =
    val file = ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(CoefPath)))))
    val grids = FslGrids[highres.type, standard.type](highres, highresGeometry, standard, standardGeometry)
    ok(FnirtCoefficientInterpretation.interpretWith(file, FnirtCoefficientContext(grids), AssetRef(CoefPath, Some(OracleFixtures.sha256Hex(CoefPath)))))

  private val standardGrid: Grid[standard.type, D3] = ok(Grid.forFrame[D3, standard.type](standard)(standardGeometry.dims, standardGeometry.voxelToWorld))

  private def voxels(dims: Vector[Int]): Vector[Vector[Int]] =
    for i <- (0 until dims(0)).toVector; j <- 0 until dims(1); k <- 0 until dims(2) yield Vector(i, j, k)

  // ------------------------------------------------------------------------------------------------------ scenario

  private def runScenario(mutation: Mutation): ScenarioResult =
    val flirt = flirtLeg(mutation)
    val chain: WorldTransform.Mapped[func.type, standard.type] = flirt.andThen(fnirtLeg)
    val toFunc = registration.inverse

    // Native FNIRT output: applywarp of the RAS coordinate ramps, exact where the two-voxel source-interior support is 1.
    val ramps = Vector(0, 1, 2).map(i => raw(s"$Case/native_coord$i.nii.gz"))
    val support = raw(s"$Case/native_support.nii.gz")
    val supported = voxels(standardGeometry.dims).filter(v => support.value(v(0), v(1), v(2)) >= 0.999)
    def nativeHighres(v: Vector[Int]): Vector[Double] = ramps.map(_.value(v(0), v(1), v(2)))

    // (1) the FLIRT leg recovers W^-1 at asymmetric points off the grid centre
    val probes = Vector(Vector(-11.3, 7.9, 4.2), Vector(23.5, -14.1, 9.7), Vector(3.3, 30.2, -12.8), Vector(-27.6, -19.4, 21.1), Vector(8.8, 2.2, 31.5), Vector(-4.4, -33.3, -6.6), Vector(17.2, 12.9, -22.4), Vector(0.5, -0.7, 1.9))
    val flirtErrors = probes.map: h =>
      flirt.pullPoint(ok(Point.fromVector(highres, h))).fold(_ => Double.PositiveInfinity, p => maxAbsDifference(p.coordinates, affineAt(toFunc, h)))

    // (2) the FNIRT leg and (3) the composed field, materialized on the standard lattice (convertwarp --absout)
    val fnirtField = fnirtLeg.materialize(standardGrid)
    val chainField = chain.materialize(standardGrid)
    def fieldErrors[S <: Frame[D3]](field: Either[TransformError, MaterializedField[S, standard.type]], expected: Vector[Int] => Vector[Double]): Either[TransformError, Vector[Double]] =
      field.map: f =>
        supported.map(v => maxAbsDifference(Vector.tabulate(3)(c => ok(f.field.coordinates.valueAt(v, Vector(c)))), expected(v)))
    val fnirtErrors = fieldErrors(fnirtField, nativeHighres)
    val chainErrors = fieldErrors(chainField, v => affineAt(toFunc, nativeHighres(v)))
    val fullCoverage = chainField.exists(f => f.coverage.counts.covered == standardGeometry.dims.product.toLong)

    // (4) intensities: the highres phantom through the FNIRT leg against applywarp's native output
    val highresRaw = raw(s"$Case/source.nii.gz")
    val highresGrid = ok(Grid.forFrame[D3, highres.type](highres)(highresGeometry.dims, highresGeometry.voxelToWorld))
    val phantom = ok(Sampled.continuous(highresGrid, NonSpatialAxes.empty, NDArray.tabulate[Double](highresGeometry.dims(0), highresGeometry.dims(1), highresGeometry.dims(2))((i, j, k) => highresRaw.value(i, j, k))))
    val nativeSource = raw(s"$Case/native_source.nii.gz")
    // Bound: the 2e-5 mm coordinate bound times the phantom's steepest voxel-to-voxel slope, plus float32 rounding of
    // the stored samples and of applywarp's arithmetic (four half-ulps of the largest magnitude).
    val phantomTolerance =
      val d = highresGeometry.dims
      val slopes =
        for v <- voxels(d); axis <- 0 until 3 if v(axis) + 1 < d(axis) yield
          val w = v.updated(axis, v(axis) + 1)
          math.abs(highresRaw.value(w(0), w(1), w(2)) - highresRaw.value(v(0), v(1), v(2))) / highresGeometry.pixdim(axis)
      val largest = voxels(d).map(v => math.abs(highresRaw.value(v(0), v(1), v(2)))).max
      slopes.max * 2e-5 + 4.0 * math.ulp(largest.toFloat).toDouble / 2.0
    val phantomErrors = fnirtLeg.resample(phantom, standardGrid, Interpolation.Linear, BoundaryPolicy.Constant(0.0)).map: r =>
      supported.map(v => math.abs(r.image.data.at(IArray(v(0), v(1), v(2))) - nativeSource.value(v(0), v(1), v(2))))

    // (5) intensities through the whole chain: a linear ramp on the functional grid is resampled exactly by trilinear
    // interpolation, so its value at the reference functional point is the expected sample
    val funcGrid = ok(Grid.forFrame[D3, func.type](func)(funcGeometry.dims, funcGeometry.voxelToWorld))
    def ramp(i: Double, j: Double, k: Double): Double = 40.0 + 1.5 * i - 0.7 * j + 2.2 * k
    val funcImage = ok(Sampled.continuous(funcGrid, NonSpatialAxes.empty, NDArray.tabulate[Double](funcGeometry.dims(0), funcGeometry.dims(1), funcGeometry.dims(2))((i, j, k) => ramp(i, j, k))))
    val funcIndexOf = funcGeometry.voxelToWorld.inverse
    val inFunc = supported.flatMap: v =>
      val index = affineAt(funcIndexOf, affineAt(toFunc, nativeHighres(v)))
      Option.when(index.zip(funcGeometry.dims).forall((c, n) => c >= 0.01 && c <= n - 1.01))(v -> ramp(index(0), index(1), index(2)))
    val rampErrors = chain.resample(funcImage, standardGrid, Interpolation.Linear, BoundaryPolicy.Constant(0.0)).map: r =>
      inFunc.map((v, expected) => math.abs(r.image.data.at(IArray(v(0), v(1), v(2))) - expected))
    // the ramp's world gradient bounds how far a 5e-5 mm point error can move a sample
    val rampGradient =
      val m = funcIndexOf.rowMajor
      val g = Vector(1.5, -0.7, 2.2)
      math.sqrt((0 until 3).map(c => math.pow((0 until 3).map(r => g(r) * m(4 * r + c)).sum, 2)).sum)

    // (6) Jacobians: the chain rule on the composed field (a law check), then FSL's own --jac on a real registration
    val detToFunc =
      val m = toFunc.rowMajor
      m(0) * (m(5) * m(10) - m(6) * m(9)) - m(1) * (m(4) * m(10) - m(6) * m(8)) + m(2) * (m(4) * m(9) - m(5) * m(8))
    val chainRule =
      for
        whole <- chain.jacobianDeterminant(standardGrid)
        leg <- fnirtLeg.jacobianDeterminant(standardGrid)
      yield voxels(standardGeometry.dims).map: v =>
        (whole.at(v), leg.at(v)) match
          case (Some(DeterminantValue.Regular(a)), Some(DeterminantValue.Regular(b))) => math.abs(a - detToFunc * b) / math.abs(detToFunc * b)
          case _                                                                      => Double.PositiveInfinity

    // (7) direction: FNIRT coefficients carry no inverse, and a numerical estimate of this field is refused, not degraded.
    // The fixed-point solver starts every functional point at the identity, which for a field containing FLIRT and
    // --aff affines lies outside the pull's support: the coverage gate must be among the failures.
    val probe = ok(Point.fromVector(func, affineAt(toFunc, affineAt(highresGeometry.voxelToWorld, highresGeometry.dims.map(_ / 2.0)))))
    val noForward = chain.push.isEmpty && chain.mapPoint(probe).left.exists(_.isInstanceOf[TransformError.NoForwardMap])
    val inversion =
      for
        field <- chainField
        lattice <- GridId.parse("fsl-chain-func").flatMap(id => Grid.createPersistent[D3, func.type](id, func)(funcGeometry.dims, funcGeometry.voxelToWorld)).left.map(TransformError.Geometry(_))
        policy <- InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 0.05, p99Residual = 0.01)
      yield field.invertNumerically(lattice, policy)
    val refused = inversion match
      case Right(Left(TransformError.Inversion(InversionError.GatesFailed(failures, _)))) if failures.exists(_.isInstanceOf[InversionGateFailure.CoverageBelowMinimum]) =>
        Right(failures.map(_.toString).mkString("; "))
      case other                                                                          => Left(other.toString)

    // (8) provenance: FLIRT then FNIRT, the coefficient file with the hash FSL's oracle manifest recorded
    val expectedProvenance = Vector(
      TransformProvenance.Step.Read(TransformFormat.FslFlirt, AssetRef("example_func2highres.mat", None)),
      TransformProvenance.Step.Read(TransformFormat.FslFnirtCoefficients, AssetRef(CoefPath, recordedSha256("neurotransform/fsl_coef_oracle/manifest.json", "srcleft_refright_aff/coef.nii.gz")))
    )

    val coefHashRecorded = expectedProvenance.exists:
      case TransformProvenance.Step.Read(TransformFormat.FslFnirtCoefficients, asset) => asset.sha256.isDefined
      case _                                                                         => false

    def errorsOf(name: String, errors: Either[TransformError, Vector[Double]], tolerance: Double): ScenarioObservation =
      errors.fold(error => ScenarioHarness.fact(name, false, error.toString), e => ScenarioHarness.scalar(name, worst(e), 0.0, ScenarioTolerance.absolute(tolerance)))

    val observations =
      Vector(
        ScenarioHarness.fact("func.neurological", funcGeometry.neurological, "the x mirror applies to the functional grid"),
        ScenarioHarness.fact("fnirt.supported-voxels", supported.size >= 100, s"${supported.size} standard voxels on the native source interior"),
        // FLIRT text keeps ten decimals: 1e-10 per entry over coordinates below 100 mm
        ScenarioHarness.scalar("flirt-leg.pullback.max-abs-mm", worst(flirtErrors), 0.0, ScenarioTolerance.absolute(1e-6)),
        // applywarp ramps are float32 trilinear arithmetic on |x| < 64 mm: 2e-5 mm (FnirtCoefficientOracleSuite)
        errorsOf("fnirt-leg.materialized-vs-applywarp.max-abs-mm", fnirtErrors, 2e-5),
        // W^-1 is within 2% of a rotation: a 2e-5 mm component error becomes at most sqrt(3) * 1.02 * 2e-5 = 3.5e-5 mm
        // in any component; 5e-5 mm leaves headroom
        errorsOf("chain.materialized-vs-reference.max-abs-mm", chainErrors, 5e-5),
        ScenarioHarness.fact("chain.materialized-coverage", fullCoverage, chainField.fold(_.toString, f => f.coverage.counts.toString)),
        errorsOf("fnirt-leg.phantom-vs-applywarp.max-abs", phantomErrors, phantomTolerance),
        ScenarioHarness.fact("chain.ramp-voxels", inFunc.size >= 100, s"${inFunc.size} supported voxels pull back inside the functional grid"),
        errorsOf("chain.ramp-resample.max-abs", rampErrors, rampGradient * 5e-5 + 1e-9),
        errorsOf("chain.jacobian-chain-rule.max-rel", chainRule, 1e-9),
        ScenarioHarness.fact("chain.push-unavailable", noForward, "no FNIRT inverse was supplied"),
        ScenarioHarness.fact("chain.inversion-refused-by-gates", refused.isRight, refused.merge),
        ScenarioHarness.fact("chain.provenance", chain.provenance.steps == expectedProvenance && coefHashRecorded, chain.provenance.describe)
      ) ++ nativeJacobian
    ScenarioHarness.result(Id, observations, Caveats)

  /** FSL 5.0.9 `fnirtfileutils --jac --withaff` of a real FLIRT+FNIRT registration to MNI152 against the determinant of
    * the same registration's `convertwarp --absout` field (a 20^3 brain block of the 2 mm template). FSL differentiates
    * the spline analytically; the dense field is differenced centrally, so the gates are statistical (neurotransform
    * measured median 0.36%, p99 2.3% over the whole brain; this block: median 0.60%, p99 3.3%, max 5.2%).
    */
  private def nativeJacobian: Vector[ScenarioObservation] =
    val dir = "fsl_jacobian"
    val warpField = ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(s"$dir/field_abs.nii.gz")))))
    val sourceGeometry = ok(FslHeaderGeometry(raw(s"$dir/source_header.nii.gz")))
    val frames = Frames[jacobianSource.type, jacobianTemplate.type](jacobianSource, jacobianTemplate)
    val warp = ok(FnirtFieldInterpretation.interpret(warpField, FnirtContext(frames, sourceGeometry, Some(FnirtDefinition.Absolute))))
    val lattice = ok(Grid.forFrame[D3, jacobianTemplate.type](jacobianTemplate)(warpField.spatialDims, ok(FslHeaderGeometry(warpField.raw)).voxelToWorld))
    val fsl = raw(s"$dir/jac.nii.gz")
    val support = raw(s"$dir/support.nii.gz")
    val shape = lattice.shape
    warp.jacobianDeterminant(lattice) match
      case Left(error) => Vector(ScenarioHarness.fact("fnirt-jacobian.determinant", false, error.toString))
      case Right(determinant) =>
        val interior = voxels(shape).filter(v => v.zip(shape).forall((x, n) => x > 0 && x < n - 1) && support.value(v(0), v(1), v(2)) > 0.5)
        val errors = interior.map: v =>
          val expected = fsl.value(v(0), v(1), v(2))
          determinant.at(v) match
            case Some(DeterminantValue.Regular(value)) if math.signum(value) == math.signum(expected) => math.abs(value - expected) / math.abs(expected)
            case _                                                                                  => Double.PositiveInfinity
        val sorted = errors.sorted
        def quantile(q: Double) = if sorted.isEmpty then Double.NaN else sorted(math.max(0, math.ceil(q * sorted.size).toInt - 1))
        Vector(
          ScenarioHarness.fact("fnirt-jacobian.interior-voxels", sorted.size == 18 * 18 * 18 && determinant.foldCount == 0L, s"${sorted.size} interior voxels, ${determinant.foldCount} folds"),
          ScenarioHarness.scalar("fnirt-jacobian.fsl-jac.median-rel", quantile(0.5), 0.0, ScenarioTolerance.absolute(0.01)),
          ScenarioHarness.scalar("fnirt-jacobian.fsl-jac.p99-rel", quantile(0.99), 0.0, ScenarioTolerance.absolute(0.05)),
          ScenarioHarness.scalar("fnirt-jacobian.fsl-jac.max-rel", quantile(1.0), 0.0, ScenarioTolerance.absolute(0.08))
        )

  // ----------------------------------------------------------------------------------------------------- tests

  test("FSL chain: example_func -> highres (FLIRT) -> standard (FNIRT --aff) composes to one field matching FSL"):
    val result = runScenario(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.PassWithCaveats, result.render)
    if !result.ciPass(Policy) then fail(result.render)

  test("every convention mutation of the FSL chain fails the scenario on the FLIRT guard"):
    Mutation.values.filterNot(_ == Mutation.Faithful).foreach: mutation =>
      val result = runScenario(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation must fail:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith("flirt-leg.pullback.max-abs-mm:")), s"$mutation must fail the FLIRT guard:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith("chain.materialized-vs-reference.max-abs-mm:")), s"$mutation must fail the composed field:\n${result.render}")

  test("the FLIRT and FNIRT legs compose only in registration order: a reversed composite does not compile"):
    val errors = compileErrors("fnirtLeg.andThen(flirtLeg(Mutation.Faithful))")
    assert(errors.contains("Required:") && errors.contains("this.standard") && errors.contains("this.func"), errors)

object FslChainScenarioSuite:
  /** Convention errors an FSL chain can make; `Faithful` is the correct reading. */
  enum Mutation derives CanEqual:
    case Faithful, FlirtDirectionSwapped, FslXFlipDropped

  /** FSL differentiates the FNIRT spline analytically; the module differences the dense field centrally. The native
    * Jacobian comparison is therefore statistical (median, p99 and max relative error), not elementwise.
    */
  val JacobianGates: ScenarioCaveat = ScenarioCaveat(
    id = "transform.fsl-jacobian-spline-vs-finite-difference",
    kind = CaveatKind.AlgorithmDivergence,
    severity = CaveatSeverity.Actionable,
    owner = "transform",
    followUp = Some("an analytic spline Jacobian for FNIRT coefficient files, or the FSL 6 --jac set (STP P4.04)"),
    detail = "fnirtfileutils --jac is matched through median < 1%, p99 < 5% and max < 8% relative-error gates, not elementwise"
  )

  /** The fixtures hold no native FLIRT output and no `convertwarp --premat` output for this pair. */
  val NoNativeComposition: ScenarioCaveat = ScenarioCaveat(
    id = "transform.fsl-chain-no-native-flirt-convertwarp",
    kind = CaveatKind.FixtureFreshness,
    severity = CaveatSeverity.Actionable,
    owner = "transform",
    followUp = Some("STP P4.04: native flirt -applyxfm and convertwarp --premat outputs for a func/highres/standard triple"),
    detail = "the FLIRT leg and the composed field are checked against a mathematical FLIRT oracle applied to native FNIRT output"
  )

  val Caveats: Vector[ScenarioCaveat] = Vector(JacobianGates, NoNativeComposition)

  /** The manifest's policy for this scenario: pass with exactly the two declared caveats. */
  val Policy: ScenarioPolicy = ScenarioPolicy(Set(ScenarioStatus.PassWithCaveats), Caveats.map(_.id).toSet)
