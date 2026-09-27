package scalafim.transform.field

import image4s.geometry.{ContinuousIndex, D3, Frame, Point}
import reframe4s.field.{BSplineOrder, CoordinateBoundaryPolicy}
import scalafim.image.world.{FrameCatalog, FslVolumeGeometry, ToolCoordinates, WorldSpace}
import scalafim.transform.*
import scalafim.transform.Conversion.EncodedTransform
import scalafim.transform.fsl.{FlirtCodec, FlirtMatrix, FslHeaderGeometry}
import scalafim.transform.nifti.{NiftiRaw, NiftiWriter}
import scalafim.transform.oracle.OracleFixtures

/** FNIRT `--cout` coefficient files against native FSL 5.0.9 (`fsl_coef_oracle`): ten cases covering cubic and
  * quadratic splines, every source/reference handedness pair, with and without `--aff`, and knot spacings 1 to 5.
  *
  * Tolerances come from float32 storage of the native outputs. `fnirtfileutils` fields hold displacements of at most a
  * few tens of mm, whose float32 half-ulp is below 2e-6 mm. `applywarp` ramps hold RAS coordinates (|x| < 64 mm, half-ulp
  * 4e-6) after float32 trilinear arithmetic, bounded by 2e-5 mm. Both are tighter than the oracle's documented 1e-4 mm.
  */
class FnirtCoefficientOracleSuite extends munit.FunSuite:
  private val FieldTolerance = 2e-6
  private val CoordinateTolerance = 2e-5

  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val Root = "neurotransform/fsl_coef_oracle"
  private def bytes(path: String): IArray[Byte] = IArray.unsafeFromArray(OracleFixtures.decoded(path))
  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(bytes(path)))
  private def coefficients(name: String): FnirtCoefficientFile =
    ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(bytes(s"$Root/$name/coef.nii.gz"))))

  /** One manifest case: id, knot spacing in voxels, spline order, and whether FNIRT ran with `--aff`. */
  private final case class Case(name: String, knots: Vector[Int], order: Int, withAff: Boolean)

  private val cases: Vector[Case] =
    val manifest = OracleFixtures.text(s"$Root/manifest.json")
    val cases = manifest.substring(manifest.indexOf("\"cases\""), manifest.indexOf("\"commands\""))
    "(?s)\"id\":\\s*\"([^\"]+)\".*?\"with_aff\":\\s*(true|false).*?\"knot_spacing_vox\":\\s*\\[([^\\]]*)\\].*?\"spline_order\":\\s*(\\d)".r
      .findAllMatchIn(cases)
      .map(m => Case(m.group(1), m.group(3).split(',').toVector.map(_.trim.toInt), m.group(4).toInt, m.group(2) == "true"))
      .toVector

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fnirt source")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fnirt reference")))

  private final case class Loaded(
      file: FnirtCoefficientFile,
      sourceGeometry: FslVolumeGeometry,
      referenceGeometry: FslVolumeGeometry,
      warp: WorldTransform.Mapped[source.type, reference.type]
  )

  private def load(name: String, boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject): Loaded =
    val file = coefficients(name)
    val src = ok(FslHeaderGeometry(raw(s"$Root/$name/source.nii.gz")))
    val ref = ok(FslHeaderGeometry(raw(s"$Root/$name/target.nii.gz")))
    val grids = FslGrids[source.type, reference.type](source, src, reference, ref)
    Loaded(file, src, ref, ok(FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(grids, boundary))))

  private def pull(warp: WorldTransform[source.type, reference.type], p: Vector[Double]) =
    warp.pullPoint(ok(Point.fromVector(reference, p)).asInstanceOf[Point[reference.type, D3]]).map(_.coordinates)

  private def voxels(dims: Vector[Int]): Iterator[(Int, Int, Int)] =
    for x <- Iterator.range(0, dims(0)); y <- Iterator.range(0, dims(1)); z <- Iterator.range(0, dims(2)) yield (x, y, z)

  private def affineAt(affine: image4s.geometry.Affine[D3], v: Vector[Double]): Vector[Double] =
    val m = affine.rowMajor
    Vector.tabulate(3)(r => m(4 * r) * v(0) + m(4 * r + 1) * v(1) + m(4 * r + 2) * v(2) + m(4 * r + 3))

  test("the oracle manifest lists the ten cubic and quadratic cases"):
    assertEquals(cases.size, 10)
    assertEquals(cases.count(_.order == 2), 2)
    assertEquals(cases.count(_.withAff), 5)

  test("headers decode to FNIRT's order, knot spacing, reference geometry and --aff matrix"):
    cases.foreach: c =>
      val file = coefficients(c.name)
      val target = raw(s"$Root/${c.name}/target.nii.gz")
      assertEquals(file.order, if c.order == 3 then FnirtSplineOrder.Cubic else FnirtSplineOrder.Quadratic, c.name)
      assertEquals(file.spline.order, if c.order == 3 then BSplineOrder.Cubic else BSplineOrder.Quadratic, c.name)
      assertEquals(file.knotSpacing, c.knots, c.name)
      assertEquals(file.referenceDims, Some(target.spatialShape), c.name)
      file.referencePixdim.getOrElse(fail(s"${c.name}: no reference voxel sizes")).zip(target.pixdim.slice(1, 4)).foreach((a, e) => assertEqualsDouble(a, e, 1e-6, c.name))
      val expected =
        if c.withAff then ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"$Root/${c.name}/premat.mat"))))
        else FlirtMatrix(Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
      file.premat.rowMajor.zip(expected.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-6, s"${c.name} --aff"))

  test("the spline on the reference FSL voxel lattice reproduces fnirtfileutils displacement fields"):
    cases.foreach: c =>
      val file = coefficients(c.name)
      val ref = ok(FslHeaderGeometry(raw(s"$Root/${c.name}/target.nii.gz")))
      val native = raw(s"$Root/${c.name}/field_noaff.nii.gz")
      assertEquals(native.spatialShape, ref.dims, c.name)
      voxels(ref.dims).foreach: (x, y, z) =>
        // FNIRT's lattice is FSL voxel coordinates: x mirrored for a neurological reference.
        val fsl = affineAt(ref.voxelToFsl, Vector(x.toDouble, y.toDouble, z.toDouble))
        val d = file.spline.at(ok(ContinuousIndex.fromVector[D3](fsl.zip(ref.pixdim).map(_ / _))))
        (0 until 3).foreach(k => assertEqualsDouble(d(k), native.value(x, y, z, k), FieldTolerance, s"${c.name} ($x,$y,$z) component $k"))

  test("the world pullback reproduces fnirtfileutils --withaff fields at every reference voxel"):
    cases.foreach: c =>
      val l = load(c.name)
      val native = raw(s"$Root/${c.name}/field_aff.nii.gz")
      val sourceWorldToFsl = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(l.sourceGeometry))
      voxels(l.referenceGeometry.dims).foreach: (x, y, z) =>
        val v = Vector(x.toDouble, y.toDouble, z.toDouble)
        val sourceFsl = affineAt(sourceWorldToFsl, ok(pull(l.warp, affineAt(l.referenceGeometry.voxelToWorld, v))))
        val referenceFsl = affineAt(l.referenceGeometry.voxelToFsl, v)
        (0 until 3).foreach(k =>
          assertEqualsDouble(sourceFsl(k) - referenceFsl(k), native.value(x, y, z, k), FieldTolerance, s"${c.name} ($x,$y,$z) component $k"))

  test("the world pullback reproduces applywarp --warp=coef coordinate ramps on the source interior"):
    cases.foreach: c =>
      val l = load(c.name)
      val ramps = Vector(0, 1, 2).map(i => raw(s"$Root/${c.name}/native_coord$i.nii.gz"))
      val support = raw(s"$Root/${c.name}/native_support.nii.gz")
      var checked = 0
      voxels(l.referenceGeometry.dims).filter((x, y, z) => support.value(x, y, z) >= 0.999).foreach: (x, y, z) =>
        val world = ok(pull(l.warp, affineAt(l.referenceGeometry.voxelToWorld, Vector(x.toDouble, y.toDouble, z.toDouble))))
        (0 until 3).foreach(k => assertEqualsDouble(world(k), ramps(k).value(x, y, z), CoordinateTolerance, s"${c.name} ($x,$y,$z) component $k"))
        checked += 1
      assert(checked >= 100, s"${c.name}: only $checked supported voxels")

  /** Independent reference: the plain tensor-product sum over stored coefficients, FNIRT knot offset 1 when k > 1. */
  private def directDisplacement(file: FnirtCoefficientFile, u: Vector[Double]): Vector[Double] =
    def weight(t: Double): Double =
      val a = math.abs(t)
      file.order match
        case FnirtSplineOrder.Cubic     => if a < 1 then 2.0 / 3 - a * a + a * a * a / 2 else if a < 2 then math.pow(2 - a, 3) / 6 else 0.0
        case FnirtSplineOrder.Quadratic => if a < 0.5 then 0.75 - a * a else if a < 1.5 then 0.5 * (a - 1.5) * (a - 1.5) else 0.0
    val dims = file.coefficientDims
    val s = Vector.tabulate(3)(i => u(i) / file.knotSpacing(i) + (if file.knotSpacing(i) > 1 then 1 else 0))
    Vector.tabulate(3): c =>
      val terms = for a <- 0 until dims(0); b <- 0 until dims(1); e <- 0 until dims(2) yield weight(s(0) - a) * weight(s(1) - b) * weight(s(2) - e) * file.raw.value(a, b, e, c)
      terms.sum

  test("off-lattice points evaluate the spline exactly, matching a direct tensor-product sum"):
    Vector("srcleft_refright_aff_quad", "srcleft_refleft_aff", "srcright_refleft_noaff").foreach: name =>
      val l = load(name)
      val ref = l.referenceGeometry
      val inverseAff = ok(image4s.geometry.Affine.fromRowMajor[D3](l.file.premat.rowMajor)).inverse
      val sourceWorldToFsl = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(l.sourceGeometry))
      Vector(Vector(4.5, 5.25, 3.75), Vector(0.3, 1.7, 2.2), Vector(6.9, 0.5, 5.5)).foreach: v =>
        val r = affineAt(ref.voxelToFsl, v)
        val d = directDisplacement(l.file, r.zip(ref.pixdim).map(_ / _))
        val expected = affineAt(inverseAff, r).zip(d).map(_ + _)
        val actual = affineAt(sourceWorldToFsl, ok(pull(l.warp, affineAt(ref.voxelToWorld, v))))
        (0 until 3).foreach(k => assertEqualsDouble(actual(k), expected(k), 1e-9, s"$name at voxel $v component $k"))

  test("points outside the reference follow the boundary policy"):
    val outside = Vector(500.0, 500.0, 500.0)
    assert(pull(load("srcleft_refleft_aff").warp, outside).left.exists(_.isInstanceOf[TransformError.Map]))
    assertEquals(pull(load("srcleft_refleft_aff", CoordinateBoundaryPolicy.PreserveSource).warp, outside), Right(outside))
    assertEquals(pull(load("srcleft_refleft_aff", CoordinateBoundaryPolicy.Constant(Vector(1.0, 2.0, 3.0))).warp, outside), Right(Vector(1.0, 2.0, 3.0)))
    // ITK's half-voxel border hold is not FSL's: refused, never aliased to another policy
    val file = coefficients("srcleft_refleft_aff")
    val grids = FslGrids[source.type, reference.type](
      source,
      ok(FslHeaderGeometry(raw(s"$Root/srcleft_refleft_aff/source.nii.gz"))),
      reference,
      ok(FslHeaderGeometry(raw(s"$Root/srcleft_refleft_aff/target.nii.gz")))
    )
    FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(grids, CoordinateBoundaryPolicy.HoldBorderDisplacement)) match
      case Left(TransformError.ItkBorderHoldUnsupported(TransformFormat.FslFnirtCoefficients)) => ()
      case other => fail(s"FNIRT coefficients must refuse HoldBorderDisplacement, got $other")
    val l = load("srcleft_refleft_aff")
    assert(l.warp.mapPoint(ok(Point.fromVector(source, Vector(0.0, 0.0, 0.0))).asInstanceOf[Point[source.type, D3]]).isLeft, "no inverse, so no forward map")

  test("a reference volume other than the one FNIRT fitted on is a typed mismatch"):
    val file = coefficients("srcleft_refleft_aff")
    val src = ok(FslHeaderGeometry(raw(s"$Root/srcleft_refleft_aff/source.nii.gz")))
    val grids = FslGrids[source.type, reference.type](source, src, reference, src)
    FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(grids)) match
      case Left(TransformError.ContextMismatch(TransformFormat.FslFnirtCoefficients, _)) => ()
      case other                                                                         => fail(s"expected a context mismatch, got $other")

  test("coefficient files are detected by intent and decode through Transforms"):
    cases.foreach: c =>
      val path = s"$Root/${c.name}/coef.nii.gz"
      assertEquals(TransformDetection.detect(TransformSource.Binary(bytes(path)), Some("coef.nii")), Right(TransformFormat.FslFnirtCoefficients), c.name)
      assert(ok(Transforms.decode(TransformSource.Binary(bytes(path)), None)).isInstanceOf[NativeTransform.FnirtCoefficients], c.name)

  private def rewritten(name: String)(edit: NiftiWriter.Header => NiftiWriter.Header): TransformSource.Binary =
    val original = raw(s"$Root/$name/coef.nii.gz")
    val values = Array.tabulate(original.voxelCount.toInt)(i => original.value(i.toLong))
    TransformSource.Binary(NiftiWriter.write(edit(NiftiWriter.headerOf(original)), values))

  test("DCT coefficients and TOPUP files are refused by name"):
    val dct = rewritten("srcleft_refleft_aff")(_.copy(intentCode = 2008))
    Vector(TransformDetection.detect(dct, None), FnirtCoefficientsCodec.decode(dct)).foreach:
      case Left(TransformIoError.Unsupported(UnsupportedFormat.FslDctCoefficients, _)) => ()
      case other                                                                     => fail(s"expected a DCT refusal, got $other")
    Vector(2016, 2017, 2018).foreach: code =>
      TransformDetection.detect(rewritten("srcleft_refleft_aff")(_.copy(intentCode = code)), None) match
        case Left(TransformIoError.Unsupported(UnsupportedFormat.FslTopup, _)) => ()
        case other                                                             => fail(s"intent $code: expected a TOPUP refusal, got $other")

  test("a reflecting --aff matrix and non-integer knot spacing are refused"):
    val reflecting = rewritten("srcleft_refleft_aff")(h => h.copy(srow = h.srow.updated(0, -h.srow(0)).updated(4, -h.srow(4)).updated(8, -h.srow(8))))
    assert(FnirtCoefficientsCodec.decode(reflecting).isLeft, "reflecting --aff")
    val fractional = rewritten("srcleft_refleft_aff")(h => h.copy(pixdim = h.pixdim.updated(0, 2.5)))
    assert(FnirtCoefficientsCodec.decode(fractional).left.exists(_.isInstanceOf[TransformIoError.Malformed]), "fractional knot spacing")

  test("optional header fields: no qform, no intent_p, no sform"):
    val bare = ok(FnirtCoefficientsCodec.decode(rewritten("srcleft_refleft_aff")(_.copy(qformCode = 0, sformCode = 0, intentP = Vector(0.0, 0.0, 0.0)))))
    assertEquals(bare.referenceDims, None)
    assertEquals(bare.referencePixdim, None)
    assertEquals(bare.premat, FlirtMatrix(Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)))
    assert(FnirtCoefficientsCodec.decode(rewritten("srcleft_refleft_aff")(_.copy(intentP = Vector(2.0, 0.0, 0.0)))).left.exists(_.isInstanceOf[TransformIoError.Malformed]))
    assert(FnirtCoefficientsCodec.decode(rewritten("srcleft_refleft_aff")(_.copy(dims = Vector(5, 6, 7, 1, 3)))).isLeft, "5D shape")
    Vector(2016, 2017, 2018).foreach: code =>
      FnirtCoefficientsCodec.decode(rewritten("srcleft_refleft_aff")(_.copy(intentCode = code))) match
        case Left(TransformIoError.Unsupported(UnsupportedFormat.FslTopup, _)) => ()
        case other                                                             => fail(s"intent $code: expected a TOPUP refusal, got $other")

  private def mismatchOf(file: FnirtCoefficientFile, name: String, boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject) =
    val src = ok(FslHeaderGeometry(raw(s"$Root/$name/source.nii.gz")))
    val ref = ok(FslHeaderGeometry(raw(s"$Root/$name/target.nii.gz")))
    FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(FslGrids[source.type, reference.type](source, src, reference, ref), boundary))

  test("reference voxel sizes, grid coverage and boundary arity are checked against the context"):
    val scaled = ok(FnirtCoefficientsCodec.decode(rewritten("srcleft_refleft_aff")(h => h.copy(intentP = h.intentP.map(_ * 1.5)))))
    assert(mismatchOf(scaled, "srcleft_refleft_aff").left.exists(_.isInstanceOf[TransformError.ContextMismatch]), "voxel sizes")
    val original = raw(s"$Root/srcleft_refleft_aff/coef.nii.gz")
    val truncated = Vector(2, original.shape(1), original.shape(2), 3)
    val values = Array.tabulate(truncated.product): i =>
      val (x, y, z, c) = (i % 2, (i / 2) % truncated(1), (i / (2 * truncated(1))) % truncated(2), i / (2 * truncated(1) * truncated(2)))
      original.value(x, y, z, c)
    val small = ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(NiftiWriter.write(NiftiWriter.headerOf(original).copy(dims = truncated), values))))
    assert(mismatchOf(small, "srcleft_refleft_aff").left.exists(_.isInstanceOf[TransformError.ContextMismatch]), "truncated grid")
    assert(mismatchOf(coefficients("srcleft_refleft_aff"), "srcleft_refleft_aff", CoordinateBoundaryPolicy.Constant(Vector(1.0))).left.exists(_.isInstanceOf[TransformError.Invalid]), "boundary arity")

  test("coefficient files round-trip value-exactly"):
    cases.foreach: c =>
      val original = coefficients(c.name)
      val again = ok(FnirtCoefficientsCodec.decode(ok(FnirtCoefficientsCodec.encode(original))))
      assertEquals((again.order, again.knotSpacing, again.referenceDims, again.referencePixdim, again.premat), (original.order, original.knotSpacing, original.referenceDims, original.referencePixdim, original.premat), c.name)
      (0L until original.raw.voxelCount).foreach(i => assertEquals(again.raw.value(i), original.raw.value(i), s"${c.name} value $i"))

  test("coefficients convert to a dense FNIRT field on a lattice, matching fnirtfileutils --withaff"):
    cases.foreach: c =>
      val native = ok(Transforms.decode(TransformSource.Binary(bytes(s"$Root/${c.name}/coef.nii.gz")), TransformFormat.FslFnirtCoefficients))
      val src = ok(FslHeaderGeometry(raw(s"$Root/${c.name}/source.nii.gz")))
      val ref = ok(FslHeaderGeometry(raw(s"$Root/${c.name}/target.nii.gz")))
      val decodeContext = ConversionContext(fsl = Some(ConversionContext.FslPair(src, ref)))
      val encodeContext = decodeContext.copy(lattice = Some(ConversionContext.Lattice(ref.dims, ref.voxelToWorld)), fnirtDefinition = Some(FnirtDefinition.Relative))
      val expected = raw(s"$Root/${c.name}/field_aff.nii.gz")
      ok(Conversion.convert(native, TransformFormat.FslFnirtField, decodeContext, encodeContext)) match
        case EncodedTransform.Source(TransformFormat.FslFnirtField, TransformSource.Binary(written)) =>
          val field = ok(NiftiRaw.parse(written))
          assertEquals(field.shape, expected.shape, c.name)
          (0L until expected.voxelCount).foreach(i => assertEqualsDouble(field.value(i), expected.value(i), FieldTolerance, s"${c.name} value $i"))
        case other => fail(s"${c.name}: unexpected $other")

  test("coefficient conversion needs FSL geometry and a lattice, and never targets linear or coefficient formats"):
    val native = ok(Transforms.decode(TransformSource.Binary(bytes(s"$Root/srcleft_refleft_aff/coef.nii.gz")), TransformFormat.FslFnirtCoefficients))
    val src = ok(FslHeaderGeometry(raw(s"$Root/srcleft_refleft_aff/source.nii.gz")))
    val ref = ok(FslHeaderGeometry(raw(s"$Root/srcleft_refleft_aff/target.nii.gz")))
    val context = ConversionContext(fsl = Some(ConversionContext.FslPair(src, ref)))
    Conversion.convert(native, TransformFormat.X5, ConversionContext.empty, context) match
      case Left(TransformError.MissingContext(TransformFormat.FslFnirtCoefficients, _)) => ()
      case other                                                                        => fail(s"expected missing FSL geometry, got $other")
    Conversion.convert(native, TransformFormat.AntsDisplacementNifti, context, context) match
      case Left(TransformError.MissingContext(TransformFormat.AntsDisplacementNifti, _)) => ()
      case other                                                                         => fail(s"expected a missing lattice, got $other")
    Vector(TransformFormat.FslFlirt, TransformFormat.ItkText, TransformFormat.FslFnirtCoefficients).foreach: format =>
      Conversion.convert(native, format, context, context) match
        case Left(TransformError.UnsupportedConversion(_, `format`, _)) => ()
        case other                                                     => fail(s"$format: expected a refusal, got $other")
