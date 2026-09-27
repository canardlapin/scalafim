package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, FreeSurferVolumeGeometry, WorldSpace}
import scalafim.transform.Conversion.EncodedTransform
import scalafim.transform.field.{DenseContext, FnirtDefinition}
import scalafim.transform.freesurfer.VolGeom
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}
import scalafim.transform.x5.X5Interpretation

/** The D4b conversion matrix: exact cells round-trip, needs-policy cells need their policy, unsupported cells refuse. */
class ConversionMatrixSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(path: String) = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private val movable = raw("freesurfer_linear/movable.nii")
  private val reference = raw("freesurfer_linear/reference.nii")
  private def sform(r: NiftiRaw) = ok(Affine.fromRowMajor[D3](r.sformRowMajor))

  private val context = ConversionContext(
    fsl = Some(ConversionContext.FslPair(ok(FslHeaderGeometry(movable)), ok(FslHeaderGeometry(reference)))),
    tkreg = Some(ConversionContext.TkRegPair(FreeSurferVolumeGeometry(movable.spatialShape, sform(movable)), FreeSurferVolumeGeometry(reference.spatialShape, sform(reference)))),
    lta = Some(ConversionContext.LtaPair(VolGeom.of(movable.spatialShape, sform(movable)), VolGeom.of(reference.spatialShape, sform(reference))))
  )

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("moving")))
  private val fixed: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fixed")))
  private val frames = Frames[moving.type, fixed.type](moving, fixed)

  private val points = OracleTable.load("freesurfer_linear/points.tsv")
  private val lta = ok(Transforms.decode(TransformSource.Text(OracleFixtures.text("freesurfer_linear/ras2ras.lta")), TransformFormat.FreeSurferLta))

  /** Read an encoded result back into a world transform over the test frames. */
  private def reread(encoded: EncodedTransform, ctx: ConversionContext): WorldTransform[moving.type, fixed.type] =
    encoded match
      case EncodedTransform.Hdf5X5(file)  => ok(X5Interpretation.interpret(file, DenseContext(frames))).composed
      case EncodedTransform.Source(format, source) =>
        val native = ok(Transforms.decode(source, format))
        val converted = ok(Conversion.convert(native, TransformFormat.X5, ctx, ctx))
        converted match
          case EncodedTransform.Hdf5X5(file) => ok(X5Interpretation.interpret(file, DenseContext(frames))).composed
          case other                         => fail(s"unexpected $other")

  private def pull(t: WorldTransform[moving.type, fixed.type], p: Vector[Double]): Vector[Double] =
    ok(t.pullPoint(ok(Point.fromVector(fixed, p)).asInstanceOf[Point[fixed.type, D3]])).coordinates

  test("a linear transform converts exactly into every linear format"):
    val linearFormats = Vector(
      TransformFormat.ItkText, TransformFormat.ItkMatlab, TransformFormat.FslFlirt, TransformFormat.AfniAff12,
      TransformFormat.FreeSurferLta, TransformFormat.FreeSurferXfm, TransformFormat.FreeSurferRegisterDat, TransformFormat.X5
    )
    linearFormats.foreach: format =>
      val back = reread(ok(Conversion.convert(lta, format, context, context)), context)
      points.rows.foreach: row =>
        pull(back, row.take(3)).zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$format at ${row.take(3)}"))

  test("dense outputs need a sampling lattice and are exact at its points"):
    assertEquals(
      Conversion.convert(lta, TransformFormat.AntsDisplacementNifti, context, context).left.map(_.getClass.getSimpleName),
      Left("MissingContext")
    )
    val lattice = ConversionContext.Lattice(Vector(6, 5, 4), ok(Affine.fromRowMajor[D3](Vector(1.9840289896782464, -0.2688309492111948, 0.07807878523127607, -30.0, 0.23923290058341865, 2.17809331642222, 0.18553837609186435, -40.0, -0.07997866837326832, -0.15375118114363753, 2.4918826646321053, -20.0, 0, 0, 0, 1))))
    // Re-sampling a dense result on its own lattice touches the lattice edge, where round-off can land a hair outside;
    // PreserveSource keeps that re-read total. Comparisons below use interior lattice points only.
    val withLattice = context.copy(lattice = Some(lattice), fnirtDefinition = Some(FnirtDefinition.Relative), boundary = reframe4s.field.CoordinateBoundaryPolicy.PreserveSource)
    val latticePoints = for x <- Vector(1, 3); y <- Vector(1, 2); z <- Vector(1, 2) yield ok(lattice.voxelToRas(Vector(x.toDouble, y.toDouble, z.toDouble)))
    val original = ok(LtaInterpretationFor(lta, frames))
    // AFNI places warps on cardinal axes, so its field is written on a cardinal (permuted, flipped) lattice.
    val cardinal = ConversionContext.Lattice(Vector(6, 5, 4), ok(Affine.fromRowMajor[D3](Vector(0, 0, -2.5, -20.0, -2.0, 0, 0, -30.0, 0, 2.2, 0, -40.0, 0, 0, 0, 1))))
    val withCardinal = withLattice.copy(lattice = Some(cardinal))
    val cardinalPoints = for x <- Vector(1, 3); y <- Vector(1, 2); z <- Vector(1, 2) yield ok(cardinal.voxelToRas(Vector(x.toDouble, y.toDouble, z.toDouble)))
    Vector(
      (TransformFormat.AntsDisplacementNifti, withLattice, latticePoints),
      (TransformFormat.AfniQwarp, withCardinal, cardinalPoints),
      (TransformFormat.FslFnirtField, withLattice, latticePoints),
      (TransformFormat.X5, withLattice, latticePoints)
    ).foreach: (format, sampling, queries) =>
      val back = reread(ok(Conversion.convert(lta, format, context, sampling)), sampling)
      queries.foreach: p =>
        // NIfTI-1 stores the lattice affine (srow) in float32, so NIfTI outputs agree to float32 geometry precision.
        pull(back, p).zip(pull(original, p)).foreach((a, e) => assertEqualsDouble(a, e, 1e-6, s"$format at $p"))
    Conversion.convert(lta, TransformFormat.AfniQwarp, context, withLattice) match
      case Left(TransformError.UnsupportedConversion(_, TransformFormat.AfniQwarp, reason)) => assert(reason.contains("cardinal"), reason)
      case other                                                                         => fail(s"an oblique AFNI warp must be refused, got $other")

  test("ITK/ANTs and AFNI fields refuse a sheared lattice"):
    val sheared = ConversionContext.Lattice(Vector(6, 5, 4), ok(Affine.fromRowMajor[D3](Vector(2.0, 0.3, 0, -30, 0, 2.2, 0, -40, 0, 0, 2.5, -20, 0, 0, 0, 1))))
    Vector(TransformFormat.AntsDisplacementNifti, TransformFormat.AfniQwarp).foreach: format =>
      assert(Conversion.convert(lta, format, context, context.copy(lattice = Some(sheared))).left.exists(_.isInstanceOf[TransformError.UnsupportedConversion]), format.toString)

  test("unsupported cells refuse with a typed reason"):
    val dense = ok(Transforms.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded("neurotransform/itk_oracle/warp.nii.gz"))), TransformFormat.AntsDisplacementNifti))
    Vector(TransformFormat.FslFlirt, TransformFormat.FreeSurferLta, TransformFormat.ItkText, TransformFormat.FslFnirtCoefficients, TransformFormat.ItkHdf5).foreach: format =>
      Conversion.convert(dense, format, context, context) match
        case Left(TransformError.UnsupportedConversion(_, `format`, _)) => ()
        case other                                                     => fail(s"$format: expected a refusal, got $other")
    Conversion.convert(lta, TransformFormat.FslFnirtCoefficients, context, context) match
      case Left(TransformError.UnsupportedConversion(_, _, _)) => ()
      case other                                               => fail(s"expected a refusal, got $other")

  test("formats that need geometry say which geometry is missing"):
    Vector(TransformFormat.FslFlirt, TransformFormat.FreeSurferLta, TransformFormat.FreeSurferRegisterDat).foreach: format =>
      Conversion.convert(lta, format, context, ConversionContext.empty) match
        case Left(TransformError.MissingContext(`format`, _)) => ()
        case other                                            => fail(s"$format: expected MissingContext, got $other")

  private object LtaInterpretationFor:
    def apply(native: NativeTransform, f: Frames[moving.type, fixed.type]): Either[TransformError, WorldTransform[moving.type, fixed.type]] =
      native match
        case NativeTransform.Lta(file) => scalafim.transform.freesurfer.LtaInterpretation.interpret(file, f)
        case other                     => Left(TransformError.Invalid(s"not an LTA: $other"))
