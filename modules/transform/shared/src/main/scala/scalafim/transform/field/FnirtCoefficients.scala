package scalafim.transform.field

import image4s.geometry.{Affine, ContinuousIndex, D3, Frame, Point}
import ravel.NDArray
import reframe4s.core.MapError
import reframe4s.field.{BSplineField, BSplineOrder, CoordinateBoundaryPolicy, CoverageReportingMap, CoveredPoint, KnotLattice, SupportOutcome}
import scalafim.image.world.ToolCoordinates
import scalafim.transform.*
import scalafim.transform.fsl.FlirtMatrix
import scalafim.transform.nifti.{NiftiRaw, NiftiWriter}

/** FNIRT spline order, fixed by the NIfTI intent code FSL writes (`FSL_CUBIC_SPLINE_COEFFICIENTS = 2007`,
  * `FSL_QUADRATIC_SPLINE_COEFFICIENTS = 2009`; 2008 is the DCT basis, which is not supported).
  */
enum FnirtSplineOrder(val intentCode: Int) derives CanEqual:
  case Quadratic extends FnirtSplineOrder(2009)
  case Cubic extends FnirtSplineOrder(2007)

  def basis: BSplineOrder =
    this match
      case Quadratic => BSplineOrder.Quadratic
      case Cubic     => BSplineOrder.Cubic

object FnirtSplineOrder:
  def fromIntent(code: Int): Option[FnirtSplineOrder] =
    values.find(_.intentCode == code)

/** A FNIRT `--cout` coefficient file. The header carries the whole parameterisation:
  *
  *   - intent code: the spline order ([[FnirtSplineOrder]]);
  *   - pixdim 1-3: the integer knot spacing, in reference voxels;
  *   - qform translation: the reference volume's dimensions (when the qform code is set);
  *   - intent_p1-p3: the reference volume's voxel sizes (when written);
  *   - sform: the `--aff` FLIRT matrix, source FSL mm to reference FSL mm (identity when FNIRT ran without one).
  *
  * The data are the displacement spline coefficients `(cx, cy, cz, 3)`, in reference FSL mm. The container is kept
  * whole, so encoding reproduces every value. Instances are valid by construction: see [[FnirtCoefficientsCodec]].
  */
final class FnirtCoefficientFile private (
    val raw: NiftiRaw,
    val order: FnirtSplineOrder,
    val knotSpacing: Vector[Int],
    val referenceDims: Option[Vector[Int]],
    val referencePixdim: Option[Vector[Double]],
    val premat: FlirtMatrix,
    private[transform] val spline: BSplineField[D3]
):
  /** Extents of the coefficient grid. */
  def coefficientDims: Vector[Int] = raw.spatialShape

  override def toString: String =
    s"FnirtCoefficientFile($order, knots ${knotSpacing.mkString("x")}, coefficients ${coefficientDims.mkString("x")}, " +
      s"reference ${referenceDims.fold("unrecorded")(_.mkString("x"))}, --aff ${premat.rowMajor.take(12).mkString(" ")})"

object FnirtCoefficientFile:
  private val Format = "FNIRT coefficients"

  /** Validate a parsed NIfTI-1 container as a FNIRT coefficient file. */
  def fromNifti(raw: NiftiRaw): Either[TransformIoError, FnirtCoefficientFile] =
    for
      order <- orderOf(raw.intentCode)
      _ <- Either.cond(
        raw.shape.size == 4 && raw.shape(3) == 3,
        (),
        TransformIoError.UnsupportedNifti(s"FNIRT coefficients need shape (x,y,z,3), got ${raw.shape.mkString("x")}")
      )
      spacing <- integral(raw.pixdim.slice(1, 4), "knot spacing (pixdim 1-3)")
      referenceDims <-
        if raw.qformCode > 0 then integral(raw.qoffset, "reference dimensions (qform translation)").map(Some(_))
        else Right(None)
      referencePixdim <-
        if raw.intentP.forall(_ == 0.0) then Right(None)
        else if raw.intentP.forall(p => p.isFinite && p > 0.0) then Right(Some(raw.intentP))
        else Left(TransformIoError.Malformed(Format, s"reference voxel sizes (intent_p1-p3) must be positive, got ${raw.intentP.mkString(" ")}"))
      premat <- prematOf(raw)
      knots <- KnotLattice.fsl[D3](spacing).left.map(e => TransformIoError.Malformed(Format, e.message))
      coefficients = NDArray.tabulate[Double](raw.shape(0), raw.shape(1), raw.shape(2), 3)((x, y, z, c) => raw.value(x, y, z, c))
      spline <- BSplineField.create[D3, ravel.Rank[4]](order.basis, knots, coefficients).left.map(e => TransformIoError.Malformed(Format, e.message))
    yield FnirtCoefficientFile(raw, order, spacing, referenceDims, referencePixdim, premat, spline)

  private def orderOf(code: Int): Either[TransformIoError, FnirtSplineOrder] =
    FnirtSplineOrder.fromIntent(code) match
      case Some(order) => Right(order)
      case None if code == 2008 =>
        Left(TransformIoError.Unsupported(UnsupportedFormat.FslDctCoefficients, "FNIRT DCT-basis coefficients (intent 2008) are out of scope"))
      case None if code >= 2016 && code <= 2018 =>
        Left(TransformIoError.Unsupported(UnsupportedFormat.FslTopup, s"TOPUP intent $code is an off-resonance field, not a spatial transform"))
      case None => Left(TransformIoError.UnsupportedNifti(s"intent $code is not a FNIRT spline coefficient file (2007 cubic, 2009 quadratic)"))

  /** Header floats that must hold positive integers (stored as float32, so compared to within 1e-3). */
  private def integral(values: Vector[Double], what: String): Either[TransformIoError, Vector[Int]] =
    val rounded = values.map(v => math.round(v).toInt)
    if values.zip(rounded).forall((v, r) => v.isFinite && r > 0 && math.abs(v - r) <= 1e-3) then Right(rounded)
    else Left(TransformIoError.Malformed(Format, s"$what must be positive integers, got ${values.mkString(" ")}"))

  /** The `--aff` matrix from the sform (identity when the sform code is unset). Singular matrices cannot be inverted;
    * reflecting ones are refused by policy (STP plan, Phase 3c): a registration premat that flips handedness is far more
    * likely a convention error than a real alignment, and FSL coordinates already carry each volume's handedness.
    */
  private def prematOf(raw: NiftiRaw): Either[TransformIoError, FlirtMatrix] =
    val m = if raw.sformCode > 0 then raw.sformRowMajor else Affine.identity[D3].rowMajor
    val det =
      m(0) * (m(5) * m(10) - m(6) * m(9)) - m(1) * (m(4) * m(10) - m(6) * m(8)) + m(2) * (m(4) * m(9) - m(5) * m(8))
    if m.exists(v => !v.isFinite) then Left(TransformIoError.Malformed(Format, "the --aff (sform) matrix has non-finite entries"))
    else if !(det > 0.0) then
      Left(TransformIoError.UnsupportedNifti(f"the --aff (sform) matrix has determinant $det%.6g; reflecting or singular --aff matrices are refused"))
    else Right(FlirtMatrix(m))

object FnirtCoefficientsCodec extends TransformCodec[FnirtCoefficientFile]:
  val format: TransformFormat = TransformFormat.FslFnirtCoefficients

  def decode(source: TransformSource): Either[TransformIoError, FnirtCoefficientFile] =
    source match
      case TransformSource.Text(_)       => Left(TransformIoError.WrongSource(format, "uncompressed NIfTI-1 bytes"))
      case TransformSource.Binary(bytes) => NiftiRaw.parse(bytes).flatMap(FnirtCoefficientFile.fromNifti)

  def encode(file: FnirtCoefficientFile): Either[TransformIoError, TransformSource] =
    val values = Array.tabulate(file.raw.voxelCount.toInt)(i => file.raw.value(i.toLong))
    Right(TransformSource.Binary(NiftiWriter.write(NiftiWriter.headerOf(file.raw), values)))

/** FNIRT coefficients need the FSL geometry of both volumes: the file stores neither placement, only the reference
  * dimensions and voxel sizes (which must agree with `grids.referenceGeometry`). `HoldBorderDisplacement` (ITK's
  * half-voxel border hold) is refused by [[FnirtCoefficientInterpretation]]: FSL does not extend a warp that way.
  */
final case class FnirtCoefficientContext[S <: Frame[D3], T <: Frame[D3]](
    grids: FslGrids[S, T],
    boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject
)

/** FNIRT's meaning of a coefficient file, as `applywarp --warp=coef` and `fnirtfileutils --withaff` evaluate it. At a
  * reference point with FSL coordinate `r`, the spline is evaluated at lattice coordinate `u = r / pixdim` (FSL voxel
  * coordinates, x mirrored for a neurological reference) with FNIRT's knot offset, giving a displacement `d(u)` in FSL
  * mm; the source FSL coordinate is `inverse(aff) * r + d(u)`, and the source RAS point its FSL-to-world image.
  *
  * The pullback evaluates the spline exactly at every query point, not a lattice interpolation of it. Points outside
  * the reference volume follow the context's boundary policy (default: reject). Unlike a linearly interpolated
  * [[reframe4s.field.DenseMap]], the cut-off is sharp at the first and last reference voxel: `Constant` and
  * `PreserveSource` do not blend across a one-voxel band. `HoldBorderDisplacement` is ITK's semantics, not FSL's, and
  * is refused with [[TransformError.UnsupportedBoundary]].
  */
object FnirtCoefficientInterpretation extends Interpretation[FnirtCoefficientFile, FnirtCoefficientContext, WorldTransform.Mapped]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](file: FnirtCoefficientFile, context: FnirtCoefficientContext[S, T]): Either[TransformError, WorldTransform.Mapped[S, T]] =
    interpretWith(file, context, AssetRef(s"FNIRT ${file.order.toString.toLowerCase} coefficients", None))

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](
      file: FnirtCoefficientFile,
      context: FnirtCoefficientContext[S, T],
      asset: AssetRef
  ): Either[TransformError, WorldTransform.Mapped[S, T]] =
    val reference = context.grids.referenceGeometry
    def affine(values: Vector[Double]) = Affine.fromRowMajor[D3](values).left.map(TransformError.Geometry(_))
    def mismatch(reason: String) = TransformError.ContextMismatch(TransformFormat.FslFnirtCoefficients, reason)
    for
      boundary <- context.boundary match
        case CoordinateBoundaryPolicy.Constant(values) if values.size != 3 || values.exists(v => !v.isFinite) =>
          Left(TransformError.Invalid(s"a constant boundary needs three finite coordinates, got ${values.mkString(" ")}"))
        case CoordinateBoundaryPolicy.Constant(values) => Right(FnirtBoundary.Constant(values))
        case CoordinateBoundaryPolicy.Reject           => Right(FnirtBoundary.Reject)
        case CoordinateBoundaryPolicy.PreserveSource   => Right(FnirtBoundary.PreserveSource)
        case CoordinateBoundaryPolicy.HoldBorderDisplacement =>
          Left(DenseLattice.itkBorderRefusal(TransformFormat.FslFnirtCoefficients, "FSL"))
      _ <- file.referenceDims match
        case Some(dims) if dims != reference.dims =>
          Left(mismatch(s"the coefficients were fitted on a ${dims.mkString("x")} reference, the context reference is ${reference.dims.mkString("x")}"))
        case _ => Right(())
      _ <- file.referencePixdim match
        case Some(sizes) if sizes.zip(reference.pixdim).exists((a, b) => math.abs(a - b) > 1e-4 * math.max(1.0, math.abs(b))) =>
          Left(mismatch(s"the coefficients were fitted on ${sizes.mkString("x")} mm reference voxels, the context reference has ${reference.pixdim.mkString("x")}"))
        case _ => Right(())
      _ <- coverage(file, reference.dims).map(mismatch).toLeft(())
      aff <- affine(file.premat.rowMajor)
      fslToLattice <- affine(Vector(1.0 / reference.pixdim(0), 0, 0, 0, 0, 1.0 / reference.pixdim(1), 0, 0, 0, 0, 1.0 / reference.pixdim(2), 0, 0, 0, 0, 1))
      worldToLattice <- ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(reference)).andThen(fslToLattice).left.map(TransformError.Geometry(_))
      latticeToAligned <- fslToLattice.inverse.andThen(aff.inverse).left.map(TransformError.Geometry(_))
      sourceFslToWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(context.grids.sourceGeometry))
    yield
      val pull = FnirtCoefficientPullback[T, S](
        context.grids.reference,
        context.grids.source,
        file.spline,
        reference.dims,
        worldToLattice.rowMajor.toArray,
        latticeToAligned.rowMajor.toArray,
        sourceFslToWorld.rowMajor.toArray,
        boundary
      )
      WorldTransform.Mapped(pull, PushAvailability.Unavailable(), TransformProvenance.read(TransformFormat.FslFnirtCoefficients, asset))

  /** A grid too small for the reference: the knot centred on the last reference voxel must be stored. (FNIRT itself
    * omits the outermost partially-weighted knots, so a stricter full-support rule would refuse its own files.)
    */
  private def coverage(file: FnirtCoefficientFile, dims: Vector[Int]): Option[String] =
    def centre(axis: Int): Int =
      val k = file.knotSpacing(axis)
      (dims(axis) - 1) / k + (if k > 1 then 1 else 0)
    (0 until 3)
      .find(axis => centre(axis) >= file.coefficientDims(axis))
      .map(axis => s"axis $axis has ${file.coefficientDims(axis)} coefficients, too few for ${dims(axis)} reference voxels at knot spacing ${file.knotSpacing(axis)}")

/** The out-of-lattice policies a FNIRT coefficient pullback implements. ITK's `HoldBorderDisplacement` is not one of
  * them, so the pullback cannot be built with it.
  */
private enum FnirtBoundary derives CanEqual:
  case Reject
  case Constant(coordinates: Vector[Double])
  case PreserveSource

/** Exact spline pullback, reference world `T` to source world `S`. Affines are precomposed row-major 4x4 arrays;
  * `lattice` is the reference FSL voxel lattice whose extents bound the support.
  */
private final class FnirtCoefficientPullback[T <: Frame[D3], S <: Frame[D3]](
    val source: T,
    val target: S,
    spline: BSplineField[D3],
    lattice: Vector[Int],
    worldToLattice: Array[Double],
    latticeToAligned: Array[Double],
    sourceFslToWorld: Array[Double],
    boundary: FnirtBoundary
) extends CoverageReportingMap[T, S, D3]:
  private val Slack = 1e-6 // lattice units: round-off at the first and last reference voxel is still inside

  def applyWithCoverage(point: Point[T, D3]): Either[MapError, CoveredPoint[S, D3]] =
    for
      _ <- reframe4s.core.SpatialMap.validateSourcePoint(source, point)
      (values, outcome) <- evaluate(point.coordinates)
      result <- Point.fromVector(target, values).left.map(MapError.Geometry(_))
      rebound <- Frame.alignOwners[D3, target.type, S](target, target).flatMap(_.pointToRight(result)).left.map(MapError.Geometry(_))
    yield CoveredPoint(rebound, outcome)

  private def evaluate(p: Vector[Double]): Either[MapError, (Vector[Double], SupportOutcome)] =
    val u = Vector.tabulate(3)(r => row(worldToLattice, r, p(0), p(1), p(2)))
    val inside = (0 until 3).forall(i => u(i) >= -Slack && u(i) <= lattice(i) - 1 + Slack)
    if inside then
      ContinuousIndex.fromVector[D3](u).left.map(MapError.Geometry(_)).map: index =>
        val d = spline.at(index)
        val aligned = Vector.tabulate(3)(r => row(latticeToAligned, r, u(0), u(1), u(2)) + d(r))
        Vector.tabulate(3)(r => row(sourceFslToWorld, r, aligned(0), aligned(1), aligned(2))) -> SupportOutcome.Covered
    else
      boundary match
        case FnirtBoundary.Reject           => Left(MapError.OutsideDomain(p))
        case FnirtBoundary.Constant(values) => Right(values -> SupportOutcome.ConstantFilled)
        case FnirtBoundary.PreserveSource   => Right(p -> SupportOutcome.SourcePreserved)

  private def row(m: Array[Double], r: Int, x: Double, y: Double, z: Double): Double =
    m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3)
