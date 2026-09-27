package scalafim.transform.field

import gale.backend.Backend.given
import gale.linalg.DMat
import image4s.{AxisKind, NonSpatialAxes, Sampled, Axis as ImageAxis}
import image4s.geometry.{Affine, D3, Frame, Grid}
import ravel.{NDArray, Rank}
import reframe4s.field.{CoordinateBoundaryPolicy, DenseMap}
import reframe4s.resample.Interpolation
import scalafim.image.world.{FslVolumeGeometry, ToolCoordinates}
import scalafim.transform.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.nifti.{NiftiRaw, NiftiWriter}

/** A three-component vector field stored in a NIfTI-1 container: ANTs/ITK `(x,y,z,1,3)` intent 1007, FSL `(x,y,z,3)`,
  * AFNI 3dQwarp `(x,y,z,1,3)`. The container is kept whole, so encoding reproduces every value.
  */
final case class VectorFieldNifti(raw: NiftiRaw):
  val spatialDims: Vector[Int] = raw.spatialShape
  val fiveDimensional: Boolean = raw.shape.size == 5

  def component(x: Int, y: Int, z: Int, c: Int): Double =
    if fiveDimensional then raw.value(x, y, z, 0, c) else raw.value(x, y, z, c)

object VectorFieldNiftiCodec extends TransformCodec[VectorFieldNifti]:
  val format: TransformFormat = TransformFormat.AntsDisplacementNifti

  def decode(source: TransformSource): Either[TransformIoError, VectorFieldNifti] =
    source match
      case TransformSource.Text(_) => Left(TransformIoError.WrongSource(format, "uncompressed NIfTI-1 bytes"))
      case TransformSource.Binary(bytes) =>
        NiftiRaw.parse(bytes).flatMap: raw =>
          val shape = raw.shape
          val ok = (shape.size == 5 && shape(3) == 1 && shape(4) == 3) || (shape.size == 4 && shape(3) == 3)
          if ok then Right(VectorFieldNifti(raw))
          else Left(TransformIoError.UnsupportedNifti(s"a vector field needs shape (x,y,z,1,3) or (x,y,z,3), got ${shape.mkString("x")}"))

  def encode(field: VectorFieldNifti): Either[TransformIoError, TransformSource] =
    val n = field.raw.voxelCount
    val values = Array.tabulate(n.toInt)(i => field.raw.value(i.toLong))
    Right(TransformSource.Binary(NiftiWriter.write(NiftiWriter.headerOf(field.raw), values)))

/** Which stored NIfTI affine a toolkit places a field's lattice with. */
enum LatticeAffine derives CanEqual:
  /** ITK 5.4 NiftiImageIO (`SetImageIOOrientationFromNIfTI`), which ANTs uses: the sform when it is orthonormal once
    * its column scales are removed and either the qform code is unset, the sform code is SCANNER_ANAT (1), or the two
    * forms agree; otherwise the qform. Spacing is always pixdim, so an sform contributes only directions and origin.
    * With neither code the lattice is pixdim scaling along LPS at the origin. A non-orthonormal sform with no qform is
    * refused, as ITK refuses it.
    */
  case Itk

  /** FSL, and AFNI's default `AFNI_NIFTI_PRIORITY=S` (`thd_niftiread.c`): the sform when its code is set, else the
    * qform, else pixdim scaling (see [[FslVolumeGeometry]]).
    */
  case SformFirst

object LatticeAffine:
  def of(raw: NiftiRaw, choice: LatticeAffine): Either[TransformError, Affine[D3]] =
    val scaling = Vector(raw.pixdim(1), 0.0, 0.0, 0.0, 0.0, raw.pixdim(2), 0.0, 0.0, 0.0, 0.0, raw.pixdim(3), 0.0, 0.0, 0.0, 0.0, 1.0)
    val values = choice match
      case Itk        => itk(raw)
      case SformFirst => Right(if raw.sformCode > 0 then raw.sformRowMajor else if raw.qformCode > 0 then raw.qformRowMajor else scaling)
    values.flatMap(Affine.fromRowMajor[D3](_).left.map(TransformError.Geometry(_)))

  private def itk(raw: NiftiRaw): Either[TransformError, Vector[Double]] =
    val spacing = Vector(raw.pixdim(1), raw.pixdim(2), raw.pixdim(3))
    val s = raw.sformRowMajor
    val columns = Vector.tabulate(3)(c => Vector(s(c), s(4 + c), s(8 + c)))
    val norms = columns.map(column => math.sqrt(column.map(v => v * v).sum))
    val directions = columns.zip(norms).map((column, norm) => column.map(_ / norm))
    val orthonormal = norms.forall(n => n > 0.0 && n.isFinite) &&
      (0 until 3).forall(i => (0 until 3).forall(j => math.abs(directions(i).zip(directions(j)).map(_ * _).sum - (if i == j then 1.0 else 0.0)) <= 1e-4))
    val formsAgree = raw.qformCode > 0 && raw.qformRowMajor.zip(s).forall((q, v) => math.abs(q - v) <= 1e-4)
    if raw.qformCode <= 0 && raw.sformCode <= 0 then
      Right(Vector(-spacing(0), 0.0, 0.0, 0.0, 0.0, -spacing(1), 0.0, 0.0, 0.0, 0.0, spacing(2), 0.0, 0.0, 0.0, 0.0, 1.0))
    else if raw.sformCode > 0 && orthonormal && (raw.qformCode <= 0 || raw.sformCode == 1 || formsAgree) then
      Right(Vector.tabulate(3)(r => Vector.tabulate(3)(c => directions(c)(r) * spacing(c)) :+ s(4 * r + 3)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0))
    else if raw.qformCode > 0 then Right(raw.qformRowMajor)
    else Left(TransformError.Invalid("ITK reads a NIfTI whose only orientation is a non-orthonormal sform as an error; so does ScalaFIM"))

/** Builds provider dense maps: a reframe4s [[DenseMap]] over an image4s grid in the target frame whose samples are
  * absolute source coordinates. Out-of-lattice behaviour is the explicit boundary policy (default: reject).
  */
object DenseLattice:
  def pullback[S <: Frame[D3], T <: Frame[D3]](
      target: T,
      source: S,
      dims: Vector[Int],
      latticeToWorld: Affine[D3],
      boundary: CoordinateBoundaryPolicy
  )(sourceCoordinate: (Int, Int, Int) => Vector[Double]): Either[TransformError, DenseMap[T, S, D3, Rank[4]]] =
    for
      grid <- Grid.forFrame[D3, T](target)(dims, latticeToWorld).left.map(TransformError.Geometry(_))
      direction <- ImageAxis.create("direction", 3, AxisKind.Direction).left.map(e => TransformError.Invalid(e.message))
      axes <- NonSpatialAxes.from(Vector(direction)).left.map(e => TransformError.Invalid(e.message))
      data = NDArray.tabulate[Double](dims(0), dims(1), dims(2), 3)((x, y, z, c) => sourceCoordinate(x, y, z)(c))
      image <- Sampled.continuous(grid, axes, data).left.map(e => TransformError.Invalid(e.message))
      dense <- DenseMap.fromCoordinates[T, S, D3, Rank[4]](image, source, Interpolation.Linear, boundary).left.map(e => TransformError.Invalid(e.message))
    yield dense

/** Context for fields whose files carry their own lattice: endpoint frames and the out-of-lattice policy. */
final case class DenseContext[S <: Frame[D3], T <: Frame[D3]](frames: Frames[S, T], boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject)

/** ITK/ANTs displacement fields (and AFNI 3dQwarp, which shares the layout): on the target lattice, an LPS
  * displacement `d` sends target point `p` to source point `p + d` (ITK `DisplacementFieldTransform`, AFNI DICOM order).
  */
final class LpsDisplacementInterpretation(format: TransformFormat, lattice: LatticeAffine) extends Interpretation[VectorFieldNifti, DenseContext, WorldTransform.Mapped]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](field: VectorFieldNifti, context: DenseContext[S, T]): Either[TransformError, WorldTransform.Mapped[S, T]] =
    for
      latticeToWorld <- LatticeAffine.of(field.raw, lattice)
      m = latticeToWorld.rowMajor
      flip = ToolCoordinates.LpsToRas.rowMajor // an LPS displacement is a vector: only the flip's diagonal applies
      dense <- DenseLattice.pullback(context.frames.target, context.frames.source, field.spatialDims, latticeToWorld, context.boundary): (x, y, z) =>
        Vector.tabulate(3): r =>
          val world = m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3)
          world + flip(5 * r) * field.component(x, y, z, r)
    yield WorldTransform.Mapped(dense, PushAvailability.Unavailable(), TransformProvenance.read(format, AssetRef(format.toString, None)))

object LpsDisplacementInterpretation:
  val Ants: LpsDisplacementInterpretation = LpsDisplacementInterpretation(TransformFormat.AntsDisplacementNifti, LatticeAffine.Itk)

  /** 3dQwarp `_WARP` datasets use the ITK displacement convention (`3dQwarp -help`: DICOM/LPS mm displacements from
    * the base grid to the source, a pullback); only the NIfTI affine choice differs, AFNI preferring the sform.
    */
  val AfniQwarp: LpsDisplacementInterpretation = LpsDisplacementInterpretation(TransformFormat.AfniQwarp, LatticeAffine.SformFirst)

/** Whether a FNIRT field stores relative displacements or absolute source coordinates (FSL scaled-voxel mm). */
enum FnirtDefinition derives CanEqual:
  case Relative, Absolute

/** FNIRT dense fields need the source volume's FSL geometry; the lattice (reference) geometry is the field's own. */
final case class FnirtContext[S <: Frame[D3], T <: Frame[D3]](
    frames: Frames[S, T],
    sourceGeometry: FslVolumeGeometry,
    definition: Option[FnirtDefinition],
    boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject
)

/** FNIRT `--fout` fields: at reference lattice point with FSL coordinate `r`, the source FSL coordinate is `r + v`
  * (relative) or `v` (absolute); the source RAS point is the source volume's FSL-to-world image of it.
  */
object FnirtFieldInterpretation extends Interpretation[VectorFieldNifti, FnirtContext, WorldTransform.Mapped]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](field: VectorFieldNifti, context: FnirtContext[S, T]): Either[TransformError, WorldTransform.Mapped[S, T]] =
    for
      reference <- FslHeaderGeometry(field.raw)
      definition <- context.definition.fold(FnirtDetection.detect(field, reference, context.sourceGeometry))(Right(_))
      toFsl = reference.voxelToFsl.rowMajor
      srcFslToWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(context.sourceGeometry)).rowMajor
      dense <- DenseLattice.pullback(context.frames.target, context.frames.source, field.spatialDims, reference.voxelToWorld, context.boundary): (x, y, z) =>
        val sourceFsl = Vector.tabulate(3): r =>
          val v = field.component(x, y, z, r)
          definition match
            case FnirtDefinition.Absolute => v
            case FnirtDefinition.Relative => toFsl(4 * r) * x + toFsl(4 * r + 1) * y + toFsl(4 * r + 2) * z + toFsl(4 * r + 3) + v
        Vector.tabulate(3)(r => srcFslToWorld(4 * r) * sourceFsl(0) + srcFslToWorld(4 * r + 1) * sourceFsl(1) + srcFslToWorld(4 * r + 2) * sourceFsl(2) + srcFslToWorld(4 * r + 3))
    yield WorldTransform.Mapped(
      dense,
      PushAvailability.Unavailable(),
      TransformProvenance.read(TransformFormat.FslFnirtField, AssetRef(s"FNIRT ${definition.toString.toLowerCase} field", None))
    )

/** Relative vs absolute detection for FNIRT fields, following neurotransform's rule: a field-of-view test first (which
  * reading keeps sampled source coordinates inside the source volume), then a volume-change test on an affine fit.
  * Inconclusive evidence is [[TransformError.AmbiguousFnirtDefinition]], never a guess.
  */
object FnirtDetection:
  def detect(field: VectorFieldNifti, reference: FslVolumeGeometry, source: FslVolumeGeometry): Either[TransformError, FnirtDefinition] =
    val dims = field.spatialDims
    val perAxis = 10
    def axis(n: Int) = (0 until perAxis).map(i => math.round(i * (n - 1).toDouble / (perAxis - 1)).toInt).distinct.toVector
    val toFsl = reference.voxelToFsl.rowMajor
    val samples =
      for
        x <- axis(dims(0)); y <- axis(dims(1)); z <- axis(dims(2))
        v = Vector.tabulate(3)(c => field.component(x, y, z, c))
        if v.forall(_.isFinite) && v.exists(_ != 0.0)
      yield (Vector.tabulate(3)(r => toFsl(4 * r) * x + toFsl(4 * r + 1) * y + toFsl(4 * r + 2) * z + toFsl(4 * r + 3)), v)
    if samples.isEmpty then Right(FnirtDefinition.Relative) // an all-zero field is the relative identity
    else
      val spacing = source.pixdim
      val upper = source.dims.zip(spacing).map((n, s) => (n - 1) * s + s)
      def inside(p: Vector[Double]) = (0 until 3).forall(i => p(i) >= -spacing(i) && p(i) <= upper(i))
      val absolute = samples.count((_, v) => inside(v)).toDouble / samples.size
      val relative = samples.count((r, v) => inside(r.zip(v).map(_ + _))).toDouble / samples.size
      if math.abs(absolute - relative) >= 0.25 then Right(if absolute > relative then FnirtDefinition.Absolute else FnirtDefinition.Relative)
      else jacobianVerdict(samples).toRight(TransformError.AmbiguousFnirtDefinition(f"field-of-view test inconclusive (absolute $absolute%.2f vs relative $relative%.2f) and volume-change test inconclusive"))

  private def jacobianVerdict(samples: Vector[(Vector[Double], Vector[Double])]): Option[FnirtDefinition] =
    if samples.size < 12 then None
    else
      val design = DMat.dense(samples.size, 4, samples.flatMap((r, _) => r :+ 1.0))
      val response = DMat.dense(samples.size, 3, samples.flatMap((_, v) => v))
      design.leastSquares(response).toOption.flatMap: coefficients =>
        val m = Vector.tabulate(3, 3)((i, j) => coefficients(j, i)) // v ~ M r + b
        def distortion(add: Double): Double =
          val a = Vector.tabulate(3, 3)((i, j) => m(i)(j) + (if i == j then add else 0.0))
          val det = a(0)(0) * (a(1)(1) * a(2)(2) - a(1)(2) * a(2)(1)) - a(0)(1) * (a(1)(0) * a(2)(2) - a(1)(2) * a(2)(0)) + a(0)(2) * (a(1)(0) * a(2)(1) - a(1)(1) * a(2)(0))
          if !det.isFinite || det == 0.0 then Double.PositiveInfinity else math.abs(math.log(math.abs(det)))
        val (abs, rel) = (distortion(0.0), distortion(1.0))
        Option.when(math.abs(abs - rel) >= math.log(2.0))(if abs < rel then FnirtDefinition.Absolute else FnirtDefinition.Relative)
