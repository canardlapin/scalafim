package scalafim.image.world

import image4s.geometry.{Affine, D3}

/** Which stored affine FSL places a volume with: the sform when its code is non-zero, else the qform when its code is
  * non-zero, else a pure pixdim scaling. This is FSL's rule (fslpy `Image.voxToWorldMat`), which can differ from the
  * policy ScalaFIM reads data with; both are kept, never merged.
  */
enum FslAffineSource derives CanEqual:
  case Sform, Qform, Scaling

/** The geometry FSL sees for one volume, enough to define its scaled-voxel ("fsl") coordinate system. */
final case class FslVolumeGeometry private (
    dims: Vector[Int],
    pixdim: Vector[Double],
    voxelToWorld: Affine[D3],
    selected: FslAffineSource
):
  /** FSL treats a volume whose selected voxel-to-world affine has positive determinant as neurological. */
  def neurological: Boolean =
    ToolCoordinates.determinant3(voxelToWorld) > 0.0

  /** Voxel index -> FSL scaled-voxel mm: `voxel * pixdim`, with x mirrored to `(nx - 1 - i) * px` when neurological. */
  def voxelToFsl: Affine[D3] =
    val (px, py, pz) = (pixdim(0), pixdim(1), pixdim(2))
    val row0 =
      if neurological then Vector(-px, 0.0, 0.0, (dims(0) - 1) * px)
      else Vector(px, 0.0, 0.0, 0.0)
    ToolCoordinates.affine(row0 ++ Vector(0.0, py, 0.0, 0.0, 0.0, 0.0, pz, 0.0, 0.0, 0.0, 0.0, 1.0))

  /** FSL scaled-voxel mm -> RAS mm. */
  def fslToWorld: Affine[D3] =
    ToolCoordinates.compose(voxelToFsl.inverse, voxelToWorld)

object FslVolumeGeometry:
  /** Build from raw NIfTI header fields, applying FSL's affine selection. `pixdim` are the three spatial pixdims. */
  def fromHeader(
      dims: Vector[Int],
      pixdim: Vector[Double],
      qformCode: Int,
      qform: Option[Affine[D3]],
      sformCode: Int,
      sform: Option[Affine[D3]]
  ): Either[SpaceError, FslVolumeGeometry] =
    if dims.size < 3 || dims.take(3).exists(_ <= 0) then Left(SpaceError.InvalidGeometry(s"FSL geometry needs three positive dims, got $dims"))
    else if pixdim.size < 3 || pixdim.take(3).exists(p => !p.isFinite || p == 0.0) then
      Left(SpaceError.InvalidGeometry(s"FSL geometry needs three finite non-zero pixdims, got $pixdim"))
    else
      val zooms = pixdim.take(3).map(math.abs)
      val chosen =
        (sformCode, sform, qformCode, qform) match
          case (s, Some(affine), _, _) if s != 0 => Right(affine -> FslAffineSource.Sform)
          case (s, None, _, _) if s != 0         => Left(SpaceError.InvalidGeometry(s"sform_code $s but no sform"))
          case (_, _, q, Some(affine)) if q != 0 => Right(affine -> FslAffineSource.Qform)
          case (_, _, q, None) if q != 0         => Left(SpaceError.InvalidGeometry(s"qform_code $q but no qform"))
          case _ =>
            Right(ToolCoordinates.affine(Vector(zooms(0), 0, 0, 0, 0, zooms(1), 0, 0, 0, 0, zooms(2), 0, 0, 0, 0, 1)) -> FslAffineSource.Scaling)
      chosen.map((affine, source) => FslVolumeGeometry(dims.take(3), zooms, affine, source))

/** FreeSurfer volume geometry. `norig` is the scanner vox2ras. FreeSurfer's tkRAS vox2ras (`Torig`,
  * `MRIxfmCRS2XYZtkreg`) uses the same direction cosines and voxel sizes with the centre RAS moved to the origin, so
  * tkRAS -> scanner RAS is `Norig * inverse(Torig)`: a translation by `c_ras = Norig * (dims / 2)`, for oblique volumes
  * too. `c_ras` must therefore come from the full geometry (dims and Norig), not from a header field alone.
  */
final case class FreeSurferVolumeGeometry(dims: Vector[Int], norig: Affine[D3]):
  require(dims.size == 3 && dims.forall(_ > 0), s"FreeSurfer geometry needs three positive dims, got $dims")

  def centerRas: Vector[Double] =
    norig(dims.map(_ / 2.0)).fold(error => throw new IllegalStateException(error.message), identity)

  def torig: Affine[D3] =
    val m = norig.rowMajor
    val linear = Vector(m(0), m(1), m(2), m(4), m(5), m(6), m(8), m(9), m(10))
    val half = dims.map(_ / 2.0)
    def offset(row: Int) = -(linear(3 * row) * half(0) + linear(3 * row + 1) * half(1) + linear(3 * row + 2) * half(2))
    ToolCoordinates.affine(
      Vector(m(0), m(1), m(2), offset(0), m(4), m(5), m(6), offset(1), m(8), m(9), m(10), offset(2), 0, 0, 0, 1)
    )

  /** tkRAS -> scanner RAS. */
  def tkrToScanner: Affine[D3] =
    ToolCoordinates.compose(torig.inverse, norig)

/** Toolkit coordinate systems, each decoded into canonical RAS millimetres in exactly one place. */
enum ToolCoordinates:
  /** RAS mm: nibabel, FreeSurfer scanner RAS, fMRIPrep, NIfTI world. */
  case RasMm

  /** LPS mm: ITK/ANTs physical space and AFNI's DICOM order. */
  case LpsMm

  /** FSL scaled-voxel mm of one volume (FLIRT/FNIRT). */
  case FslScaledVoxel(geometry: FslVolumeGeometry)

  /** FreeSurfer surface RAS (tkRAS) of one volume. */
  case TkRas(geometry: FreeSurferVolumeGeometry)

  /** Voxel indices of one volume (LTA VOX_TO_VOX, register.dat inputs). */
  case VoxelIndex(voxelToWorld: Affine[D3])

object ToolCoordinates:
  /** LPS <-> RAS. Self-inverse. The only definition of this flip in ScalaFIM. */
  val LpsToRas: Affine[D3] =
    affine(Vector(-1.0, 0, 0, 0, 0, -1.0, 0, 0, 0, 0, 1.0, 0, 0, 0, 0, 1.0))

  /** The affine taking coordinates in `system` to RAS millimetres. */
  def toRas(system: ToolCoordinates): Affine[D3] =
    system match
      case RasMm                      => Affine.identity[D3]
      case LpsMm                      => LpsToRas
      case FslScaledVoxel(geometry)   => geometry.fslToWorld
      case TkRas(geometry)            => geometry.tkrToScanner
      case VoxelIndex(voxelToWorld)   => voxelToWorld

  def fromRas(system: ToolCoordinates): Affine[D3] =
    toRas(system).inverse

  /** Re-express a matrix acting in `from` coordinates as one acting in `to` coordinates: `to <- from`. */
  def between(from: ToolCoordinates, to: ToolCoordinates): Affine[D3] =
    compose(toRas(from), fromRas(to))

  private[world] def affine(rowMajor: Vector[Double]): Affine[D3] =
    Affine.fromRowMajor[D3](rowMajor).fold(error => throw new IllegalArgumentException(s"invalid convention affine: ${error.message}"), identity)

  /** `first` then `second`, through the provider's checked composition. */
  private[world] def compose(first: Affine[D3], second: Affine[D3]): Affine[D3] =
    first.andThen(second).fold(error => throw new IllegalStateException(s"convention composition failed: ${error.message}"), identity)

  private[world] def determinant3(a: Affine[D3]): Double =
    val m = a.rowMajor
    m(0) * (m(5) * m(10) - m(6) * m(9)) - m(1) * (m(4) * m(10) - m(6) * m(8)) + m(2) * (m(4) * m(9) - m(5) * m(8))
