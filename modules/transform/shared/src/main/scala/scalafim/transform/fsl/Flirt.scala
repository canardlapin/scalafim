package scalafim.transform.fsl

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.world.{FslVolumeGeometry, ToolCoordinates}
import scalafim.transform.*
import scalafim.transform.nifti.NiftiRaw

/** A FLIRT matrix as stored: 4x4, row-major, mapping the input (source) volume's FSL scaled-voxel coordinates to the
  * reference volume's.
  */
final case class FlirtMatrix(rowMajor: Vector[Double]) derives CanEqual:
  require(rowMajor.size == 16, s"a FLIRT matrix has 16 values, got ${rowMajor.size}")

object FlirtCodec extends TransformCodec[FlirtMatrix]:
  val format: TransformFormat = TransformFormat.FslFlirt

  def decode(source: TransformSource): Either[TransformIoError, FlirtMatrix] =
    val text = source match
      case TransformSource.Text(t)       => t
      case TransformSource.Binary(bytes) => String(IArray.genericWrapArray(bytes).toArray, "UTF-8")
    val rows = text.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toVector
    val values = rows.map(_.split("\\s+").toVector.map(_.toDoubleOption))
    if rows.size != 4 || values.exists(row => row.size != 4 || row.exists(_.isEmpty)) then
      Left(TransformIoError.Malformed("FLIRT matrix", s"expected 4 rows of 4 numbers, got ${rows.size} rows"))
    else
      val flat = values.flatten.flatten
      if flat.exists(v => !v.isFinite) then Left(TransformIoError.Malformed("FLIRT matrix", "values must be finite"))
      else if flat.drop(12).zip(Vector(0.0, 0.0, 0.0, 1.0)).exists((a, b) => math.abs(a - b) > 1e-9) then
        Left(TransformIoError.Malformed("FLIRT matrix", s"last row must be 0 0 0 1, got ${flat.drop(12).mkString(" ")}"))
      else Right(FlirtMatrix(flat))

  def encode(matrix: FlirtMatrix): Either[TransformIoError, TransformSource] =
    val lines = matrix.rowMajor.grouped(4).map(_.map(NumberText.format).mkString("  ")).mkString("", "\n", "\n")
    Right(TransformSource.Text(lines))

/** FLIRT's meaning: `F` maps source FSL coordinates to reference FSL coordinates, so the RAS pullback (reference ->
  * source) is `srcFslToWorld * inverse(F) * refWorldToFsl`. FSL's own affine selection and radiological flip for each
  * volume live in [[FslVolumeGeometry]].
  */
object FlirtInterpretation extends Interpretation[FlirtMatrix, FslGrids, WorldTransform.Linear]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](matrix: FlirtMatrix, grids: FslGrids[S, T]): Either[TransformError, WorldTransform.Linear[S, T]] =
    interpretWith(matrix, grids, AssetRef("FLIRT matrix", None))

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](matrix: FlirtMatrix, grids: FslGrids[S, T], asset: AssetRef): Either[TransformError, WorldTransform.Linear[S, T]] =
    for
      flirt <- Affine.fromRowMajor[D3](matrix.rowMajor).left.map(TransformError.Geometry(_))
      refToFsl = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(grids.referenceGeometry))
      srcToWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(grids.sourceGeometry))
      pullback <- refToFsl.andThen(flirt.inverse).flatMap(_.andThen(srcToWorld)).left.map(TransformError.Geometry(_))
    yield WorldTransform.Linear(
      FramedAffine.betweenFrames[T, S, D3](grids.reference, grids.source)(pullback),
      TransformProvenance.read(TransformFormat.FslFlirt, asset)
    )

object FlirtExpression extends Expression[FlirtMatrix, FslGrids, WorldTransform.Linear]:
  def express[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform.Linear[S, T], grids: FslGrids[S, T]): Either[TransformError, FlirtMatrix] =
    val srcToFsl = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(grids.sourceGeometry))
    val refFslToWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(grids.referenceGeometry))
    // forward = source fsl -> source world -> (push) -> reference world -> reference fsl
    srcToFsl.inverse
      .andThen(transform.framed.operator.inverse)
      .flatMap(_.andThen(refFslToWorld.inverse))
      .left
      .map(TransformError.Geometry(_))
      .map(forward => FlirtMatrix(forward.rowMajor))

/** FSL's geometry for a volume read from its raw NIfTI-1 header, applying FSL's own qform/sform selection. */
object FslHeaderGeometry:
  def apply(raw: NiftiRaw): Either[TransformError, FslVolumeGeometry] =
    def affine(values: Vector[Double]) = Affine.fromRowMajor[D3](values).left.map(TransformError.Geometry(_))
    for
      q <- if raw.qformCode > 0 then affine(raw.qformRowMajor).map(Some(_)) else Right(None)
      s <- if raw.sformCode > 0 then affine(raw.sformRowMajor).map(Some(_)) else Right(None)
      g <- FslVolumeGeometry
        .fromHeader(raw.spatialShape, raw.pixdim.slice(1, 4), raw.qformCode, q, raw.sformCode, s)
        .left
        .map(TransformError.Space(_))
    yield g
