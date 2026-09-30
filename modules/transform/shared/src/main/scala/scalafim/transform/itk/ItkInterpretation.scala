package scalafim.transform.itk

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.world.ToolCoordinates
import scalafim.transform.*

/** What a linear ITK file means. ITK stores the resampling pullback in LPS: `TransformPoint` maps a point of the fixed
  * (target) space to the moving (source) space. The RAS pullback is therefore `F * M * F` with `F` the LPS flip, and a
  * composite applies its components last-first.
  */
object ItkLinearInterpretation extends Interpretation[ItkTransformFile, Frames, TransformChain]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](file: ItkTransformFile, frames: Frames[S, T]): Either[TransformError, TransformChain[S, T]] =
    interpretWith(file, frames, AssetRef("ITK transform", None), TransformFormat.ItkText)

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](
      file: ItkTransformFile,
      frames: Frames[S, T],
      asset: AssetRef,
      format: TransformFormat
  ): Either[TransformError, TransformChain[S, T]] =
    val components = file.components
    if components.isEmpty then Left(TransformError.Invalid("ITK file has no transform components"))
    else if !components.forall(ItkLinear.isLinear) then
      val offending = components.filterNot(ItkLinear.isLinear).head
      Left(ItkLinear.matrixOffset(offending).fold(TransformError.Io(_), _ => TransformError.Invalid(s"${offending.typeName} is not linear")))
    else
      val provenance = TransformProvenance.read(format, asset)
      for
        lps <- components.foldLeft[Either[TransformError, Vector[Affine[D3]]]](Right(Vector.empty)): (acc, entry) =>
          acc.flatMap(done => ItkLinear.matrixOffset(entry).left.map(TransformError.Io(_)).flatMap(mo => affine(mo.rowMajor4)).map(done :+ _))
        // A point goes through the last component first.
        lpsPullback <- lps.reverse match
          case first +: rest =>
            rest.foldLeft[Either[TransformError, Affine[D3]]](Right(first)): (acc, next) =>
              acc.flatMap(_.andThen(next).left.map(TransformError.Geometry(_)))
          case _ => Left(TransformError.Invalid("empty ITK composite"))
        rasPullback <- rasConjugate(lpsPullback)
      yield
        val composed = WorldTransform.Linear(FramedAffine.betweenFrames[T, S, D3](frames.target, frames.source)(rasPullback), provenance)
        val stages = components.map(c => TransformChain.Stage(c.typeName, provenance))
        TransformChain(stages, composed)

  /** The single linear transform of a file, fused when it is a composite of linear components. */
  def linear[S <: Frame[D3], T <: Frame[D3]](file: ItkTransformFile, frames: Frames[S, T], asset: AssetRef, format: TransformFormat): Either[TransformError, WorldTransform.Linear[S, T]] =
    interpretWith(file, frames, asset, format).flatMap:
      _.composed match
        case linear: WorldTransform.Linear[S, T] @unchecked => Right(linear)
        case _                                           => Left(TransformError.Invalid("ITK file is not linear"))

  private def affine(rowMajor: Vector[Double]): Either[TransformError, Affine[D3]] =
    Affine.fromRowMajor[D3](rowMajor).left.map(TransformError.Geometry(_))

  private def rasConjugate(lps: Affine[D3]): Either[TransformError, Affine[D3]] =
    ToolCoordinates.LpsToRas.andThen(lps).flatMap(_.andThen(ToolCoordinates.LpsToRas)).left.map(TransformError.Geometry(_))

/** Writes a linear world transform as an ITK `AffineTransform_double_3_3` (centre folded into the translation). */
object ItkLinearExpression extends Expression[ItkTransformFile, Frames, WorldTransform.Linear]:
  def express[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform.Linear[S, T], frames: Frames[S, T]): Either[TransformError, ItkTransformFile] =
    ToolCoordinates.LpsToRas
      .andThen(transform.framed.operator)
      .flatMap(_.andThen(ToolCoordinates.LpsToRas))
      .left
      .map(TransformError.Geometry(_))
      .map: lps =>
        val m = lps.rowMajor
        val parameters = Vector(m(0), m(1), m(2), m(4), m(5), m(6), m(8), m(9), m(10), m(3), m(7), m(11))
        ItkTransformFile(Vector(ItkEntry("AffineTransform_double_3_3", parameters, Vector(0.0, 0.0, 0.0))))
