package scalafim.transform.afni

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.world.ToolCoordinates
import scalafim.transform.*

/** An AFNI `.aff12.1D` file: one or more 3x4 affines in RAI/DICOM (numerically LPS) millimetres, each mapping base
  * coordinates to source coordinates (`Xsource = M Xbase`, per `3dAllineate -help`): already the resampling pullback.
  * Header comments are kept as written; every row is kept value-exactly, negative zeros included.
  */
final case class Aff12Series(rows: Vector[Vector[Double]], comments: Vector[String]) derives CanEqual:
  require(rows.forall(_.size == 12), "every aff12 row has 12 values")

object Aff12Codec extends TransformCodec[Aff12Series]:
  val format: TransformFormat = TransformFormat.AfniAff12

  def decode(source: TransformSource): Either[TransformIoError, Aff12Series] =
    val text = source match
      case TransformSource.Text(t)       => t
      case TransformSource.Binary(bytes) => String(IArray.genericWrapArray(bytes).toArray, "UTF-8")
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val comments = lines.filter(_.startsWith("#"))
    val numeric = lines.filterNot(_.startsWith("#")).map(_.split("\\s+").toVector.map(_.toDoubleOption))
    if numeric.isEmpty then Left(TransformIoError.Malformed("AFNI aff12", "no matrix rows"))
    else if numeric.exists(_.exists(_.isEmpty)) then Left(TransformIoError.Malformed("AFNI aff12", "non-numeric value"))
    else
      val values = numeric.map(_.flatten)
      val rows =
        if values.forall(_.size == 12) then Right(values)
        else if values.forall(_.size == 4) && values.size % 3 == 0 then Right(values.grouped(3).map(_.flatten).toVector) // cat_matvec 3x4 form
        else Left(TransformIoError.Malformed("AFNI aff12", s"rows must hold 12 values (or 3x4 blocks), got sizes ${values.map(_.size).distinct.mkString(",")}"))
      rows.flatMap: r =>
        if r.flatten.exists(v => !v.isFinite) then Left(TransformIoError.Malformed("AFNI aff12", "values must be finite"))
        else Right(Aff12Series(r, comments))

  def encode(series: Aff12Series): Either[TransformIoError, TransformSource] =
    if series.rows.isEmpty then Left(TransformIoError.Malformed("AFNI aff12", "no rows to write"))
    else
      val body = series.rows.map(_.map(NumberText.format).mkString(" ")).mkString("\n")
      Right(TransformSource.Text((series.comments :+ body).mkString("", "\n", "\n")))

/** AFNI's cardinal/real distinction for oblique datasets (`THD_dicom_real_to_card`): the cardinal matrix drops the
  * obliquity of a dataset's voxel-to-world affine, keeping voxel sizes and origin.
  */
object AfniCardinal:
  /** Obliquity of a dataset from its RAS voxel-to-world affine, or `None` when it is cardinal already. */
  def obliquity(voxelToWorld: Affine[D3], thresholdDegrees: Double = 0.01): Option[AfniObliquity] =
    val m = voxelToWorld.rowMajor
    val columns = Vector.tabulate(3)(c => Vector(m(c), m(4 + c), m(8 + c)))
    val angles = columns.map: column =>
      val norm = math.sqrt(column.map(v => v * v).sum)
      val cos = if norm == 0.0 then 1.0 else math.min(1.0, column.map(math.abs).max / norm)
      math.toDegrees(math.acos(cos))
    Option.when(angles.max > thresholdDegrees):
      val cardinal = realToCardinalAffine(m, columns)
      AfniObliquity(cardinalToRealOf(voxelToWorld, cardinal))

  private def realToCardinalAffine(m: Vector[Double], columns: Vector[Vector[Double]]): Affine[D3] =
    val linear = Vector.tabulate(3, 3): (r, c) =>
      val column = columns(c)
      val maxAbs = column.map(math.abs).max
      val cosine = if maxAbs == 0.0 then 0.0 else column(r) / maxAbs
      val size = math.round(math.sqrt(column.map(v => v * v).sum) * 1e4) / 1e4
      if math.abs(cosine) < 1.0 then 0.0 else cosine * size
    Affine
      .fromRowMajor[D3](Vector(linear(0)(0), linear(0)(1), linear(0)(2), m(3), linear(1)(0), linear(1)(1), linear(1)(2), m(7), linear(2)(0), linear(2)(1), linear(2)(2), m(11), 0, 0, 0, 1))
      .fold(error => throw new IllegalStateException(error.message), identity)

  /** cardinal -> real = oblique * inverse(cardinal). */
  private def cardinalToRealOf(oblique: Affine[D3], cardinal: Affine[D3]): Affine[D3] =
    cardinal.inverse.andThen(oblique).fold(error => throw new IllegalStateException(error.message), identity)

/** AFNI affines as world transforms: the RAS pullback is `F M F`, corrected for oblique base/source datasets when the
  * context asks for it (source card->real on the left, base real->card on the right).
  */
object Aff12Interpretation extends Interpretation[Aff12Series, AfniContext, LinearSeries]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](series: Aff12Series, context: AfniContext[S, T]): Either[TransformError, LinearSeries[S, T]] =
    interpretWith(series, context, AssetRef("AFNI aff12", None))

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](series: Aff12Series, context: AfniContext[S, T], asset: AssetRef): Either[TransformError, LinearSeries[S, T]] =
    val provenance = TransformProvenance.read(TransformFormat.AfniAff12, asset)
    series.rows
      .foldLeft[Either[TransformError, Vector[WorldTransform.Linear[S, T]]]](Right(Vector.empty)): (acc, row) =>
        for
          done <- acc
          rai <- Affine.fromRowMajor[D3](row ++ Vector(0.0, 0.0, 0.0, 1.0)).left.map(TransformError.Geometry(_))
          ras <- ToolCoordinates.LpsToRas.andThen(rai).flatMap(_.andThen(ToolCoordinates.LpsToRas)).left.map(TransformError.Geometry(_))
          corrected <- correct(ras, context.correction)
        yield done :+ WorldTransform.Linear(FramedAffine.betweenFrames[T, S, D3](context.frames.target, context.frames.source)(corrected), provenance)
      .map(LinearSeries(_))

  private def correct(ras: Affine[D3], correction: CardinalCorrection): Either[TransformError, Affine[D3]] =
    correction match
      case CardinalCorrection.Off => Right(ras)
      case CardinalCorrection.On(source, base) =>
        // pullback applies base real->card first, then the matrix, then source card->real
        val withBase = base.fold(Right(ras))(b => b.cardinalToReal.inverse.andThen(ras))
        withBase.flatMap(m => source.fold(Right(m))(s => m.andThen(s.cardinalToReal))).left.map(TransformError.Geometry(_))

object Aff12Expression extends Expression[Aff12Series, AfniContext, LinearSeries]:
  def express[S <: Frame[D3], T <: Frame[D3]](series: LinearSeries[S, T], context: AfniContext[S, T]): Either[TransformError, Aff12Series] =
    series.transforms
      .foldLeft[Either[TransformError, Vector[Vector[Double]]]](Right(Vector.empty)): (acc, transform) =>
        for
          done <- acc
          uncorrected <- uncorrect(transform.framed.operator, context.correction)
          rai <- ToolCoordinates.LpsToRas.andThen(uncorrected).flatMap(_.andThen(ToolCoordinates.LpsToRas)).left.map(TransformError.Geometry(_))
        yield done :+ rai.rowMajor.take(12)
      .map(Aff12Series(_, Vector.empty))

  private def uncorrect(ras: Affine[D3], correction: CardinalCorrection): Either[TransformError, Affine[D3]] =
    correction match
      case CardinalCorrection.Off => Right(ras)
      case CardinalCorrection.On(source, base) =>
        val withoutSource = source.fold(Right(ras))(s => ras.andThen(s.cardinalToReal.inverse))
        withoutSource.flatMap(m => base.fold(Right(m))(b => b.cardinalToReal.andThen(m))).left.map(TransformError.Geometry(_))
