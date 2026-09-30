package scalafim.transform.freesurfer

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.world.{FreeSurferVolumeGeometry, FslVolumeGeometry, ToolCoordinates}
import scalafim.transform.*

/** A FreeSurfer volume geometry block (`VOL_GEOM`): dims, voxel sizes, column direction cosines and centre RAS. */
final case class VolGeom(
    valid: Boolean,
    filename: String,
    dims: Vector[Int],
    voxelSize: Vector[Double],
    xras: Vector[Double],
    yras: Vector[Double],
    zras: Vector[Double],
    cras: Vector[Double]
) derives CanEqual:
  /** `VGgetVoxelToRasXform`: `[Mdc * D | c_ras - Mdc * D * dims / 2]`. */
  def voxelToRas: Either[TransformError, Affine[D3]] =
    val columns = Vector(xras, yras, zras).zip(voxelSize).map((c, s) => c.map(_ * s))
    val linear = Vector.tabulate(3, 3)((r, c) => columns(c)(r))
    val half = dims.map(_ / 2.0)
    val offset = Vector.tabulate(3)(r => cras(r) - (0 until 3).map(c => linear(r)(c) * half(c)).sum)
    Affine
      .fromRowMajor[D3](Vector.tabulate(3)(r => linear(r) :+ offset(r)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0))
      .left
      .map(TransformError.Geometry(_))

  def freeSurferGeometry: Either[TransformError, FreeSurferVolumeGeometry] =
    voxelToRas.map(FreeSurferVolumeGeometry(dims, _))

  /** FSL's view of this volume: the LTA stores one vox2ras, which FSL would select like an sform. */
  def fslGeometry: Either[TransformError, FslVolumeGeometry] =
    voxelToRas.flatMap(v2r => FslVolumeGeometry.fromHeader(dims, voxelSize, 0, None, 1, Some(v2r)).left.map(TransformError.Space(_)))

object VolGeom:
  def of(dims: Vector[Int], voxelToRas: Affine[D3], filename: String = ""): VolGeom =
    val m = voxelToRas.rowMajor
    val columns = Vector.tabulate(3)(c => Vector(m(c), m(4 + c), m(8 + c)))
    val sizes = columns.map(col => math.sqrt(col.map(v => v * v).sum))
    val cosines = columns.zip(sizes).map((col, s) => col.map(_ / s))
    val center = Vector.tabulate(3)(r => m(4 * r + 3) + (0 until 3).map(c => m(4 * r + c) * dims(c) / 2.0).sum)
    VolGeom(valid = true, filename, dims, sizes, cosines(0), cosines(1), cosines(2), center)

/** LTA transform types (FreeSurfer `transform.h`). */
enum LtaKind(val code: Int) derives CanEqual:
  case VoxToVox extends LtaKind(0)
  case RasToRas extends LtaKind(1)
  case PhysVoxToPhysVox extends LtaKind(2)
  case RegisterDat extends LtaKind(14)
  case FslReg extends LtaKind(15)
  case CoronalRasToCoronalRas extends LtaKind(21)
  case Other(override val code: Int) extends LtaKind(code)

object LtaKind:
  def fromCode(code: Int): LtaKind =
    Vector(VoxToVox, RasToRas, PhysVoxToPhysVox, RegisterDat, FslReg, CoronalRasToCoronalRas).find(_.code == code).getOrElse(Other(code))

/** A single-transform LTA file. `source` is the movable volume and `destination` the reference ("movsrc->refdst"). */
final case class LtaFile(
    kind: LtaKind,
    matrix: Vector[Double],
    source: VolGeom,
    destination: VolGeom,
    subject: Option[String],
    fscale: Option[Double],
    sigma: Double
) derives CanEqual

object LtaCodec extends TransformCodec[LtaFile]:
  val format: TransformFormat = TransformFormat.FreeSurferLta

  def decode(source: TransformSource): Either[TransformIoError, LtaFile] =
    val text = FreeSurferText.of(source)
    val lines = text.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toVector
    def field(name: String): Option[String] =
      lines.find(l => l.startsWith(name) && l.drop(name.length).trim.startsWith("=")).map(_.split("=", 2)(1).split("#")(0).trim)
    for
      kind <- field("type").flatMap(_.split("\\s+").headOption).flatMap(_.toIntOption).map(LtaKind.fromCode)
        .toRight(TransformIoError.Malformed("LTA", "missing 'type ='"))
      count = field("nxforms").flatMap(_.toIntOption).getOrElse(1)
      _ <- Either.cond(count == 1, (), TransformIoError.Malformed("LTA", s"only single-transform LTAs are supported, nxforms = $count"))
      sizeLine = lines.indexWhere(_.matches("\\d+\\s+4\\s+4"))
      _ <- Either.cond(sizeLine >= 0 && sizeLine + 4 < lines.size, (), TransformIoError.Malformed("LTA", "missing '1 4 4' matrix block"))
      matrix <- FreeSurferText.numbers(lines.slice(sizeLine + 1, sizeLine + 5), 16, "LTA matrix")
      src <- volGeom(lines, "src volume info")
      dst <- volGeom(lines, "dst volume info")
    yield LtaFile(
      kind,
      matrix,
      src,
      dst,
      lines.find(_.startsWith("subject ")).map(_.stripPrefix("subject ").trim),
      lines.find(_.startsWith("fscale ")).flatMap(_.stripPrefix("fscale ").trim.toDoubleOption),
      field("sigma").flatMap(_.toDoubleOption).getOrElse(1.0)
    )

  private def volGeom(lines: Vector[String], header: String): Either[TransformIoError, VolGeom] =
    val start = lines.indexWhere(_.startsWith(header))
    if start < 0 then Left(TransformIoError.Malformed("LTA", s"missing '$header'"))
    else
      val block = lines.slice(start + 1, start + 9)
      def value(name: String): Option[String] =
        block.find(l => l.startsWith(name) && l.drop(name.length).trim.startsWith("=")).map(_.split("=", 2)(1).split("#")(0).trim)
      def vector(name: String, n: Int): Either[TransformIoError, Vector[Double]] =
        value(name).map(_.split("\\s+").toVector.flatMap(_.toDoubleOption)).filter(_.size == n)
          .toRight(TransformIoError.Malformed("LTA", s"$header: bad '$name'"))
      for
        dims <- vector("volume", 3).map(_.map(_.toInt))
        size <- vector("voxelsize", 3)
        x <- vector("xras", 3)
        y <- vector("yras", 3)
        z <- vector("zras", 3)
        c <- vector("cras", 3)
      yield VolGeom(value("valid").flatMap(_.split("\\s+").headOption).contains("1"), value("filename").getOrElse(""), dims, size, x, y, z, c)

  def encode(file: LtaFile): Either[TransformIoError, TransformSource] =
    def vg(label: String, g: VolGeom) = Vector(
      s"$label volume info",
      s"valid = ${if g.valid then 1 else 0}  # volume info valid",
      s"filename = ${g.filename}",
      s"volume = ${g.dims.mkString(" ")}",
      s"voxelsize = ${g.voxelSize.map(NumberText.format).mkString(" ")}",
      s"xras   = ${g.xras.map(NumberText.format).mkString(" ")}",
      s"yras   = ${g.yras.map(NumberText.format).mkString(" ")}",
      s"zras   = ${g.zras.map(NumberText.format).mkString(" ")}",
      s"cras   = ${g.cras.map(NumberText.format).mkString(" ")}"
    )
    val lines = Vector(
      "# transform file written by ScalaFIM",
      s"type      = ${file.kind.code}",
      "nxforms   = 1",
      "mean      = 0.0000 0.0000 0.0000",
      s"sigma     = ${NumberText.format(file.sigma)}",
      "1 4 4"
    ) ++ file.matrix.grouped(4).map(_.map(NumberText.format).mkString(" ")) ++ vg("src", file.source) ++ vg("dst", file.destination) ++
      file.subject.map(s => s"subject $s").toVector ++ file.fscale.map(f => s"fscale ${NumberText.format(f)}").toVector
    Right(TransformSource.Text(lines.mkString("", "\n", "\n")))

/** LTA geometry for writing: the endpoint frames plus the volume geometries FreeSurfer tools need. */
final case class LtaGeometry[S <: Frame[D3], T <: Frame[D3]](source: S, sourceGeometry: VolGeom, target: T, targetGeometry: VolGeom)

/** What an LTA means. Every interpreted type is reduced to the RAS pullback reference -> movable, following FreeSurfer
  * `LTAchangeType`, `LTAgetV2V`, `MRItkReg2Native` and `MRIfsl2TkReg`.
  */
object LtaInterpretation extends Interpretation[LtaFile, Frames, WorldTransform.Linear]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](file: LtaFile, frames: Frames[S, T]): Either[TransformError, WorldTransform.Linear[S, T]] =
    interpretWith(file, frames, AssetRef("LTA", None))

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](file: LtaFile, frames: Frames[S, T], asset: AssetRef): Either[TransformError, WorldTransform.Linear[S, T]] =
    for
      m <- FreeSurferText.affine(file.matrix)
      srcV2R <- file.source.voxelToRas
      dstV2R <- file.destination.voxelToRas
      pullback <- file.kind match
        case LtaKind.RasToRas => Right(m.inverse)
        case LtaKind.VoxToVox =>
          // forward RAS = dstV2R * M * inv(srcV2R)
          FreeSurferText.chain(srcV2R.inverse, m, dstV2R).map(_.inverse)
        case LtaKind.PhysVoxToPhysVox =>
          val sScale = FreeSurferText.diagonal(file.source.voxelSize)
          val dScale = FreeSurferText.diagonal(file.destination.voxelSize)
          // M = dSize * V2V * inv(sSize)  =>  V2V = inv(dSize) * M * sSize
          FreeSurferText.chain(srcV2R.inverse, sScale, m, dScale.inverse, dstV2R).map(_.inverse)
        case LtaKind.RegisterDat =>
          tkregPullback(m, file.source, file.destination)
        case LtaKind.FslReg =>
          for
            srcFsl <- file.source.fslGeometry
            dstFsl <- file.destination.fslGeometry
            pull <- FreeSurferText.chain(
              ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(dstFsl)),
              m.inverse,
              ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(srcFsl))
            )
          yield pull
        case LtaKind.CoronalRasToCoronalRas | LtaKind.Other(_) =>
          Left(TransformError.Io(TransformIoError.UnsupportedLtaType(file.kind.code, "convert it to LINEAR_RAS_TO_RAS with lta_convert; FreeSurfer itself notes CORONAL_RAS_TO_CORONAL_RAS is not equivalent to REGISTER_DAT")))
    yield WorldTransform.Linear(
      FramedAffine.betweenFrames[T, S, D3](frames.target, frames.source)(pullback),
      TransformProvenance.read(TransformFormat.FreeSurferLta, asset)
    )

  /** tkregister R maps reference tkRAS -> movable tkRAS; the scanner pullback is `Tmov * inv(Kmov) * R * Kref * inv(Tref)`. */
  private[freesurfer] def tkregPullback(r: Affine[D3], movable: VolGeom, reference: VolGeom): Either[TransformError, Affine[D3]] =
    for
      mov <- movable.freeSurferGeometry
      ref <- reference.freeSurferGeometry
      pull <- FreeSurferText.chain(ref.norig.inverse, ref.torig, r, mov.torig.inverse, mov.norig)
    yield pull

object LtaExpression extends Expression[LtaFile, LtaGeometry, WorldTransform.Linear]:
  /** Always writes LINEAR_RAS_TO_RAS, the type every FreeSurfer tool reads without volume guesses. */
  def express[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform.Linear[S, T], geometry: LtaGeometry[S, T]): Either[TransformError, LtaFile] =
    Right(LtaFile(LtaKind.RasToRas, transform.framed.operator.inverse.rowMajor, geometry.sourceGeometry, geometry.targetGeometry, None, None, 1.0))

/** An MNI `.xfm` linear transform (e.g. `talairach.xfm`): a 3x4 map from source RAS to target RAS. */
final case class MniXfm(rows: Vector[Double], comments: Vector[String]) derives CanEqual:
  require(rows.size == 12, "an MNI linear xfm holds 12 values")

object MniXfmCodec extends TransformCodec[MniXfm]:
  val format: TransformFormat = TransformFormat.FreeSurferXfm

  def decode(source: TransformSource): Either[TransformIoError, MniXfm] =
    val text = FreeSurferText.of(source)
    val lines = text.linesIterator.map(_.trim).toVector
    if !lines.headOption.exists(_.startsWith("MNI Transform File")) then Left(TransformIoError.WrongSource(format, "an 'MNI Transform File' header"))
    else if !lines.exists(l => l.startsWith("Transform_Type") && l.contains("Linear")) then
      Left(TransformIoError.Malformed("MNI xfm", "only Transform_Type = Linear is supported"))
    else
      val start = lines.indexWhere(_.startsWith("Linear_Transform"))
      if start < 0 then Left(TransformIoError.Malformed("MNI xfm", "missing Linear_Transform"))
      else
        val body = (lines(start).split("=", 2).lift(1).toVector ++ lines.drop(start + 1)).mkString(" ")
        val numbers = body.takeWhile(_ != ';').split("\\s+").toVector.filter(_.nonEmpty).map(_.toDoubleOption)
        if numbers.size != 12 || numbers.exists(_.isEmpty) then Left(TransformIoError.Malformed("MNI xfm", s"expected 12 numbers, got ${numbers.size}"))
        else Right(MniXfm(numbers.flatten, lines.filter(_.startsWith("%"))))

  def encode(xfm: MniXfm): Either[TransformIoError, TransformSource] =
    val rows = xfm.rows.grouped(4).map(_.map(NumberText.format).mkString(" ")).toVector
    val text = (Vector("MNI Transform File") ++ xfm.comments ++ Vector("", "Transform_Type = Linear;", "Linear_Transform =") ++
      rows.init.map(" " + _) :+ s" ${rows.last};").mkString("", "\n", "\n")
    Right(TransformSource.Text(text))

object MniXfmInterpretation extends Interpretation[MniXfm, Frames, WorldTransform.Linear]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](xfm: MniXfm, frames: Frames[S, T]): Either[TransformError, WorldTransform.Linear[S, T]] =
    FreeSurferText.affine(xfm.rows ++ Vector(0.0, 0.0, 0.0, 1.0)).map: forward =>
      WorldTransform.Linear(
        FramedAffine.betweenFrames[T, S, D3](frames.target, frames.source)(forward.inverse),
        TransformProvenance.read(TransformFormat.FreeSurferXfm, AssetRef("MNI xfm", None))
      )

/** A tkregister `register.dat`: subject, in-plane and between-plane resolution, intensity, the 4x4 matrix mapping
  * reference (target) tkRAS to movable tkRAS, and optional trailing flags such as `round`.
  */
final case class RegisterDat(subject: String, inPlane: Double, betweenPlane: Double, intensity: Double, matrix: Vector[Double], flags: Vector[String]) derives CanEqual

object RegisterDatCodec extends TransformCodec[RegisterDat]:
  val format: TransformFormat = TransformFormat.FreeSurferRegisterDat

  def decode(source: TransformSource): Either[TransformIoError, RegisterDat] =
    val lines = FreeSurferText.of(source).linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    def scalar(i: Int, name: String) = lines.lift(i).flatMap(_.toDoubleOption).toRight(TransformIoError.Malformed("register.dat", s"bad $name"))
    for
      _ <- Either.cond(lines.size >= 8, (), TransformIoError.Malformed("register.dat", s"expected at least 8 lines, got ${lines.size}"))
      inPlane <- scalar(1, "in-plane resolution")
      between <- scalar(2, "between-plane resolution")
      intensity <- scalar(3, "intensity")
      matrix <- FreeSurferText.numbers(lines.slice(4, 8), 16, "register.dat matrix")
    yield RegisterDat(lines.head, inPlane, between, intensity, matrix, lines.drop(8))

  def encode(dat: RegisterDat): Either[TransformIoError, TransformSource] =
    val text = (Vector(dat.subject, f"${dat.inPlane}%f", f"${dat.betweenPlane}%f", f"${dat.intensity}%f") ++
      dat.matrix.grouped(4).map(_.map(NumberText.format).mkString(" ")) ++ dat.flags).mkString("", "\n", "\n")
    Right(TransformSource.Text(text))

object RegisterDatInterpretation extends Interpretation[RegisterDat, TkRegGrids, WorldTransform.Linear]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](dat: RegisterDat, grids: TkRegGrids[S, T]): Either[TransformError, WorldTransform.Linear[S, T]] =
    for
      r <- FreeSurferText.affine(dat.matrix)
      pull <- FreeSurferText.chain(grids.targetGeometry.norig.inverse, grids.targetGeometry.torig, r, grids.movableGeometry.torig.inverse, grids.movableGeometry.norig)
    yield WorldTransform.Linear(
      FramedAffine.betweenFrames[T, S, D3](grids.target, grids.movable)(pull),
      TransformProvenance.read(TransformFormat.FreeSurferRegisterDat, AssetRef("register.dat", None))
    )

private[freesurfer] object FreeSurferText:
  def of(source: TransformSource): String =
    source match
      case TransformSource.Text(t)       => t
      case TransformSource.Binary(bytes) => String(IArray.genericWrapArray(bytes).toArray, "UTF-8")

  def numbers(lines: Vector[String], expected: Int, what: String): Either[TransformIoError, Vector[Double]] =
    val values = lines.flatMap(_.split("\\s+").toVector.filter(_.nonEmpty)).map(_.toDoubleOption)
    if values.size != expected || values.exists(_.isEmpty) then Left(TransformIoError.Malformed(what, s"expected $expected numbers, got ${values.size}"))
    else Right(values.flatten)

  def affine(values: Vector[Double]): Either[TransformError, Affine[D3]] =
    Affine.fromRowMajor[D3](values).left.map(TransformError.Geometry(_))

  def diagonal(scale: Vector[Double]): Affine[D3] =
    Affine.fromRowMajor[D3](Vector(scale(0), 0, 0, 0, 0, scale(1), 0, 0, 0, 0, scale(2), 0, 0, 0, 0, 1))
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Apply `steps` in order (first applied first). */
  def chain(steps: Affine[D3]*): Either[TransformError, Affine[D3]] =
    steps.tail.foldLeft[Either[TransformError, Affine[D3]]](Right(steps.head))((acc, next) => acc.flatMap(_.andThen(next).left.map(TransformError.Geometry(_))))
