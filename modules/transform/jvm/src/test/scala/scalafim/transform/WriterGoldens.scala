package scalafim.transform

import java.nio.file.{Files, Path, Paths}

import image4s.geometry.{Affine, D3}
import scalafim.image.world.{FrameCatalog, FreeSurferVolumeGeometry, WorldSpace}
import scalafim.transform.Conversion.EncodedTransform
import scalafim.transform.freesurfer.VolGeom
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.OracleFixtures
import reframe4s.lie.FramedAffine

/** P5.03 writer goldens: one fixed transform written by every writer. `tools/transform/writer-acceptance.sh` renders
  * them with `main`, has the native tools read them, and records a receipt; [[WriterGoldensSuite]] fails when a writer's
  * bytes drift from the committed goldens or the receipt no longer matches them.
  */
object WriterGoldens:
  val Directory = "writer_goldens"

  /** Reference-space RAS points and the source RAS point each must map to under the golden pullback. */
  val queries: Vector[Vector[Double]] = Vector(Vector(10.0, -20.0, 30.0), Vector(-35.5, 12.25, -8.0), Vector(0.0, 0.0, 0.0), Vector(52.0, -61.0, 17.5))

  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => throw new IllegalStateException(error.toString), identity)

  /** The golden pullback (reference -> source), sheared and oblique so axis/sign errors cannot cancel. */
  val pullback: Affine[D3] =
    ok(Affine.fromRowMajor[D3](Vector(0.97, 0.04, -0.03, 2.5, -0.02, 1.03, 0.05, -3.75, 0.03, -0.04, 0.99, 1.25, 0, 0, 0, 1)))

  def expected: Vector[Vector[Double]] = queries.map(p => ok(pullback(p)))

  def render(): Vector[(String, Array[Byte])] =
    val source = FrameCatalog.frame(ok(WorldSpace.declare("golden source")))
    val target = FrameCatalog.frame(ok(WorldSpace.declare("golden reference")))
    val linear = WorldTransform.Linear(FramedAffine.betweenFrames[target.type, source.type, D3](target, source)(pullback), TransformProvenance.constructed("writer golden"))
    val movable = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded("freesurfer_linear/movable.nii"))))
    val reference = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded("freesurfer_linear/reference.nii"))))
    def sform(r: NiftiRaw) = ok(Affine.fromRowMajor[D3](r.sformRowMajor))
    val context = ConversionContext(
      fsl = Some(ConversionContext.FslPair(ok(FslHeaderGeometry(movable)), ok(FslHeaderGeometry(reference)))),
      tkreg = Some(ConversionContext.TkRegPair(FreeSurferVolumeGeometry(movable.spatialShape, sform(movable)), FreeSurferVolumeGeometry(reference.spatialShape, sform(reference)))),
      lta = Some(ConversionContext.LtaPair(VolGeom.of(movable.spatialShape, sform(movable), "movable.nii"), VolGeom.of(reference.spatialShape, sform(reference), "reference.nii"))),
      lattice = Some(ConversionContext.Lattice(Vector(7, 6, 5), ok(Affine.fromRowMajor[D3](Vector(3.9822238549275513, -0.3548541629536086, -0.1871652382859354, -12.0, 0.3192592856443811, 4.163977053899107, -0.46569706363352226, -10.0, 0.19991667708271332, 0.41877633366298295, 4.471923011244769, -8.0, 0, 0, 0, 1)))))
    )
    val native = NativeTransform.Itk(ok(itk.ItkLinearExpression.express(linear, Frames(source, target))), TransformFormat.ItkText)
    val outputs = Vector(
      "affine.tfm" -> TransformFormat.ItkText,
      "affine.mat" -> TransformFormat.ItkMatlab,
      "flirt.mat" -> TransformFormat.FslFlirt,
      "affine.aff12.1D" -> TransformFormat.AfniAff12,
      "affine.lta" -> TransformFormat.FreeSurferLta,
      "talairach.xfm" -> TransformFormat.FreeSurferXfm,
      "register.dat" -> TransformFormat.FreeSurferRegisterDat,
      "field_1Warp.nii" -> TransformFormat.AntsDisplacementNifti,
      "field_fnirt_relative.nii" -> TransformFormat.FslFnirtField
    )
    outputs.map: (name, format) =>
      name -> bytes(ok(Conversion.convert(native, format, context, context)))

  private def bytes(encoded: EncodedTransform): Array[Byte] =
    encoded match
      case EncodedTransform.Source(_, TransformSource.Text(t))   => t.getBytes("UTF-8")
      case EncodedTransform.Source(_, TransformSource.Binary(b)) => IArray.genericWrapArray(b).toArray
      case EncodedTransform.Hdf5X5(_)                            => throw new IllegalStateException("X5 has no file writer")

  /** Write goldens and the query table into `dir`. */
  def main(args: Array[String]): Unit =
    val dir: Path = Paths.get(args.headOption.getOrElse(sys.error("usage: WriterGoldens <directory>")))
    Files.createDirectories(dir)
    val goldens = render()
    goldens.foreach((name, data) => Files.write(dir.resolve(name), data))
    Files.writeString(dir.resolve("goldens.txt"), goldens.map(_._1).mkString("", "\n", "\n"))
    val rows = queries.zip(expected).map((q, e) => (q ++ e).map(v => v.toString).mkString("\t"))
    Files.writeString(dir.resolve("queries.tsv"), ("x\ty\tz\tsx\tsy\tsz" +: rows).mkString("", "\n", "\n"))
    Files.writeString(dir.resolve("pullback.json"), pullback.rowMajor.mkString("{\"pullback\": [", ", ", "]}\n"))
    println(s"wrote ${goldens.size} goldens to $dir")
