package scalafim.transform

import java.nio.file.{Files, Path, Paths}

import scalafim.transform.afni.Aff12Codec
import scalafim.transform.field.VectorFieldNiftiCodec
import scalafim.transform.freesurfer.{LtaCodec, MniXfmCodec, RegisterDatCodec}
import scalafim.transform.fsl.FlirtCodec
import scalafim.transform.itk.{ItkMatlabCodec, ItkTextCodec}

import scala.jdk.CollectionConverters.*

/** P5.01: every oracle file of every writable format decodes, encodes and decodes again to the same native value.
  * Value-exact, not lexical: comments, whitespace and number spelling are not preserved. ITK HDF5 and X5 are read-only:
  * the JVM HDF5 library cannot write the variable-length strings both require (found by writer acceptance, P5.03).
  */
class RoundTripSuite extends munit.FunSuite:
  private val root: Path = Paths.get(getClass.getResource("/scalafim/transform/oracle").toURI)

  private def files: Vector[Path] =
    Files.walk(root).iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString)

  private def rel(p: Path): String = root.relativize(p).toString

  private def isField(native: NativeTransform): Boolean =
    native match
      case NativeTransform.AntsField(_) | NativeTransform.FnirtField(_) | NativeTransform.FnirtCoefficients(_) | NativeTransform.AfniQwarp(_) => true
      case _ => false

  /** NIfTI containers are compared field by field: the writer uses float64 data and its own header layout. */
  private def fieldRoundTrip(path: Path, f: scalafim.transform.field.VectorFieldNifti) =
    VectorFieldNiftiCodec.encode(f).flatMap(VectorFieldNiftiCodec.decode).map: g =>
      assertEquals(g.raw.shape, f.raw.shape, rel(path))
      assertEquals((g.raw.intentCode, g.raw.qformCode, g.raw.sformCode), (f.raw.intentCode, f.raw.qformCode, f.raw.sformCode), rel(path))
      assertEquals(g.raw.intentP, f.raw.intentP, rel(path))
      assertEquals(g.raw.sformRowMajor, f.raw.sformRowMajor, rel(path))
      assertEquals(g.raw.qformRowMajor, f.raw.qformRowMajor, rel(path))
      (0L until f.raw.voxelCount).foreach(i => assertEquals(g.raw.value(i), f.raw.value(i)))
      g

  test("every oracle transform file round-trips value-exactly through its codec"):
    var checked = Map.empty[TransformFormat, Int]
    files.foreach: path =>
      TransformFiles.load(path) match
        case Left(_) => () // not a transform file (images, tables, manifests)
        case Right(loaded) =>
          val again: Either[TransformIoError, NativeTransform] = loaded.native match
            case NativeTransform.Itk(file, TransformFormat.ItkMatlab) =>
              ItkMatlabCodec.encode(file).flatMap(ItkMatlabCodec.decode).map(NativeTransform.Itk(_, TransformFormat.ItkMatlab))
            case NativeTransform.Itk(file, storage) =>
              ItkTextCodec.encode(file).flatMap(ItkTextCodec.decode).map(NativeTransform.Itk(_, storage))
            case NativeTransform.ItkHdf5(file) => Right(NativeTransform.ItkHdf5(file)) // read-only: jHDF cannot write the variable-length strings ITK needs
            case NativeTransform.X5(file)      => Right(NativeTransform.X5(file)) // read-only: jHDF cannot write the variable-length strings X5 needs
            case NativeTransform.Flirt(m)      => FlirtCodec.encode(m).flatMap(FlirtCodec.decode).map(NativeTransform.Flirt(_))
            case NativeTransform.Afni(s)       => Aff12Codec.encode(s).flatMap(Aff12Codec.decode).map(NativeTransform.Afni(_))
            case NativeTransform.Lta(f)        => LtaCodec.encode(f).flatMap(LtaCodec.decode).map(NativeTransform.Lta(_))
            case NativeTransform.Xfm(x)        => MniXfmCodec.encode(x).flatMap(MniXfmCodec.decode).map(NativeTransform.Xfm(_))
            case NativeTransform.RegisterDatFile(d) => RegisterDatCodec.encode(d).flatMap(RegisterDatCodec.decode).map(NativeTransform.RegisterDatFile(_))
            case NativeTransform.AntsField(f)         => fieldRoundTrip(path, f).map(NativeTransform.AntsField(_))
            case NativeTransform.FnirtField(f)        => fieldRoundTrip(path, f).map(NativeTransform.FnirtField(_))
            case NativeTransform.FnirtCoefficients(f) => fieldRoundTrip(path, f).map(NativeTransform.FnirtCoefficients(_))
            case NativeTransform.AfniQwarp(f)         => fieldRoundTrip(path, f).map(NativeTransform.AfniQwarp(_))
          again match
            case Left(error)  => fail(s"${rel(path)}: ${error.message}")
            case Right(value) => if !isField(value) then assertEquals(value, loaded.native, rel(path))
          checked = checked.updated(loaded.format, checked.getOrElse(loaded.format, 0) + 1)
    val expected = Set(
      TransformFormat.ItkText, TransformFormat.ItkMatlab, TransformFormat.ItkHdf5, TransformFormat.AntsDisplacementNifti,
      TransformFormat.FslFlirt, TransformFormat.FslFnirtField, TransformFormat.FslFnirtCoefficients, TransformFormat.AfniAff12,
      TransformFormat.FreeSurferLta, TransformFormat.FreeSurferXfm, TransformFormat.FreeSurferRegisterDat, TransformFormat.X5
    )
    assertEquals(expected -- checked.keySet, Set.empty[TransformFormat], s"formats without an oracle round trip; checked $checked")
