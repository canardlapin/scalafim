package scalafim.transform

import scalafim.transform.afni.{Aff12Codec, Aff12Series}
import scalafim.transform.field.{VectorFieldNifti, VectorFieldNiftiCodec}
import scalafim.transform.freesurfer.{LtaCodec, LtaFile, MniXfm, MniXfmCodec, RegisterDat, RegisterDatCodec}
import scalafim.transform.fsl.{FlirtCodec, FlirtMatrix}
import scalafim.transform.itk.{ItkHdf5File, ItkMatlabCodec, ItkTextCodec, ItkTransformFile}
import scalafim.transform.x5.X5File

/** A decoded toolkit transform file, before interpretation: exactly one native model per supported format. */
enum NativeTransform:
  case Itk(file: ItkTransformFile, storage: TransformFormat)
  case ItkHdf5(file: ItkHdf5File)
  case Flirt(matrix: FlirtMatrix)
  case FnirtField(field: VectorFieldNifti)
  case FnirtCoefficients(field: VectorFieldNifti)
  case Afni(series: Aff12Series)
  case AfniQwarp(field: VectorFieldNifti)
  case AntsField(field: VectorFieldNifti)
  case Lta(file: LtaFile)
  case Xfm(xfm: MniXfm)
  case RegisterDatFile(dat: RegisterDat)
  case X5(file: X5File)

  def format: TransformFormat =
    this match
      case Itk(_, f)            => f
      case ItkHdf5(_)           => TransformFormat.ItkHdf5
      case Flirt(_)             => TransformFormat.FslFlirt
      case FnirtField(_)        => TransformFormat.FslFnirtField
      case FnirtCoefficients(_) => TransformFormat.FslFnirtCoefficients
      case Afni(_)              => TransformFormat.AfniAff12
      case AfniQwarp(_)         => TransformFormat.AfniQwarp
      case AntsField(_)         => TransformFormat.AntsDisplacementNifti
      case Lta(_)               => TransformFormat.FreeSurferLta
      case Xfm(_)               => TransformFormat.FreeSurferXfm
      case RegisterDatFile(_)   => TransformFormat.FreeSurferRegisterDat
      case X5(_)                => TransformFormat.X5

/** Platform-neutral decoding entry point. HDF5 containers (ITK composites, X5) are decoded by the JVM container readers
  * and enter as [[NativeTransform.ItkHdf5]] / [[NativeTransform.X5]] values.
  */
object Transforms:
  /** Decode uncompressed content of a known format. */
  def decode(source: TransformSource, format: TransformFormat): Either[TransformIoError, NativeTransform] =
    format match
      case TransformFormat.ItkText               => ItkTextCodec.decode(source).map(NativeTransform.Itk(_, format))
      case TransformFormat.ItkMatlab             => ItkMatlabCodec.decode(source).map(NativeTransform.Itk(_, format))
      case TransformFormat.FslFlirt              => FlirtCodec.decode(source).map(NativeTransform.Flirt(_))
      case TransformFormat.FslFnirtField         => VectorFieldNiftiCodec.decode(source).map(NativeTransform.FnirtField(_))
      case TransformFormat.FslFnirtCoefficients  => VectorFieldNiftiCodec.decode(source).map(NativeTransform.FnirtCoefficients(_))
      case TransformFormat.AfniAff12             => Aff12Codec.decode(source).map(NativeTransform.Afni(_))
      case TransformFormat.AfniQwarp             => VectorFieldNiftiCodec.decode(source).map(NativeTransform.AfniQwarp(_))
      case TransformFormat.AntsDisplacementNifti => VectorFieldNiftiCodec.decode(source).map(NativeTransform.AntsField(_))
      case TransformFormat.FreeSurferLta         => LtaCodec.decode(source).map(NativeTransform.Lta(_))
      case TransformFormat.FreeSurferXfm         => MniXfmCodec.decode(source).map(NativeTransform.Xfm(_))
      case TransformFormat.FreeSurferRegisterDat => RegisterDatCodec.decode(source).map(NativeTransform.RegisterDatFile(_))
      case TransformFormat.ItkHdf5 | TransformFormat.X5 =>
        Left(TransformIoError.WrongSource(format, "an HDF5 container decoded by the JVM reader"))

  /** Detect the format from content (file name only breaks ties), then decode. */
  def decode(source: TransformSource, fileName: Option[String]): Either[TransformIoError, NativeTransform] =
    TransformDetection.detect(source, fileName).flatMap(decode(source, _))
