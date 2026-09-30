package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Point}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{FrameCatalog, FreeSurferVolumeGeometry, FslVolumeGeometry, ToolCoordinates, WorldSpace}
import scalafim.transform.afni.{Aff12Codec, Aff12Expression, Aff12Interpretation, AfniCardinal}
import scalafim.transform.field.{DenseContext, FnirtCoefficientContext, FnirtCoefficientInterpretation, FnirtContext, FnirtDefinition, FnirtFieldInterpretation, LpsDisplacementInterpretation}
import scalafim.transform.freesurfer.{LtaCodec, LtaExpression, LtaGeometry, LtaInterpretation, MniXfm, MniXfmCodec, MniXfmInterpretation, RegisterDat, RegisterDatCodec, RegisterDatInterpretation, VolGeom}
import scalafim.transform.fsl.{FlirtCodec, FlirtExpression, FlirtInterpretation}
import scalafim.transform.itk.{ItkHdf5Interpretation, ItkLinearExpression, ItkLinearInterpretation, ItkMatlabCodec, ItkTextCodec}
import scalafim.transform.nifti.NiftiWriter
import scalafim.transform.x5.{X5Domain, X5File, X5Interpretation, X5Node}

/** Geometry a conversion may need, supplied at runtime. Anything missing for the requested formats is a typed
  * [[TransformError.MissingContext]], never a default.
  */
final case class ConversionContext(
    fsl: Option[ConversionContext.FslPair] = None,
    tkreg: Option[ConversionContext.TkRegPair] = None,
    lta: Option[ConversionContext.LtaPair] = None,
    afni: CardinalCorrection = CardinalCorrection.Off,
    fnirtDefinition: Option[FnirtDefinition] = None,
    lattice: Option[ConversionContext.Lattice] = None,
    boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject
)

object ConversionContext:
  /** FSL geometry of the source (input) and reference volumes. */
  final case class FslPair(source: FslVolumeGeometry, reference: FslVolumeGeometry)
  /** FreeSurfer geometry of the movable and target volumes (register.dat). */
  final case class TkRegPair(movable: FreeSurferVolumeGeometry, target: FreeSurferVolumeGeometry)
  /** Volume geometries written into an LTA. */
  final case class LtaPair(source: VolGeom, destination: VolGeom)
  /** Target-space grid on which a dense output is sampled: dims and voxel-to-RAS. */
  final case class Lattice(dims: Vector[Int], voxelToRas: Affine[D3])

  val empty: ConversionContext = ConversionContext()

/** Conversion between toolkit formats: decode, interpret with the decode context, express with the encode context,
  * encode. Linear transforms convert exactly between all linear formats; dense maps (including evaluated FNIRT
  * coefficients) convert to dense formats only on a sampling lattice; dense to linear formats and anything to FNIRT
  * coefficients (which would need fitting) are refused.
  */
object Conversion:
  /** Convert a decoded file. HDF5 inputs (ITK composites, X5) are decoded by the JVM container readers first. */
  def convert(native: NativeTransform, to: TransformFormat, decode: ConversionContext, encode: ConversionContext): Either[TransformError, EncodedTransform] =
    // Conversion is frame-agnostic: the endpoints are fresh declared spaces that exist only for this call.
    val moving: Frame[D3] = FrameCatalog.frame(WorldSpace.declare("conversion source").fold(error => throw new IllegalStateException(error.message), identity))
    val fixed: Frame[D3] = FrameCatalog.frame(WorldSpace.declare("conversion target").fold(error => throw new IllegalStateException(error.message), identity))
    val frames = Frames[moving.type, fixed.type](moving, fixed)
    interpret(native, frames, decode).flatMap(transform => express(transform, to, frames, encode))

  /** The content of an encoded transform: text or bytes for file formats, or an in-memory X5 model. There is no X5 or
    * ITK HDF5 file writer: the JVM HDF5 library cannot write the variable-length strings both formats require.
    */
  enum EncodedTransform:
    case Source(format: TransformFormat, source: TransformSource)
    case Hdf5X5(file: X5File)

  private def interpret[S <: Frame[D3], T <: Frame[D3]](native: NativeTransform, frames: Frames[S, T], ctx: ConversionContext): Either[TransformError, WorldTransform[S, T]] =
    def need[A](value: Option[A], format: TransformFormat, what: String) = value.toRight(TransformError.MissingContext(format, what))
    native match
      case NativeTransform.Itk(file, format) => ItkLinearInterpretation.interpretWith(file, frames, AssetRef("input", None), format).map(_.composed)
      case NativeTransform.ItkHdf5(file)     => ItkHdf5Interpretation.interpret(file, DenseContext(frames, ctx.boundary)).map(_.composed)
      case NativeTransform.X5(file)          => X5Interpretation.interpret(file, DenseContext(frames, ctx.boundary)).map(_.composed)
      case NativeTransform.Flirt(m) =>
        need(ctx.fsl, TransformFormat.FslFlirt, "source and reference FSL geometry").flatMap(p =>
          FlirtInterpretation.interpret(m, FslGrids(frames.source, p.source, frames.target, p.reference)))
      case NativeTransform.Afni(series) =>
        Aff12Interpretation.interpret(series, AfniContext(frames, ctx.afni)).flatMap: s =>
          if s.transforms.size == 1 then Right(s.transforms.head)
          else Left(TransformError.Invalid(s"the AFNI file holds ${s.transforms.size} affines; convert a series element by element"))
      case NativeTransform.Lta(file)          => LtaInterpretation.interpret(file, frames)
      case NativeTransform.Xfm(xfm)           => MniXfmInterpretation.interpret(xfm, frames)
      case NativeTransform.RegisterDatFile(d) =>
        need(ctx.tkreg, TransformFormat.FreeSurferRegisterDat, "movable and target FreeSurfer geometry").flatMap(p =>
          RegisterDatInterpretation.interpret(d, TkRegGrids(frames.source, p.movable, frames.target, p.target)))
      case NativeTransform.AntsField(f) => LpsDisplacementInterpretation.Ants.interpret(f, DenseContext(frames, ctx.boundary))
      case NativeTransform.AfniQwarp(f) => LpsDisplacementInterpretation.AfniQwarp.interpret(f, DenseContext(frames, ctx.boundary))
      case NativeTransform.FnirtField(f) =>
        need(ctx.fsl, TransformFormat.FslFnirtField, "source FSL geometry").flatMap(p =>
          FnirtFieldInterpretation.interpret(f, FnirtContext(frames, p.source, ctx.fnirtDefinition, ctx.boundary)))
      case NativeTransform.FnirtCoefficients(file) =>
        need(ctx.fsl, TransformFormat.FslFnirtCoefficients, "source and reference FSL geometry").flatMap(p =>
          FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(FslGrids(frames.source, p.source, frames.target, p.reference), ctx.boundary)))

  private def express[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform[S, T], to: TransformFormat, frames: Frames[S, T], ctx: ConversionContext): Either[TransformError, EncodedTransform] =
    def need[A](value: Option[A], what: String) = value.toRight(TransformError.MissingContext(to, what))
    def io(e: Either[TransformIoError, TransformSource]) = e.left.map(TransformError.Io(_)).map(EncodedTransform.Source(to, _))
    transform match
      case linear: WorldTransform.Linear[S, T] @unchecked =>
        to match
          case TransformFormat.ItkText   => ItkLinearExpression.express(linear, frames).flatMap(f => io(ItkTextCodec.encode(f)))
          case TransformFormat.ItkMatlab => ItkLinearExpression.express(linear, frames).flatMap(f => io(ItkMatlabCodec.encode(f)))
          case TransformFormat.ItkHdf5 => itkHdf5Unsupported("affine")
          case TransformFormat.FslFlirt =>
            need(ctx.fsl, "source and reference FSL geometry").flatMap(p =>
              FlirtExpression.express(linear, FslGrids(frames.source, p.source, frames.target, p.reference)).flatMap(m => io(FlirtCodec.encode(m))))
          case TransformFormat.AfniAff12 =>
            Aff12Expression.express(LinearSeries(Vector(linear)), AfniContext(frames, ctx.afni)).flatMap(s => io(Aff12Codec.encode(s)))
          case TransformFormat.FreeSurferLta =>
            need(ctx.lta, "source and destination volume geometry").flatMap(p =>
              LtaExpression.express(linear, LtaGeometry(frames.source, p.source, frames.target, p.destination)).flatMap(f => io(LtaCodec.encode(f))))
          case TransformFormat.FreeSurferXfm =>
            io(MniXfmCodec.encode(MniXfm(linear.framed.operator.inverse.rowMajor.take(12), Vector("% written by ScalaFIM"))))
          case TransformFormat.FreeSurferRegisterDat =>
            need(ctx.tkreg, "movable and target FreeSurfer geometry").flatMap: p =>
              // R = Kmov * inv(Tmov) * pull * Tref * inv(Kref)
              chain(p.target.torig.inverse, p.target.norig, linear.framed.operator, p.movable.norig.inverse, p.movable.torig)
                .flatMap(r => io(RegisterDatCodec.encode(RegisterDat("scalafim", p.movable.voxelSizes(0), p.movable.voxelSizes(2), 0.15, r.rowMajor, Vector("round")))))
          case TransformFormat.X5 =>
            Right(EncodedTransform.Hdf5X5(X5File(Vector(X5Node(0, "linear", Some("affine"), Some("matrix"), Vector(4, 4), IArray.from(linear.framed.operator.rowMajor), None)), Vector.empty)))
          case dense @ (TransformFormat.AntsDisplacementNifti | TransformFormat.AfniQwarp | TransformFormat.FslFnirtField) =>
            sampled(transform, dense, frames, ctx)
          case TransformFormat.FslFnirtCoefficients =>
            Left(TransformError.UnsupportedConversion("affine", to, "no FNIRT coefficient parameterisation can be recovered; fitting is deferred"))
      case _ =>
        to match
          case TransformFormat.AntsDisplacementNifti | TransformFormat.AfniQwarp | TransformFormat.FslFnirtField | TransformFormat.X5 =>
            sampled(transform, to, frames, ctx)
          case TransformFormat.ItkHdf5 => itkHdf5Unsupported("dense map")
          case TransformFormat.FslFnirtCoefficients =>
            Left(TransformError.UnsupportedConversion("dense map", to, "fitting FNIRT coefficients needs a FittingPolicy; deferred"))
          case other =>
            Left(TransformError.UnsupportedConversion("dense map", other, "a linear format cannot represent a nonlinear map"))

  /** Needs-policy cell: sample the pullback on the requested lattice and write it in the target field encoding. */
  private def sampled[S <: Frame[D3], T <: Frame[D3]](transform: WorldTransform[S, T], to: TransformFormat, frames: Frames[S, T], ctx: ConversionContext): Either[TransformError, EncodedTransform] =
    for
      lattice <- ctx.lattice.toRight(TransformError.MissingContext(to, "a sampling lattice (dims and voxel-to-RAS) for the dense output"))
      dims = lattice.dims
      m = lattice.voxelToRas.rowMajor
      count = dims.product
      sources <- (0 until count).toVector.foldLeft[Either[TransformError, Array[Double]]](Right(new Array[Double](3 * count))): (acc, flat) =>
        acc.flatMap: out =>
          val (x, y, z) = (flat % dims(0), (flat / dims(0)) % dims(1), flat / (dims(0) * dims(1)))
          val world = Vector.tabulate(3)(r => m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3))
          Point.fromVector(frames.target, world).left.map(TransformError.Geometry(_)).flatMap: p =>
            transform.pullPoint(p.asInstanceOf[Point[T, D3]]).map: src =>
              (0 until 3).foreach(c => out(3 * flat + c) = src.coordinates(c))
              out
      _ <- to match
        case TransformFormat.AntsDisplacementNifti | TransformFormat.AfniQwarp if !orthogonal(lattice) =>
          Left(TransformError.UnsupportedConversion("dense map", to, "ITK/ANTs and AFNI fields need orthogonal lattice axes (a rotated, scaled grid); this lattice is sheared"))
        case TransformFormat.AfniQwarp if AfniCardinal.obliquity(lattice.voxelToRas).nonEmpty =>
          Left(TransformError.UnsupportedConversion("dense map", to, "AFNI places a warp on an oblique grid's cardinalised axes, which is not yet qualified; sample on a cardinal lattice"))
        case _ => Right(())
      encoded <- to match
        case TransformFormat.AntsDisplacementNifti | TransformFormat.AfniQwarp =>
          // (x,y,z,1,3), components slowest, LPS displacements
          val values = Array.tabulate(3 * count): i =>
            val (c, flat) = (i / count, i % count)
            val world = worldAt(m, dims, flat, c)
            val d = sources(3 * flat + c) - world
            if c < 2 then -d else d
          val intent = if to == TransformFormat.AntsDisplacementNifti then 1007 else 1006
          Right(EncodedTransform.Source(to, TransformSource.Binary(NiftiWriter.write(header(Vector(dims(0), dims(1), dims(2), 1, 3), lattice, intent), values))))
        case TransformFormat.FslFnirtField =>
          for
            pair <- ctx.fsl.toRight(TransformError.MissingContext(to, "source FSL geometry"))
            definition = ctx.fnirtDefinition.getOrElse(FnirtDefinition.Relative)
            srcW2F = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(pair.source)).rowMajor
            // the written field's own lattice is its FSL reference: readers take FSL coordinates from its header
            latticeFsl <- FslVolumeGeometry
              .fromHeader(dims, latticeSpacing(lattice), 0, None, 1, Some(lattice.voxelToRas))
              .left
              .map(TransformError.Space(_))
            refV2F = latticeFsl.voxelToFsl.rowMajor
          yield
            val values = Array.tabulate(3 * count): i =>
              val (c, flat) = (i / count, i % count)
              val s = Vector.tabulate(3)(k => sources(3 * flat + k))
              val srcFsl = srcW2F(4 * c) * s(0) + srcW2F(4 * c + 1) * s(1) + srcW2F(4 * c + 2) * s(2) + srcW2F(4 * c + 3)
              definition match
                case FnirtDefinition.Absolute => srcFsl
                case FnirtDefinition.Relative =>
                  val (x, y, z) = (flat % dims(0), (flat / dims(0)) % dims(1), flat / (dims(0) * dims(1)))
                  srcFsl - (refV2F(4 * c) * x + refV2F(4 * c + 1) * y + refV2F(4 * c + 2) * z + refV2F(4 * c + 3))
            EncodedTransform.Source(to, TransformSource.Binary(NiftiWriter.write(header(Vector(dims(0), dims(1), dims(2), 3), lattice, 2006), values)))
        case TransformFormat.X5 =>
          // C order (x slowest, component fastest), RAS displacements
          val values = Array.tabulate(3 * count): i =>
            val c = i % 3
            val cell = i / 3
            val (x, y, z) = (cell / (dims(1) * dims(2)), (cell / dims(2)) % dims(1), cell % dims(2))
            val flat = x + dims(0) * (y + dims(1) * z)
            sources(3 * flat + c) - worldAt(m, dims, flat, c)
          Right(EncodedTransform.Hdf5X5(X5File(Vector(X5Node(0, "nonlinear", Some("densefield"), Some("displacements"), dims :+ 3, IArray.unsafeFromArray(values), Some(X5Domain(grid = true, dims, m)))), Vector.empty)))
        case other =>
          Left(TransformError.UnsupportedConversion("dense map", other, "not a dense field format"))
    yield encoded

  private def worldAt(m: Vector[Double], dims: Vector[Int], flat: Int, row: Int): Double =
    val (x, y, z) = (flat % dims(0), (flat / dims(0)) % dims(1), flat / (dims(0) * dims(1)))
    m(4 * row) * x + m(4 * row + 1) * y + m(4 * row + 2) * z + m(4 * row + 3)

  /** Lattice axes (voxel-to-RAS columns) mutually orthogonal to within 1e-6 of their lengths' product. */
  private def orthogonal(lattice: ConversionContext.Lattice): Boolean =
    val m = lattice.voxelToRas.rowMajor
    val columns = Vector.tabulate(3)(c => Vector(m(c), m(4 + c), m(8 + c)))
    def dot(a: Vector[Double], b: Vector[Double]) = a.zip(b).map(_ * _).sum
    val norms = columns.map(c => math.sqrt(dot(c, c)))
    Vector((0, 1), (0, 2), (1, 2)).forall((i, j) => math.abs(dot(columns(i), columns(j))) <= 1e-6 * norms(i) * norms(j))

  private def latticeSpacing(lattice: ConversionContext.Lattice): Vector[Double] =
    val m = lattice.voxelToRas.rowMajor
    Vector.tabulate(3)(c => math.sqrt(m(c) * m(c) + m(4 + c) * m(4 + c) + m(8 + c) * m(8 + c)))

  private def header(dims: Vector[Int], lattice: ConversionContext.Lattice, intent: Int): NiftiWriter.Header =
    NiftiWriter.Header(dims, latticeSpacing(lattice) ++ Vector.fill(dims.size - 3)(1.0), intentCode = intent, sformCode = 1, srow = lattice.voxelToRas.rowMajor.take(12))

  /** Writer acceptance (P5.03) showed ITK rejects jHDF's fixed-length strings: ITK HDF5 needs variable-length ones. */
  private def itkHdf5Unsupported(what: String): Either[TransformError, EncodedTransform] =
    Left(TransformError.UnsupportedConversion(what, TransformFormat.ItkHdf5, "the JVM HDF5 writer cannot produce the variable-length strings ITK requires; write ITK text, MATLAB v4, or a NIfTI displacement field instead"))

  private def chain(steps: Affine[D3]*): Either[TransformError, Affine[D3]] =
    steps.tail.foldLeft[Either[TransformError, Affine[D3]]](Right(steps.head))((acc, next) => acc.flatMap(_.andThen(next).left.map(TransformError.Geometry(_))))

