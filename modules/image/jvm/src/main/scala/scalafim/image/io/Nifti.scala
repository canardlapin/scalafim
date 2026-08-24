package scalafim.image.io

import scalafim.image.SampleSpaces.*

import image4s.AxisKind
import image4s.Continuous
import image4s.ImageError
import image4s.SampleSpace
import image4s.Sampled
import image4s.SomeSampled
import image4s.geometry.Affine as ImageAffine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.nifti.DecodedNifti
import image4s.nifti.Nifti as ImageNifti
import image4s.nifti.NiftiByteOrder
import image4s.nifti.NiftiError
import image4s.nifti.NiftiExtension
import image4s.nifti.NiftiFiles
import image4s.nifti.NiftiIoLimits
import image4s.nifti.NiftiIoStrategy
import image4s.nifti.NiftiReadOptions
import image4s.nifti.NiftiScalarStored
import image4s.nifti.NiftiTemporalUnit
import image4s.nifti.NiftiUnknownTemporalUnitPolicy
import image4s.nifti.NiftiWriteOptions
import ravel.Rank
import ravel.AnyRank
import scalafim.image.*
import scalafim.image.NeuroAffineSyntax.*

import java.nio.ByteOrder
import java.nio.file.Path

/** Neuroimaging-facing view of the authoritative image4s NIfTI header.
  *
  * The provider header is retained directly. Compatibility field spellings
  * are derived views used by ScalaFIM's bounded block readers; there is no
  * second parser or separately owned header state.
  */
final class NiftiHeader private[io] (
    val native: image4s.nifti.NiftiHeader
):
  inline def dims: Vector[Int] =
    native.dimensions

  inline def pixdim: Vector[Double] =
    native.pixelDimensions

  inline def datatype: Int =
    native.datatype.code

  inline def bitpix: Int =
    native.datatype.bitsPerValue

  inline def voxOffset: Int =
    native.voxelOffset

  inline def slope: Double =
    native.slope

  inline def intercept: Double =
    native.intercept

  lazy val qoffset: Vector[Double] =
    native.qform
      .map: affine =>
        val values = affine.rowMajor
        Vector(values(3), values(7), values(11))
      .getOrElse(Vector.fill(3)(0.0))

  inline def qformCode: Int =
    native.qformCode

  lazy val qform: Option[ImageAffine[D3]] =
    native.qform

  inline def sformCode: Int =
    native.sformCode

  lazy val sform: Option[ImageAffine[D3]] =
    native.sform

  lazy val byteOrder: ByteOrder =
    native.byteOrder match
      case NiftiByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
      case NiftiByteOrder.BigEndian    => ByteOrder.BIG_ENDIAN

  lazy val preferredAffine: Option[ImageAffine[D3]] =
    native.sform.orElse(native.qform)

  lazy val selectedAffine: ImageAffine[D3] =
    native.sform.orElse(native.qform).getOrElse(native.fallbackAffine)

  lazy val space: SomeSampleSpace =
    val affine = selectedAffine
    val origin =
      Vector(
        affine.matrix(0, 3),
        affine.matrix(1, 3),
        affine.matrix(2, 3)
      )
    SampleSpaces(
      dims,
      spacing = Some(affine.neuroVoxelSizes),
      origin = Some(origin),
      affine = Some(affine)
    )

object NiftiHeader:
  private[io] def fromNative(
      header: image4s.nifti.NiftiHeader
  ): NiftiHeader =
    new NiftiHeader(header)

enum NiftiImageReadError derives CanEqual:
  case Provider(error: NiftiError)
  case Image(error: NeuroImageError)

  def message: String =
    this match
      case Provider(error) => error.message
      case Image(error)    => error.message

/** Native NIfTI I/O over image4s streaming and Ravel-owned destinations.
  *
  * Image reads retain provider and ScalaFIM refinement failures as typed
  * causes. Other entry points return provider errors directly. All paths
  * preserve the native image4s/Ravel representation without compatibility
  * staging.
  */
object Nifti:
  type Error = NiftiError
  type ImageReadError = NiftiImageReadError
  type ReadOptions = NiftiReadOptions
  type WriteOptions = NiftiWriteOptions
  type IoLimits = NiftiIoLimits
  type StoredScalar = NiftiScalarStored

  val ReadOptions = NiftiReadOptions
  val WriteOptions = NiftiWriteOptions
  val IoLimits = NiftiIoLimits

  def ioStrategy(path: Path): NiftiIoStrategy =
    ImageNifti.ioStrategy(path)

  def readHeader(
      path: Path,
      limits: NiftiIoLimits = NiftiIoLimits.default
  ): Either[NiftiError, NiftiHeader] =
    ImageNifti.readHeader(path, limits).map(NiftiHeader.fromNative)

  def readStored(
      path: Path,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[NiftiError, DecodedNifti[NiftiScalarStored]] =
    ImageNifti.readScalarStored(path, options)

  def readVolume(
      path: Path,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[
    NiftiImageReadError,
    DecodedNifti[SomeScalarVolume[Double]]
  ] =
    ImageNifti
      .readScaledDouble(path, options)
      .left
      .map(NiftiImageReadError.Provider.apply)
      .flatMap: decoded =>
        volumeFrom(decoded.image).map: volume =>
          DecodedNifti(
            volume,
            decoded.header,
            decoded.affineSelection
          )

  def readSeries(
      path: Path,
      options: NiftiReadOptions = Nifti.defaultSeriesReadOptions
  ): Either[
    NiftiImageReadError,
    DecodedNifti[SomeScalarSeries[Double]]
  ] =
    ImageNifti
      .readScaledDouble(path, options)
      .left
      .map(NiftiImageReadError.Provider.apply)
      .flatMap: decoded =>
        seriesFrom(decoded.image).map: series =>
          DecodedNifti(
            series,
            decoded.header,
            decoded.affineSelection
          )

  def writeVolume(
      path: Path,
      volume: SomeScalarVolume[Double],
      options: NiftiWriteOptions = NiftiWriteOptions.default,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[Path]] =
    writeScalar(
      path,
      volume.sampled,
      options,
      extensions
    )

  def writeSeries(
      path: Path,
      series: SomeScalarSeries[Double],
      options: NiftiWriteOptions = Nifti.defaultSeriesWriteOptions,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[Path]] =
    writeScalar(
      path,
      series.sampled,
      options,
      extensions
    )

  def readDisplacementField(
      path: Path,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[NiftiError, DisplacementField] =
    readDenseVectorFieldData(path, options).map: (grid, data) =>
      DenseVectorField.displacement(grid, data)

  def readSourceCoordinateField(
      path: Path,
      options: NiftiReadOptions = NiftiReadOptions.default
  ): Either[NiftiError, SourceCoordinateField] =
    readDenseVectorFieldData(path, options).map: (grid, data) =>
      DenseVectorField.sourceCoordinates(grid, data)

  private val defaultSeriesReadOptions: NiftiReadOptions =
    NiftiReadOptions.default.copy(
      unknownTemporalUnit = NiftiUnknownTemporalUnitPolicy.AssumeSeconds
    )

  private val defaultSeriesWriteOptions: NiftiWriteOptions =
    NiftiWriteOptions
      .create(
        datatype = image4s.nifti.NiftiDatatype.Float64,
        slope = 1.0,
        intercept = 0.0,
        nonSpatialPixelDimensions = Vector(1.0),
        temporalUnit = NiftiTemporalUnit.Second
      )
      .fold(
        error => throw new IllegalStateException(error.message),
        identity
      )

  private def volumeFrom(
      image: SomeSampled[Double, Continuous]
  ): Either[NiftiImageReadError, SomeScalarVolume[Double]] =
    image.fold(
      d2 =>
        Left(
          NiftiImageReadError.Provider(
            NiftiError.Image(
              ImageError.SpatialDimensionMismatch(3, d2.spatialRank)
            )
          )
        ),
      d3 =>
        d3.storageRank match
          case 3 =>
            d3.value
              .requireDataRank[3]
              .flatMap(persistDecoded)
              .left
              .map(providerImageError)
              .map(SomeNeuroVolume.unsafeFromSampled)
          case 4
              if d3.value.nonSpatialAxes.size == 1 &&
                d3.value.nonSpatialAxes.values.head.extent == 1 =>
            d3.value
              .requireDataRank[4]
              .flatMap(_.selectNonSpatial(0, 0))
              .flatMap(_.requireDataRank[3])
              .flatMap(persistDecoded)
              .left
              .map(providerImageError)
              .map(SomeNeuroVolume.unsafeFromSampled)
          case actual =>
            Left(
              NiftiImageReadError.Provider(
                NiftiError.Image(
                  ImageError.StorageRankMismatch(3, actual)
                )
              )
            )
    )

  private def seriesFrom(
      image: SomeSampled[Double, Continuous]
  ): Either[NiftiImageReadError, SomeScalarSeries[Double]] =
    image.fold(
      d2 =>
        Left(
          NiftiImageReadError.Provider(
            NiftiError.Image(
              ImageError.SpatialDimensionMismatch(3, d2.spatialRank)
            )
          )
        ),
      d3 =>
        d3.value
          .requireDataRank[4]
          .left
          .map(providerImageError)
          .flatMap: ranked =>
            val axes = ranked.nonSpatialAxes.values
            if axes.size != 1 || axes.head.kind != AxisKind.Time then
              Left(
                NiftiImageReadError.Provider(
                  NiftiError.Image(
                    ImageError.MissingNonSpatialAxisKind(AxisKind.Time)
                  )
                )
              )
            else
              persistDecoded(ranked)
                .left
                .map(providerImageError)
                .flatMap: persistent =>
                  NeuroSeries
                    .fromSampled(persistent)
                    .left
                    .map(NiftiImageReadError.Image.apply)
                    .map(SomeNeuroSeries.eraseSpace)
    )

  /** Rebind an external decode to deterministic persistent geometry without
    * copying its Ravel data owner.
    */
  private def persistDecoded[A, R <: AnyRank](
      sampled: Sampled[
        ? <: SampleSpace[?, D3],
        A,
        Continuous,
        R
      ]
  )(using
      image4s.ValueSemantics[A, Continuous]
  ): Either[
    ImageError,
    Sampled[? <: SampleSpace[?, D3], A, Continuous, R]
  ] =
    for
      persistent <- SampleSpaces
        .persistentD3(sampled.sampleSpace)
        .left
        .map(ImageError.Geometry.apply)
      rebound <- Sampled
        .continuous(persistent, sampled.data, sampled.metadata)
    yield rebound

  private def readDenseVectorFieldData(
      path: Path,
      options: NiftiReadOptions
  ): Either[
    NiftiError,
    (GridSpec, ravel.NDArray[Double, Rank[4]])
  ] =
    ImageNifti
      .readScaledDouble(path, options)
      .flatMap: decoded =>
        decoded.image.fold(
          d2 =>
            Left(
              NiftiError.Image(
                ImageError.SpatialDimensionMismatch(3, d2.spatialRank)
              )
            ),
          d3 =>
            if d3.value.nonSpatialAxes.size != 1 ||
              d3.value.nonSpatialAxes.values.head.extent != 3
            then
              Left(
                NiftiError.Image(
                  ImageError.SampledShapeMismatch(
                    d3.value.grid.shape :+ 3,
                    d3.value.logicalShape
                  )
                )
              )
            else
              d3.value
                .requireDataRank[4]
                .left
                .map(NiftiError.Image.apply)
                .map: ranked =>
                  val space =
                    SampleSpaces.fromCanonical(ranked.sampleSpace).spatialSpace
                  GridSpec.fromSpace(space) -> ranked.data
        )

  private def providerImageError(error: ImageError): NiftiImageReadError =
    NiftiImageReadError.Provider(NiftiError.Image(error))

  private[scalafim] def readHeaderUnsafe(path: Path): NiftiHeader =
    readHeader(path).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

  private[scalafim] def writeDenseVectorField[
      Role <: DenseVectorFieldKind
  ](
      path: Path,
      field: DenseVectorField[Role]
  ): Path =
    writeScalar(
      path,
      field.sampled
    )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        files =>
          files match
            case NiftiFiles.SingleFile(value) => value
            case NiftiFiles.PairFile(header, _) => header
      )

  /** Capture the existential sample-space owner before entering image4s' NIfTI
    * writer.
    *
    * `Sampled` is immutable, and `S` is already bounded by a D3 sample space.
    * Scala cannot retain the relationship between the two nested existential
    * owners when inferring image4s' separate `F` and `S` parameters, so the
    * single erased cast below restores that relationship at this format
    * boundary. No object or storage is copied or widened outside the call.
    */
  private def writeScalar[
      S <: SampleSpace[?, D3],
      R <: AnyRank
  ](
      path: Path,
      image: Sampled[S, Double, Continuous, R],
      options: NiftiWriteOptions = NiftiWriteOptions.default,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[Path]] =
    val captured = image.asInstanceOf[
      Sampled[
        SampleSpace[Frame[D3], D3],
        Double,
        Continuous,
        R
      ]
    ]
    ImageNifti.writeScalar(
      path,
      captured,
      options,
      extensions
    )
