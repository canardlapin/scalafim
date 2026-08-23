package scalafim.image.io

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
import scalafim.image.*

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

  lazy val qform: Option[DMat] =
    native.qform.map(NiftiHeader.toDMat)

  inline def sformCode: Int =
    native.sformCode

  lazy val sform: Option[DMat] =
    native.sform.map(NiftiHeader.toDMat)

  lazy val byteOrder: ByteOrder =
    native.byteOrder match
      case NiftiByteOrder.LittleEndian => ByteOrder.LITTLE_ENDIAN
      case NiftiByteOrder.BigEndian    => ByteOrder.BIG_ENDIAN

  lazy val preferredAffine: Option[DMat] =
    native.sform.orElse(native.qform).map(NiftiHeader.toDMat)

  lazy val selectedAffine: DMat =
    NiftiHeader.toDMat(
      native.sform.orElse(native.qform).getOrElse(native.fallbackAffine)
    )

  lazy val space: NeuroSpace =
    val affine = selectedAffine
    val origin = Vector(affine(0, 3), affine(1, 3), affine(2, 3))
    NeuroSpace(
      dims,
      spacing = Some(Affine.voxelSizes(affine)),
      origin = Some(origin),
      trans = Some(affine)
    )

object NiftiHeader:
  private[io] def fromNative(
      header: image4s.nifti.NiftiHeader
  ): NiftiHeader =
    new NiftiHeader(header)

  private def toDMat(affine: ImageAffine[D3]): DMat =
    DMat.fromRowMajorOwned(4, 4, affine.rowMajor.toArray)

/** Native NIfTI I/O over image4s streaming and Ravel-owned destinations.
  *
  * Public entry points return typed provider errors. The package-scoped old
  * names remain only while ScalaFIM consumers migrate their dense type names;
  * they delegate to these methods without reordering or staging values.
  */
object Nifti:
  type Error = NiftiError
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
    NiftiError,
    DecodedNifti[SomeScalarVolume[Double]]
  ] =
    ImageNifti
      .readScaledDouble(path, options)
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
    NiftiError,
    DecodedNifti[SomeScalarSeries[Double]]
  ] =
    ImageNifti
      .readScaledDouble(path, options)
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
    ImageNifti.writeScalar(
      path,
      volume.asInstanceOf[
        Sampled[
          SampleSpace[Frame[D3], D3],
          Double,
          Continuous,
          Rank[3]
        ]
      ],
      options,
      extensions
    )

  def writeSeries(
      path: Path,
      series: SomeScalarSeries[Double],
      options: NiftiWriteOptions = Nifti.defaultSeriesWriteOptions,
      extensions: Vector[NiftiExtension] = Vector.empty
  ): Either[NiftiError, NiftiFiles[Path]] =
    ImageNifti.writeScalar(
      path,
      series.asInstanceOf[
        Sampled[
          SampleSpace[Frame[D3], D3],
          Double,
          Continuous,
          Rank[4]
        ]
      ],
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
  ): Either[NiftiError, SomeScalarVolume[Double]] =
    image.fold(
      d2 =>
        Left(
          NiftiError.Image(
            ImageError.SpatialDimensionMismatch(3, d2.spatialRank)
          )
        ),
      d3 =>
        d3.storageRank match
          case 3 =>
            d3.value
              .requireDataRank[3]
              .left
              .map(NiftiError.Image.apply)
              .map(SomeNeuroVolume.unsafeFromSampled)
          case 4
              if d3.value.nonSpatialAxes.size == 1 &&
                d3.value.nonSpatialAxes.values.head.extent == 1 =>
            d3.value
              .requireDataRank[4]
              .flatMap(_.selectNonSpatial(0, 0))
              .flatMap(_.requireDataRank[3])
              .left
              .map(NiftiError.Image.apply)
              .map(SomeNeuroVolume.unsafeFromSampled)
          case actual =>
            Left(
              NiftiError.Image(
                ImageError.StorageRankMismatch(3, actual)
              )
            )
    )

  private def seriesFrom(
      image: SomeSampled[Double, Continuous]
  ): Either[NiftiError, SomeScalarSeries[Double]] =
    image.fold(
      d2 =>
        Left(
          NiftiError.Image(
            ImageError.SpatialDimensionMismatch(3, d2.spatialRank)
          )
        ),
      d3 =>
        d3.value
          .requireDataRank[4]
          .left
          .map(NiftiError.Image.apply)
          .flatMap: ranked =>
            val axes = ranked.nonSpatialAxes.values
            if axes.size != 1 || axes.head.kind != AxisKind.Time then
              Left(
                NiftiError.Image(
                  ImageError.MissingNonSpatialAxisKind(AxisKind.Time)
                )
              )
            else
              NeuroSeries
                .fromSampled(ranked)
                .left
                .map(nativeImageError)
                .map(SomeNeuroSeries.eraseSpace)
    )

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
                    NeuroSpace.fromCanonical(ranked.sampleSpace).spatialSpace
                  GridSpec.fromSpace(space) -> ranked.data
        )

  private def nativeImageError(error: NativeImageError): NiftiError =
    error match
      case NativeImageError.Image(cause) =>
        NiftiError.Image(cause)
      case _ =>
        NiftiError.Image(
          ImageError.MissingNonSpatialAxisKind(AxisKind.Time)
        )

  private[scalafim] def readHeaderUnsafe(path: Path): NiftiHeader =
    readHeader(path).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

  private[scalafim] def readVol(path: Path): NeuroVol[Double] =
    readVolume(path)
      .map: decoded =>
        NeuroVol.fromPacked(
          AnyNeuroVolume.eraseSemantics(decoded.image)
        )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  private[scalafim] def readVec(path: Path): NeuroVec[Double] =
    readSeries(path)
      .map(decoded => NeuroVec.fromNative(decoded.image))
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  private[scalafim] def writeVol(
      path: Path,
      volume: NeuroVol[Double]
  ): Path =
    ImageNifti
      .writeScalar(path, ContinuousImageRefinement.volume(volume))
      .fold(
        error => throw new IllegalArgumentException(error.message),
        _.paths.head
      )

  private[scalafim] def writeVec(
      path: Path,
      series: NeuroVec[Double]
  ): Path =
    ImageNifti
      .writeScalar(
        path,
        ContinuousImageRefinement.series(series),
        defaultSeriesWriteOptions
      )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        _.paths.head
      )

  private[scalafim] def writeDenseVectorField[
      Role <: DenseVectorFieldKind
  ](
      path: Path,
      field: DenseVectorField[Role]
  ): Path =
    ImageNifti
      .writeScalar(
        path,
        field.sampled.asInstanceOf[
          Sampled[
            SampleSpace[Frame[D3], D3],
            Double,
            Continuous,
            Rank[4]
          ]
        ]
      )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        _.paths.head
      )
