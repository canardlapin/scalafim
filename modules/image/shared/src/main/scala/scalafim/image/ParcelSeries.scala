package scalafim.image

import image4s.Axis
import image4s.AxisKind
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomainError
import locus4s.Index
import locus4s.Region
import locus4s.SpaceMismatch
import locus4s.data.Field as LocusField
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import ravel.map
import ravel.select
import spire.algebra.Field as ScalarField

enum EmptyParcelPolicy[+A]:
  case Reject
  case Fill[A](value: A) extends EmptyParcelPolicy[A]

enum ParcelSeriesError:
  case WrongGrid(error: GridDomainError)
  case WrongSupport(error: SpaceMismatch)
  case WrongParcelOwner(error: SpaceMismatch)
  case DataShapeMismatch(expected: Vector[Int], actual: Vector[Int])
  case ExpectedTimeAxis(actual: AxisKind)
  case TimeIndexOutOfBounds(index: Int, extent: Int)
  case EmptyParcel(parcelOrdinal: Int)
  case ParcellationMismatch
  case TimeAxisMismatch(left: image4s.AxisRecord, right: image4s.AxisRecord)
  case InvalidImage(error: image4s.ImageError)
  case InvalidNeuroImage(error: NeuroImageError)

  def message: String =
    this match
      case WrongGrid(error) => error.message
      case WrongSupport(error) => error.message
      case WrongParcelOwner(error) => error.message
      case DataShapeMismatch(expected, actual) =>
        s"parcel series requires Ravel shape $expected, found $actual"
      case ExpectedTimeAxis(actual) =>
        s"parcel series requires a Time axis, found $actual"
      case TimeIndexOutOfBounds(index, extent) =>
        s"time index $index is outside [0, $extent)"
      case EmptyParcel(parcel) =>
        s"parcel $parcel has no voxels in the requested support"
      case ParcellationMismatch =>
        "parcel series operands do not share the same exact parcellation"
      case TimeAxisMismatch(left, right) =>
        s"parcel series time axes differ: $left versus $right"
      case InvalidImage(error) => error.message
      case InvalidNeuroImage(error) => error.message

type SomeParcelSeries[A, Sem] =
  ParcelSeries[? <: Frame[D3], ?, ?, A, Sem]

type SomeScalarParcelSeries[A] =
  SomeParcelSeries[A, image4s.Continuous]

/** Parcel-major samples retaining one exact parcellation and one Ravel array.
  *
  * Storage shape is `(parcel, time)`. Ravel's last-axis-fastest layout makes
  * each parcel's complete time series a contiguous zero-copy row.
  */
final class ParcelSeries[
    F <: Frame[D3],
    S,
    P,
    A,
    Sem
] private (
    val parcellation: VolumeParcellation[F, S, P, ?],
    val timeAxis: Axis,
    val data: NDArray[A, Rank[2]],
    val metadata: ImageMetadata,
    private val storedSemantics: ValueSemantics[A, Sem]
):
  def nParcels: Int =
    parcellation.parcels.size

  def nTime: Int =
    timeAxis.extent

  inline def apply(parcel: Index[P], time: Int): A =
    data(parcel.ordinal, time)

  /** Contiguous zero-copy time series for one parcel. */
  def seriesAt(parcel: Index[P]): NDArray[A, Rank[1]] =
    data.select(0, parcel.ordinal)

  def fieldAt(time: Int): Either[ParcelSeriesError, LocusField[P, A]] =
    if time < 0 || time >= nTime then
      Left(ParcelSeriesError.TimeIndexOutOfBounds(time, nTime))
    else
      Right(
        LocusField.view(parcellation.parcels): parcel =>
          data(parcel.ordinal, time)
      )

  def mapValues[B, OutSem](
      f: A => B
  )(using
      DType[B],
      ValueSemantics[B, OutSem]
  ): ParcelSeries[F, S, P, B, OutSem] =
    ParcelSeries.unsafe(
      parcellation,
      timeAxis,
      data.map(f),
      metadata
    )

  /** Explicitly scatter parcel values back to the dense voxel grid.
    *
    * Unassigned voxels require a caller-provided value; no numeric zero or
    * categorical code is inferred as background.
    */
  def toDense(
      unassigned: A,
      outputMetadata: ImageMetadata = metadata
  ): Either[ParcelSeriesError, SomeNeuroSeries[A, Sem]] =
    given DType[A] = data.dtype
    given ValueSemantics[A, Sem] = storedSemantics
    val shape = parcellation.domain.grid.shape
    val dense =
      NDArray.build[A, Rank[4]](
        Shape(shape(0), shape(1), shape(2), nTime)
      ): output =>
        var voxelOrdinal = 0
        while voxelOrdinal < parcellation.domain.space.size do
          val voxel =
            parcellation.domain.space.indexAtValidatedOrdinal(voxelOrdinal)
          val parcel = parcellation.parcelAt(voxel)
          var time = 0
          while time < nTime do
            val value =
              parcel.fold(unassigned)(index => data(index.ordinal, time))
            output.writeLinear(voxelOrdinal * nTime + time, value)
            time += 1
          voxelOrdinal += 1

    NonSpatialAxes
      .from(Vector(timeAxis))
      .left
      .map(ParcelSeriesError.InvalidImage.apply)
      .flatMap: axes =>
        val sampleSpace = SampleSpace.create(parcellation.domain.grid, axes)
        NeuroSeries
          .fromRavel(sampleSpace, dense, outputMetadata)
          .left
          .map(ParcelSeriesError.InvalidNeuroImage.apply)
          .map(SomeNeuroSeries.eraseSpace)

object ParcelSeries:
  def create[F <: Frame[D3], S, P, A, Sem](
      parcellation: VolumeParcellation[F, S, P, ?],
      timeAxis: Axis,
      data: NDArray[A, Rank[2]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      semantics: ValueSemantics[A, Sem]
  ): Either[ParcelSeriesError, ParcelSeries[F, S, P, A, Sem]] =
    val expected = Vector(parcellation.parcels.size, timeAxis.extent)
    val actual = Vector.tabulate(data.shape.rank)(data.shape.apply)
    if timeAxis.kind != AxisKind.Time then
      Left(ParcelSeriesError.ExpectedTimeAxis(timeAxis.kind))
    else if actual != expected then
      Left(ParcelSeriesError.DataShapeMismatch(expected, actual))
    else
      Right(
        new ParcelSeries(
          parcellation,
          timeAxis,
          data,
          metadata,
          semantics
        )
      )

  /** Mean-reduce an exact voxel series onto parcel fibers.
    *
    * `support` may restrict contributing voxels. Parcels made empty by that
    * restriction must be rejected or filled explicitly.
    */
  def reduceMean[F <: Frame[D3], S, P, M, T, A, Sem](
      series: SomeNeuroSeries[A, Sem],
      parcellation: VolumeParcellation[F, S, P, M],
      support: Region[T],
      empty: EmptyParcelPolicy[A]
  )(using
      values: ScalarField[A],
      semantics: ValueSemantics[A, Sem]
  ): Either[ParcelSeriesError, ParcelSeries[F, S, P, A, Sem]] =
    parcellation.support
      .intersectChecked(support)
      .left
      .map(ParcelSeriesError.WrongSupport.apply)
      .flatMap: activeSupport =>
        val sampled = SomeNeuroSeries.sampled(series)
        parcellation.domain
          .validateGrid(sampled.grid)
          .left
          .map(ParcelSeriesError.WrongGrid.apply)
          .flatMap: _ =>
            val fibers =
              Vector.tabulate(parcellation.parcels.size): parcelOrdinal =>
                val parcel =
                  parcellation.parcels.indexAtValidatedOrdinal(parcelOrdinal)
                parcellation.region(parcel).intersect(activeSupport)
            val firstEmpty = fibers.indexWhere(_.isEmpty)
            val rejectsEmpty =
              empty match
                case EmptyParcelPolicy.Reject  => true
                case EmptyParcelPolicy.Fill(_) => false
            if firstEmpty >= 0 && rejectsEmpty then
              Left(ParcelSeriesError.EmptyParcel(firstEmpty))
            else
              given DType[A] = sampled.dtype
              val timeAxis = sampled.nonSpatialAxes.values.head
              val nTime = timeAxis.extent
              val spatialShape = parcellation.domain.grid.shape
              val plane = spatialShape(1) * spatialShape(2)
              val data =
                NDArray.build[A, Rank[2]](
                  Shape(parcellation.parcels.size, nTime)
                ): output =>
                  var parcelOrdinal = 0
                  while parcelOrdinal < fibers.length do
                    val fiber = fibers(parcelOrdinal)
                    var time = 0
                    while time < nTime do
                      val result =
                        if fiber.isEmpty then
                          empty match
                            case EmptyParcelPolicy.Fill(value) => value
                            case EmptyParcelPolicy.Reject => values.zero
                        else
                          var sum = values.zero
                          fiber.foreachIndex: voxel =>
                            val ordinal = voxel.ordinal
                            val x = ordinal / plane
                            val withinPlane = ordinal % plane
                            val y = withinPlane / spatialShape(2)
                            val z = withinPlane % spatialShape(2)
                            sum = values.plus(
                              sum,
                              sampled.data(x, y, z, time)
                            )
                          values.div(
                            sum,
                            values.fromInt(fiber.cardinality)
                          )
                      output.writeLinear(
                        parcelOrdinal * nTime + time,
                        result
                      )
                      time += 1
                    parcelOrdinal += 1
              create(
                parcellation,
                timeAxis,
                data,
                sampled.metadata
              )

  def reduceMean[F <: Frame[D3], S, P, M, A, Sem](
      series: SomeNeuroSeries[A, Sem],
      parcellation: VolumeParcellation[F, S, P, M],
      empty: EmptyParcelPolicy[A] = EmptyParcelPolicy.Reject
  )(using
      ScalarField[A],
      ValueSemantics[A, Sem]
  ): Either[ParcelSeriesError, ParcelSeries[F, S, P, A, Sem]] =
    reduceMean(series, parcellation, parcellation.support, empty)

  def zipExact[F <: Frame[D3], S, P, A, B, C, LeftSem, RightSem, OutSem](
      left: ParcelSeries[F, S, P, A, LeftSem],
      right: ParcelSeries[F, S, P, B, RightSem]
  )(
      combine: (A, B) => C
  )(using
      DType[C],
      ValueSemantics[C, OutSem]
  ): Either[ParcelSeriesError, ParcelSeries[F, S, P, C, OutSem]] =
    if !(left.parcellation eq right.parcellation)
    then Left(ParcelSeriesError.ParcellationMismatch)
    else if left.timeAxis.record != right.timeAxis.record then
      Left(
        ParcelSeriesError.TimeAxisMismatch(
          left.timeAxis.record,
          right.timeAxis.record
        )
      )
    else
      val combined =
        NDArray.tabulate[C](left.nParcels, left.nTime): (parcel, time) =>
          combine(left.data(parcel, time), right.data(parcel, time))
      create(
        left.parcellation,
        left.timeAxis,
        combined,
        left.metadata
      )

  private[image] def unsafe[F <: Frame[D3], S, P, A, Sem](
      parcellation: VolumeParcellation[F, S, P, ?],
      timeAxis: Axis,
      data: NDArray[A, Rank[2]],
      metadata: ImageMetadata
  )(using
      semantics: ValueSemantics[A, Sem]
  ): ParcelSeries[F, S, P, A, Sem] =
    new ParcelSeries(
      parcellation,
      timeAxis,
      data,
      metadata,
      semantics
    )
