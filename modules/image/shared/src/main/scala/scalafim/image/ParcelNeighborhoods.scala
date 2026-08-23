package scalafim.image

import image4s.ImageMetadata
import image4s.geometry.D3
import image4s.geometry.Frame
import locus4s.Index
import locus4s.Region
import locus4s.Relation
import locus4s.RelationError
import locus4s.Selection
import locus4s.SelectionError
import locus4s.SpaceMismatch
import locus4s.data.Field
import ravel.DType
import ravel.NDArray
import ravel.Rank
import ravel.select

enum ParcelNeighborhoodError:
  case WrongParcelOwner(error: SpaceMismatch)
  case InvalidRelation(error: RelationError)
  case InvalidSelection(error: SelectionError)
  case InvalidGeometry(error: VolumeParcellationGeometryError)
  case InvalidNeighborCount(value: Int)
  case InvalidRadius(value: Double)
  case NonEmptyOutsideCenters(parcelOrdinal: Int)
  case MissingCenter(parcelOrdinal: Int)
  case CenterUnavailable(parcelOrdinal: Int)
  case ParcellationMismatch
  case TimeIndexOutOfBounds(index: Int, extent: Int)

  def message: String =
    this match
      case WrongParcelOwner(error) => error.message
      case InvalidRelation(error) => error.message
      case InvalidSelection(error) => error.message
      case InvalidGeometry(error) => error.message
      case InvalidNeighborCount(value) =>
        s"parcel neighbor count must be positive, found $value"
      case InvalidRadius(value) =>
        s"parcel neighborhood radius must be finite and non-negative, found $value"
      case NonEmptyOutsideCenters(parcel) =>
        s"parcel neighborhood row $parcel is non-empty outside the center region"
      case MissingCenter(parcel) =>
        s"parcel neighborhood row $parcel does not contain its center"
      case CenterUnavailable(parcel) =>
        s"parcel $parcel is not an allowed neighborhood center"
      case ParcellationMismatch =>
        "parcel series and neighborhoods do not share the same exact parcellation"
      case TimeIndexOutOfBounds(index, extent) =>
        s"time index $index is outside [0, $extent)"

/** Exact parcel-to-parcel neighborhood policy for one parcellation. */
final class ParcelNeighborhoods[
    F <: Frame[D3],
    S,
    P
] private[image] (
    val parcellation: VolumeParcellation[F, S, P, ?],
    val centers: Region[P],
    val relation: Relation[P, P]
):
  def regionAt(center: Index[P]): Option[Region[P]] =
    Option.when(centers.contains(center))(relation.row(center))

object ParcelNeighborhoods:
  def fromRelation[F <: Frame[D3], S, P](
      parcellation: VolumeParcellation[F, S, P, ?],
      centers: Region[P],
      relation: Relation[P, P]
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    val parcels = parcellation.parcels
    if !parcels.sameRuntimeOwnerAs(centers.space) then
      Left(
        ParcelNeighborhoodError.WrongParcelOwner(
          SpaceMismatch.between(parcels, centers.space)
        )
      )
    else if !parcels.sameRuntimeOwnerAs(relation.from) then
      Left(
        ParcelNeighborhoodError.WrongParcelOwner(
          SpaceMismatch.between(parcels, relation.from)
        )
      )
    else if !parcels.sameRuntimeOwnerAs(relation.to) then
      Left(
        ParcelNeighborhoodError.WrongParcelOwner(
          SpaceMismatch.between(parcels, relation.to)
        )
      )
    else
      var parcelOrdinal = 0
      var error = Option.empty[ParcelNeighborhoodError]
      while parcelOrdinal < parcels.size && error.isEmpty do
        val parcel = parcels.indexAtValidatedOrdinal(parcelOrdinal)
        val row = relation.row(parcel)
        if centers.contains(parcel) then
          if !row.contains(parcel) then
            error = Some(ParcelNeighborhoodError.MissingCenter(parcelOrdinal))
        else if !row.isEmpty then
          error = Some(
            ParcelNeighborhoodError.NonEmptyOutsideCenters(parcelOrdinal)
          )
        parcelOrdinal += 1
      error match
        case Some(value) => Left(value)
        case None =>
          Right(new ParcelNeighborhoods(parcellation, centers, relation))

object ExactParcelSearchlight:
  def nearest[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      count: Int,
      centers: Region[P],
      frame: SpatialCoordinateFrame = SpatialCoordinateFrame.World,
      centroidMethod: ParcelCentroidMethod = ParcelCentroidMethod.CenterOfMass
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    if count <= 0 then
      Left(ParcelNeighborhoodError.InvalidNeighborCount(count))
    else
      requireCenters(parcellation, centers).flatMap: _ =>
        VolumeParcellationGeometry
          .centroids(parcellation, centroidMethod, frame)
          .left
          .map(ParcelNeighborhoodError.InvalidGeometry.apply)
          .flatMap: centroidField =>
            build(
              parcellation,
              centers,
              nearestRows(centroidField, centers, count)
            )

  def nearest[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      count: Int,
      frame: SpatialCoordinateFrame
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    nearest(
      parcellation,
      count,
      Region.whole(parcellation.parcels),
      frame
    )

  def nearest[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      count: Int
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    nearest(parcellation, count, SpatialCoordinateFrame.World)

  def withinRadius[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      radius: Double,
      centers: Region[P],
      frame: SpatialCoordinateFrame = SpatialCoordinateFrame.World,
      centroidMethod: ParcelCentroidMethod = ParcelCentroidMethod.CenterOfMass
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    if !radius.isFinite || radius < 0.0 then
      Left(ParcelNeighborhoodError.InvalidRadius(radius))
    else
      requireCenters(parcellation, centers).flatMap: _ =>
        VolumeParcellationGeometry
          .centroids(parcellation, centroidMethod, frame)
          .left
          .map(ParcelNeighborhoodError.InvalidGeometry.apply)
          .flatMap: centroidField =>
            build(
              parcellation,
              centers,
              radiusRows(centroidField, centers, radius)
            )

  def withinRadius[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      radius: Double,
      frame: SpatialCoordinateFrame
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    withinRadius(
      parcellation,
      radius,
      Region.whole(parcellation.parcels),
      frame
    )

  def withinRadius[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      radius: Double
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    withinRadius(parcellation, radius, SpatialCoordinateFrame.World)

  private def requireCenters[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      centers: Region[P]
  ): Either[ParcelNeighborhoodError, Unit] =
    if parcellation.parcels.sameRuntimeOwnerAs(centers.space) then Right(())
    else
      Left(
        ParcelNeighborhoodError.WrongParcelOwner(
          SpaceMismatch.between(parcellation.parcels, centers.space)
        )
      )

  private def build[F <: Frame[D3], S, P, M](
      parcellation: VolumeParcellation[F, S, P, M],
      centers: Region[P],
      rows: Vector[Vector[Int]]
  ): Either[
    ParcelNeighborhoodError,
    ParcelNeighborhoods[F, S, P]
  ] =
    Relation
      .fromOrdinalRows(parcellation.parcels, parcellation.parcels, rows)
      .left
      .map(ParcelNeighborhoodError.InvalidRelation.apply)
      .flatMap(ParcelNeighborhoods.fromRelation(parcellation, centers, _))

  private def nearestRows[P](
      centroids: Field[P, Vector[Double]],
      centers: Region[P],
      count: Int
  ): Vector[Vector[Int]] =
    val values = centroids.toVector
    val actualCount = math.min(count, values.length)
    Vector.tabulate(values.length): centerOrdinal =>
      val center = centroids.space.indexAtValidatedOrdinal(centerOrdinal)
      if !centers.contains(center) then Vector.empty
      else
        Vector
          .tabulate(values.length)(identity)
          .sortBy: targetOrdinal =>
            (squaredDistance(
              values(centerOrdinal),
              values(targetOrdinal)
            ), targetOrdinal)
          .take(actualCount)

  private def radiusRows[P](
      centroids: Field[P, Vector[Double]],
      centers: Region[P],
      radius: Double
  ): Vector[Vector[Int]] =
    val values = centroids.toVector
    val squaredRadius = radius * radius
    Vector.tabulate(values.length): centerOrdinal =>
      val center = centroids.space.indexAtValidatedOrdinal(centerOrdinal)
      if !centers.contains(center) then Vector.empty
      else
        Vector
          .tabulate(values.length)(identity)
          .filter: targetOrdinal =>
            squaredDistance(
              values(centerOrdinal),
              values(targetOrdinal)
            ) <= squaredRadius

  private def squaredDistance(
      left: Vector[Double],
      right: Vector[Double]
  ): Double =
    val dx = left(0) - right(0)
    val dy = left(1) - right(1)
    val dz = left(2) - right(2)
    dx * dx + dy * dy + dz * dz

/** One compact parcel searchlight with exact support order. */
final class ParcelSeriesWindow[
    F <: Frame[D3],
    S,
    P,
    A,
    Sem
] private[image] (
    val parcellation: VolumeParcellation[F, S, P, ?],
    val center: Index[P],
    val centerPosition: Int,
    val selection: Selection[P],
    val timeAxis: image4s.Axis,
    val data: NDArray[A, Rank[2]],
    val metadata: ImageMetadata
):
  def nPositions: Int = selection.size

  def nTime: Int = timeAxis.extent

  def parcelAt(position: Int): Option[Index[P]] =
    selection.get(position)

  def seriesAtPosition(position: Int): NDArray[A, Rank[1]] =
    data.select(0, position)

  def fieldAt(
      time: Int
  ): Either[ParcelNeighborhoodError, Field[selection.I, A]] =
    if time < 0 || time >= nTime then
      Left(
        ParcelNeighborhoodError.TimeIndexOutOfBounds(time, nTime)
      )
    else
      Right(
        Field.view(selection.positions): position =>
          data(position.ordinal, time)
      )

object ParcelSeriesWindow:
  def materialize[F <: Frame[D3], S, P, A, Sem](
      series: ParcelSeries[F, S, P, A, Sem],
      neighborhoods: ParcelNeighborhoods[F, S, P],
      center: Index[P]
  ): Either[
    ParcelNeighborhoodError,
    ParcelSeriesWindow[F, S, P, A, Sem]
  ] =
    if !(series.parcellation.asInstanceOf[AnyRef] eq
        neighborhoods.parcellation.asInstanceOf[AnyRef])
    then Left(ParcelNeighborhoodError.ParcellationMismatch)
    else
      neighborhoods.regionAt(center) match
        case None =>
          Left(ParcelNeighborhoodError.CenterUnavailable(center.ordinal))
        case Some(region) =>
          Selection
            .fromRegion(region)
            .left
            .map(ParcelNeighborhoodError.InvalidSelection.apply)
            .map: selection =>
              given DType[A] = series.data.dtype
              val compact =
                NDArray.tabulate[A](selection.size, series.nTime):
                  (position, time) =>
                    val positionIndex =
                      selection.positions.indexAtValidatedOrdinal(position)
                    val parcel = selection(positionIndex)
                    series.data(parcel.ordinal, time)
              var centerPosition = -1
              var position = 0
              while position < selection.size && centerPosition < 0 do
                val positionIndex =
                  selection.positions.indexAtValidatedOrdinal(position)
                if selection(positionIndex).ordinal == center.ordinal then
                  centerPosition = position
                position += 1
              new ParcelSeriesWindow(
                series.parcellation,
                center,
                centerPosition,
                selection,
                series.timeAxis,
                compact,
                series.metadata
              )
