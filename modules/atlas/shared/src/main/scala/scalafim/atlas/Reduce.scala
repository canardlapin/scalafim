package scalafim.atlas

import image4s.Continuous
import image4s.geometry.D3
import image4s.geometry.Frame
import locus4s.data.Field as LocusField
import ravel.NDArray as RavelArray
import ravel.Shape
import scalafim.image.*

final case class ParcelValue(
    region: AtlasRegionMetadata,
    value: Double
)

final case class ParcelValues(atlas: VolumeAtlas, values: Vector[ParcelValue]):
  require(values.map(_.region.id).toSet == atlas.regions.ids.toSet, "parcel values must cover atlas regions exactly")

  def value(regionId: RegionId): Option[Double] =
    values.find(_.region.id == regionId).map(_.value)

object Reducers:
  val mean: Array[Double] => Double =
    values =>
      if values.length == 0 then Double.NaN
      else
        var i = 0
        var sum = 0.0
        var n = 0
        while i < values.length do
          val v = values(i)
          if !v.isNaN then
            sum += v
            n += 1
          i += 1
        if n == 0 then Double.NaN else sum / n.toDouble

  val sum: Array[Double] => Double =
    values =>
      var i = 0
      var sum = 0.0
      while i < values.length do
        val v = values(i)
        if !v.isNaN then sum += v
        i += 1
      sum

object AtlasReduce:
  def summarizeVolumeEither(
      atlas: VolumeAtlas,
      data: SomeScalarVolume[Double],
      reducer: Array[Double] => Double = Reducers.mean
  ): Either[AtlasError, ParcelValues] =
    val realization = atlas.realization
    realization.domain
      .spatialField(data)
      .left
      .map(_ => nativeGridError(atlas, data.grid.shape))
      .map: field =>
        if sameReducer(reducer, Reducers.mean) ||
            sameReducer(reducer, Reducers.sum)
        then summarizeStandard(atlas, realization, field, reducer)
        else summarizeCustom(atlas, realization, field, reducer)

  def summarizeVolume(
      atlas: VolumeAtlas,
      data: SomeScalarVolume[Double],
      reducer: Array[Double] => Double = Reducers.mean
  ): ParcelValues =
    summarizeVolumeEither(atlas, data, reducer)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def reduceSeriesEither(
      atlas: VolumeAtlas,
      data: SomeScalarSeries[Double],
      mask: Option[SomeMaskVolume] = None,
      reducer: Array[Double] => Double = Reducers.mean,
      empty: EmptyParcelPolicy[Double] =
        EmptyParcelPolicy.Fill(Double.NaN)
  ): Either[AtlasError, SomeScalarParcelSeries[Double]] =
    val realization = atlas.realization
    for
      _ <- realization.domain
        .seriesField(data)
        .left
        .map(_ => nativeGridError(atlas, data.grid.shape))
      maskField <- mask match
        case None => Right(None)
        case Some(value) =>
          realization.domain
            .spatialField(value)
            .left
            .map(_ => nativeGridError(atlas, value.grid.shape))
            .map(Some(_))
      reduced <- buildSeries(
        realization.parcellation,
        data,
        maskField,
        reducer,
        empty
      )
    yield reduced

  def reduceSeries(
      atlas: VolumeAtlas,
      data: SomeScalarSeries[Double],
      mask: Option[SomeMaskVolume] = None,
      reducer: Array[Double] => Double = Reducers.mean,
      empty: EmptyParcelPolicy[Double] =
        EmptyParcelPolicy.Fill(Double.NaN)
  ): SomeScalarParcelSeries[Double] =
    reduceSeriesEither(atlas, data, mask, reducer, empty)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def summarizeStandard[
      F0 <: Frame[D3],
      X0,
      P0
  ](
      atlas: VolumeAtlas,
      realization: VolumeAtlasRealization {
        type F = F0
        type X = X0
        type P = P0
      },
      field: LocusField[X0, Double],
      reducer: Array[Double] => Double
  ): ParcelValues =
    val sums = Array.fill(realization.parcelDomain.size)(0.0)
    val counts = Array.fill(realization.parcelDomain.size)(0L)
    realization.domain.space.foreachIndex: voxel =>
      realization.parcelAssignment(voxel).foreach: parcel =>
        val value = field(voxel)
        if !value.isNaN then
          sums(parcel.ordinal) += value
          counts(parcel.ordinal) += 1L
    val values =
      realization.displayOrder.indices.map: parcel =>
        val value =
          if sameReducer(reducer, Reducers.sum) then sums(parcel.ordinal)
          else if counts(parcel.ordinal) == 0L then Double.NaN
          else sums(parcel.ordinal) / counts(parcel.ordinal).toDouble
        ParcelValue(realization.metadata(parcel), value)
      .toVector
    ParcelValues(atlas, values)

  private def summarizeCustom[
      F0 <: Frame[D3],
      X0,
      P0
  ](
      atlas: VolumeAtlas,
      realization: VolumeAtlasRealization {
        type F = F0
        type X = X0
        type P = P0
      },
      field: LocusField[X0, Double],
      reducer: Array[Double] => Double
  ): ParcelValues =
    val values =
      realization.displayOrder.indices.map: parcel =>
        val fiber = realization.parcelAssignment.fiber(parcel)
        val input = Array.ofDim[Double](fiber.cardinality)
        var position = 0
        fiber.foreachIndex: voxel =>
          input(position) = field(voxel)
          position += 1
        ParcelValue(realization.metadata(parcel), reducer(input))
      .toVector
    ParcelValues(atlas, values)

  private def buildSeries[F <: Frame[D3], S, P](
      parcellation: VolumeParcellation[F, S, P, ?],
      data: SomeScalarSeries[Double],
      mask: Option[LocusField[S, Boolean]],
      reducer: Array[Double] => Double,
      empty: EmptyParcelPolicy[Double]
  ): Either[
    AtlasError,
    ParcelSeries[F, S, P, Double, Continuous]
  ] =
    val parcelCount = parcellation.parcels.size
    val timeCount = data.nonSpatialAxes.values.head.extent
    val standard =
      sameReducer(reducer, Reducers.mean) ||
        sameReducer(reducer, Reducers.sum)
    val selectedCount = Array.fill(parcelCount)(0)
    val ordinalBuilders =
      if standard then None
      else Some(Array.fill(parcelCount)(Array.newBuilder[Int]))
    val sums =
      if standard then Array.fill(parcelCount * timeCount)(0.0)
      else Array.emptyDoubleArray
    val counts =
      if standard then Array.fill(parcelCount * timeCount)(0L)
      else Array.emptyLongArray
    val shape = parcellation.domain.grid.shape
    val plane = shape(1) * shape(2)

    parcellation.domain.space.foreachIndex: voxel =>
      if mask.forall(_(voxel)) then
        parcellation.assignment(voxel).foreach: parcel =>
          val parcelOrdinal = parcel.ordinal
          selectedCount(parcelOrdinal) += 1
          ordinalBuilders.foreach(_(parcelOrdinal) += voxel.ordinal)
          if standard then
            val x = voxel.ordinal / plane
            val withinPlane = voxel.ordinal % plane
            val y = withinPlane / shape(2)
            val z = withinPlane % shape(2)
            var time = 0
            while time < timeCount do
              val value = data.data(x, y, z, time)
              if !value.isNaN then
                val output = parcelOrdinal * timeCount + time
                sums(output) += value
                counts(output) += 1L
              time += 1

    val firstEmpty = selectedCount.indexWhere(_ == 0)
    empty match
      case EmptyParcelPolicy.Reject if firstEmpty >= 0 =>
        Left(
          AtlasError.InvalidReduction(
            s"parcel $firstEmpty has no voxels in the requested support"
          )
        )
      case _ =>
        val selected = ordinalBuilders.map(_.map(_.result()))
        val scratch = selected.map(_.map(values => Array.ofDim[Double](values.length)))
        val output =
          RavelArray.tabulate[Double](parcelCount, timeCount): (parcel, time) =>
            if selectedCount(parcel) == 0 then
              empty match
                case EmptyParcelPolicy.Fill(value) => value
                case EmptyParcelPolicy.Reject => Double.NaN
            else if standard then
              val index = parcel * timeCount + time
              if sameReducer(reducer, Reducers.sum) then sums(index)
              else if counts(index) == 0L then Double.NaN
              else sums(index) / counts(index).toDouble
            else
              val ordinals = selected.get(parcel)
              val values = scratch.get(parcel)
              var position = 0
              while position < ordinals.length do
                val ordinal = ordinals(position)
                val x = ordinal / plane
                val withinPlane = ordinal % plane
                val y = withinPlane / shape(2)
                val z = withinPlane % shape(2)
                values(position) = data.data(x, y, z, time)
                position += 1
              reducer(values)
        ParcelSeries
          .create(
            parcellation,
            data.nonSpatialAxes.values.head,
            output,
            data.metadata
          )
          .left
          .map(error => AtlasError.InvalidReduction(error.message))

  private def nativeGridError(
      atlas: VolumeAtlas,
      actualShape: Vector[Int]
  ): AtlasError =
    val expected = atlas.realization.domain.grid.shape
    if expected != actualShape then
      AtlasError.SpaceMismatch(expected, actualShape)
    else
      AtlasError.ExactGridRequired(
        atlas.realization.domain.record.grid.key.toString,
        s"shape=${actualShape.mkString("x")} with a different live grid owner"
      )

  private def sameReducer(
      left: Array[Double] => Double,
      right: Array[Double] => Double
  ): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
