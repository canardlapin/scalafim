package scalafim.atlas

import image4s.Continuous
import locus4s.SpaceMismatch
import locus4s.data.Field
import ravel.NDArray as RavelArray
import scalafim.image.*
import scala.util.control.NonFatal

enum ParcelReducer:
  case Mean, Sum
  /** Receives a fresh array for each parcel/sample and may retain or mutate it. */
  case Custom(run: Array[Double] => Double)

enum MissingValuePolicy:
  case SkipNaN, PropagateNaN, RejectNaN

/** Empty means no assigned samples after masking, distinct from all-NaN data.
  * SkipNaN gives NaN for an all-missing mean, zero for a sum, and an empty array
  * to a custom reducer. Infinities remain values under every missing policy.
  */
final case class ParcelReductionPolicy(
    missing: MissingValuePolicy = MissingValuePolicy.SkipNaN,
    empty: EmptyParcelPolicy[Double] = EmptyParcelPolicy.Fill(Double.NaN)
)

enum AtlasReductionError:
  case WrongInputOwner(cause: SpaceMismatch)
  case WrongMaskOwner(cause: SpaceMismatch)
  case EmptyParcel(id: RegionId)
  case MissingValue(id: RegionId, sample: Int)
  case CallbackFailed(id: RegionId, sample: Int, detail: String)
  case InvalidSeries(cause: ParcelSeriesError)

  def message: String = this match
    case WrongInputOwner(cause) => s"input has a different live spatial owner: $cause"
    case WrongMaskOwner(cause) => s"mask has a different live spatial owner: $cause"
    case EmptyParcel(id) => s"parcel ${id.value} has no samples in the requested support"
    case MissingValue(id, sample) => s"parcel ${id.value} contains NaN at sample $sample"
    case CallbackFailed(id, sample, detail) => s"parcel ${id.value} reducer failed at sample $sample: $detail"
    case InvalidSeries(cause) => cause.message

object AtlasReduce:
  /** Portable reduction on an exact realization support, including surfaces. */
  def reduceField(realization: AtlasRealization)(
      input: Field[realization.X, Double],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[Field[realization.X, Boolean]] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[AtlasReductionError, Field[realization.P, Double]] =
    if !realization.parcelAssignment.from.sameRuntimeOwnerAs(input.space) then
      Left(AtlasReductionError.WrongInputOwner(SpaceMismatch.between(realization.parcelAssignment.from, input.space)))
    else
      reduceSamples(realization)(1, (voxel, _) => input(realization.parcelAssignment.from.indexAtValidatedOrdinal(voxel)), mask, reducer, policy)
        .map(output => Field.view(realization.parcelDomain)(parcel => output(parcel.ordinal)))

  def summarizeVolumeEither(
      atlas: VolumeAtlas,
      data: SomeScalarVolume[Double],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[SomeMaskVolume] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[AtlasError, Field[atlas.realization.P, Double]] =
    val realization: atlas.realization.type = atlas.realization
    val sampled = SomeNeuroVolume.sampled(data)
    for
      input <- realization.domain.spatialField(sampled)
        .left.map(_ => nativeGridError(atlas, sampled.grid.shape))
      admittedMask <- admitMask(atlas)(mask)
      result <- reduceField(realization)(input, reducer, admittedMask, policy)
        .left.map(AtlasError.Reduction.apply)
    yield result

  def summarizeVolume(
      atlas: VolumeAtlas,
      data: SomeScalarVolume[Double],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[SomeMaskVolume] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Field[atlas.realization.P, Double] =
    summarizeVolumeEither(atlas, data, reducer, mask, policy)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def reduceSeriesEither(
      atlas: VolumeAtlas,
      data: SomeScalarSeries[Double],
      mask: Option[SomeMaskVolume] = None,
      reducer: ParcelReducer = ParcelReducer.Mean,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[AtlasError, ParcelSeries[atlas.realization.F, atlas.realization.X, atlas.realization.P, Double, Continuous]] =
    val realization: atlas.realization.type = atlas.realization
    val sampled = SomeNeuroSeries.sampled(data)
    val shape = realization.domain.grid.shape
    val plane = shape(1) * shape(2)
    val timeCount = sampled.nonSpatialAxes.values.head.extent
    for
      _ <- realization.domain.seriesField(sampled)
        .left.map(_ => nativeGridError(atlas, sampled.grid.shape))
      admittedMask <- admitMask(atlas)(mask)
      output <- reduceSamples(realization)(
        timeCount,
        (ordinal, time) => sampled.data(ordinal / plane, (ordinal % plane) / shape(2), ordinal % shape(2), time),
        admittedMask, reducer, policy
      ).left.map(AtlasError.Reduction.apply)
      result <- ParcelSeries.create(
        realization.parcellation,
        sampled.nonSpatialAxes.values.head,
        RavelArray.tabulate[Double](realization.parcelDomain.size, timeCount)((parcel, time) => output(parcel * timeCount + time)),
        sampled.metadata
      ).left.map(error => AtlasError.Reduction(AtlasReductionError.InvalidSeries(error)))
    yield result

  def reduceSeries(
      atlas: VolumeAtlas,
      data: SomeScalarSeries[Double],
      mask: Option[SomeMaskVolume] = None,
      reducer: ParcelReducer = ParcelReducer.Mean,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): ParcelSeries[atlas.realization.F, atlas.realization.X, atlas.realization.P, Double, Continuous] =
    reduceSeriesEither(atlas, data, mask, reducer, policy)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def admitMask(atlas: VolumeAtlas)(
      mask: Option[SomeMaskVolume]
  ): Either[AtlasError, Option[Field[atlas.realization.X, Boolean]]] =
    mask match
      case None => Right(None)
      case Some(value) =>
        val sampled = SomeNeuroVolume.sampled(value)
        atlas.realization.domain.spatialField(sampled)
          .left.map(_ => nativeGridError(atlas, sampled.grid.shape))
          .map(Some(_))

  /** One policy kernel for scalar and series inputs. Outputs are fully evaluated
    * before exposure; neither input mutation nor callback lookup reruns a fit.
    */
  private def reduceSamples(realization: AtlasRealization)(
      sampleCount: Int,
      valueAt: (Int, Int) => Double,
      mask: Option[Field[realization.X, Boolean]],
      reducer: ParcelReducer,
      policy: ParcelReductionPolicy
  ): Either[AtlasReductionError, Array[Double]] =
    val source = realization.parcelAssignment.from
    mask match
      case Some(value) if !source.sameRuntimeOwnerAs(value.space) =>
        return Left(AtlasReductionError.WrongMaskOwner(SpaceMismatch.between(source, value.space)))
      case _ => ()
    val parcelCount = realization.parcelDomain.size
    val selectedCount = Array.fill(parcelCount)(0)
    val sums = Array.fill(parcelCount * sampleCount)(0.0)
    val counts = Array.fill(parcelCount * sampleCount)(0)
    val propagated = Array.fill(parcelCount * sampleCount)(false)
    val customOrdinals = reducer match
      case ParcelReducer.Custom(_) => Some(Array.fill(parcelCount)(Array.newBuilder[Int]))
      case _ => None
    var ordinal = 0
    while ordinal < source.size do
      val voxel = source.indexAtValidatedOrdinal(ordinal)
      if mask.forall(_(voxel)) then
        realization.parcelAssignment(voxel) match
          case None => ()
          case Some(parcel) =>
            selectedCount(parcel.ordinal) += 1
            customOrdinals match
              case Some(builders) => builders(parcel.ordinal) += ordinal
              case None =>
                var sample = 0
                while sample < sampleCount do
                  val value = valueAt(ordinal, sample)
                  val output = parcel.ordinal * sampleCount + sample
                  if value.isNaN then
                    policy.missing match
                      case MissingValuePolicy.SkipNaN => ()
                      case MissingValuePolicy.PropagateNaN => propagated(output) = true
                      case MissingValuePolicy.RejectNaN =>
                        return Left(AtlasReductionError.MissingValue(realization.metadata(parcel).id, sample))
                  else
                    sums(output) += value
                    counts(output) += 1
                  sample += 1
      ordinal += 1
    val output = Array.ofDim[Double](parcelCount * sampleCount)
    val selected = customOrdinals.map(_.map(_.result()))
    var parcelOrdinal = 0
    while parcelOrdinal < parcelCount do
      val parcel = realization.parcelDomain.indexAtValidatedOrdinal(parcelOrdinal)
      val id = realization.metadata(parcel).id
      var sample = 0
      while sample < sampleCount do
        val position = parcelOrdinal * sampleCount + sample
        if selectedCount(parcelOrdinal) == 0 then
          policy.empty match
            case EmptyParcelPolicy.Reject => return Left(AtlasReductionError.EmptyParcel(id))
            case EmptyParcelPolicy.Fill(value) => output(position) = value
        else reducer match
          case ParcelReducer.Mean =>
            output(position) =
              if propagated(position) || counts(position) == 0 then Double.NaN
              else sums(position) / counts(position).toDouble
          case ParcelReducer.Sum =>
            output(position) = if propagated(position) then Double.NaN else sums(position)
          case ParcelReducer.Custom(run) =>
            val ordinals = selected.get(parcelOrdinal)
            val values = Array.ofDim[Double](ordinals.length)
            var kept = 0
            var i = 0
            var hasNaN = false
            while i < ordinals.length do
              val value = valueAt(ordinals(i), sample)
              if value.isNaN then
                policy.missing match
                  case MissingValuePolicy.SkipNaN => ()
                  case MissingValuePolicy.PropagateNaN => hasNaN = true
                  case MissingValuePolicy.RejectNaN => return Left(AtlasReductionError.MissingValue(id, sample))
              else
                values(kept) = value
                kept += 1
              i += 1
            if hasNaN then output(position) = Double.NaN
            else
              try output(position) = run(if kept == values.length then values else values.take(kept))
              catch
                case NonFatal(error) =>
                  return Left(AtlasReductionError.CallbackFailed(id, sample, Option(error.getMessage).getOrElse(error.getClass.getName)))
        sample += 1
      parcelOrdinal += 1
    Right(output)

  private def nativeGridError(atlas: VolumeAtlas, actualShape: Vector[Int]): AtlasError =
    val expected = atlas.realization.domain.grid.shape
    if expected != actualShape then AtlasError.SpaceMismatch(expected, actualShape)
    else AtlasError.ExactGridRequired(
      atlas.realization.domain.record.grid.key.toString,
      s"shape=${actualShape.mkString("x")} with a different live grid owner"
    )
