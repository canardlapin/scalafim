package scalafim.image

import SampleSpaces.*

import image4s.AxisConcatenationPolicy
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.geometry.Grid

/** A logical time concatenation of native series that preserves value
  * semantics and delays allocation until `materialize` is requested.
  */
final case class NeuroSeriesSeq[A, Sem](series: Vector[SomeNeuroSeries[A, Sem]]):
  require(series.nonEmpty, "NeuroSeriesSeq cannot be empty")

  private val baseSpace = series.head.sampled.sampleSpace.spatialOnly
  private val spatialSize = baseSpace.spatialDims.product

  series.foreach: value =>
    Grid
      .exactCongruence(baseSpace.grid, value.grid)
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  val frameCounts: Vector[Int] = series.map(_.nVolumes)
  val frameCount: Int = frameCounts.sum
  private val concatenatedAxis =
    series.tail
      .foldLeft[Either[image4s.ImageError, image4s.Axis]](
        Right(series.head.sampled.nonSpatialAxes.values.head)
      ): (result, next) =>
        result.flatMap: axis =>
          axis.concatenate(
            next.sampled.nonSpatialAxes.values.head,
            AxisConcatenationPolicy.AppendDeclaredCoordinates
          )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )
  private val concatenatedAxes =
    series.head.sampled.nonSpatialAxes
      .updated(0, concatenatedAxis)
      .fold(
        error => throw new IllegalStateException(error.message),
        identity
      )
  val space: SomeSampleSpace =
    SampleSpace.create(baseSpace.grid, concatenatedAxes)

  private val offsets: Vector[Int] =
    frameCounts.scanLeft(0)(_ + _).dropRight(1)

  private def locate(time: Int): (SomeNeuroSeries[A, Sem], Int) =
    require(time >= 0 && time < frameCount, "time index out of bounds")
    var block = 0
    while block < frameCounts.length do
      val start = offsets(block)
      val end = start + frameCounts(block)
      if time >= start && time < end then
        return series(block) -> (time - start)
      block += 1
    throw new IllegalStateException("unreachable")

  def volumeAt(time: Int): SomeNeuroVolume[A, Sem] =
    val (value, localTime) = locate(time)
    value.volume(localTime)

  def selectTimes(times: Seq[Int])(using
      ValueSemantics[A, Sem]
  ): NeuroSeriesSeq[A, Sem] =
    require(times.nonEmpty, "times must be non-empty")
    require(times.forall(time => time >= 0 && time < frameCount), "time index out of bounds")
    val selected = Vector.newBuilder[SomeNeuroSeries[A, Sem]]
    var currentBlock = -1
    var currentTimes = Vector.empty[Int]

    def flush(): Unit =
      if currentTimes.nonEmpty then
        selected += series(currentBlock)
          .selectTimes(currentTimes)
          .fold(
            error =>
              throw new IllegalStateException(
                s"validated series selection failed: ${error.message}"
              ),
            identity
          )
        currentTimes = Vector.empty

    times.foreach: time =>
      var block = 0
      while block < frameCounts.length do
        val start = offsets(block)
        val end = start + frameCounts(block)
        if time >= start && time < end then
          if block != currentBlock then
            flush()
            currentBlock = block
          currentTimes = currentTimes :+ (time - start)
          block = frameCounts.length
        else block += 1
    flush()

    NeuroSeriesSeq(selected.result())

  private[scalafim] def valueAtCanonicalOrdinal(index: Int): A =
    require(index >= 0 && index < spatialSize * frameCount, "canonical ordinal out of bounds")
    val voxelOrdinal = index / frameCount
    val time = index % frameCount
    val (value, localTime) = locate(time)
    value.valueAtVoxelOrdinal(voxelOrdinal, localTime)

  def materialize(
      axisPolicy: AxisConcatenationPolicy,
      metadataPolicy: SeriesMetadataPolicy
  )(using
      ValueSemantics[A, Sem]
  ): Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
    series.tail.foldLeft[Either[NeuroImageError, SomeNeuroSeries[A, Sem]]](
      Right(series.head)
    ): (result, next) =>
      result.flatMap(_.concatenate(next)(axisPolicy, metadataPolicy))
