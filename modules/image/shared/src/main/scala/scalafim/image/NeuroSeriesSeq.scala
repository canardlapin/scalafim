package scalafim.image

import image4s.ValueSemantics

/** A logical time concatenation of native series that preserves value
  * semantics and delays allocation until `materialize` is requested.
  */
final case class NeuroSeriesSeq[A, Sem](series: Vector[SomeNeuroSeries[A, Sem]]):
  require(series.nonEmpty, "NeuroSeriesSeq cannot be empty")

  private val baseSpace = series.head.space.spatialSpace
  private val spatialSize = baseSpace.spatialDims.product

  series.foreach(value => GridCompatibility.requireSpatial(baseSpace, value.space))

  val frameCounts: Vector[Int] = series.map(_.nVolumes)
  val frameCount: Int = frameCounts.sum
  val space: SomeSampleSpace = baseSpace.addDim(frameCount, Some(Axis.Time))

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
    val perBlock = Array.fill(frameCounts.length)(Vector.empty[Int])
    times.foreach: time =>
      var block = 0
      while block < frameCounts.length do
        val start = offsets(block)
        val end = start + frameCounts(block)
        if time >= start && time < end then
          perBlock(block) = perBlock(block) :+ (time - start)
          block = frameCounts.length
        else block += 1

    NeuroSeriesSeq(
      perBlock.zipWithIndex.collect:
        case (localTimes, block) if localTimes.nonEmpty =>
          series(block).selectTimes(localTimes)
      .toVector
    )

  private[scalafim] def valueAtCanonicalOrdinal(index: Int): A =
    require(index >= 0 && index < spatialSize * frameCount, "canonical ordinal out of bounds")
    val voxelOrdinal = index / frameCount
    val time = index % frameCount
    val (value, localTime) = locate(time)
    value.valueAtVoxelOrdinal(voxelOrdinal, localTime)

  def materialize(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
    series.reduce(_.concatenate(_))
