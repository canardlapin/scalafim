package scalafim.fmri.hrf.design

import scalafim.fmri.hrf.*

final case class SamplingFrame private (
    blockLens: Vector[Int],
    tr: Vector[Seconds],
    startTime: Vector[Seconds],
    precision: Seconds
):
  val nBlocks: Int = blockLens.length

  def allBlocks: BlockSelection =
    BlockSelection.all(nBlocks)

  def blockIdsPerSample: Vector[Int] =
    blockLens.zipWithIndex.flatMap { case (l, b) => Vector.fill(l)(b) }

  def samples(blocks: Seq[Int] = 0 until nBlocks, global: Boolean = false): Vector[Seconds] =
    val selection = BlockSelection
      .fromInts(blocks, nBlocks)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
    sampleTimes(selection, if global then TimeReference.Global else TimeReference.Local)

  def sampleTimes(blocks: BlockSelection, reference: TimeReference): Vector[Seconds] =
    val blockIds = blocks.toVector
    val lens = blockIds.map(blockLens)
    val out = Vector.newBuilder[Seconds]
    val offsets = reference match
      case TimeReference.Global =>
        val durations = blockLens.zip(tr).map { case (l, trb) => l.toDouble * trb.value }
        val cum = durations.scanLeft(0.0)(_ + _)
        cum
      case TimeReference.Local =>
        Vector.fill(nBlocks + 1)(0.0)

    var blkIdx = 0
    while blkIdx < blockIds.length do
      val b = blockIds(blkIdx)
      val len = lens(blkIdx)
      var i = 0
      while i < len do
        val t = startTime(b).value + i.toDouble * tr(b).value + offsets(b)
        out += Seconds(t)
        i += 1
      blkIdx += 1
    out.result()

  /** Alias for global acquisition times (R: acquisition_onsets). */
  def acquisitionOnsets(blocks: Seq[Int] = 0 until nBlocks): Vector[Seconds] =
    samples(blocks = blocks, global = true)

  def globalOnsets(onsets: Seq[Seconds], blocks: Seq[Int]): Vector[Seconds] =
    val selection = BlockSelection
      .fromInts(blocks, nBlocks)
      .fold(err => throw new IllegalArgumentException(err.message), identity)
    globalOnsets(onsets, selection)

  def globalOnsets(onsets: Seq[Seconds], blocks: BlockSelection): Vector[Seconds] =
    val blockIds = blocks.toVector
    require(onsets.length == blockIds.length, "onsets and blocks must match length")
    val durations = blockLens.zip(tr).map { case (l, trb) => l.toDouble * trb.value }
    val cum = durations.scanLeft(0.0)(_ + _)
    onsets.zip(blockIds).map { case (o, b) => Seconds(o.value + cum(b)) }.toVector

object SamplingFrame:

  private def recycle[A](xs: Seq[A], n: Int, name: String): Either[SamplingFrameError, Vector[A]] =
    if xs.length == n then Right(xs.toVector)
    else if xs.length == 1 then Right(Vector.fill(n)(xs.head))
    else Left(SamplingFrameError.LengthMismatch(name, n, xs.length))

  private def secondsVector(values: Seq[Double], label: String): Either[SamplingFrameError, Vector[Seconds]] =
    val out = Vector.newBuilder[Seconds]
    val xs = values.toVector
    var i = 0
    while i < xs.length do
      Seconds.fromDouble(xs(i), label) match
        case Left(error) => return Left(SamplingFrameError.InvalidTime(label, error))
        case Right(seconds) => out += seconds
      i += 1
    Right(out.result())

  def validated(
      blockLens: Seq[Int],
      tr: Seq[Double],
      startTime: Seq[Double] = Seq.empty,
      precision: Double = 0.1
  ): Either[SamplingFrameError, SamplingFrame] =
    if blockLens.isEmpty then Left(SamplingFrameError.EmptyBlockLens)
    else
      val lens = blockLens.toVector
      var i = 0
      while i < lens.length do
        if lens(i) <= 0 then return Left(SamplingFrameError.NonPositiveBlockLength(i, lens(i)))
        i += 1
      val nBlocks = lens.length
      for
        trRaw <- secondsVector(tr, "tr")
        trSec <- recycle(trRaw, nBlocks, "tr")
        _ <- validatePositiveTimes(trSec, "tr")
        startRaw <- if startTime.isEmpty then Right(Vector.empty) else secondsVector(startTime, "startTime")
        st <- if startRaw.isEmpty then Right(trSec.map(t => Seconds(t.value / 2.0))) else recycle(startRaw, nBlocks, "startTime")
        _ <- validateNonNegativeTimes(st, "startTime")
        precision0 <- Seconds.fromDouble(precision, "precision").left.map(SamplingFrameError.InvalidTime("precision", _))
        _ <- validatePrecision(precision0, trSec.minBy(_.value))
      yield SamplingFrame(lens, trSec, st, precision0)

  def apply(
      blockLens: Seq[Int],
      tr: Seq[Double],
      startTime: Seq[Double] = Seq.empty,
      precision: Double = 0.1
  ): SamplingFrame =
    validated(blockLens, tr, startTime, precision)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  private def validatePositiveTimes(values: Vector[Seconds], label: String): Either[SamplingFrameError, Unit] =
    var i = 0
    while i < values.length do
      if values(i).value <= 0.0 then return Left(SamplingFrameError.NonPositiveTime(label, i, values(i)))
      i += 1
    Right(())

  private def validateNonNegativeTimes(values: Vector[Seconds], label: String): Either[SamplingFrameError, Unit] =
    var i = 0
    while i < values.length do
      if values(i).value < 0.0 then return Left(SamplingFrameError.NegativeTime(label, i, values(i)))
      i += 1
    Right(())

  private def validatePrecision(precision: Seconds, minTr: Seconds): Either[SamplingFrameError, Unit] =
    if precision.value > 0.0 && precision.value < minTr.value then Right(())
    else Left(SamplingFrameError.InvalidPrecision(precision, minTr))
