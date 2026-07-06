package scalafim.fmri.hrf.design

import scalafim.fmri.hrf.*

final case class SamplingFrame private (
    blockLens: Vector[Int],
    tr: Vector[Seconds],
    startTime: Vector[Seconds],
    precision: Seconds
):
  val nBlocks: Int = blockLens.length

  def blockIdsPerSample: Vector[Int] =
    blockLens.zipWithIndex.flatMap { case (l, b) => Vector.fill(l)(b) }

  def samples(blocks: Seq[Int] = 0 until nBlocks, global: Boolean = false): Vector[Seconds] =
    require(blocks.forall(b => b >= 0 && b < nBlocks), "invalid block index")
    val lens = blocks.map(blockLens)
    val out = Vector.newBuilder[Seconds]
    val offsets =
      if global then
        val durations = blockLens.zip(tr).map { case (l, trb) => l.toDouble * trb.value }
        val cum = durations.scanLeft(0.0)(_ + _)
        cum
      else Vector.fill(nBlocks + 1)(0.0)

    var blkIdx = 0
    while blkIdx < blocks.length do
      val b = blocks(blkIdx)
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
    require(onsets.length == blocks.length, "onsets and blocks must match length")
    require(blocks.forall(b => b >= 0 && b < nBlocks), "invalid block index")
    val durations = blockLens.zip(tr).map { case (l, trb) => l.toDouble * trb.value }
    val cum = durations.scanLeft(0.0)(_ + _)
    onsets.zip(blocks).map { case (o, b) => Seconds(o.value + cum(b)) }.toVector

object SamplingFrame:

  private def recycle[A](xs: Seq[A], n: Int, name: String): Vector[A] =
    if xs.length == n then xs.toVector
    else if xs.length == 1 then Vector.fill(n)(xs.head)
    else throw new IllegalArgumentException(s"$name must have length 1 or $n")

  def apply(
      blockLens: Seq[Int],
      tr: Seq[Double],
      startTime: Seq[Double] = Seq.empty,
      precision: Double = 0.1
  ): SamplingFrame =
    require(blockLens.nonEmpty, "blockLens must be non-empty")
    require(blockLens.forall(_ > 0), "blockLens must be positive")
    val nBlocks = blockLens.length
    val trSec: Vector[Seconds] = recycle(tr.map(Seconds(_)), nBlocks, "tr")
    require(trSec.forall(_.value > 0), "tr must be positive")
    val st: Vector[Seconds] =
      if startTime.isEmpty then trSec.map(t => Seconds(t.value / 2.0))
      else recycle(startTime.map(Seconds(_)), nBlocks, "startTime")
    require(st.forall(_.value >= 0.0), "startTime must be non-negative")
    require(precision > 0.0 && precision < trSec.map(_.value).min, "precision must be positive and less than min TR")
    SamplingFrame(blockLens.toVector, trSec, st, precision.s)
