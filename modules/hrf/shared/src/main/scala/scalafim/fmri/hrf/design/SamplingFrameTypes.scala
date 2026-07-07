package scalafim.fmri.hrf.design

import scalafim.fmri.hrf.{Seconds, TimeError}

enum SamplingFrameError:
  case EmptyBlockLens
  case NonPositiveBlockLength(index: Int, value: Int)
  case LengthMismatch(name: String, expected: Int, actual: Int)
  case InvalidTime(label: String, error: TimeError)
  case NonPositiveTime(label: String, index: Int, value: Seconds)
  case NegativeTime(label: String, index: Int, value: Seconds)
  case InvalidPrecision(value: Seconds, minTr: Seconds)
  case InvalidBlockIndex(value: Int, nBlocks: Int)
  case OnsetBlockLengthMismatch(expected: Int, actual: Int)

  def message: String =
    this match
      case EmptyBlockLens =>
        "blockLens must be non-empty"
      case NonPositiveBlockLength(index, value) =>
        s"blockLens value at index ${index + 1} must be positive, got $value"
      case LengthMismatch(name, expected, actual) =>
        s"$name must have length 1 or $expected, got $actual"
      case InvalidTime(label, error) =>
        s"invalid $label: ${error.message}"
      case NonPositiveTime(label, index, value) =>
        s"$label value at index ${index + 1} must be > 0, got ${value.value}"
      case NegativeTime(label, index, value) =>
        s"$label value at index ${index + 1} must be non-negative, got ${value.value}"
      case InvalidPrecision(value, minTr) =>
        s"precision must be positive and less than min TR (${minTr.value}), got ${value.value}"
      case InvalidBlockIndex(value, nBlocks) =>
        s"invalid block index $value for $nBlocks blocks"
      case OnsetBlockLengthMismatch(expected, actual) =>
        s"onsets and blocks must match length: expected $expected, got $actual"

opaque type BlockIndex = Int

object BlockIndex:
  def fromInt(value: Int, nBlocks: Int): Either[SamplingFrameError, BlockIndex] =
    if value >= 0 && value < nBlocks then Right(value)
    else Left(SamplingFrameError.InvalidBlockIndex(value, nBlocks))

  inline def unsafe(value: Int): BlockIndex =
    value

  extension (block: BlockIndex)
    inline def value: Int =
      block

final case class BlockSelection private (
    indices: Vector[BlockIndex]
):
  def toVector: Vector[Int] =
    indices.map(_.value)

object BlockSelection:
  def all(nBlocks: Int): BlockSelection =
    unsafe(Vector.tabulate(nBlocks)(BlockIndex.unsafe))

  def fromInts(blocks: Seq[Int], nBlocks: Int): Either[SamplingFrameError, BlockSelection] =
    val out = Vector.newBuilder[BlockIndex]
    val xs = blocks.toVector
    var i = 0
    while i < xs.length do
      BlockIndex.fromInt(xs(i), nBlocks) match
        case Left(error) => return Left(error)
        case Right(block) => out += block
      i += 1
    Right(new BlockSelection(out.result()))

  private[design] def unsafe(indices: Vector[BlockIndex]): BlockSelection =
    new BlockSelection(indices)

enum TimeReference:
  case Local, Global
