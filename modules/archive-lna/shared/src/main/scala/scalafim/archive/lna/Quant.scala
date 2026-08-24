package scalafim.archive.lna

import scalafim.archive.ArchiveError
import gale.linalg.DMat

object Quant:
  final case class Encoded(
      quantized: Payload.IntMatrix,
      scale: Payload.DoubleVector,
      offset: Payload.DoubleVector,
      params: QuantParams,
      report: QuantReport
  )

  def encode(data: DMat, params: QuantParams = QuantParams()): Either[ArchiveError, Encoded] =
    firstNonFinite(data) match
      case Some(index) => Left(ArchiveError.NonFiniteValue(index))
      case None =>
        params.scaleScope match
          case QuantScaleScope.Global => encodeWithStats(data, params, globalStats(data, params))
          case QuantScaleScope.Voxel  => encodeWithStats(data, params, voxelStats(data, params))

  def decode(encoded: Encoded): DMat =
    decode(encoded.quantized, encoded.scale.values, encoded.offset.values)

  def decode(quantized: Payload.IntMatrix, scale: Double, offset: Double): DMat =
    decode(quantized, Vector(scale), Vector(offset))

  def decode(quantized: Payload.IntMatrix, scale: IndexedSeq[Double], offset: IndexedSeq[Double]): DMat =
    decodeChecked(quantized, scale, offset).fold(err => throw IllegalArgumentException(err.message), identity)

  def decodeChecked(encoded: Encoded): Either[ArchiveError, DMat] =
    decodeChecked(encoded.quantized, encoded.scale.values, encoded.offset.values)

  def decodeChecked(quantized: Payload.IntMatrix, scale: Double, offset: Double): Either[ArchiveError, DMat] =
    decodeChecked(quantized, Vector(scale), Vector(offset))

  def decodeChecked(
      quantized: Payload.IntMatrix,
      scale: IndexedSeq[Double],
      offset: IndexedSeq[Double]
  ): Either[ArchiveError, DMat] =
    if scale.isEmpty then Left(ArchiveError.ShapeMismatch("quant scale vector must be non-empty"))
    else if offset.isEmpty then Left(ArchiveError.ShapeMismatch("quant offset vector must be non-empty"))
    else if scale.length != 1 && scale.length != quantized.cols then
      Left(ArchiveError.ShapeMismatch("quant scale vector must have length 1 or match columns"))
    else if offset.length != 1 && offset.length != quantized.cols then
      Left(ArchiveError.ShapeMismatch("quant offset vector must have length 1 or match columns"))
    else if scale.length != offset.length then
      Left(ArchiveError.ShapeMismatch("quant scale and offset vectors must have the same length"))
    else
      Right(DMat.tabulate(quantized.rows, quantized.cols): (row, column) =>
        val scaleAt = if scale.length == 1 then scale(0) else scale(column)
        val offsetAt = if offset.length == 1 then offset(0) else offset(column)
        quantized(row, column).toDouble * scaleAt + offsetAt
      )

  private final case class ColumnStats(min: Double, max: Double, mean: Double, sampleSd: Double)
  private final case class QuantStats(scale: Vector[Double], offset: Vector[Double])

  private def encodeWithStats(data: DMat, params: QuantParams, stats: QuantStats): Either[ArchiveError, Encoded] =
    val bits = params.quantBits
    val levels = bits.levels
    val dtype = bits.storageDType
    val out = Vector.newBuilder[Int]
    out.sizeHint(data.rows * data.cols)

    var nClipped = 0
    var r = 0
    while r < data.rows do
      var c = 0
      while c < data.cols do
        val scale = if stats.scale.length == 1 then stats.scale(0) else stats.scale(c)
        val offset = if stats.offset.length == 1 then stats.offset(0) else stats.offset(c)
        val raw =
          if scale == 0.0 then 0
          else math.rint((data(r, c) - offset) / scale).toInt
        val clipped = math.max(0, math.min(levels, raw))
        if raw != clipped then nClipped += 1
        out += clipped
        c += 1
      r += 1

    val report =
      QuantReport(
        bits = params.bits,
        method = params.method,
        scaleScope = params.scaleScope,
        nClippedTotal = nClipped,
        clipPct = 100.0 * nClipped.toDouble / (data.rows * data.cols).toDouble
      )

    if nClipped > 0 && !params.clipPolicy.allowClip then
      val pctText = f"${report.clipPct}%.3f"
      Left(
        ArchiveError.InvalidArchive(
          s"quantization would clip ${report.nClippedTotal} samples ($pctText%); set allowClip=true to permit clipping"
        )
      )
    else
      Right(
        Encoded(
          quantized = Payload.IntMatrix(data.rows, data.cols, out.result(), dtype),
          scale = Payload.DoubleVector(stats.scale),
          offset = Payload.DoubleVector(stats.offset),
          params = params,
          report = report
        )
      )

  private def scaleOffset(stats: Vector[ColumnStats], params: QuantParams): QuantStats =
    val levels = params.quantBits.levels
    val offsets = Vector.newBuilder[Double]
    val scales = Vector.newBuilder[Double]
    var i = 0
    while i < stats.length do
      val s = stats(i)
      val (offset, upper) =
        params.method match
          case QuantMethod.Range =>
            if params.centeringPolicy.enabled then
              val maxAbs = math.max(math.abs(s.max - s.mean), math.abs(s.min - s.mean))
              (s.mean - maxAbs, s.mean + maxAbs)
            else
              (s.min, s.max)
          case QuantMethod.Sd =>
            val maxAbs = 3.0 * s.sampleSd
            (s.mean - maxAbs, s.mean + maxAbs)
      val scale =
        if upper == offset then 1.0
        else (upper - offset) / levels.toDouble
      offsets += offset
      scales += scale
      i += 1
    QuantStats(scales.result(), offsets.result())

  private def globalStats(data: DMat, params: QuantParams): QuantStats =
    scaleOffset(Vector(matrixStats(data)), params)

  private def voxelStats(data: DMat, params: QuantParams): QuantStats =
    val out = Vector.newBuilder[ColumnStats]
    var c = 0
    while c < data.cols do
      var min = Double.PositiveInfinity
      var max = Double.NegativeInfinity
      var sum = 0.0
      var sumSquares = 0.0
      var r = 0
      while r < data.rows do
        val value = data(r, c)
        if value < min then min = value
        if value > max then max = value
        sum += value
        sumSquares += value * value
        r += 1
      out += statsFromMoments(min, max, sum, sumSquares, data.rows)
      c += 1
    scaleOffset(out.result(), params)

  private def matrixStats(data: DMat): ColumnStats =
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity
    var sum = 0.0
    var sumSquares = 0.0
    data.foreachRowMajor: value =>
      if value < min then min = value
      if value > max then max = value
      sum += value
      sumSquares += value * value
    statsFromMoments(min, max, sum, sumSquares, data.rows * data.cols)

  private def statsFromMoments(min: Double, max: Double, sum: Double, sumSquares: Double, n: Int): ColumnStats =
    val mean = sum / n.toDouble
    val sampleVariance =
      if n <= 1 then 0.0
      else math.max(0.0, (sumSquares - (sum * sum) / n.toDouble) / (n - 1).toDouble)
    ColumnStats(min, max, mean, math.sqrt(sampleVariance))

  private def firstNonFinite(data: DMat): Option[Int] =
    var index = 0
    while index < data.rows * data.cols do
      if !data(index / data.cols, index % data.cols).isFinite then return Some(index)
      index += 1
    None
