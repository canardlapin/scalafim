package scalafim.image

import image4s.Continuous
import image4s.Mask as MaskSemantics
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import spire.algebra.Order

import VolumeDomain.*

object NeuroStats:

  final case class ScalarSummary(
    count: Int,
    missing: Int,
    sum: Double,
    product: Double,
    min: Double,
    max: Double,
    mean: Double,
    sd: Double,
    zeros: Int,
    nonZeros: Int
  ):
    def range: (Double, Double) = (min, max)

  final case class NeuroVolSummary(
    kind: String,
    dims: Vector[Int],
    spacing: Vector[Double],
    origin: Vector[Double],
    orientation: String,
    stats: ScalarSummary
  )

  final case class NeuroVecSummary(
    kind: String,
    dims: Vector[Int],
    spacing: Vector[Double],
    origin: Vector[Double],
    orientation: String,
    timePoints: Int,
    global: ScalarSummary,
    temporalMeanRange: (Double, Double),
    temporalSdRange: (Double, Double),
    nonZeroVoxels: Int,
    totalVoxels: Int
  )

  def summarize(values: Array[Double], naRm: Boolean = true): ScalarSummary =
    summarizeIndexed(values.length, values.apply, naRm)

  def summarize(values: Array1[Double], naRm: Boolean): ScalarSummary =
    summarizeIndexed(values.size, index => values(index), naRm)

  private def summarizeIndexed(
      length: Int,
      valueAt: Int => Double,
      naRm: Boolean
  ): ScalarSummary =
    var i = 0
    var n = 0
    var missing = 0
    var zeros = 0
    var nonZeros = 0
    var sum = 0.0
    var sumSq = 0.0
    var product = 1.0
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity

    while i < length do
      val v = valueAt(i)
      if v.isNaN then
        missing += 1
        if !naRm then
          sum = Double.NaN
          sumSq = Double.NaN
          product = Double.NaN
      else
        n += 1
        sum += v
        sumSq += v * v
        product *= v
        if v < min then min = v
        if v > max then max = v
        if v == 0.0 then zeros += 1 else nonZeros += 1
      i += 1

    if !naRm && missing > 0 then
      ScalarSummary(n, missing, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, zeros, nonZeros)
    else
      val mean = if n == 0 then Double.NaN else sum / n.toDouble
      val sd =
        if n <= 1 then Double.NaN
        else
          val variance = (sumSq - (sum * sum) / n.toDouble) / (n - 1).toDouble
          math.sqrt(math.max(0.0, variance))
      ScalarSummary(n, missing, sum, product, min, max, mean, sd, zeros, nonZeros)

  def summarize(vol: NeuroVol[Double]): NeuroVolSummary =
    summarize(vol, naRm = true)

  def summarize(vol: NeuroVol[Double], naRm: Boolean): NeuroVolSummary =
    NeuroVolSummary(
      kind = "NeuroVol",
      dims = vol.space.spatialDims,
      spacing = vol.space.spacing,
      origin = vol.space.origin,
      orientation = orientation(vol.space),
      stats = summarizeIndexed(
        vol.values.size,
        vol.valueAtCanonicalOrdinal,
        naRm
      )
    )

  def summarize[F <: Frame[D3], S](
      volume: SelectedVolume[F, S, Double, Continuous]
  ): NeuroVolSummary =
    summarize(volume, naRm = true)

  def summarize[F <: Frame[D3], S](
      volume: SelectedVolume[F, S, Double, Continuous],
      naRm: Boolean
  ): NeuroVolSummary =
    val space = volume.domain.volumeSpace.toNeuroSpace
    NeuroVolSummary(
      kind = "SelectedVolume",
      dims = space.spatialDims,
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      stats = summarizeIndexed(
        volume.data.size,
        volume.data.apply,
        naRm
      )
    )

  def summarize(vec: NeuroVec[Double]): NeuroVecSummary =
    summarize(vec, naRm = true)

  def summarize(vec: NeuroVec[Double], naRm: Boolean): NeuroVecSummary =
    summarizeVec(
      "NeuroVec",
      vec.space,
      vec.values.size,
      vec.valueAtCanonicalOrdinal,
      vec.nVolumes,
      vec.space.spatialDims.product,
      naRm
    )

  def summarize[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous]
  ): NeuroVecSummary =
    summarize(series, naRm = true)

  def summarize[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous],
      naRm: Boolean
  ): NeuroVecSummary =
    val space = series.domain.volumeSpace.toNeuroSpace.addDim(
      series.nTime,
      Some(Axis.Time)
    )
    summarizeSparseVec(
      "SelectedSeries",
      space,
      series.nTime,
      series.selection.size,
      (time, position) => series.data(position, time),
      naRm
    )

  def temporalMean(vec: NeuroVec[Double]): NeuroVol[Double] =
    val spatialNels = vec.space.spatialDims.product
    val tLen = vec.nVolumes
    val out = PrimitiveBuffers.ofSize[Double](spatialNels)

    var lin = 0
    while lin < spatialNels do
      var t = 0
      var sum = 0.0
      while t < tLen do
        sum += vec.valueAtVoxelOrdinal(lin, t)
        t += 1
      out(lin) = sum / tLen.toDouble
      lin += 1

    NeuroVol.copyFromCanonicalArray(out, vec.space.spatialSpace, vec.label)

  def temporalMean[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous]
  ): Either[
    SelectedImageError,
    SelectedVolume[F, S, Double, Continuous]
  ] =
    given DType[Double] = series.dtype
    val tLen = series.nTime
    val out = RavelArray.tabulate[Double](series.selection.size): position =>
      var time = 0
      var sum = 0.0
      while time < tLen do
        sum += series.data(position, time)
        time += 1
      sum / tLen.toDouble
    SelectedVolume.continuous(
      series.domain,
      series.selection,
      out,
      series.metadata
    )

  private def summarizeVec(
    kind: String,
    space: NeuroSpace,
    dataLength: Int,
    valueAt: Int => Double,
    tLen: Int,
    spatialNels: Int,
    naRm: Boolean
  ): NeuroVecSummary =
    val global = summarizeIndexed(dataLength, valueAt, naRm)
    val means = PrimitiveBuffers.ofSize[Double](spatialNels)
    val sds = PrimitiveBuffers.ofSize[Double](spatialNels)
    var nonZero = 0

    var lin = 0
    while lin < spatialNels do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = valueAt(lin * tLen + t)
        sum += v
        sumSq += v * v
        t += 1
      val mean = sum / tLen.toDouble
      means(lin) = mean
      if mean != 0.0 then nonZero += 1
      sds(lin) =
        if tLen <= 1 then 0.0
        else math.sqrt(math.max(0.0, (sumSq - (sum * sum) / tLen.toDouble) / (tLen - 1).toDouble))
      lin += 1

    NeuroVecSummary(
      kind = kind,
      dims = space.dims.take(4),
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      timePoints = tLen,
      global = global,
      temporalMeanRange = summarize(means).range,
      temporalSdRange = summarize(sds).range,
      nonZeroVoxels = nonZero,
      totalVoxels = spatialNels
    )

  private def summarizeSparseVec(
    kind: String,
    space: NeuroSpace,
    tLen: Int,
    nColumns: Int,
    valueAt: (Int, Int) => Double,
    naRm: Boolean
  ): NeuroVecSummary =
    val global =
      summarizeIndexed(
        tLen * nColumns,
        index => valueAt(index / nColumns, index % nColumns),
        naRm
      )
    val means = PrimitiveBuffers.ofSize[Double](nColumns)
    val sds = PrimitiveBuffers.ofSize[Double](nColumns)
    var nonZero = 0

    var col = 0
    while col < nColumns do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = valueAt(t, col)
        sum += v
        sumSq += v * v
        t += 1
      val mean = sum / tLen.toDouble
      means(col) = mean
      if mean != 0.0 then nonZero += 1
      sds(col) =
        if tLen <= 1 then 0.0
        else math.sqrt(math.max(0.0, (sumSq - (sum * sum) / tLen.toDouble) / (tLen - 1).toDouble))
      col += 1

    NeuroVecSummary(
      kind = kind,
      dims = space.dims.take(4),
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      timePoints = tLen,
      global = global,
      temporalMeanRange = summarize(means).range,
      temporalSdRange = summarize(sds).range,
      nonZeroVoxels = nonZero,
      totalVoxels = space.spatialDims.product
    )

  private def orientation(space: NeuroSpace): String =
    space.axes.spatialAxes.map(_.toString).mkString(" / ")

object NeuroCompare:

  enum Predicate:
    case LT, LTE, GT, GTE, EQV, NEQ

  def compare[A: Order](x: NeuroVol[A], y: NeuroVol[A], predicate: Predicate): NeuroVol[Boolean] =
    requireSameSpace(x.space, y.space)
    val out = PrimitiveBuffers.ofSize[Boolean](x.values.size)
    var i = 0
    while i < out.length do
      out(i) = test(
        x.valueAtCanonicalOrdinal(i),
        y.valueAtCanonicalOrdinal(i),
        predicate
      )
      i += 1
    NeuroVol.copyFromCanonicalArray(out, x.space, x.label)

  def compare[A: Order](x: NeuroVol[A], scalar: A, predicate: Predicate): NeuroVol[Boolean] =
    val out = PrimitiveBuffers.ofSize[Boolean](x.values.size)
    var i = 0
    while i < out.length do
      out(i) = test(x.valueAtCanonicalOrdinal(i), scalar, predicate)
      i += 1
    NeuroVol.copyFromCanonicalArray(out, x.space, x.label)

  def compare[F <: Frame[D3], S, A: Order, Sem](
      volume: SelectedVolume[F, S, A, Sem],
      scalar: A,
      predicate: Predicate
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): SelectedVolume[F, S, Boolean, MaskSemantics] =
    SelectedVolume.fromSelected(
      volume.selected.mapValues[Boolean, MaskSemantics]: value =>
        test(value, scalar, predicate)
    )

  def compare[F <: Frame[D3], S, A: Order, Sem](
      scalar: A,
      volume: SelectedVolume[F, S, A, Sem],
      predicate: Predicate
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): SelectedVolume[F, S, Boolean, MaskSemantics] =
    SelectedVolume.fromSelected(
      volume.selected.mapValues[Boolean, MaskSemantics]: value =>
        test(scalar, value, predicate)
    )

  def compare[A: Order](scalar: A, x: NeuroVol[A], predicate: Predicate): NeuroVol[Boolean] =
    val out = PrimitiveBuffers.ofSize[Boolean](x.values.size)
    var i = 0
    while i < out.length do
      out(i) = test(scalar, x.valueAtCanonicalOrdinal(i), predicate)
      i += 1
    NeuroVol.copyFromCanonicalArray(out, x.space, x.label)

  @scala.annotation.targetName("compareNeuroVecPair")
  def compare[A: Order](x: NeuroVec[A], y: NeuroVec[A], predicate: Predicate): NeuroVec[Boolean] =
    requireSameSpace(x.space, y.space)
    val out = PrimitiveBuffers.ofSize[Boolean](x.values.size)
    var i = 0
    while i < out.length do
      out(i) = test(
        x.valueAtCanonicalOrdinal(i),
        y.valueAtCanonicalOrdinal(i),
        predicate
      )
      i += 1
    NeuroVec.copyFromCanonicalArray(out, x.space, x.label)

  @scala.annotation.targetName("compareNeuroVecScalar")
  def compare[A: Order](x: NeuroVec[A], scalar: A, predicate: Predicate): NeuroVec[Boolean] =
    val out = PrimitiveBuffers.ofSize[Boolean](x.values.size)
    var i = 0
    while i < out.length do
      out(i) = test(x.valueAtCanonicalOrdinal(i), scalar, predicate)
      i += 1
    NeuroVec.copyFromCanonicalArray(out, x.space, x.label)

  def gt[A: Order](x: NeuroVol[A], scalar: A): NeuroVol[Boolean] =
    compare(x, scalar, Predicate.GT)

  def lt[A: Order](x: NeuroVol[A], scalar: A): NeuroVol[Boolean] =
    compare(x, scalar, Predicate.LT)

  def gte[A: Order](x: NeuroVol[A], scalar: A): NeuroVol[Boolean] =
    compare(x, scalar, Predicate.GTE)

  def lte[A: Order](x: NeuroVol[A], scalar: A): NeuroVol[Boolean] =
    compare(x, scalar, Predicate.LTE)

  def eqv[A: Order](x: NeuroVol[A], y: NeuroVol[A]): NeuroVol[Boolean] =
    compare(x, y, Predicate.EQV)

  def neq[A: Order](x: NeuroVol[A], y: NeuroVol[A]): NeuroVol[Boolean] =
    compare(x, y, Predicate.NEQ)

  private def test[A: Order](left: A, right: A, predicate: Predicate): Boolean =
    val ord = summon[Order[A]]
    predicate match
      case Predicate.LT => ord.lt(left, right)
      case Predicate.LTE => ord.lteqv(left, right)
      case Predicate.GT => ord.gt(left, right)
      case Predicate.GTE => ord.gteqv(left, right)
      case Predicate.EQV => ord.eqv(left, right)
      case Predicate.NEQ => !ord.eqv(left, right)

  private def requireSameSpace(a: NeuroSpace, b: NeuroSpace): Unit =
    GridCompatibility.requireExact(a, b)
