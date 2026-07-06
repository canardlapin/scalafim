package scalafim.image

import narr.NArray
import scala.reflect.ClassTag
import spire.algebra.{Order, Ring}

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

  def summarize(values: NArray[Double], naRm: Boolean = true): ScalarSummary =
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

    while i < values.length do
      val v = values(i)
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
      stats = summarize(vol.values.data, naRm)
    )

  def summarize(svol: SparseNeuroVol[Double]): NeuroVolSummary =
    summarize(svol, naRm = true)

  def summarize(svol: SparseNeuroVol[Double], naRm: Boolean): NeuroVolSummary =
    NeuroVolSummary(
      kind = "SparseNeuroVol",
      dims = svol.space.spatialDims,
      spacing = svol.space.spacing,
      origin = svol.space.origin,
      orientation = orientation(svol.space),
      stats = summarize(svol.data, naRm)
    )

  def summarize(vec: NeuroVec[Double]): NeuroVecSummary =
    summarize(vec, naRm = true)

  def summarize(vec: NeuroVec[Double], naRm: Boolean): NeuroVecSummary =
    summarizeVec("NeuroVec", vec.space, vec.values.data, vec.nVolumes, vec.space.spatialDims.product, naRm)

  def summarize(svec: SparseNeuroVec[Double]): NeuroVecSummary =
    summarize(svec, naRm = true)

  def summarize(svec: SparseNeuroVec[Double], naRm: Boolean): NeuroVecSummary =
    summarizeSparseVec("SparseNeuroVec", svec.space, svec.data.data, svec.space.dims(3), svec.map.cardinality, naRm)

  def summarize(cvec: ClusteredNeuroVec[Double]): NeuroVecSummary =
    summarize(cvec, naRm = true)

  def summarize(cvec: ClusteredNeuroVec[Double], naRm: Boolean): NeuroVecSummary =
    summarizeSparseVec("ClusteredNeuroVec", cvec.space, cvec.ts.data, cvec.nVolumes, cvec.numClusters, naRm)

  def temporalMean(vec: NeuroVec[Double]): NeuroVol[Double] =
    val spatialNels = vec.space.spatialDims.product
    val tLen = vec.nVolumes
    val out = NArrayUtil.ofSize[Double](spatialNels)

    var lin = 0
    while lin < spatialNels do
      var t = 0
      var sum = 0.0
      while t < tLen do
        sum += vec.values.data(lin + t * spatialNels)
        t += 1
      out(lin) = sum / tLen.toDouble
      lin += 1

    NeuroVol.fromLinear(out, vec.space.spatialSpace, vec.label)

  def temporalMean(svec: SparseNeuroVec[Double]): SparseNeuroVol[Double] =
    val tLen = svec.space.dims(3)
    val nVox = svec.map.cardinality
    val out = NArrayUtil.ofSize[Double](nVox)

    var p = 0
    while p < nVox do
      var t = 0
      var sum = 0.0
      while t < tLen do
        sum += svec.data(t, p)
        t += 1
      out(p) = sum / tLen.toDouble
      p += 1

    SparseNeuroVol(out, svec.map.indices, svec.space.spatialSpace, svec.label)

  private def summarizeVec(
    kind: String,
    space: NeuroSpace,
    data: NArray[Double],
    tLen: Int,
    spatialNels: Int,
    naRm: Boolean
  ): NeuroVecSummary =
    val global = summarize(data, naRm)
    val means = NArrayUtil.ofSize[Double](spatialNels)
    val sds = NArrayUtil.ofSize[Double](spatialNels)
    var nonZero = 0

    var lin = 0
    while lin < spatialNels do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = data(lin + t * spatialNels)
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
    data: NArray[Double],
    tLen: Int,
    nColumns: Int,
    naRm: Boolean
  ): NeuroVecSummary =
    val global = summarize(data, naRm)
    val means = NArrayUtil.ofSize[Double](nColumns)
    val sds = NArrayUtil.ofSize[Double](nColumns)
    var nonZero = 0

    var col = 0
    while col < nColumns do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = data(t + col * tLen)
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
    val out = NArrayUtil.ofSize[Boolean](x.values.data.length)
    var i = 0
    while i < out.length do
      out(i) = test(x.values.data(i), y.values.data(i), predicate)
      i += 1
    NeuroVol.fromLinear(out, x.space, x.label)

  def compare[A: Order](x: NeuroVol[A], scalar: A, predicate: Predicate): NeuroVol[Boolean] =
    val out = NArrayUtil.ofSize[Boolean](x.values.data.length)
    var i = 0
    while i < out.length do
      out(i) = test(x.values.data(i), scalar, predicate)
      i += 1
    NeuroVol.fromLinear(out, x.space, x.label)

  def compare[A: Order](scalar: A, x: NeuroVol[A], predicate: Predicate): NeuroVol[Boolean] =
    val out = NArrayUtil.ofSize[Boolean](x.values.data.length)
    var i = 0
    while i < out.length do
      out(i) = test(scalar, x.values.data(i), predicate)
      i += 1
    NeuroVol.fromLinear(out, x.space, x.label)

  def compare[A: Order: Ring: ClassTag](x: SparseNeuroVol[A], scalar: A, predicate: Predicate): NeuroVol[Boolean] =
    compare(x.toDense, scalar, predicate)

  def compare[A: Order: Ring: ClassTag](scalar: A, x: SparseNeuroVol[A], predicate: Predicate): NeuroVol[Boolean] =
    compare(scalar, x.toDense, predicate)

  def compare(x: ClusteredNeuroVol, scalar: Int, predicate: Predicate): NeuroVol[Boolean] =
    compare(x.toDense, scalar, predicate)

  def compare(scalar: Int, x: ClusteredNeuroVol, predicate: Predicate): NeuroVol[Boolean] =
    compare(scalar, x.toDense, predicate)

  def compare[A: Order](x: NeuroVec[A], y: NeuroVec[A], predicate: Predicate): NeuroVec[Boolean] =
    requireSameSpace(x.space, y.space)
    val out = NArrayUtil.ofSize[Boolean](x.values.data.length)
    var i = 0
    while i < out.length do
      out(i) = test(x.values.data(i), y.values.data(i), predicate)
      i += 1
    NeuroVec.fromLinear(out, x.space, x.label)

  def compare[A: Order](x: NeuroVec[A], scalar: A, predicate: Predicate): NeuroVec[Boolean] =
    val out = NArrayUtil.ofSize[Boolean](x.values.data.length)
    var i = 0
    while i < out.length do
      out(i) = test(x.values.data(i), scalar, predicate)
      i += 1
    NeuroVec.fromLinear(out, x.space, x.label)

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
    require(a.dims == b.dims && a.spacing == b.spacing && a.origin == b.origin, "NeuroSpace mismatch")
