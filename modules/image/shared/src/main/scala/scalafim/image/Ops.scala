package scalafim.image

import scala.reflect.ClassTag
import narr.NArray
import spire.algebra.{Field, Order, Ring}
import spire.syntax.field.*
import spire.syntax.ring.*
import scala.annotation.targetName

object Ops:

  extension [A](x: NDArray[A])
    def zipWith[B, C](y: NDArray[B])(f: (A, B) => C)(using ClassTag[C]): NDArray[C] =
      x.zipMap(y)(f)

  extension [A: Ring](x: NDArray[A])
    def +(y: NDArray[A])(using ClassTag[A]): NDArray[A] =
      x.zipMap(y)(_ + _)
    def -(y: NDArray[A])(using ClassTag[A]): NDArray[A] =
      x.zipMap(y)(_ - _)
    def *(y: NDArray[A])(using ClassTag[A]): NDArray[A] =
      x.zipMap(y)(_ * _)

    def +(a: A)(using ClassTag[A]): NDArray[A] = x.map(_ + a)
    def -(a: A)(using ClassTag[A]): NDArray[A] = x.map(_ - a)
    def *(a: A)(using ClassTag[A]): NDArray[A] = x.map(_ * a)

  extension [A: Field](x: NDArray[A])
    def /(y: NDArray[A])(using ClassTag[A]): NDArray[A] =
      x.zipMap(y)(_ / _)
    def /(a: A)(using ClassTag[A]): NDArray[A] =
      x.map(_ / a)

  private def requireCompat(a: NeuroSpace, b: NeuroSpace): Unit =
    require(
      a.dims == b.dims && a.spacing == b.spacing && a.origin == b.origin,
      "NeuroSpace mismatch"
    )

  private def requireCompatSpatial(vecSpace: NeuroSpace, volSpace: NeuroSpace): Unit =
    require(
      vecSpace.spatialDims == volSpace.spatialDims &&
        vecSpace.spacing == volSpace.spacing &&
        vecSpace.origin == volSpace.origin,
      "NeuroSpace spatial mismatch"
    )

  extension [A: Ring](x: NeuroVol[A])
    def +(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      NeuroVol(x.values + y.values, x.space, x.label)
    def -(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      NeuroVol(x.values - y.values, x.space, x.label)
    def *(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      NeuroVol(x.values * y.values, x.space, x.label)

    def +(a: A)(using ClassTag[A]): NeuroVol[A] =
      NeuroVol(x.values + a, x.space, x.label)
    def -(a: A)(using ClassTag[A]): NeuroVol[A] =
      NeuroVol(x.values - a, x.space, x.label)
    def *(a: A)(using ClassTag[A]): NeuroVol[A] =
      NeuroVol(x.values * a, x.space, x.label)

  extension [A: Field](x: NeuroVol[A])
    def /(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      NeuroVol(x.values / y.values, x.space, x.label)
    def /(a: A)(using ClassTag[A]): NeuroVol[A] =
      NeuroVol(x.values / a, x.space, x.label)

  extension [A: Ring](x: NeuroVec[A])
    def +(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      NeuroVec(x.values + y.values, x.space, x.label)
    def -(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      NeuroVec(x.values - y.values, x.space, x.label)
    def *(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      NeuroVec(x.values * y.values, x.space, x.label)

    def +(a: A)(using ClassTag[A]): NeuroVec[A] =
      NeuroVec(x.values + a, x.space, x.label)
    def -(a: A)(using ClassTag[A]): NeuroVec[A] =
      NeuroVec(x.values - a, x.space, x.label)
    def *(a: A)(using ClassTag[A]): NeuroVec[A] =
      NeuroVec(x.values * a, x.space, x.label)

    def +(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      val spatialNels = x.space.spatialDims.product
      val out = NArray.ofSize[A](x.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(i) + v.values.data(linSpatial)
        i += 1
      NeuroVec.fromLinear(out, x.space, x.label)

    def -(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      val spatialNels = x.space.spatialDims.product
      val out = NArray.ofSize[A](x.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(i) - v.values.data(linSpatial)
        i += 1
      NeuroVec.fromLinear(out, x.space, x.label)

    def *(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      val spatialNels = x.space.spatialDims.product
      val out = NArray.ofSize[A](x.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(i) * v.values.data(linSpatial)
        i += 1
      NeuroVec.fromLinear(out, x.space, x.label)

  extension [A: Field](x: NeuroVec[A])
    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      NeuroVec(x.values / y.values, x.space, x.label)
    def /(a: A)(using ClassTag[A]): NeuroVec[A] =
      NeuroVec(x.values / a, x.space, x.label)

    def /(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      val spatialNels = x.space.spatialDims.product
      val out = NArray.ofSize[A](x.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(i) / v.values.data(linSpatial)
        i += 1
      NeuroVec.fromLinear(out, x.space, x.label)

  extension [A: Ring](x: ClusteredNeuroVec[A])
    def +(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts + y.ts, x.clMap, x.space, x.label)
    def -(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts - y.ts, x.clMap, x.space, x.label)
    def *(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts * y.ts, x.clMap, x.space, x.label)

    def +(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts + a, x.clMap, x.space, x.label)
    def -(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts - a, x.clMap, x.space, x.label)
    def *(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts * a, x.clMap, x.space, x.label)

  extension [A: Field](x: ClusteredNeuroVec[A])
    def /(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts / y.ts, x.clMap, x.space, x.label)
    def /(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts / a, x.clMap, x.space, x.label)

  extension [A: Ring](x: NeuroVol[A])
    def +(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      y + x
    def -(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(y.space, x.space)
      val spatialNels = y.space.spatialDims.product
      val out = NArray.ofSize[A](y.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(linSpatial) - y.values.data(i)
        i += 1
      NeuroVec.fromLinear(out, y.space, y.label)
    def *(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      y * x

  extension [A: Field](x: NeuroVol[A])
    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(y.space, x.space)
      val spatialNels = y.space.spatialDims.product
      val out = NArray.ofSize[A](y.values.data.length)
      var i = 0
      while i < out.length do
        val linSpatial = i % spatialNels
        out(i) = x.values.data(linSpatial) / y.values.data(i)
        i += 1
      NeuroVec.fromLinear(out, y.space, y.label)

  extension (s: NeuroSpace)
    def reorient(orientation: Orientation3D): NeuroSpace =
      Orientation.reorient(s, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): NeuroSpace =
      Orientation.reorient(s, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): NeuroSpace =
      Orientation.reorient(s, orient)

  extension [A](v: NeuroVol[A])
    def reorient(orientation: Orientation3D): NeuroVol[A] =
      Orientation.reorient(v, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): NeuroVol[A] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): NeuroVol[A] =
      Orientation.reorient(v, orient)

  extension (v: NeuroVol[Int])
    def mapValues[B](lookup: Map[Int, B], default: B)(using ClassTag[B]): NeuroVol[B] =
      v.map(i => lookup.getOrElse(i, default))

    @targetName("mapValuesStringKeys")
    def mapValues[B](lookup: Map[String, B], default: B)(using ClassTag[B]): NeuroVol[B] =
      val parsed =
        lookup.map { case (k, value) =>
          val key =
            k.toIntOption.getOrElse {
              throw new IllegalArgumentException("mapValues: lookup keys must be numeric")
            }
          (key, value)
        }
      v.mapValues(parsed, default)

  extension [A](v: NeuroVec[A])
    def reorient(orientation: Orientation3D): NeuroVec[A] =
      Orientation.reorient(v, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): NeuroVec[A] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): NeuroVec[A] =
      Orientation.reorient(v, orient)

  extension (c: ClusteredNeuroVol)
    def reorient(orientation: Orientation3D): ClusteredNeuroVol =
      Orientation.reorient(c, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): ClusteredNeuroVol =
      Orientation.reorient(c, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): ClusteredNeuroVol =
      Orientation.reorient(c, orient)

  extension (v: NeuroVol[Double])
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): NeuroVol[Double] =
      Resample.resampleTo(v, target)

    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): NeuroVol[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): NeuroVol[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String, engine: String)(using Resample.HasSpace[T]): NeuroVol[Double] =
      Resample.resampleTo(v, target, method, engine)

  extension (v: NeuroVec[Double])
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target)

    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String, engine: String)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target, method, engine)

  extension (c: ClusteredNeuroVol)
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): ClusteredNeuroVol =
      Resample.resampleTo(c, target)

    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): ClusteredNeuroVol =
      Resample.resampleTo(c, target, method)

    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): ClusteredNeuroVol =
      Resample.resampleTo(c, target, method)

    def resampleTo[T](target: T, method: String, engine: String)(using Resample.HasSpace[T]): ClusteredNeuroVol =
      Resample.resampleTo(c, target, method, engine)

  private def unionSparse[A](
    x: SparseNeuroVec[A],
    y: SparseNeuroVec[A]
  )(op: (A, A) => A)(using Ring[A], ClassTag[A]): SparseNeuroVec[A] =
    requireCompat(x.space, y.space)

    def toVec(arr: NArray[Int]): Vector[Int] =
      Vector.tabulate(arr.length)(i => arr(i))

    val idx1 = toVec(x.map.indices)
    val idx2 = toVec(y.map.indices)
    val unionIdx = (idx1 ++ idx2).distinct.sorted
    require(unionIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")

    val tLen = x.space.dims(3)
    val zero = summon[Ring[A]].zero
    val ret = narr.NArray.ofSize[A](tLen * unionIdx.length)
    val keep = Array.ofDim[Boolean](unionIdx.length)

    var c = 0
    while c < unionIdx.length do
      val lin = unionIdx(c)
      val p1 = x.map.lookup(lin)
      val p2 = y.map.lookup(lin)
      var t = 0
      var anyNz = false
      while t < tLen do
        val v1 = if p1 >= 0 then x.data(t, p1) else zero
        val v2 = if p2 >= 0 then y.data(t, p2) else zero
        val r = op(v1, v2)
        ret(t + c * tLen) = r
        if r != zero then anyNz = true
        t += 1
      keep(c) = anyNz
      c += 1

    val keptIdx = unionIdx.zipWithIndex.collect { case (lin, i) if keep(i) => lin }
    require(keptIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")

    val keptArr = NArrayUtil.fromArray(keptIdx.toArray)
    val newMask = Mask.fromIndices(x.space.spatialSpace, keptArr)
    val newMap = IndexLookupVol(x.space, keptArr)

    val filtered = narr.NArray.ofSize[A](tLen * keptIdx.length)
    var newCol = 0
    c = 0
    while c < unionIdx.length do
      if keep(c) then
        narr.NArray.copy(ret, c * tLen, filtered, newCol * tLen, tLen)
        newCol += 1
      c += 1

    SparseNeuroVec(NDArray(filtered, Vector(tLen, keptIdx.length)), x.space, newMask, newMap, x.label)

  extension [A: Ring](x: SparseNeuroVec[A])
    def +(y: SparseNeuroVec[A])(using ClassTag[A]): SparseNeuroVec[A] =
      unionSparse(x, y)(_ + _)
    def -(y: SparseNeuroVec[A])(using ClassTag[A]): SparseNeuroVec[A] =
      unionSparse(x, y)(_ - _)
    def *(y: SparseNeuroVec[A])(using ClassTag[A]): SparseNeuroVec[A] =
      unionSparse(x, y)(_ * _)

    def +(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense + y
    def -(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense - y
    def *(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense * y

    def +(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense + v
    def -(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense - v
    def *(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense * v

  extension [A: Field](x: SparseNeuroVec[A])
    def /(y: SparseNeuroVec[A])(using ClassTag[A]): SparseNeuroVec[A] =
      unionSparse(x, y)(_ / _)

    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense / y
    def /(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense / v

  extension [A: Ring](x: NeuroVec[A])
    def +(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x + y.toDense
    def -(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x - y.toDense
    def *(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x * y.toDense

  extension [A: Field](x: NeuroVec[A])
    def /(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x / y.toDense

  extension (x: NeuroVol[Double])
    def summary: NeuroStats.NeuroVolSummary =
      NeuroStats.summarize(x)

  extension (x: SparseNeuroVol[Double])
    def summary: NeuroStats.NeuroVolSummary =
      NeuroStats.summarize(x)

  extension (x: NeuroVec[Double])
    def temporalMean: NeuroVol[Double] =
      NeuroStats.temporalMean(x)

    def summary: NeuroStats.NeuroVecSummary =
      NeuroStats.summarize(x)

  extension (x: SparseNeuroVec[Double])
    def temporalMean: SparseNeuroVol[Double] =
      NeuroStats.temporalMean(x)

    def summary: NeuroStats.NeuroVecSummary =
      NeuroStats.summarize(x)

  extension [A: Order](x: NeuroVol[A])
    def lt(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LT)
    def lte(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LTE)
    def gt(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GT)
    def gte(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GTE)
    def eqv(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.EQV)
    def neq(y: NeuroVol[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.NEQ)

    def lt(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    def lte(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    def gt(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    def gte(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    def eqv(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    def neq(a: A): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)

  extension [A: Order: Ring](x: SparseNeuroVol[A])
    def lt(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    def lte(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    def gt(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    def gte(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    def eqv(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    def neq(a: A)(using ClassTag[A]): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)

  extension (x: ClusteredNeuroVol)
    def lt(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    def lte(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    def gt(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    def gte(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    def eqv(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    def neq(a: Int): NeuroVol[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)

  extension [A: Order](x: NeuroVec[A])
    def lt(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LT)
    def lte(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LTE)
    def gt(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GT)
    def gte(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GTE)
    def eqv(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.EQV)
    def neq(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.NEQ)

    def lt(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    def lte(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    def gt(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    def gte(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    def eqv(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    def neq(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)
