package scalafim.image

import scala.reflect.ClassTag
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import ravel.Shape
import ravel.{map, zipMapExact}
import spire.algebra.{Field, Order, Ring}
import spire.syntax.field.*
import spire.syntax.ring.*
import scala.annotation.targetName

object Ops:

  private def requireCompat(a: NeuroSpace, b: NeuroSpace): Unit =
    GridCompatibility.requireExact(a, b)

  private def requireCompatSpatial(vecSpace: NeuroSpace, volSpace: NeuroSpace): Unit =
    GridCompatibility.requireSpatial(vecSpace, volSpace)

  extension [A: Ring: DType](x: NeuroVol[A])
    def +(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ + _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    def -(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ - _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    def *(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ * _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

    def +(a: A)(using ClassTag[A]): NeuroVol[A] =
      x.map(_ + a)
    def -(a: A)(using ClassTag[A]): NeuroVol[A] =
      x.map(_ - a)
    def *(a: A)(using ClassTag[A]): NeuroVol[A] =
      x.map(_ * a)

  extension [A: Field: DType](x: NeuroVol[A])
    def /(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ / _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    def /(a: A)(using ClassTag[A]): NeuroVol[A] =
      x.map(_ / a)

  extension [A: Ring: DType](x: NeuroVec[A])
    @scala.annotation.targetName("neuroVecPlusVec")
    def +(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ + _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    @scala.annotation.targetName("neuroVecMinusVec")
    def -(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ - _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    @scala.annotation.targetName("neuroVecTimesVec")
    def *(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ * _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)

    @scala.annotation.targetName("neuroVecPlusScalar")
    def +(a: A)(using ClassTag[A]): NeuroVec[A] =
      x.map(_ + a)
    @scala.annotation.targetName("neuroVecMinusScalar")
    def -(a: A)(using ClassTag[A]): NeuroVec[A] =
      x.map(_ - a)
    @scala.annotation.targetName("neuroVecTimesScalar")
    def *(a: A)(using ClassTag[A]): NeuroVec[A] =
      x.map(_ * a)

    @scala.annotation.targetName("neuroVecPlusVol")
    def +(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples((voxel, _, value) => value + v(voxel))

    @scala.annotation.targetName("neuroVecMinusVol")
    def -(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples((voxel, _, value) => value - v(voxel))

    @scala.annotation.targetName("neuroVecTimesVol")
    def *(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples((voxel, _, value) => value * v(voxel))

  extension [A: Field: DType](x: NeuroVec[A])
    @scala.annotation.targetName("neuroVecDivideVec")
    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ / _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    @scala.annotation.targetName("neuroVecDivideScalar")
    def /(a: A)(using ClassTag[A]): NeuroVec[A] =
      x.map(_ / a)

    @scala.annotation.targetName("neuroVecDivideVol")
    def /(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples((voxel, _, value) => value / v(voxel))

  extension [A: Ring: DType](x: ClusteredNeuroVec[A])
    def +(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts.zipMapExact(y.ts)(_ + _), x.clMap, x.space, x.label)
    def -(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts.zipMapExact(y.ts)(_ - _), x.clMap, x.space, x.label)
    def *(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts.zipMapExact(y.ts)(_ * _), x.clMap, x.space, x.label)

    def +(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts.map(_ + a), x.clMap, x.space, x.label)
    def -(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts.map(_ - a), x.clMap, x.space, x.label)
    def *(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts.map(_ * a), x.clMap, x.space, x.label)

  extension [A: Field: DType](x: ClusteredNeuroVec[A])
    def /(y: ClusteredNeuroVec[A])(using ClassTag[A]): ClusteredNeuroVec[A] =
      x.requireCompat(y)
      ClusteredNeuroVec(x.cvol, x.ts.zipMapExact(y.ts)(_ / _), x.clMap, x.space, x.label)
    def /(a: A)(using ClassTag[A]): ClusteredNeuroVec[A] =
      ClusteredNeuroVec(x.cvol, x.ts.map(_ / a), x.clMap, x.space, x.label)

  extension [A: Ring: DType](x: NeuroVol[A])
    @scala.annotation.targetName("neuroVolPlusVec")
    def +(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      y + x
    @scala.annotation.targetName("neuroVolMinusVec")
    def -(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(y.space, x.space)
      y.mapSamples((voxel, _, value) => x(voxel) - value)
    @scala.annotation.targetName("neuroVolTimesVec")
    def *(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      y * x

  extension [A: Field: DType](x: NeuroVol[A])
    @scala.annotation.targetName("neuroVolDivideVec")
    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      requireCompatSpatial(y.space, x.space)
      y.mapSamples((voxel, _, value) => x(voxel) / value)

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
    def mapValues[B](lookup: Map[Int, B], default: B)(using
        ClassTag[B],
        DType[B]
    ): NeuroVol[B] =
      v.map(i => lookup.getOrElse(i, default))

    @targetName("mapValuesStringKeys")
    def mapValues[B](lookup: Map[String, B], default: B)(using
        ClassTag[B],
        DType[B]
    ): NeuroVol[B] =
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
    @scala.annotation.targetName("reorientNeuroVecOrientation")
    def reorient(orientation: Orientation3D): NeuroVec[A] =
      Orientation.reorient(v, orientation)

    @scala.annotation.targetName("reorientNeuroVecAxisLabels")
    def reorient(axis1: String, axis2: String, axis3: String): NeuroVec[A] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    @scala.annotation.targetName("reorientNeuroVecAxes")
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
    @scala.annotation.targetName("resampleNeuroVecDefault")
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target)

    @scala.annotation.targetName("resampleNeuroVecMethod")
    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target, method)

    @scala.annotation.targetName("resampleNeuroVecNamedMethod")
    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): NeuroVec[Double] =
      Resample.resampleTo(v, target, method)

    @scala.annotation.targetName("resampleNeuroVecEngine")
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
  )(op: (A, A) => A)(using Ring[A], DType[A], ClassTag[A]): SparseNeuroVec[A] =
    requireCompat(x.space, y.space)

    def toVec(arr: Array1[Int]): Vector[Int] =
      Vector.tabulate(arr.size)(i => arr(i))

    val idx1 = toVec(x.map.indices)
    val idx2 = toVec(y.map.indices)
    val unionIdx = (idx1 ++ idx2).distinct.sorted
    require(unionIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")

    val tLen = x.space.dims(3)
    val zero = summon[Ring[A]].zero
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
        if r != zero then anyNz = true
        t += 1
      keep(c) = anyNz
      c += 1

    val kept =
      unionIdx.zipWithIndex.collect { case (lin, i) if keep(i) => (lin, i) }
    val keptIdx = kept.map(_._1)
    require(keptIdx.nonEmpty, "Resulting SparseNeuroVec has no non-zero elements")

    val keptArr =
      RavelArray.fromSeq(Shape(keptIdx.length), keptIdx)
    val newMask = Mask.fromIndices(x.space.spatialSpace, keptArr)
    val newMap = IndexLookupVol(x.space, keptArr)

    val compact =
      RavelArray.tabulate[A](tLen, keptIdx.length) { (time, column) =>
        val linearVoxel = keptIdx(column)
        val leftPosition = x.support.positionOf(linearVoxel)
        val rightPosition = y.support.positionOf(linearVoxel)
        val left =
          if leftPosition >= 0 then x.data(time, leftPosition)
          else zero
        val right =
          if rightPosition >= 0 then y.data(time, rightPosition)
          else zero
        op(left, right)
      }

    SparseNeuroVec(compact, x.space, newMask, newMap, x.label)

  extension [A: Ring: DType](x: SparseNeuroVec[A])
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

    @scala.annotation.targetName("sparseNeuroVecPlusVol")
    def +(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense + v
    @scala.annotation.targetName("sparseNeuroVecMinusVol")
    def -(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense - v
    @scala.annotation.targetName("sparseNeuroVecTimesVol")
    def *(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense * v

  extension [A: Field: DType](x: SparseNeuroVec[A])
    def /(y: SparseNeuroVec[A])(using ClassTag[A]): SparseNeuroVec[A] =
      unionSparse(x, y)(_ / _)

    def /(y: NeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense / y
    @scala.annotation.targetName("sparseNeuroVecDivideVol")
    def /(v: NeuroVol[A])(using ClassTag[A]): NeuroVec[A] =
      x.toDense / v

  extension [A: Ring: DType](x: NeuroVec[A])
    def +(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x + y.toDense
    def -(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x - y.toDense
    def *(y: SparseNeuroVec[A])(using ClassTag[A]): NeuroVec[A] =
      x * y.toDense

  extension [A: Field: DType](x: NeuroVec[A])
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

  extension [A: Order: Ring: DType](x: SparseNeuroVol[A])
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
    @scala.annotation.targetName("neuroVecLtVec")
    def lt(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LT)
    @scala.annotation.targetName("neuroVecLteVec")
    def lte(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LTE)
    @scala.annotation.targetName("neuroVecGtVec")
    def gt(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GT)
    @scala.annotation.targetName("neuroVecGteVec")
    def gte(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GTE)
    @scala.annotation.targetName("neuroVecEqvVec")
    def eqv(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.EQV)
    @scala.annotation.targetName("neuroVecNeqVec")
    def neq(y: NeuroVec[A]): NeuroVec[Boolean] =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.NEQ)

    @scala.annotation.targetName("neuroVecLtScalar")
    def lt(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    @scala.annotation.targetName("neuroVecLteScalar")
    def lte(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    @scala.annotation.targetName("neuroVecGtScalar")
    def gt(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    @scala.annotation.targetName("neuroVecGteScalar")
    def gte(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    @scala.annotation.targetName("neuroVecEqvScalar")
    def eqv(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    @scala.annotation.targetName("neuroVecNeqScalar")
    def neq(a: A): NeuroVec[Boolean] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)
