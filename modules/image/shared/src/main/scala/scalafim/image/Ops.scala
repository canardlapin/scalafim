package scalafim.image

import image4s.Continuous
import image4s.Mask as MaskSemantics
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import scala.reflect.ClassTag
import ravel.DType
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

  extension [A: Ring: DType: MigrationValueSemantics](x: NeuroVol[A])
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

  extension [A: Field: DType: MigrationValueSemantics](x: NeuroVol[A])
    def /(y: NeuroVol[A])(using ClassTag[A]): NeuroVol[A] =
      requireCompat(x.space, y.space)
      x.zipWith(y)(_ / _)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    def /(a: A)(using ClassTag[A]): NeuroVol[A] =
      x.map(_ / a)

  extension [A: Ring: DType: MigrationValueSemantics](x: NeuroVec[A])
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

  extension [A: Field: DType: MigrationValueSemantics](x: NeuroVec[A])
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

  extension [A: Ring: DType: MigrationValueSemantics](x: NeuroVol[A])
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

  extension [A: Field: DType: MigrationValueSemantics](x: NeuroVol[A])
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

  extension [A: MigrationValueSemantics](v: NeuroVol[A])
    def reorient(orientation: Orientation3D): NeuroVol[A] =
      Orientation.reorient(v, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): NeuroVol[A] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): NeuroVol[A] =
      Orientation.reorient(v, orient)

  extension (v: NeuroVol[Int])
    def mapValues[B](lookup: Map[Int, B], default: B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
    ): NeuroVol[B] =
      v.map(i => lookup.getOrElse(i, default))

    @targetName("mapValuesStringKeys")
    def mapValues[B](lookup: Map[String, B], default: B)(using
        ClassTag[B],
        DType[B],
        MigrationValueSemantics[B]
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

  extension [A: MigrationValueSemantics](v: NeuroVec[A])
    @scala.annotation.targetName("reorientNeuroVecOrientation")
    def reorient(orientation: Orientation3D): NeuroVec[A] =
      Orientation.reorient(v, orientation)

    @scala.annotation.targetName("reorientNeuroVecAxisLabels")
    def reorient(axis1: String, axis2: String, axis3: String): NeuroVec[A] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    @scala.annotation.targetName("reorientNeuroVecAxes")
    def reorient(orient: Seq[String]): NeuroVec[A] =
      Orientation.reorient(v, orient)

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

  extension [F <: Frame[D3], S, A: Ring: DType](
      x: SelectedVolume[F, S, A, Continuous]
  )
    def addExact(
        y: SelectedVolume[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedVolume[F, S, A, Continuous]
    ] =
      SelectedVolume.zipExact(x, y)(_ + _)

    def subtractExact(
        y: SelectedVolume[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedVolume[F, S, A, Continuous]
    ] =
      SelectedVolume.zipExact(x, y)(_ - _)

    def multiplyExact(
        y: SelectedVolume[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedVolume[F, S, A, Continuous]
    ] =
      SelectedVolume.zipExact(x, y)(_ * _)

  extension [F <: Frame[D3], S, A: Field: DType](
      x: SelectedVolume[F, S, A, Continuous]
  )
    def divideExact(
        y: SelectedVolume[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedVolume[F, S, A, Continuous]
    ] =
      SelectedVolume.zipExact(x, y)(_ / _)

  extension [F <: Frame[D3], S, A: Ring: DType](
      x: SelectedSeries[F, S, A, Continuous]
  )
    @targetName("selectedSeriesAddExact")
    def addExact(
        y: SelectedSeries[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedSeries[F, S, A, Continuous]
    ] =
      SelectedSeries.zipExact(x, y)(_ + _)

    @targetName("selectedSeriesSubtractExact")
    def subtractExact(
        y: SelectedSeries[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedSeries[F, S, A, Continuous]
    ] =
      SelectedSeries.zipExact(x, y)(_ - _)

    @targetName("selectedSeriesMultiplyExact")
    def multiplyExact(
        y: SelectedSeries[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedSeries[F, S, A, Continuous]
    ] =
      SelectedSeries.zipExact(x, y)(_ * _)

  extension [F <: Frame[D3], S, A: Field: DType](
      x: SelectedSeries[F, S, A, Continuous]
  )
    @targetName("selectedSeriesDivideExact")
    def divideExact(
        y: SelectedSeries[F, S, A, Continuous]
    )(using
        ValueSemantics[A, Continuous]
    ): Either[
      SelectedImageError,
      SelectedSeries[F, S, A, Continuous]
    ] =
      SelectedSeries.zipExact(x, y)(_ / _)

  extension (x: NeuroVol[Double])
    def summary: NeuroStats.NeuroVolSummary =
      NeuroStats.summarize(x)

  extension [F <: Frame[D3], S](
      x: SelectedVolume[F, S, Double, Continuous]
  )
    def summary: NeuroStats.NeuroVolSummary =
      NeuroStats.summarize(x)

  extension (x: NeuroVec[Double])
    def temporalMean: NeuroVol[Double] =
      NeuroStats.temporalMean(x)

    def summary: NeuroStats.NeuroVecSummary =
      NeuroStats.summarize(x)

  extension [F <: Frame[D3], S](
      x: SelectedSeries[F, S, Double, Continuous]
  )
    def temporalMean: Either[
      SelectedImageError,
      SelectedVolume[F, S, Double, Continuous]
    ] =
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

  extension [F <: Frame[D3], S, A: Order, Sem](
      x: SelectedVolume[F, S, A, Sem]
  )
    def lt(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)

    def lte(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)

    def gt(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)

    def gte(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)

    def eqv(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)

    def neq(a: A)(using
        DType[Boolean],
        ValueSemantics[Boolean, MaskSemantics]
    ): SelectedVolume[F, S, Boolean, MaskSemantics] =
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
