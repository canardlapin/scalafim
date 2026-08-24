package scalafim.image

import image4s.Categorical
import image4s.Continuous
import image4s.Mask as MaskSemantics
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import scala.reflect.ClassTag
import ravel.ArithmeticDType
import ravel.DType
import ravel.FloatingDType
import ravel.{`*` as ravelTimes, `+` as ravelPlus, `-` as ravelMinus, `/` as ravelDivide}
import spire.algebra.{Field, Order, Ring}
import spire.syntax.field.*
import spire.syntax.ring.*
import scala.annotation.targetName

object Ops:

  private def requireCompat(a: SomeSampleSpace, b: SomeSampleSpace): Unit =
    GridCompatibility.requireExact(a, b)

  private def requireCompatSpatial(vecSpace: SomeSampleSpace, volSpace: SomeSampleSpace): Unit =
    GridCompatibility.requireSpatial(vecSpace, volSpace)

  extension [A: Ring: ArithmeticDType](x: SomeScalarVolume[A])
    def +(y: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      requireCompat(x.space, y.space)
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](ravelPlus(x.values)(y.values), x.space, x.label)
    def -(y: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      requireCompat(x.space, y.space)
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](ravelMinus(x.values)(y.values), x.space, x.label)
    def *(y: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      requireCompat(x.space, y.space)
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](ravelTimes(x.values)(y.values), x.space, x.label)

    def +(a: A)(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](
        ravelPlus(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )
    def -(a: A)(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](
        ravelMinus(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )
    def *(a: A)(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](
        ravelTimes(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )

  extension [A: Field: FloatingDType](x: SomeScalarVolume[A])
    def /(y: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      requireCompat(x.space, y.space)
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](ravelDivide(x.values)(y.values), x.space, x.label)
    def /(a: A)(using ValueSemantics[A, Continuous]): SomeScalarVolume[A] =
      SomeNeuroVolume.unsafeFromRavel[A, Continuous](
        ravelDivide(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )

  extension [A: Ring: ArithmeticDType](x: SomeScalarSeries[A])
    @scala.annotation.targetName("neuroVecPlusVec")
    def +(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompat(x.space, y.space)
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](ravelPlus(x.values)(y.values), x.space, x.label)
    @scala.annotation.targetName("neuroVecMinusVec")
    def -(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompat(x.space, y.space)
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](ravelMinus(x.values)(y.values), x.space, x.label)
    @scala.annotation.targetName("neuroVecTimesVec")
    def *(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompat(x.space, y.space)
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](ravelTimes(x.values)(y.values), x.space, x.label)

    @scala.annotation.targetName("neuroVecPlusScalar")
    def +(a: A)(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](
        ravelPlus(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )
    @scala.annotation.targetName("neuroVecMinusScalar")
    def -(a: A)(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](
        ravelMinus(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )
    @scala.annotation.targetName("neuroVecTimesScalar")
    def *(a: A)(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](
        ravelTimes(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )

    @scala.annotation.targetName("neuroVecPlusVol")
    def +(v: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples[A, Continuous]((voxel, _, value) => value + v(voxel))

    @scala.annotation.targetName("neuroVecMinusVol")
    def -(v: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples[A, Continuous]((voxel, _, value) => value - v(voxel))

    @scala.annotation.targetName("neuroVecTimesVol")
    def *(v: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples[A, Continuous]((voxel, _, value) => value * v(voxel))

  extension [A: Field: FloatingDType](x: SomeScalarSeries[A])
    @scala.annotation.targetName("neuroVecDivideVec")
    def /(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompat(x.space, y.space)
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](ravelDivide(x.values)(y.values), x.space, x.label)
    @scala.annotation.targetName("neuroVecDivideScalar")
    def /(a: A)(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      SomeNeuroSeries.unsafeFromRavel[A, Continuous](
        ravelDivide(x.values)(ravel.NDArray.scalar(a)),
        x.space,
        x.label
      )

    @scala.annotation.targetName("neuroVecDivideVol")
    def /(v: SomeScalarVolume[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(x.space, v.space)
      x.mapSamples[A, Continuous]((voxel, _, value) => value / v(voxel))

  extension [A: Ring: ArithmeticDType](x: SomeScalarVolume[A])
    @scala.annotation.targetName("neuroVolPlusVec")
    def +(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      y + x
    @scala.annotation.targetName("neuroVolMinusVec")
    def -(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(y.space, x.space)
      y.mapSamples[A, Continuous]((voxel, _, value) => x(voxel) - value)
    @scala.annotation.targetName("neuroVolTimesVec")
    def *(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      y * x

  extension [A: Field: FloatingDType](x: SomeScalarVolume[A])
    @scala.annotation.targetName("neuroVolDivideVec")
    def /(y: SomeScalarSeries[A])(using ValueSemantics[A, Continuous]): SomeScalarSeries[A] =
      requireCompatSpatial(y.space, x.space)
      y.mapSamples[A, Continuous]((voxel, _, value) => x(voxel) / value)

  extension (s: SomeSampleSpace)
    def reorient(orientation: Orientation3D): SomeSampleSpace =
      Orientation.reorient(s, orientation)

    def reorient(axis1: String, axis2: String, axis3: String): SomeSampleSpace =
      Orientation.reorient(s, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String]): SomeSampleSpace =
      Orientation.reorient(s, orient)

  extension [A, Sem](v: SomeNeuroVolume[A, Sem])
    def reorient(orientation: Orientation3D)(using ValueSemantics[A, Sem]): SomeNeuroVolume[A, Sem] =
      Orientation.reorient(v, orientation)

    def reorient(axis1: String, axis2: String, axis3: String)(using ValueSemantics[A, Sem]): SomeNeuroVolume[A, Sem] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    def reorient(orient: Seq[String])(using ValueSemantics[A, Sem]): SomeNeuroVolume[A, Sem] =
      Orientation.reorient(v, orient)

  extension (v: SomeLabelVolume[Int])
    def mapValues[B](lookup: Map[Int, B], default: B)(using
        ClassTag[B],
        DType[B],
        ValueSemantics[B, Categorical]
    ): SomeLabelVolume[B] =
      v.map[B, Categorical](i => lookup.getOrElse(i, default))

    @targetName("mapValuesStringKeys")
    def mapValues[B](lookup: Map[String, B], default: B)(using
        ClassTag[B],
        DType[B],
        ValueSemantics[B, Categorical]
    ): SomeLabelVolume[B] =
      val parsed =
        lookup.map { case (k, value) =>
          val key =
            k.toIntOption.getOrElse {
              throw new IllegalArgumentException("mapValues: lookup keys must be numeric")
            }
          (key, value)
        }
      v.mapValues[B](parsed, default)

  extension [A, Sem](v: SomeNeuroSeries[A, Sem])
    @scala.annotation.targetName("reorientNeuroSeriesOrientation")
    def reorient(orientation: Orientation3D)(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
      Orientation.reorient(v, orientation)

    @scala.annotation.targetName("reorientNeuroSeriesAxisLabels")
    def reorient(axis1: String, axis2: String, axis3: String)(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
      Orientation.reorient(v, Seq(axis1, axis2, axis3))

    @scala.annotation.targetName("reorientNeuroSeriesAxes")
    def reorient(orient: Seq[String])(using ValueSemantics[A, Sem]): SomeNeuroSeries[A, Sem] =
      Orientation.reorient(v, orient)

  extension (v: SomeScalarVolume[Double])
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): SomeScalarVolume[Double] =
      Resample.resampleTo(v, target)

    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): SomeScalarVolume[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): SomeScalarVolume[Double] =
      Resample.resampleTo(v, target, method)

    def resampleTo[T](target: T, method: String, engine: String)(using Resample.HasSpace[T]): SomeScalarVolume[Double] =
      Resample.resampleTo(v, target, method, engine)

  extension (v: SomeScalarSeries[Double])
    @scala.annotation.targetName("resampleNeuroSeriesDefault")
    def resampleTo[T](target: T)(using Resample.HasSpace[T]): SomeScalarSeries[Double] =
      Resample.resampleTo(v, target)

    @scala.annotation.targetName("resampleNeuroSeriesMethod")
    def resampleTo[T](target: T, method: Resample.Method)(using Resample.HasSpace[T]): SomeScalarSeries[Double] =
      Resample.resampleTo(v, target, method)

    @scala.annotation.targetName("resampleNeuroSeriesNamedMethod")
    def resampleTo[T](target: T, method: String)(using Resample.HasSpace[T]): SomeScalarSeries[Double] =
      Resample.resampleTo(v, target, method)

    @scala.annotation.targetName("resampleNeuroSeriesEngine")
    def resampleTo[T](target: T, method: String, engine: String)(using Resample.HasSpace[T]): SomeScalarSeries[Double] =
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

  extension (x: SomeScalarVolume[Double])
    def summary: NeuroStats.NeuroVolumeSummary =
      NeuroStats.summarize(x)

  extension [F <: Frame[D3], S](
      x: SelectedVolume[F, S, Double, Continuous]
  )
    def summary: NeuroStats.NeuroVolumeSummary =
      NeuroStats.summarize(x)

  extension (x: SomeScalarSeries[Double])
    def temporalMean: SomeScalarVolume[Double] =
      NeuroStats.temporalMean(x)

    def summary: NeuroStats.NeuroSeriesSummary =
      NeuroStats.summarize(x)

  extension [F <: Frame[D3], S](
      x: SelectedSeries[F, S, Double, Continuous]
  )
    def temporalMean: Either[
      SelectedImageError,
      SelectedVolume[F, S, Double, Continuous]
    ] =
      NeuroStats.temporalMean(x)

    def summary: NeuroStats.NeuroSeriesSummary =
      NeuroStats.summarize(x)

  extension [A: Order, Sem](x: SomeNeuroVolume[A, Sem])
    def lt(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LT)
    def lte(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LTE)
    def gt(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GT)
    def gte(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GTE)
    def eqv(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.EQV)
    def neq(y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.NEQ)

    def lt(a: A): SomeMaskVolume =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    def lte(a: A): SomeMaskVolume =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    def gt(a: A): SomeMaskVolume =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    def gte(a: A): SomeMaskVolume =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    def eqv(a: A): SomeMaskVolume =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    def neq(a: A): SomeMaskVolume =
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

  extension [A: Order, Sem](x: SomeNeuroSeries[A, Sem])
    @scala.annotation.targetName("neuroVecLtVec")
    def lt(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LT)
    @scala.annotation.targetName("neuroVecLteVec")
    def lte(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.LTE)
    @scala.annotation.targetName("neuroVecGtVec")
    def gt(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GT)
    @scala.annotation.targetName("neuroVecGteVec")
    def gte(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.GTE)
    @scala.annotation.targetName("neuroVecEqvVec")
    def eqv(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.EQV)
    @scala.annotation.targetName("neuroVecNeqVec")
    def neq(y: SomeNeuroSeries[A, Sem]): SomeMaskSeries =
      NeuroCompare.compare(x, y, NeuroCompare.Predicate.NEQ)

    @scala.annotation.targetName("neuroVecLtScalar")
    def lt(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LT)
    @scala.annotation.targetName("neuroVecLteScalar")
    def lte(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.LTE)
    @scala.annotation.targetName("neuroVecGtScalar")
    def gt(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GT)
    @scala.annotation.targetName("neuroVecGteScalar")
    def gte(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.GTE)
    @scala.annotation.targetName("neuroVecEqvScalar")
    def eqv(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.EQV)
    @scala.annotation.targetName("neuroVecNeqScalar")
    def neq(a: A): SomeMaskSeries =
      NeuroCompare.compare(x, a, NeuroCompare.Predicate.NEQ)
