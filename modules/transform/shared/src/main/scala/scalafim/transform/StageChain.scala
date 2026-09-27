package scalafim.transform

import image4s.geometry.{D3, Frame, Point}
import reframe4s.core.{MapError, SpatialMap}
import reframe4s.field.{CoverageReportingMap, CoveredPoint}

/** Composes pullback stages that a point travels through in order (target first, source last) via ephemeral
  * intermediate frames, and re-attaches the chain's static endpoint types. Ownership is still checked on every call.
  * The chain reports coverage per evaluation: the first stage whose boundary policy supplied a value is not hidden.
  */
private[transform] object StageChain:
  type Stage = (Frame[D3], Frame[D3]) => Either[TransformError, SpatialMap[Frame[D3], Frame[D3], D3]]

  def compose[S <: Frame[D3], T <: Frame[D3]](target: T, source: S, label: String, stages: Vector[Stage]): Either[TransformError, SpatialMap[T, S, D3]] =
    if stages.isEmpty then Left(TransformError.Invalid(s"$label has no stages"))
    else
      for
        middle <- (1 until stages.size).toVector.foldLeft[Either[TransformError, Vector[Frame[D3]]]](Right(Vector.empty)): (acc, i) =>
          acc.flatMap(done => Frame.named[D3](s"$label-stage-$i").left.map(TransformError.Geometry(_)).map(done :+ _))
        boundaries = (target +: middle) :+ source
        maps <- stages.zipWithIndex.foldLeft[Either[TransformError, Vector[SpatialMap[Frame[D3], Frame[D3], D3]]]](Right(Vector.empty)): (acc, stage) =>
          val (build, i) = stage
          acc.flatMap(done => build(boundaries(i), boundaries(i + 1)).map(done :+ _))
      yield Typed(target, source, maps.map(CoverageReportingMap.lift).reduceLeft((a, b) => CoverageReportingMap.compose(a, b)))

  private final class Typed[T <: Frame[D3], S <: Frame[D3]](val source: T, val target: S, erased: CoverageReportingMap[Frame[D3], Frame[D3], D3])
      extends CoverageReportingMap[T, S, D3]:
    def applyWithCoverage(point: Point[T, D3]): Either[MapError, CoveredPoint[S, D3]] =
      for
        in <- Frame.alignOwners[D3, T, Frame[D3]](source, erased.source).flatMap(_.pointToRight(point)).left.map(MapError.Geometry(_))
        out <- erased.applyWithCoverage(in)
        result <- Frame.alignOwners[D3, Frame[D3], S](erased.target, target).flatMap(_.pointToRight(out.point)).left.map(MapError.Geometry(_))
      yield CoveredPoint(result, out.outcome)
