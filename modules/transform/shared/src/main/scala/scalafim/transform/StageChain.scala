package scalafim.transform

import image4s.geometry.{D3, Frame, Point}
import reframe4s.core.{MapError, SpatialMap}

/** Composes pullback stages that a point travels through in order (target first, source last) via ephemeral
  * intermediate frames, and re-attaches the chain's static endpoint types. Ownership is still checked on every call.
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
      yield Typed(target, source, maps.reduceLeft((a, b) => a.andThen(b)))

  private final class Typed[T <: Frame[D3], S <: Frame[D3]](val source: T, val target: S, erased: SpatialMap[Frame[D3], Frame[D3], D3]) extends SpatialMap[T, S, D3]:
    def apply(point: Point[T, D3]): Either[MapError, Point[S, D3]] =
      for
        in <- Frame.alignOwners[D3, T, Frame[D3]](source, erased.source).flatMap(_.pointToRight(point)).left.map(MapError.Geometry(_))
        out <- erased(in)
        result <- Frame.alignOwners[D3, Frame[D3], S](erased.target, target).flatMap(_.pointToRight(out)).left.map(MapError.Geometry(_))
      yield result
