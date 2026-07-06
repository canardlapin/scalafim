package scalafim.image

import slash.vector.Vec

final case class AxisSet private (axes: Vector[Axis]):
  def ndim: Int = axes.length
  def apply(i: Int): Axis = axes(i)

  def drop(dim: Int): AxisSet =
    if dim < 0 || dim >= axes.length then this
    else AxisSet(axes.patch(dim, Nil, 1))

  def spatialAxes: Vector[Axis] = axes.take(3)
  def additionalAxes: Vector[Axis] = axes.drop(3)

  def directions: Vector[Vec[3]] = axes.flatMap(_.direction)

object AxisSet:
  val empty: AxisSet = AxisSet(Vector.empty)

  def apply(axes: Axis*): AxisSet =
    AxisSet(axes.toVector)

  def standard(ndim: Int): AxisSet =
    val spatial = Vector(Axis.LeftRight, Axis.PosteriorAnterior, Axis.InferiorSuperior)
    val base =
      if ndim <= 0 then Vector.empty
      else if ndim <= 3 then spatial.take(ndim)
      else spatial ++ Vector.fill(ndim - 3)(Axis.NoneAxis)

    val withTime =
      if ndim >= 4 then base.updated(3, Axis.Time) else base

    AxisSet(withTime)
