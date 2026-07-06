package scalafim.image

import scala.collection.mutable.ArrayBuffer

final case class MorphismExecutionPlan private (
    morphism: SpatialMorphism,
    steps: Vector[SpatialMorphism],
    fusedAffinePairs: Int
):
  require(steps.nonEmpty, "execution plan must contain at least one step")

  def source: SpatialDomainId =
    morphism.source

  def target: SpatialDomainId =
    morphism.target

  def transform(points: Vector[Vector[Double]]): Vector[Vector[Double]] =
    morphism.transform(points)

object MorphismExecutionPlan:

  def make(steps: Vector[SpatialMorphism]): Either[MorphismError, MorphismExecutionPlan] =
    if steps.isEmpty then Left(MorphismError.EmptyPath)
    else
      MorphismPath.make(steps).flatMap { _ =>
        val compact = ArrayBuffer.empty[SpatialMorphism]
        var fused = 0
        var error = Option.empty[MorphismError]
        var i = 0
        while i < steps.length && error.isEmpty do
          val step = steps(i)
          step match
            case _: IdentityMorphism if steps.length > 1 =>
              ()
            case affine: Affine3DMorphism =>
              compact.lastOption match
                case Some(previous: Affine3DMorphism) =>
                  previous.andThen(affine) match
                    case Left(err) =>
                      error = Some(err)
                    case Right(fusedAffine) =>
                      compact.remove(compact.length - 1)
                      compact += fusedAffine
                      fused += 1
                case Some(previous) if previous.target != affine.source =>
                  error = Some(MorphismError.DomainMismatch(previous.target, affine.source))
                case _ =>
                  compact += affine
            case other =>
              compact.lastOption match
                case Some(previous) if previous.target != other.source =>
                  error = Some(MorphismError.DomainMismatch(previous.target, other.source))
                case _ =>
                  compact += other
          i += 1

        error match
          case Some(err) => Left(err)
          case None =>
            val planned =
              if compact.isEmpty then Vector(steps.last) else compact.toVector
            SpatialMorphism.path(planned).map { morphism =>
              MorphismExecutionPlan(morphism, planned, fused)
            }
      }

  def from(morphism: SpatialMorphism): Either[MorphismError, MorphismExecutionPlan] =
    morphism match
      case path: MorphismPath => make(path.steps)
      case other => make(Vector(other))
