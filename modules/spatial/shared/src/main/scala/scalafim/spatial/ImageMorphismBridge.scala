package scalafim.spatial

import scalafim.image.{
  Affine3DMorphism,
  IdentityMorphism,
  SpatialDomainId,
  SpatialMorphism as ImageSpatialMorphism
}

object ImageMorphismBridge:

  def lower(morphism: Morphism): Either[SpatialError, ImageSpatialMorphism] =
    morphism.coordinateMap match
      case CoordinateMap.Identity =>
        if morphism.source != morphism.target then
          Left(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
        else Right(IdentityMorphism(SpatialDomainId(morphism.source.value)))
      case CoordinateMap.Affine3D(matrix) =>
        Affine3DMorphism
          .make(
            SpatialDomainId(morphism.source.value),
            SpatialDomainId(morphism.target.value),
            matrix,
            morphism.cost,
            morphism.id.value
          )
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.MissingCoordinateMap(morphism.id))

  def lower(path: MorphismPath): Either[SpatialError, ImageSpatialMorphism] =
    val steps = Vector.newBuilder[ImageSpatialMorphism]
    var i = 0
    var error = Option.empty[SpatialError]
    while i < path.morphisms.length && error.isEmpty do
      lower(path.morphisms(i)) match
        case Right(step) => steps += step
        case Left(err) => error = Some(err)
      i += 1

    error match
      case Some(err) => Left(err)
      case None =>
        ImageSpatialMorphism.path(steps.result())
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
