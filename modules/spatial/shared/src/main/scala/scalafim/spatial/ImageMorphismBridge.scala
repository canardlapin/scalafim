package scalafim.spatial

import scalafim.image.{
  Affine3DMorphism,
  DenseFieldMorphism,
  DMat,
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
      case CoordinateMap.Dense3D(map) =>
        Right(map.pullback)
      case CoordinateMap.Composite3D(map) =>
        lowerComposite(morphism, map)
      case CoordinateMap.VolumeSamples(_) | CoordinateMap.SurfaceVertices(_) =>
        Left(SpatialError.UnsupportedMorphismForCompilation(morphism.id, morphism.kind))
      case CoordinateMap.Unspecified =>
        Left(SpatialError.MissingCoordinateMap(morphism.id))

  private def lowerComposite(
    morphism: Morphism,
    composite: CompositeCoordinateMap
  ): Either[SpatialError, ImageSpatialMorphism] =
    val steps = Vector.newBuilder[ImageSpatialMorphism]
    var index = 0
    var error = Option.empty[SpatialError]
    while index < composite.components.length && error.isEmpty do
      val source =
        if index == 0 then SpatialDomainId(morphism.source.value)
        else SpatialDomainId(s"${morphism.id.value}:component:$index")
      val target =
        if index == composite.components.length - 1 then SpatialDomainId(morphism.target.value)
        else SpatialDomainId(s"${morphism.id.value}:component:${index + 1}")
      val componentCost = morphism.cost / composite.components.length.toDouble
      lowerComponent(composite.components(index), source, target, componentCost, morphism.id, index) match
        case Left(err) => error = Some(err)
        case Right(step) => steps += step
      index += 1

    error match
      case Some(err) => Left(err)
      case None =>
        ImageSpatialMorphism
          .path(steps.result())
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))

  private def lowerComponent(
    component: CoordinateMap,
    source: SpatialDomainId,
    target: SpatialDomainId,
    cost: Double,
    morphismId: MorphismId,
    index: Int
  ): Either[SpatialError, ImageSpatialMorphism] =
    val tag = s"${morphismId.value}:component:$index"
    component match
      case CoordinateMap.Identity =>
        Affine3DMorphism
          .make(source, target, DMat.eye(4), cost, tag)
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
      case CoordinateMap.Affine3D(matrix) =>
        Affine3DMorphism
          .make(source, target, matrix, cost, tag)
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
      case CoordinateMap.Dense3D(map) =>
        DenseFieldMorphism
          .make(
            source,
            target,
            map.pullback.grid,
            map.pullback.field,
            map.pullback.fieldKind,
            map.pullback.interpolation,
            cost,
            tag
          )
          .left.map(err => SpatialError.CoordinateTransformFailed(err.message))
      case _ =>
        Left(SpatialError.InvalidCompositeCoordinateMap(s"component $index is not geometrically executable"))

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
