package external.consumer

import scala.compiletime.testing.typeCheckErrors

class ProviderTransformBoundaryCompileSuite extends munit.FunSuite:
  test("the removed image transform hierarchy cannot be imported"):
    val removed = Vector(
      "SpatialMorphism" ->
        typeCheckErrors("import scalafim.image.SpatialMorphism"),
      "Affine3DMorphism" ->
        typeCheckErrors("import scalafim.image.Affine3DMorphism"),
      "IdentityMorphism" ->
        typeCheckErrors("import scalafim.image.IdentityMorphism"),
      "DenseFieldMorphism" ->
        typeCheckErrors("import scalafim.image.DenseFieldMorphism"),
      "SpatialDomainId" ->
        typeCheckErrors("import scalafim.image.SpatialDomainId"),
      "MorphismExecutionPlan" ->
        typeCheckErrors("import scalafim.image.MorphismExecutionPlan"),
      "DenseFieldInterpolationPlan" ->
        typeCheckErrors("import scalafim.image.DenseFieldInterpolationPlan"),
      "DenseFieldInverse" ->
        typeCheckErrors("import scalafim.image.DenseFieldInverse"),
      "JacobianField" ->
        typeCheckErrors("import scalafim.image.JacobianField"),
      "JacobianMode" ->
        typeCheckErrors("import scalafim.image.JacobianMode"),
      "MorphismFields" ->
        typeCheckErrors("import scalafim.image.MorphismFields")
    )

    removed.foreach: (name, errors) =>
      assert(
        errors.nonEmpty,
        clue = s"removed transform API returned: $name"
      )

  test("the image boundary consumes the provider spatial-map contract"):
    val errors = typeCheckErrors(
      """
        import image4s.geometry.{D3, Frame}
        import reframe4s.core.SpatialMap
        import scalafim.image.{SpatialPullback, SpatialPullbacks}

        def consume(map: SpatialPullback): SpatialMap[Frame[D3], Frame[D3], D3] =
          map

        def applyAt(
          map: SpatialPullback,
          point: scalafim.image.SpatialPoint
        ) = SpatialPullbacks.transform(map, point)
      """
    )

    assertEquals(errors, Nil)
