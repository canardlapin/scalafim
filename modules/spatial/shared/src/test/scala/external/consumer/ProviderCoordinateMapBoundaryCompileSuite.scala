package external.consumer

import scala.compiletime.testing.typeCheckErrors

class ProviderCoordinateMapBoundaryCompileSuite extends munit.FunSuite:
  test("parallel spatial geometric variants cannot be imported or constructed"):
    val removed = Vector(
      "DenseCoordinateMap" ->
        typeCheckErrors("import scalafim.spatial.DenseCoordinateMap"),
      "CompositeCoordinateMap" ->
        typeCheckErrors("import scalafim.spatial.CompositeCoordinateMap"),
      "CoordinateMap.Affine3D" ->
        typeCheckErrors("val value = scalafim.spatial.CoordinateMap.Affine3D"),
      "CoordinateMap.Dense3D" ->
        typeCheckErrors("val value = scalafim.spatial.CoordinateMap.Dense3D"),
      "CoordinateMap.Composite3D" ->
        typeCheckErrors("val value = scalafim.spatial.CoordinateMap.Composite3D"),
      "ProviderMapBinding.copy" ->
        typeCheckErrors(
          """
            import scalafim.image.SpatialPullback
            import scalafim.spatial.ProviderMapBinding

            def forge(binding: ProviderMapBinding, replacement: SpatialPullback) =
              binding.copy(pullback = replacement)
          """
        )
    )

    removed.foreach: (name, errors) =>
      assert(errors.nonEmpty, clue = s"parallel geometric API returned: $name")

  test("spatial routing exposes one provider-map payload"):
    val errors = typeCheckErrors(
      """
        import scalafim.image.SpatialPullback
        import scalafim.spatial.{CoordinateMap, ProviderMapBinding}

        def provider(map: CoordinateMap): Option[SpatialPullback] =
          map match
            case CoordinateMap.Geometric(binding: ProviderMapBinding) =>
              Some(binding.pullback)
            case _ => None
      """
    )

    assertEquals(errors, Nil)
