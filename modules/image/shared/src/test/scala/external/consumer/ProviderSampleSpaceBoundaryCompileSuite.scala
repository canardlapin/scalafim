package external.consumer

import scala.compiletime.testing.typeCheckErrors

class ProviderSampleSpaceBoundaryCompileSuite extends munit.FunSuite:
  test("provider-state aliases are not part of the external SampleSpaces API"):
    val removed = Vector(
      "package spatialDims export" ->
        typeCheckErrors("import scalafim.image.spatialDims"),
      "dims alias" ->
        typeCheckErrors(
          """
            import scalafim.image.SampleSpaces
            import scalafim.image.SampleSpaces.*
            val space = SampleSpaces(Vector(2, 2, 2))
            val dims = space.dims
          """
        ),
      "addDim reconstruction" ->
        typeCheckErrors(
          """
            import image4s.{Axis, AxisKind}
            import scalafim.image.SampleSpaces
            import scalafim.image.SampleSpaces.*
            val time = Axis.ordinal("time", AxisKind.Time, 2).toOption.get
            val series = SampleSpaces(Vector(2, 2, 2)).addDim(time)
          """
        )
    )

    removed.foreach: (name, errors) =>
      assert(errors.nonEmpty, clue = s"removed sample-space facade returned: $name")

  test("the shortest volume and series paths return provider values directly"):
    val errors = typeCheckErrors(
      """
        import image4s.{Axis, AxisKind, SomeSampleSpace}
        import scalafim.image.SampleSpaces

        val volume: SomeSampleSpace = SampleSpaces(Vector(2, 2, 2))
        val time = Axis.ordinal("time", AxisKind.Time, 2).toOption.get
        val series: SomeSampleSpace =
          volume.typed.appendNonSpatial(time).toOption.get
      """
    )

    assertEquals(errors, Nil)
