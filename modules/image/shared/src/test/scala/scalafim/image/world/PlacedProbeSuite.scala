package scalafim.image.world

import scala.compiletime.testing.typeCheckErrors

/** Compiler facts the frame-typing design relies on, probed with `typeCheckErrors` so a compiler or provider change
  * that invalidates one fails here rather than silently changing which encodings are sound.
  */
class PlacedProbeSuite extends munit.FunSuite:
  test("a runtime-decoded GridSpec[?] packages only through Placed.of, not the singleton-typed Placed.apply"):
    val viaApply = typeCheckErrors(
      """
        import scalafim.image.GridSpec
        val decoded: GridSpec[?] = GridSpec.identity(Vector(2, 2, 2))
        Placed[GridSpec](decoded.frame)(decoded)
      """
    )
    assert(viaApply.exists(_.message.contains("Required")), clue = viaApply.map(_.message))
    val viaOf = typeCheckErrors(
      """
        import scalafim.image.GridSpec
        val decoded: GridSpec[?] = GridSpec.identity(Vector(2, 2, 2))
        Placed.of(decoded.frame)(decoded)
      """
    )
    assertEquals(viaOf.map(_.message), Nil)

  test("an existential Sampled cannot be rebound through SamplingAlignment.exact, so plans re-wrap instead"):
    val errors = typeCheckErrors(
      """
        import image4s.{Continuous, SampleSpace, Sampled, SamplingAlignment}
        import image4s.geometry.{D3, Frame}
        import ravel.Rank
        def probe[S <: Frame[D3]](sampled: Sampled[? <: SampleSpace[?, D3], Double, Continuous, Rank[3]], admitted: SampleSpace[S, D3]) =
          SamplingAlignment.exact(sampled.sampleSpace, admitted).map(alignment => sampled.rebind(alignment))
      """
    )
    assert(errors.exists(_.message.contains("Required")), clue = errors.map(_.message))
