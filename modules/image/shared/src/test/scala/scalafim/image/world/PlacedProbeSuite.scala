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
    // The mismatch is the capture: the decoded value is GridSpec[?], but apply requires it at the frame's own type.
    assert(
      viaApply.exists(e => e.message.contains("Found:    (decoded : scalafim.image.GridSpec[?])") && e.message.contains("Required: scalafim.image.GridSpec[")),
      clue = viaApply.map(_.message)
    )
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
    // rebind needs the alignment typed at the Sampled's own space owner; exact yields it at the singleton instead.
    assert(
      errors.exists(e => e.message.contains("SamplingAlignment[(sampled.sampleSpace") && e.message.contains("Required: image4s.SamplingAlignment[sampled.S")),
      clue = errors.map(_.message)
    )
