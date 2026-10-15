package scalafim.surface.view

class SurfaceScalarKeyQualificationFixtureSuite extends munit.FunSuite:
  test("supplementary scalar-key variants retain declared endpoints, palette, and threshold semantics"):
    val baseline = SurfaceScalarKeyQualificationFixture.prepare(SurfaceScalarKeyQualificationFixture.Baseline).fold(fail(_), identity)
    val changed = SurfaceScalarKeyQualificationFixture.prepare(SurfaceScalarKeyQualificationFixture.Changed).fold(fail(_), identity)
    assertEquals(baseline.endpoints.map(_.value), Vector(-4.0, 8.0))
    assertEquals(baseline.endpoints.map(_.position), Vector(0.0, 1.0))
    assertEquals(changed.endpoints.map(_.value), Vector(-8.0, 16.0))
    assertEquals(changed.endpoints.map(_.position), Vector(0.0, 1.0))
    assertEquals(changed.threshold.map(_.map(_.value)), Some(Vector(-2.0, 4.0)))
    assertEquals(changed.threshold.map(_.map(_.position)), Some(Vector(.25, .5)))
    val midpoint = changed.legend.content match
      case SurfaceLegendContent.Continuous(value) => value.mapping.color(4.0)
      case other => fail(s"expected scalar legend, got $other")
    // The reference uses floor(t * (palette.length - 1)); .5 selects 127.
    assertEquals(midpoint, intaglio.Rgba32.unsafe(127, 180, 105))
    assertNotEquals(baseline.legend.canonicalKey, changed.legend.canonicalKey)

  test("publication presets retain their declared two output sizes and a bound scalar key"):
    for
      variant <- SurfaceScalarKeyQualificationFixture.Variants
      preset <- SurfaceScalarKeyQualificationFixture.Presets
      publication = SurfaceScalarKeyQualificationFixture.publication(variant, preset).fold(fail(_), identity)
    do
      assertEquals(publication.receipt.publication.width, preset.width)
      assertEquals(publication.receipt.publication.height, preset.height)
      assertEquals(publication.receipt.legends.length, 1)
      assertEquals(publication.receipt.legends.head.sources.map(_.layer), Vector(SurfaceScalarKeyQualificationFixture.Layer))
