package scalafim.surface.view

import scalafim.surface.*

class SurfaceSceneVeryInflatedSuite extends munit.FunSuite:
  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val canonicalKind = "\"kind\":\"veryinflated\""

  test("a very-inflated scene saves the canonical kind and round trips to the same state"):
    val (model, state, bindings, provenance) = fixture(SurfaceKind.VeryInflated)
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val encoded = SurfaceSceneCodec.encode(document)
    val decoded = SurfaceSceneCodec.decode(encoded).toOption.get

    assertEquals(document.assets.map(_.kind), Vector(SurfaceKind.VeryInflated, SurfaceKind.VeryInflated))
    assertEquals(encoded.sliding(canonicalKind.length).count(_ == canonicalKind), 2)
    assertEquals(decoded.assets.map(_.kind), document.assets.map(_.kind))
    assertEquals(SurfaceSceneCodec.encode(decoded), encoded)
    assertEquals(decoded.restore(model, bindings).toOption, Some(state))

  test("legacy and GIFTI spellings of the saved kind decode to VeryInflated and re-encode canonically"):
    val (model, state, bindings, provenance) = fixture(SurfaceKind.VeryInflated)
    val encoded = SurfaceSceneCodec.encode(
      SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    )
    Vector("very_inflated", "VeryInflated", "very-inflated").foreach: spelling =>
      val legacy = encoded.replace(canonicalKind, s"\"kind\":\"$spelling\"")
      assertNotEquals(legacy, encoded)
      val decoded = SurfaceSceneCodec.decode(legacy).toOption.get
      assertEquals(decoded.assets.map(_.kind), Vector(SurfaceKind.VeryInflated, SurfaceKind.VeryInflated), spelling)
      assertEquals(SurfaceSceneCodec.encode(decoded), encoded, spelling)
      assertEquals(decoded.restore(model, bindings).toOption, Some(state), spelling)

  test("the display kind is part of the saved surface identity: inflated and very-inflated do not substitute"):
    val (veryModel, veryState, bindings, provenance) = fixture(SurfaceKind.VeryInflated)
    val (inflatedModel, inflatedState, _, _) = fixture(SurfaceKind.Inflated)
    val veryDocument = SurfaceSceneDocument.capture(veryModel, veryState, bindings, provenance).toOption.get
    val inflatedDocument = SurfaceSceneDocument.capture(inflatedModel, inflatedState, bindings, provenance).toOption.get

    assertNotEquals(SurfaceSceneCodec.encode(veryDocument), SurfaceSceneCodec.encode(inflatedDocument))
    assert(veryDocument.restore(inflatedModel, bindings).left.exists(_.message.contains("identity differs")))
    assert(inflatedDocument.restore(veryModel, bindings).left.exists(_.message.contains("identity differs")))

  private def fixture(
    kind: SurfaceKind
  ): (SurfaceViewerModel, SurfaceViewerState, SurfaceSceneBindings, SurfaceProvenance) =
    val model = SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, geometry(Hemisphere.Left, kind, -1.0)).toOption.get,
        SurfaceAsset.make(rightId, geometry(Hemisphere.Right, kind, 1.0)).toOption.get
      ),
      Vector.empty
    ).toOption.get
    val state = SurfaceViewer.reduce(
      model,
      SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(leftId, rightId, BilateralOrder.LeftThenRight))
    ).toOption.get
    val bindings = SurfaceSceneBindings(
      Map(
        leftId -> reference("asset://surfaces/left", 'a'),
        rightId -> reference("asset://surfaces/right", 'b')
      ),
      Map.empty
    )
    val provenance = SurfaceProvenance.make(
      "scalafim-test",
      "1.0.0",
      "2026-09-30T00:00:00Z",
      Vector("template" -> "synthetic")
    ).toOption.get
    (model, state, bindings, provenance)

  private def reference(uri: String, digit: Char): SurfaceExternalReference =
    SurfaceExternalReference(SurfaceAssetUri.unsafe(uri), SurfaceContentDigest.unsafe(digit.toString * 64))

  private def geometry(hemisphere: Hemisphere, kind: SurfaceKind, x: Double): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(x, 0.0, 0.0),
          Vector(x + 0.4, 0.0, 0.0),
          Vector(x, 0.8, 0.0),
          Vector(x, 0.0, 1.2)
        ),
        Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
      ),
      hemisphere,
      kind
    )
