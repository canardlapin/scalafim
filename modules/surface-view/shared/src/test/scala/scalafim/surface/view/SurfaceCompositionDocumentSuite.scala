package scalafim.surface.view

import scalafim.image.world.Spaces

class SurfaceCompositionDocumentSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"$error"), identity)
  private val model = SurfaceFaceFixture.model
  private val state = SurfaceFaceFixture.state(model)
  private val frame = Spaces.MNI152NLin2009cAsym
  private val display = ok(SurfaceDisplayFrame.declare(model, SurfaceFaceFixture.Surface, frame))
  private val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
  private val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture://composition"), SurfaceContentDigest.unsafe("a" * 64))
  private val bindings = SurfaceSceneBindings(Map(SurfaceFaceFixture.Surface -> reference), Map(SurfaceFaceFixture.Layer -> reference))
  private val provenance = ok(SurfaceProvenance.make("composition-test", "1", "2026-10-14"))
  private val views = Vector.tabulate(4): i =>
    SurfaceCompositionInputView(ok(SurfaceViewId.make(s"view-$i")), SurfaceViewport((i % 2) * .5, (i / 2) * .5, .5, .5),
      state.camera, pose, Vector(SurfaceCompositionInputInstance(ok(SurfaceInstanceId.make(s"instance-$i")),
        SurfaceFaceFixture.Surface, display, translationX = i * .1)), state.lighting, state.clipping)
  private val navigation = ok(SurfaceCompositionState.make(views, Vector(Set(views(0).id, views(1).id))))

  test("four-view document roundtrip preserves camera links, instances, presentation, and compiled identity"):
    val moved = ok(navigation.navigate(views(0).id, SurfaceCompositionNavigation.OrbitBy(10, -15)))
    val document = ok(SurfaceCompositionDocument.capture(model, state, moved, bindings, provenance))
    val encoded = SurfaceCompositionCodec.encode(document)
    val decoded = ok(SurfaceCompositionCodec.decode(encoded))
    assertEquals(SurfaceCompositionCodec.encode(decoded), encoded)
    val restored = ok(decoded.restore(model, bindings, Map(SurfaceFaceFixture.Surface -> display)))
    assertEquals(restored.navigation.cameraLinks, moved.cameraLinks)
    val before = ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), moved.views))
    val after = ok(SurfaceCompositionCompiler.compile(model, restored.presentation, restored.navigation.views))
    assertEquals(after.receipt, before.receipt)
    assertEquals(after.views.map(_.instances), before.views.map(_.instances))

  test("restoration checks external digests and the caller's frame declaration"):
    val document = ok(SurfaceCompositionDocument.capture(model, state, navigation, bindings, provenance))
    val bad = reference.copy(sha256 = SurfaceContentDigest.unsafe("b" * 64))
    assert(document.restore(model, bindings.copy(surfaces = Map(SurfaceFaceFixture.Surface -> bad)),
      Map(SurfaceFaceFixture.Surface -> display)).isLeft)
    val json = SurfaceCompositionCodec.encode(document)
    val key = document.views.head.frame.id
    val altered = ok(SurfaceCompositionCodec.decode(json.replace(key, "another-frame")))
    assert(altered.restore(model, bindings, Map(SurfaceFaceFixture.Surface -> display)).isLeft)
    assert(document.restore(model, bindings, Map.empty[SurfaceId, SurfaceDisplayFrame[frame.type]]).isLeft)

  test("malformed camera links, coordinates, and schema revisions fail decoding"):
    val json = SurfaceCompositionCodec.encode(ok(SurfaceCompositionDocument.capture(model, state, navigation, bindings, provenance)))
    assert(SurfaceCompositionCodec.decode(json.replace("\"revision\":1", "\"revision\":99")).isLeft)
    assert(SurfaceCompositionCodec.decode(json.replace("\"cameraLinks\":[[\"view-0\",\"view-1\"]]", "\"cameraLinks\":[[\"view-0\",\"missing\"]]")).isLeft)
    assert(SurfaceCompositionCodec.decode(json.replaceAll("\"translation\":\\[[^\\]]+\\]", "\"translation\":[0,0]")).isLeft)
    assert(SurfaceCompositionCodec.decode(json.dropRight(1) + ",\"unknown\":true}").isLeft)
