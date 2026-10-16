package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfacePlaybackDocumentSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(s"$error"), identity)
  private val geometry = SurfaceFaceFixture.geometry
  private val axis = ok(SurfaceTimeAxis.uniform(3))
  private val source = ok(SurfaceTemporalField.fromMatrix(SurfaceTemporalFieldId.unsafe("time-series"),
    SurfaceMatrix.fromRows(geometry, Vector.tabulate(4)(VertexId(_)),
      Vector.tabulate(4)(i => Vector(i.toDouble, i + 10.0, i + 20.0))), axis))
  private val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture://time-series"), SurfaceContentDigest.unsafe("c" * 64))
  private val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 24),
    ScalarRamp.linear(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(255, 255, 255))))
  private val state = SurfacePlaybackState(SurfaceSeconds.unsafe(.25), SurfacePlaybackStatus.Playing,
    SurfacePlaybackRate.unsafe(-2), looping = true)
  private def document = ok(SurfacePlaybackDocument.capture(source, SurfaceFaceFixture.Layer, SurfaceFaceFixture.Surface,
    reference, mapping, SurfaceInterpolation.Linear, state))

  test("fractional playhead and controls roundtrip and reconstruct the independently known interpolated field"):
    val json = SurfacePlaybackDocument.encode(document)
    val restored = ok(SurfacePlaybackDocument.decode(json))
    assertEquals(SurfacePlaybackDocument.encode(restored), json)
    assertEquals(restored.state, state)
    val sampled = ok(restored.prepare(source, reference, mapping, SurfaceFrameCache.Disabled))
    assertEquals(sampled.receipt.sample, SurfaceFrameSample(0, 1, .25))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(SurfaceFaceFixture.Surface, geometry))), Vector(sampled.layer)))
    val selected = ok(SurfaceViewer.reduce(model, SurfaceViewerState.initial(model),
      SurfaceViewerAction.Select(SurfaceFaceFixture.Surface, VertexId(2))))
    val plan = ok(SurfaceCompiler.compile(model, selected))
    assertEqualsDouble(plan.readouts.head.layerValues.head._2.toDouble, 4.5, 0.0)

  test("restore refuses changed source binding, mapping and time axis"):
    assert(document.prepare(source, reference.copy(sha256 = SurfaceContentDigest.unsafe("d" * 64)), mapping, SurfaceFrameCache.Disabled).isLeft)
    val changed = mapping.copy(invalid = Rgba32.unsafe(1, 2, 3))
    assert(document.prepare(source, reference, changed, SurfaceFrameCache.Disabled).isLeft)
    val other = ok(SurfaceTemporalField.fromMatrix(source.id,
      SurfaceMatrix.fromRows(geometry, Vector.tabulate(4)(VertexId(_)), Vector.fill(4)(Vector(0.0, 1.0, 2.0))),
      ok(SurfaceTimeAxis.uniform(3, step = SurfaceSeconds.unsafe(2)))))
    assert(document.prepare(other, reference, mapping, SurfaceFrameCache.Disabled).isLeft)

  test("restore discards cached frames belonging to another source with the same field id"):
    val populated = ok(SurfaceFrameCache.empty(4).prefetch(source, Vector(0, 1))).cache
    val replacement = ok(SurfaceTemporalField.fromMatrix(source.id,
      SurfaceMatrix.fromRows(geometry, Vector.tabulate(4)(VertexId(_)),
        Vector.fill(4)(Vector(100.0, 120.0, 140.0))), axis))
    val replacementReference = reference.copy(sha256 = SurfaceContentDigest.unsafe("d" * 64))
    val saved = ok(SurfacePlaybackDocument.capture(replacement, SurfaceFaceFixture.Layer, SurfaceFaceFixture.Surface,
      replacementReference, mapping, SurfaceInterpolation.Linear, state))
    val sampled = ok(saved.prepare(replacement, replacementReference, mapping, populated))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(SurfaceFaceFixture.Surface, geometry))), Vector(sampled.layer)))
    val selected = ok(SurfaceViewer.reduce(model, SurfaceViewerState.initial(model),
      SurfaceViewerAction.Select(SurfaceFaceFixture.Surface, VertexId(2))))
    assertEqualsDouble(ok(SurfaceCompiler.compile(model, selected)).readouts.head.layerValues.head._2.toDouble, 105.0, 0.0)

  test("malformed or out-of-axis persisted controls are rejected"):
    val json = SurfacePlaybackDocument.encode(document)
    for changed <- Vector(json.replaceAll("\"rate\":[^,}]+", "\"rate\":0"),
      json.replace("\"playhead\":0.25", "\"playhead\":9"),
      json.replace("\"status\":\"Playing\"", "\"status\":\"unknown\""),
      json.dropRight(1) + ",\"unknown\":true}") do
      assertNotEquals(changed, json)
      assert(SurfacePlaybackDocument.decode(changed).isLeft)
