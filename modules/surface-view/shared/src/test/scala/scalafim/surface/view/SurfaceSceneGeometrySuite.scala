package scalafim.surface.view

import scalafim.surface.*

class SurfaceSceneGeometrySuite extends munit.FunSuite:
  private val id = SurfaceId.unsafe("left")
  private def ref(name: String, digit: String): SurfaceExternalReference =
    SurfaceExternalReference(SurfaceAssetUri.unsafe(s"asset://$name"), SurfaceContentDigest.unsafe(digit * 64))
  private val pialReference = ref("pial", "a")
  private val inflatedReference = ref("inflated", "b")
  private val bindings = SurfaceSceneBindings(Map(id -> pialReference), Map.empty,
    Map(id -> Map(SurfaceKind.Pial -> pialReference, SurfaceKind.Inflated -> inflatedReference)))
  private val provenance = SurfaceProvenance.make("test", "1", "2026-09-12T00:00:00Z").toOption.get
  private def geometry(kind: SurfaceKind, scale: Double): SurfaceGeometry = SurfaceGeometry(
    TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(scale, 0.0, 0.0), Vector(0.0, scale, 0.0)),
      Vector((0, 1, 2))), Hemisphere.Left, kind)
  private val pial = geometry(SurfaceKind.Pial, 1)
  private val inflated = geometry(SurfaceKind.Inflated, 2)
  private val family = SurfaceSet.of(SurfaceKind.Pial, pial, SurfaceKind.Inflated -> inflated)
  private val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, family).toOption.get), Vector.empty).toOption.get
  private def reduce(state: SurfaceViewerState, action: SurfaceViewerAction): SurfaceViewerState =
    SurfaceViewer.reduce(model, state, action).toOption.get

  test("fixed inflated scene roundtrips full native state and rendered geometry"):
    val initial = SurfaceViewerState.initial(model)
    val geometry = reduce(initial, SurfaceViewerAction.SetGeometryState(id, SurfaceKind.Inflated))
    val selected = reduce(geometry, SurfaceViewerAction.Select(id, VertexId(1)))
    val state = reduce(selected, SurfaceViewerAction.SetOrbit(SurfaceOrbit.unsafe(12, -7)))
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    assertEquals(document.revision, SurfaceDocumentRevision.V8)
    val encoded = SurfaceSceneCodec.encode(document)
    val restored = SurfaceSceneCodec.decode(encoded).flatMap(_.restore(model, bindings)).toOption.get
    assertEquals(restored, state)
    assertEquals(SurfaceCompiler.compile(model, restored).toOption.get.meshes.map(_.geometryKey),
      SurfaceCompiler.compile(model, state).toOption.get.meshes.map(_.geometryKey))
    assertEquals(SurfaceSceneCodec.encode(SurfaceSceneCodec.decode(encoded).toOption.get), encoded)

  test("morph fraction preserves interpolated physical positions and selection"):
    val start = reduce(SurfaceViewerState.initial(model), SurfaceViewerAction.BeginGeometryMorph(id, SurfaceKind.Inflated))
    val state = reduce(start, SurfaceViewerAction.SetGeometryMorphFraction(id, SurfaceMorphFraction.unsafe(0.25)))
    val saved = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val restored = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(saved)).flatMap(_.restore(model, bindings)).toOption.get
    assertEquals(restored, state)
    val mesh = SurfaceCompiler.compile(model, restored).toOption.get.meshes.head
    // Independent affine morph oracle: vertex 1 x = 1 + .25*(2-1).
    assertEqualsDouble(mesh.positions(3).toDouble, 1.25, 1e-6)

  test("missing family binding and stale variant digest fail closed"):
    val state = SurfaceViewerState.initial(model)
    assert(SurfaceSceneDocument.capture(model, state, bindings.copy(geometries = Map.empty), provenance).isLeft)
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val stale = bindings.copy(geometries = Map(id -> bindings.geometries(id).updated(SurfaceKind.Inflated, ref("inflated", "c"))))
    assert(document.restore(model, stale).left.exists(_.message.contains("geometry-family")))
    val missingModel = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, pial).toOption.get), Vector.empty).toOption.get
    assert(document.restore(missingModel, bindings).isLeft)

  test("unknown geometry bindings and omitted or extra variants refuse fixed and morph scenes"):
    val initial = SurfaceViewerState.initial(model)
    val fixed = reduce(initial, SurfaceViewerAction.SetGeometryState(id, SurfaceKind.Inflated))
    val morph = reduce(reduce(initial, SurfaceViewerAction.BeginGeometryMorph(id, SurfaceKind.Inflated)),
      SurfaceViewerAction.SetGeometryMorphFraction(id, SurfaceMorphFraction.unsafe(0.25)))
    val invalid = Vector(
      bindings.copy(geometries = bindings.geometries.updated(SurfaceId.unsafe("unknown"), bindings.geometries(id))),
      bindings.copy(geometries = Map(id -> (bindings.geometries(id) - SurfaceKind.Inflated))),
      bindings.copy(geometries = Map(id -> bindings.geometries(id).updated(SurfaceKind.White, ref("white", "d")))))
    Vector(fixed, morph).foreach { state =>
      val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
      invalid.foreach { rejected =>
        assert(SurfaceSceneDocument.capture(model, state, rejected, provenance).isLeft)
        assert(document.restore(model, rejected).isLeft)
      }
    }

  test("revision 8 requires exactly one geometry state per declared surface"):
    val document = SurfaceSceneDocument.capture(model, SurfaceViewerState.initial(model), bindings, provenance).toOption.get
    val entry = document.geometryStates.head
    val json = SurfaceSceneCodec.encode(document)
    val encodedEntry = SurfaceSceneGeometryCodec.encode(entry).render
    Vector(Vector.empty, Vector(entry, entry), Vector(entry.copy(surface = SurfaceId.unsafe("unknown")))).foreach { entries =>
      val replacement = entries.map(SurfaceSceneGeometryCodec.encode(_).render).mkString("[", ",", "]")
      assert(SurfaceSceneCodec.decode(json.replace(s"\"geometryStates\":[$encodedEntry]", s"\"geometryStates\":$replacement")).isLeft)
    }
    assert(SurfaceSceneCodec.decode(json.replace("\"geometryStates\":", "\"missingGeometryStates\":")).isLeft)

  test("legacy revision 7 restores declared default without inventing a variant"):
    val state = reduce(SurfaceViewerState.initial(model), SurfaceViewerAction.SetGeometryState(id, SurfaceKind.Inflated))
    val captured = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val encodedEntry = SurfaceSceneGeometryCodec.encode(captured.geometryStates.head).render
    val json = SurfaceSceneCodec.encode(captured).replace("\"revision\":8", "\"revision\":7")
      .replace(s",\"geometryStates\":[$encodedEntry]", "")
    assert(!json.contains("geometryStates"))
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(SurfaceSceneCodec.encode(decoded), json)
    assertEquals(decoded.restore(model, bindings).toOption.get.geometryPresentations(id),
      SurfaceGeometryPresentation.Fixed(SurfaceKind.Pial))

  test("unknown presentation, undeclared variant, malformed fraction and duplicate entries refuse"):
    val start = reduce(SurfaceViewerState.initial(model), SurfaceViewerAction.BeginGeometryMorph(id, SurfaceKind.Inflated))
    val state = reduce(start, SurfaceViewerAction.SetGeometryMorphFraction(id, SurfaceMorphFraction.unsafe(0.25)))
    val json = SurfaceSceneCodec.encode(SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get)
    assert(SurfaceSceneCodec.decode(json.replace("\"kind\":\"morphing\"", "\"kind\":\"reveal-lens\"")).isLeft)
    assert(SurfaceSceneCodec.decode(json.replace("\"fraction\":0.25", "\"fraction\":1.25")).isLeft)
    assert(SurfaceSceneCodec.decode(json.replace("\"to\":\"inflated\"", "\"to\":\"white\"")).isLeft)
    assert(SurfaceSceneCodec.decode(json.replace("\"revision\":8", "\"revision\":7")).isLeft)
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val entry = SurfaceSceneGeometryCodec.encode(document.geometryStates.head).render
    assert(SurfaceSceneCodec.decode(json.replace(s"\"geometryStates\":[$entry]", s"\"geometryStates\":[$entry,$entry]")).isLeft)
