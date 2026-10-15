package scalafim.surface.view

class SurfaceQualificationFixtureSuite extends munit.FunSuite:
  private val camera = """{"projection":"orthographic","eye":[0,0,4],"target":[0,0,0],"up":[1,0,0],"left":-2,"right":6,"bottom":-3,"top":1,"near":0.5,"far":10,"aspect":1}"""
  private def input(layers: String = "[]", extra: String = ""): String =
    s"""{"width":100,"height":80,"camera":$camera,"profile":{"name":"controlled-unlit","ambientIntensity":3.141592653589793,"directionalIntensity":0},"timeSeconds":0,"surfaces":[{"id":"plane","hemisphere":"left","vertices":[[-1,-1,0],[1,-1,0],[-1,1,0],[1,1,0]],"faces":[[0,1,2],[1,3,2]],"layers":$layers$extra}]}"""

  test("volume display samples source data through the declared affine and voxel kernel"):
    val voxels = (for k <- 0 until 5; j <- 0 until 4; i <- 0 until 3 yield i + 2 * j + 3 * k).mkString(",")
    def layer(method: String) = s"""[{"id":"volume","kind":"volume","dims":[3,4,5,1],"frame":0,"depthOffsetsMm":[0],"reducer":"mean","outside":"transparent","nearestTie":"floor(x + .5)","matrixOrder":"row-major","interpolation":"$method","voxelToWorld":[1,0,0,-1.25,0,1,0,-1.5,0,0,1,-1.75,0,0,0,1],"voxelValues":[$voxels],"mapping":{"range":[0,20],"threshold":[0,0],"palette":[[0,0,1],[1,0,0]],"opacity":0.85}}]"""
    // Left lower vertex maps to (.25,.5,1.75). Right vertices lie outside the
    // linear centre hull, but inside the nearest half-voxel support.
    val linear = SurfaceQualificationFixture.decode(input(layer("linear"))).fold(fail(_), identity)
    val nearest = SurfaceQualificationFixture.decode(input(layer("nearest"))).fold(fail(_), identity)
    val lv = Vector.tabulate(4)(i => linear.model.layers.head.describe(i, 0).toDouble)
    val nv = Vector.tabulate(4)(i => nearest.model.layers.head.describe(i, 0).toDouble)
    assertEqualsDouble(lv(0), .25 + 2 * .5 + 3 * 1.75, 1e-12)
    assertEqualsDouble(nv(0), 8, 1e-12)
    assert(lv(1).isNaN)
    assertEqualsDouble(nv(1), 10, 1e-12)
    val readout = linear.numericReadouts.head
    assertEquals(readout._1, SurfaceId.unsafe("plane"))
    assertEquals(readout._2, SurfaceLayerId.unsafe("plane/volume"))
    assertEqualsDouble(readout._3(0), lv(0), 0.0)
    assert(readout._3(1).isNaN)

  test("source coordinates and IDs survive an explicit rotated asymmetric camera"):
    val prepared = SurfaceQualificationFixture.decode(input()).fold(fail(_), identity)
    assertEquals(prepared.sourceSummaries.map(s => (s.surface.value, s.vertices, s.faces)), Vector(("plane", 4, 2)))
    val mesh = prepared.plan.resources.meshes.values.head
    assertEqualsDouble(mesh.positions(0), -1.0, 0.0)
    assertEqualsDouble(mesh.positions(1), -1.0, 0.0)
    assertEquals(mesh.indices(4), 3)
    val packet = prepared.plan.views.head.camera
    assertEqualsDouble(packet.viewMatrix(1), -1.0, 1e-6)
    assertEqualsDouble(packet.projectionMatrix(3), -.5, 1e-6)
    assertEqualsDouble(packet.projectionMatrix(7), .5, 1e-6)

  test("C04 views share source resources while retaining independent cameras and occurrences"):
    def view(id: String, viewport: String, eye: String) =
      s"""{"id":"$id","viewport":$viewport,"camera":${camera.replace("\"eye\":[0,0,4]", s"\"eye\":$eye")},"surfaces":["plane"]}"""
    def scene(secondEye: String) =
      input().dropRight(1) + s""", "views":[${view("northwest", "[0,0,0.5,0.5]", "[0,0,4]")},${view("northeast", "[0.5,0,0.5,0.5]", secondEye)},${view("southwest", "[0,0.5,0.5,0.5]", "[0,0,6]")},${view("southeast", "[0.5,0.5,0.5,0.5]", "[0,0,8]")}] }"""
    val first = SurfaceQualificationFixture.decode(scene("[0,0,5]")).fold(fail(_), identity)
    assertEquals(first.plan.views.map(_.id.value), Vector("northwest", "northeast", "southwest", "southeast"))
    assertEquals(first.plan.resources.meshes.size, 1)
    assertEquals(first.plan.views.flatMap(_.instances).map(_.mesh).distinct.length, 1)
    assertEquals(first.plan.views.flatMap(_.instances).map(_.id).distinct.length, 4)
    val changed = SurfaceQualificationFixture.decode(scene("[0,0,7]")).fold(fail(_), identity)
    def values(matrix: FloatBufferView) = Vector.tabulate(matrix.length)(matrix(_))
    val before = first.plan.views.map(view => view.id.value -> values(view.camera.viewMatrix)).toMap
    val after = changed.plan.views.map(view => view.id.value -> values(view.camera.viewMatrix)).toMap
    assertNotEquals(before("northeast"), after("northeast"))
    Vector("northwest", "southwest", "southeast").foreach: id =>
      assertEquals(before(id), after(id))
    assert(SurfaceQualificationFixture.decode(scene("[0,0,5]").replace("\"northeast\"", "\"northwest\"")).isLeft)

  test("source scalar equality neighbors and NaN are retained before mapping"):
    val layer = """[{"id":"scalar","kind":"scalar","values":[-0.25,-0.2500000298023224,0.25,"NaN"],"range":[-1,1],"threshold":[-0.25,0.25],"palette":[[0,0,1],[1,0,0]],"thresholdEdges":true}]"""
    val prepared = SurfaceQualificationFixture.decode(input(layer)).fold(fail(_), identity)
    val values = prepared.model.layers.head.thresholdEdgeSamples.get
    assertEqualsDouble(values(0), -.25, 0.0)
    assertEqualsDouble(values(1), -.2500000298023224, 0.0)
    assert(values(3).isNaN)
    assert(prepared.plan.resources.layers.values.head.thresholdEdge.nonEmpty)
    assert(prepared.plan.views.head.fragmentSurfaces(prepared.sourceSummaries.head.surface))

  test("unsupported inputs are rejected instead of silently dropping their display elements"):
    assert(SurfaceQualificationFixture.decode(input("""[{"id":"volume","kind":"volume"}]""")).isLeft)
    assert(SurfaceQualificationFixture.decode(input(extra = """, "annotations":[{"vertex":0}]""")).isLeft)
    assert(SurfaceQualificationFixture.decode(input().replace("\"timeSeconds\":0", "\"timeSeconds\":1")).isRight)

  test("nonzero temporal source values reach scalar interpolation before mapping"):
    val temporal = """[{"id":"temporal","kind":"temporal","frames":[[-2,-1,0,1],[2,1,0,-1]],"times":[0,2],"range":[-3,3],"palette":[[0,0,1],[1,0,0]]}]"""
    val prepared = SurfaceQualificationFixture.decode(input(temporal).replace("\"timeSeconds\":0", "\"timeSeconds\":0.5")).fold(fail(_), identity)
    assertEquals(prepared.state.timepoint, 0)
    val layer = prepared.model.layers.head
    assertEqualsDouble(layer.describe(0, 0).toDouble, -1.0, 0.0)
    assertEqualsDouble(layer.describe(3, 0).toDouble, .5, 0.0)
    assertEquals(layer.interpolation, SurfaceMapInterpolation.VertexColor)

  test("morphed annotations retain stable anchors and lower production decorations"):
    val extra = """, "morph":{"vertices":[[-2,-1,0],[2,-1,0],[-2,1,0],[2,1,0]],"weight":0.5},"annotations":[{"id":"marker-a","vertex":1,"radius":2.5,"colorOn":16711935,"colorOff":255,"active":true}],"crosshair":{"vertex":0,"options":{"size":12,"color":16711935},"label":"Landmark A"}"""
    val prepared = SurfaceQualificationFixture.decode(input(extra = extra)).fold(fail(_), identity)
    assertEquals(prepared.model.surfaces.head.geometries.labels.toSet, Set("midthickness", "qualification-morph"))
    assertEquals(prepared.decorations.meshes.map(_.id.decoration.value).toSet, Set("marker-a", "plane/crosshair"))
    val marker = prepared.decorations.meshes.find(_.id.decoration.value == "marker-a").get
    assertEquals(marker.target, SurfaceDecorationTarget.Anchor(SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(SurfaceId.unsafe("plane"), scalafim.surface.VertexId(1)))))
    assertEquals(prepared.decorations.labels.map(_.text), Vector("Landmark A"))

  test("label fills and boundary policy use source labels and lower contours"):
    val layers = """[{"id":"parcels","kind":"labels","labels":[0,1,1,2],"definitions":[{"id":1,"color":14833494},{"id":2,"color":5011592}],"defaultColor":10066329},{"id":"boundaries","kind":"outline","labels":[0,1,1,2],"color":0,"width":2,"halo":true,"haloColor":16777215,"haloWidth":2,"offset":0.2}]"""
    val prepared = SurfaceQualificationFixture.decode(input(layers)).fold(fail(_), identity)
    assertEquals(prepared.model.layers.head.labelMapping.get.colors.keySet, Set(1, 2))
    assert(prepared.decorations.meshes.exists(_.id.decoration.value == "plane/boundaries"))
    val outline = prepared.sourceDecorations.collectFirst { case value: SurfaceContourDecoration => value }.get
    assertEqualsDouble(outline.offset.worldUnits, .2, 0.0)

  test("signed C09 network input filters top-N edges, scales world tubes, and honors showNodes"):
    val network = """[{"id":"network","kind":"network","edges":[{"source":0,"target":1,"weight":0.8},{"source":1,"target":2,"weight":-0.6},{"source":2,"target":3,"weight":0.4},{"source":3,"target":0,"weight":-0.2}],"colorMap":[[0,0,1],[1,0,0]],"weightRange":[-1,1],"threshold":0,"renderMode":"tube","tubeRadius":0.5,"tubeRadiusScale":true,"showNodes":false,"nodeRadius":2,"nodeColor":10066329,"topN":2}]"""
    val prepared = SurfaceQualificationFixture.decode(input(network)).fold(fail(_), identity)
    val edges = prepared.decorations.meshes.collect:
      case mesh @ SurfaceDecorationMesh(_, _, SurfaceDecorationTarget.NetworkEdge(_, _), _, _, _, _, _, _, _) => mesh
    assertEquals(edges.length, 2)
    assertEquals(edges.map(_.color).toSet, Set(intaglio.Rgba32.unsafe(255, 0, 0), intaglio.Rgba32.unsafe(0, 0, 255)))
    assertEquals(edges.map(_.target).toSet, Set(
      SurfaceDecorationTarget.NetworkEdge(SurfaceVertexAnchor.make(SurfaceId.unsafe("plane"), scalafim.surface.VertexId(0)), SurfaceVertexAnchor.make(SurfaceId.unsafe("plane"), scalafim.surface.VertexId(1))),
      SurfaceDecorationTarget.NetworkEdge(SurfaceVertexAnchor.make(SurfaceId.unsafe("plane"), scalafim.surface.VertexId(1)), SurfaceVertexAnchor.make(SurfaceId.unsafe("plane"), scalafim.surface.VertexId(2)))
    ))
    assertEquals(prepared.decorations.meshes.count(_.target.isInstanceOf[SurfaceDecorationTarget.Anchor]), 0)
    assert(edges.forall(_.depthPolicy == SurfaceDecorationDepthPolicy.DepthTest))
    val overlayInput = network.replace("\"kind\":\"network\"", "\"kind\":\"network\",\"presentation\":\"signed-absolute-overlay\"")
    val overlay = SurfaceQualificationFixture.decode(input(overlayInput)).fold(fail(_), identity)
    assert(overlay.decorations.meshes.forall(_.depthPolicy == SurfaceDecorationDepthPolicy.Overlay))
    assert(SurfaceQualificationFixture.decode(input(overlayInput.replace("signed-absolute-overlay", "unknown"))).isLeft)
    val redRadius = math.abs(edges.find(_.color == intaglio.Rgba32.unsafe(255, 0, 0)).get.positions(2))
    val blueRadius = math.abs(edges.find(_.color == intaglio.Rgba32.unsafe(0, 0, 255)).get.positions(2))
    assert(redRadius > blueRadius)
    val thresholded = SurfaceQualificationFixture.decode(input(network.replace("\"threshold\":0", "\"threshold\":0.5").replace("\"topN\":2", "\"topN\":4"))).fold(fail(_), identity)
    assertEquals(thresholded.decorations.meshes.count(_.target.isInstanceOf[SurfaceDecorationTarget.NetworkEdge]), 2)
    val resized = prepared.lowerDecorations(prepared.plan, 240, 160).fold(fail(_), identity)
    assertEquals((resized.canvasWidth, resized.canvasHeight), (240, 160))
    assertNotEquals(resized.binding, prepared.decorations.binding)
    assertEquals(resized.meshes.count(_.target.isInstanceOf[SurfaceDecorationTarget.NetworkEdge]), 2)
    assertEquals(resized.meshes.count(_.target.isInstanceOf[SurfaceDecorationTarget.Anchor]), 0)
    assert(SurfaceQualificationFixture.decode(input(network.replace("\"renderMode\":\"tube\"", "\"renderMode\":\"pixel\""))).isLeft)

  test("instance alpha and clipping keep-side are preserved from source inputs"):
    for flip <- Vector(false, true) do
      val extra = s""", "alpha":0.5,"clip":{"normal":[2,0,0],"point":[0.25,0,0],"flip":$flip}"""
      val decoded = SurfaceQualificationFixture.decode(input(extra = extra)).fold(fail(_), identity)
      assertEquals(decoded.plan.views.head.instances.head.opacity.toDouble, .5)
      decoded.plan.views.head.clipping match
        case SurfaceClipping.WorldPlanes(Vector(plane)) =>
          assertEqualsDouble(plane.orientedDistance(.75, 0, 0), if flip then -.5 else .5, 1e-12)
          assertEqualsDouble(plane.orientedDistance(.25, 0, 0), 0, 1e-12)
        case other => fail(s"unexpected clipping $other")
    assert(SurfaceQualificationFixture.decode(input(extra = """, "alpha":1.1""")).isLeft)
    assert(SurfaceQualificationFixture.decode(input().replace("\"timeSeconds\":0", "\"culling\":\"back\"")).isLeft)

  test("distinct per-instance clipping is refused instead of widened to the whole view"):
    val json = input(extra = """, "clip":{"normal":[1,0,0],"point":[0,0,0]}""")
    val second = """,{"id":"other","hemisphere":"left","vertices":[[-1,-1,0],[1,-1,0],[-1,1,0]],"faces":[[0,1,2]],"layers":[]}"""
    val two = json.dropRight(2) + second + "]}"
    assert(SurfaceQualificationFixture.decode(two).left.toOption.exists(_.contains("same clipping plane")))

  test("only the top visible normal scalar uses the reference threshold-edge policy"):
    val scalar = """{"id":"a","kind":"scalar","values":[-1,1,-1,1],"range":[-1,1],"threshold":[-0.25,0.25],"palette":[[0,0,1],[1,0,0]],"thresholdEdges":true}"""
    for blend <- Vector("normal", "additive", "multiply") do
      val top = scalar.replace("\"a\"", "\"b\"").dropRight(1) + s""", "blendMode":"$blend"}"""
      val prepared = SurfaceQualificationFixture.decode(input(s"[$scalar,$top]")).fold(fail(_), identity)
      val layers = prepared.plan.resources.layers.values.toVector.sortBy(_.layer.value)
      assert(layers.head.thresholdEdge.isEmpty)
      assertEquals(layers.last.thresholdEdge.nonEmpty, blend == "normal")
      if blend == "additive" then assertEquals(layers.last.blendMode, intaglio.DisplayBlendMode.PlusLighter)
