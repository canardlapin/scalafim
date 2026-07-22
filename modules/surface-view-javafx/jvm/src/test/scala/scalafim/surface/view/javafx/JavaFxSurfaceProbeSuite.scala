package scalafim.surface.view.javafx

import javafx.scene.{ParallelCamera, PerspectiveCamera}
import javafx.scene.shape.VertexFormat

import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.view.*

class JavaFxSurfaceProbeSuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")
  private val layerId = SurfaceLayerId.unsafe("color")

  test("JavaFX consumes CPU-projected fields and network tubes as ordinary resources"):
    val plan = SurfaceFeatureFixture.plan
    val compiled = JavaFxSurfaceProbe.compile(plan).toOption.get
    assertEquals(plan.layers.length, 2)
    assert(compiled.receipt.facesUploaded > 4)
    assertEquals(compiled.receipt.verticesUploaded, plan.meshes.head.positions.length / 3)

  test("JavaFX capability mapping is explicit about admission and clipping"):
    val capabilities = JavaFxCapabilityReport(true, true, true, None).admissionCapabilities
    assertEquals(capabilities.id.value, "javafx-scene3d")
    assert(capabilities.supports(SurfaceBackendFeature.NativePicking))
    assert(!capabilities.supports(SurfaceBackendFeature.WorldClipping))
    assert(capabilities.caveats.exists(_.contains("world clipping")))

  private def geometry(faceCopies: Int = 1): SurfaceGeometry =
    val faces = Vector.fill(faceCopies)((0, 1, 2))
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.5, 1.0, 0.0)),
        faces
      ),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )

  private def plan(
    geometry: SurfaceGeometry,
    color: Rgba32 = Rgba32.unsafe(255, 0, 0),
    viewpoint: SurfaceViewpoint = SurfaceViewpoint.Dorsal,
    projection: CameraProjection = CameraProjection.Perspective(FieldOfViewDegrees.Default)
  ): SurfaceRenderPlan =
    val layer = SurfaceLayer.packedRgba(
      layerId,
      surfaceId,
      geometry,
      Vector.fill(geometry.vertexCount)(color)
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, geometry).toOption.get),
      Vector(layer)
    ).toOption.get
    val viewed = SurfaceViewer.reduce(
      model,
      SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetViewpoint(viewpoint)
    ).toOption.get
    val state = SurfaceViewer.reduce(
      model,
      viewed,
      SurfaceViewerAction.SetProjection(projection)
    ).toOption.get
    SurfaceCompiler.compile(model, state).toOption.get

  test("atlas configuration rejects unsafe sizes"):
    assert(JavaFxAtlasConfig.make(tileSize = 3).isLeft)
    assert(JavaFxAtlasConfig.make(maxTextureSize = 32).isLeft)
    val config = JavaFxAtlasConfig.make(tileSize = 4, maxTextureSize = 64).toOption.get
    assertEquals(config.tilesPerRow, 16)
    assertEquals(config.facesPerAtlas, 256)

  test("publication preset configures an exact high-resolution snapshot"):
    val config = JavaFxSnapshotConfig.publication(SurfacePublicationPreset.ManuscriptDoubleColumn)
    assertEquals(config.width, 2400)
    assertEquals(config.height, 1400)
    assert(!config.transparent)

  test("POINT_NORMAL_TEXCOORD mesh and direct per-face atlas use supported JavaFX APIs"):
    val renderPlan = plan(geometry())
    val result = JavaFxSurfaceProbe.compile(renderPlan).toOption.get
    assertEquals(result.chunks.length, 1)
    val chunk = result.chunks.head
    assertEquals(chunk.mesh.getVertexFormat, VertexFormat.POINT_NORMAL_TEXCOORD)
    assertEquals(chunk.mesh.getPoints.size, 9)
    assertEquals(chunk.mesh.getNormals.size, 9)
    assertEquals(chunk.mesh.getTexCoords.size, 6)
    assertEquals(chunk.mesh.getFaces.size, 9)
    assert(chunk.atlas.direct)
    assertEquals(chunk.atlas.width, 4)
    assertEquals(chunk.atlas.height, 4)
    assert(chunk.material.getSelfIlluminationMap eq chunk.atlas.image)
    assertEquals(result.receipt.directAtlasBytes, 64L)

  test("lit and unlit material modes bind only public PhongMaterial maps"):
    val renderPlan = plan(geometry())
    val lit = JavaFxSurfaceProbe.compile(renderPlan, JavaFxMaterialMode.Lit).toOption.get.chunks.head
    val unlit = JavaFxSurfaceProbe.compile(renderPlan, JavaFxMaterialMode.Unlit).toOption.get.chunks.head
    assert(lit.material.getDiffuseMap eq lit.atlas.image)
    assertEquals(lit.material.getSelfIlluminationMap, null)
    assert(unlit.material.getSelfIlluminationMap eq unlit.atlas.image)

  test("style and data updates touch atlases without rebuilding geometry"):
    val mesh = geometry()
    val redPlan = plan(mesh, Rgba32.unsafe(255, 0, 0))
    val bluePlan = plan(mesh, Rgba32.unsafe(0, 0, 255))
    val result = JavaFxSurfaceProbe.compile(redPlan).toOption.get
    val triangleMesh = result.chunks.head.mesh
    val update = result.updateColors(bluePlan).toOption.get
    assert(!update.geometryRebuilt)
    assertEquals(update.atlasesUpdated, 1)
    assertEquals(update.dirtyPixels, 16L)
    assert(result.chunks.head.mesh eq triangleMesh)

  test("camera motion updates no mesh and no atlas"):
    val mesh = geometry()
    val dorsal = plan(mesh, viewpoint = SurfaceViewpoint.Dorsal)
    val anterior = plan(mesh, viewpoint = SurfaceViewpoint.Anterior)
    val result = JavaFxSurfaceProbe.compile(dorsal).toOption.get
    val triangleMesh = result.chunks.head.mesh
    val atlas = result.chunks.head.atlas
    val update = result.applyCamera(anterior).toOption.get
    assert(!update.geometryRebuilt)
    assertEquals(update.atlasesUpdated, 0)
    assertEquals(update.dirtyPixels, 0L)
    assert(result.chunks.head.mesh eq triangleMesh)
    assert(result.chunks.head.atlas eq atlas)

  test("topology-preserving geometry changes mutate points and normals without rebuilding atlases"):
    val original = geometry()
    val shifted = SurfaceGeometry(
      TriangleMesh.fromRows(
        original.mesh.vertices.map(point => Seq(point.x + 2.0, point.y, point.z)),
        original.mesh.faces.map(face => (face.a.index, face.b.index, face.c.index))
      ),
      original.hemisphere,
      original.kind,
      original.surfaceToWorld
    )
    val before = plan(original)
    val after = plan(shifted)
    val result = JavaFxSurfaceProbe.compile(before).toOption.get
    val triangleMesh = result.chunks.head.mesh
    val atlas = result.chunks.head.atlas
    val firstX = triangleMesh.getPoints.get(0)
    val update = result.updateGeometry(after).toOption.get

    assert(!update.geometryRebuilt)
    assertEquals(update.verticesUpdated, original.vertexCount)
    assertEquals(update.bytesUpdated, original.vertexCount.toLong * 3L * 2L * 4L)
    assertEqualsDouble(triangleMesh.getPoints.get(0).toDouble, firstX + 2.0, 0.0)
    assert(result.chunks.head.mesh eq triangleMesh)
    assert(result.chunks.head.atlas eq atlas)

  test("orthographic plans select a parallel camera without rebuilding resources"):
    val mesh = geometry()
    val perspective = plan(mesh)
    val orthographic = plan(
      mesh,
      projection = CameraProjection.Orthographic(OrthographicScale.unsafe(0.75))
    )
    val result = JavaFxSurfaceProbe.compile(perspective).toOption.get
    assert(result.camera.isInstanceOf[PerspectiveCamera])

    result.applyCamera(orthographic).toOption.get
    assert(result.camera.isInstanceOf[ParallelCamera])

    result.applyCamera(perspective).toOption.get
    assert(result.camera.isInstanceOf[PerspectiveCamera])

  test("large face sets chunk at the configured safe texture capacity"):
    val mesh = geometry(faceCopies = 300)
    val config = JavaFxAtlasConfig.make(tileSize = 4, maxTextureSize = 64).toOption.get
    val result = JavaFxSurfaceProbe.compile(plan(mesh), config = config).toOption.get
    assertEquals(result.chunks.map(_.faceCount), Vector(256, 44))
    assertEquals(result.receipt.chunks, 2)
    assertEquals(result.receipt.facesUploaded, 300)
    assertEquals(result.receipt.verticesUploaded, 6)
    assertEquals(result.receipt.atlasPixels, 4864L)

  test("face tuples retain point, normal, and face-local atlas indices"):
    val chunk = JavaFxSurfaceProbe.compile(plan(geometry())).toOption.get.chunks.head
    val faces = chunk.mesh.getFaces
    assertEquals(Vector.tabulate(9)(faces.get), Vector(0, 0, 0, 1, 1, 1, 2, 2, 2))

  test("toolkit-free programs classify camera, layer, material, and geometry dirt exactly"):
    val mesh = geometry()
    val red = plan(mesh, Rgba32.unsafe(255, 0, 0), SurfaceViewpoint.Dorsal)
    val same = JavaFxSurfaceProgram.compile(Some(red), red)
    assert(same.dirty.isClean)
    assertEquals(same.commands, Vector.empty)

    val camera = plan(mesh, Rgba32.unsafe(255, 0, 0), SurfaceViewpoint.Anterior)
    val cameraProgram = JavaFxSurfaceProgram.compile(Some(red), camera)
    assert(cameraProgram.dirty.camera)
    assert(!cameraProgram.dirty.geometry)
    assert(!cameraProgram.dirty.layerData)
    assertEquals(cameraProgram.commands.map(_.productPrefix), Vector("UpdateCamera"))

    val blue = plan(mesh, Rgba32.unsafe(0, 0, 255), SurfaceViewpoint.Dorsal)
    val layerProgram = JavaFxSurfaceProgram.compile(Some(red), blue)
    assert(layerProgram.dirty.layerData)
    assert(!layerProgram.dirty.geometry)
    assertEquals(layerProgram.commands.map(_.productPrefix).sorted, Vector("DisposeResources", "UpdateAtlases"))

    val unlit = red.copy(lighting = SurfaceLighting.Unlit)
    val materialProgram = JavaFxSurfaceProgram.compile(Some(red), unlit)
    assert(materialProgram.dirty.material)
    assertEquals(materialProgram.commands.map(_.productPrefix), Vector("UpdateMaterial"))

    val changedMesh = plan(geometry(faceCopies = 2), Rgba32.unsafe(255, 0, 0), SurfaceViewpoint.Dorsal)
    val geometryProgram = JavaFxSurfaceProgram.compile(Some(red), changedMesh)
    assert(geometryProgram.dirty.geometry)
    assert(geometryProgram.commands.exists(_.productPrefix == "RebuildGeometry"))

    val shiftedGeometry = SurfaceGeometry(
      TriangleMesh.fromRows(
        mesh.mesh.vertices.map(point => Seq(point.x + 1.0, point.y, point.z)),
        mesh.mesh.faces.map(face => (face.a.index, face.b.index, face.c.index))
      ),
      mesh.hemisphere,
      mesh.kind,
      mesh.surfaceToWorld
    )
    val shifted = plan(shiftedGeometry, Rgba32.unsafe(255, 0, 0), SurfaceViewpoint.Dorsal)
    val updateProgram = JavaFxSurfaceProgram.compile(Some(red), shifted)
    assert(updateProgram.dirty.geometry)
    assert(updateProgram.commands.exists(_.productPrefix == "UpdateGeometry"))
    assert(!updateProgram.commands.exists(_.productPrefix == "RebuildGeometry"))
    assert(!updateProgram.commands.exists(_.productPrefix == "UpdateAtlases"))

  test("production interpreter rejects non-Application-Thread access"):
    assert(JavaFxSurfaceBackend.create().isLeft)

  test("world clipping is rejected explicitly before JavaFX interpretation"):
    val clipping = SurfaceClipping.worldPlanes(Vector(
      WorldClipPlane.unsafe(1.0, 0.0, 0.0, 0.0)
    )).toOption.get
    val result = JavaFxSurfaceBackend.validateCapabilities(plan(geometry()).copy(clipping = clipping))
    assert(result.left.exists(_.message.contains("world clipping planes are unsupported")))
