package scalafim.surface.view.three

import intaglio.*
import scalafim.surface.view.*

class ThreeSurfaceScalarSuite extends munit.FunSuite:
  private val size = ThreeCanvasSize.unsafe(128, 128)

  test("scalar shaders interpolate the full sample envelope with original face provenance"):
    val plan = SurfaceScalarFixture.plan(SurfaceScalarFixture.thresholded)
    val program = ThreeSurfaceProgram.compile(None, plan, size).toOption.get
    val packet = program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadFragments(packets) => packets.head
    .get
    assertEquals(packet.attributes.head.values.grouped(4).map(_(0)).toVector, Vector[Float](0, 1, 1, 0, 1, 0))
    val mesh = program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadGeometry(meshes) => meshes.head
    .get
    assertEquals(mesh.positions.length, 18)
    assertEquals(mesh.sourceFaceVertices(0), (0, 1, 2))
    assertEquals(mesh.sourceFaceVertices(1), (0, 2, 3))
    assertEquals(mesh.pickedVertex(1, 0.2, 0.3, 0.5), 3)
    assertEquals(packet.attributeBytes, 6L * 4 * 4)

  test("camera changes reuse attributes and style changes upload no geometry"):
    val plan = SurfaceScalarFixture.plan()
    val camera = plan.copy(receipt = plan.receipt.copy(cameraKey = "changed-camera"))
    val cameraProgram = ThreeSurfaceProgram.compile(Some(plan -> size), camera, size).toOption.get
    assert(!cameraProgram.dirty.geometry && !cameraProgram.dirty.layerData)
    assert(!cameraProgram.commands.exists(_.isInstanceOf[ThreeSurfaceCommand.UploadFragments]))
    val styled = plan.copy(layers = plan.layers.map(_.copy(opacity = DisplayOpacity.unsafe(0.5))))
    val program = ThreeSurfaceProgram.compile(Some(plan -> size), styled, size).toOption.get
    assert(!program.dirty.geometry && program.dirty.layerData)
    assert(program.commands.exists(_.isInstanceOf[ThreeSurfaceCommand.UploadFragments]))
    assert(!program.commands.exists(_.isInstanceOf[ThreeSurfaceCommand.UploadGeometry]))

  test("nonfinite scalar samples carry a separate contributing-weight flag"):
    val model = SurfaceScalarFixture.model(values = Array(Double.NaN, 4, 4, -2))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val packet = ThreeFragmentShader.compile(plan.meshes.head, plan.layers).toOption.get
    val values = packet.attributes.head.values
    assert(values.forall(_.isFinite))
    assertEquals(values.grouped(4).map(_(1)).toVector, Vector[Float](1, 0, 0, 1, 0, 0))

  test("mapping boundaries that collapse in GPU floats fail before native allocation"):
    val model = SurfaceScalarFixture.model(values = Array(-1e100, 1e100, 1e100, -1e100))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    val error = ThreeSurfaceProgram.compile(None, plan, size).left.toOption.get
    assert(error.message.contains("collapse"))

  test("uncovered samples cannot dilute scalar precision"):
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(1e8 - 1, 1e8 + 1),
      SurfaceScalarFixture.mapping.scale.segments.head.ramp))
    val model = SurfaceScalarFixture.model(mapping, Array(1e8 - 2, 1e8 + 2, 1e8 + 2, 0))
    val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
    assert(ThreeFragmentShader.compile(plan.meshes.head, plan.layers).isLeft)
    val covered = plan.layers.map(_.copy(coverage = SurfaceLayerCoverage.Faces(0, 1)))
    val packet = ThreeFragmentShader.compile(plan.meshes.head, covered).toOption.get
    assertEquals(packet.attributes.head.values.take(12).grouped(4).map(_(0)).toVector, Vector[Float](0, 1, 1))
    assert(packet.attributes.head.values.drop(12).forall(_ == 0f))
