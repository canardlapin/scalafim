package scalafim.examples.surfaceview

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel

import scalafim.surface.view.three.*

/** Public Scala.js entry point for the P0/P1 capture runner. It deliberately
  * compiles and renders a public `SurfaceViewer` plan: the harness must not
  * reach into a ThreeSurfaceBackend's private state to manufacture camera or
  * pixel evidence.
  */
object SurfaceParityAdapter:
  private var active: Option[ThreeSurfaceBackend] = None

  @JSExportTopLevel("mountScalafimSurfaceParityBaseline")
  def mount(three: js.Dynamic, canvas: js.Dynamic, caseId: String): js.Dynamic =
    val contract = SurfaceParityFixture.contract(caseId)
    val example = SurfaceParityFixture.example(caseId)
    val runtime = ThreeJsRuntime.create(three, canvas)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime)
      .fold(error => throw new IllegalStateException(error.message), identity)
    active.foreach(_.dispose().fold(error => throw new IllegalStateException(error.message), identity))
    active = Some(backend)
    val rendered = backend.render(example.plan, ThreeCanvasSize.unsafe(512, 512, 1.0))
      .fold(error => throw new IllegalStateException(error.message), identity)
    js.Dynamic.literal(
      schema = "scalafim.surface-parity-adapter.v1",
      caseId = caseId,
      fixture = js.JSON.parse(contract),
      framebuffer = canvas.applyDynamic("toDataURL")("image/png"),
      observed = js.Dynamic.literal(
        backend = backend.capabilities.id.value,
        matrixOrder = "row-major",
        viewMatrix = Vector.tabulate(16)(example.plan.camera.viewMatrix.apply).toJSArray,
        projectionMatrix = Vector.tabulate(16)(example.plan.camera.projectionMatrix.apply).toJSArray,
        vertices = example.plan.profile.verticesPacked,
        faces = example.plan.profile.facesPacked,
        layers = example.state.layerOrder.map(_.value).toJSArray,
        drawCalls = rendered.drawCalls,
        geometryUploads = rendered.geometryUploads,
        colorUploads = rendered.colorUploads
      ),
      cameraSeam = "public SurfaceViewer/SurfaceCompiler plan only; explicit camera-up is not available on baseline"
    )

  @JSExportTopLevel("disposeScalafimSurfaceParityBaseline")
  def dispose(): Unit =
    active.foreach(_.dispose().fold(error => throw new IllegalStateException(error.message), identity))
    active = None
