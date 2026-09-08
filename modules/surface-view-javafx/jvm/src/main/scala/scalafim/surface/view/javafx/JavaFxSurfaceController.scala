package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.event.EventHandler
import javafx.scene.SubScene
import javafx.scene.input.{KeyCode, KeyEvent, MouseButton, MouseEvent, PickResult, ScrollEvent}
import javafx.scene.shape.MeshView

import scalafim.surface.*
import scalafim.surface.view.*

enum JavaFxInteractionError:
  case View(error: SurfaceViewError)
  case Backend(error: JavaFxSurfaceError)
  case PickMiss
  case UnknownPickNode
  case InvalidPickedFace(face: Int)
  case Disposed

  def message: String =
    this match
      case View(error) => error.message
      case Backend(error) => error.message
      case PickMiss => "JavaFX pick did not intersect a surface"
      case UnknownPickNode => "JavaFX pick node does not belong to a surface chunk"
      case InvalidPickedFace(face) => s"JavaFX returned invalid local face $face"
      case Disposed => "JavaFX surface controller has been disposed"

final case class JavaFxSurfacePick(
  surface: SurfaceId,
  face: FaceId,
  vertex: VertexId,
  barycentricA: Double,
  barycentricB: Double,
  barycentricC: Double
)

final case class JavaFxInteractionReceipt(
  viewpoint: SurfaceViewpoint,
  selectedFace: Option[Int],
  selectedVertex: Option[Int],
  meshUploads: Int,
  atlasUploads: Int,
  navigationP50Millis: Double,
  navigationP95Millis: Double,
  pickP50Millis: Double,
  pickP95Millis: Double,
  disposed: Boolean
)

final class JavaFxSurfaceController private (
  model: SurfaceViewerModel,
  backend: JavaFxSurfaceBackend,
  scene: SubScene,
  private var currentState: SurfaceViewerState,
  private var currentPlan: SurfaceRenderPlan
):
  private var pressedX = 0.0
  private var pressedY = 0.0
  private var lastHoverNanos = 0L
  private var latestPick: Option[JavaFxSurfacePick] = None
  private var lastFailure: Option[JavaFxInteractionError] = None
  private var navigationNanos = Vector.empty[Long]
  private var pickNanos = Vector.empty[Long]
  private var meshUploads = currentPlan.meshes.length
  private var atlasUploads = currentPlan.layers.length
  private var isDisposed = false

  private val pressedHandler: EventHandler[MouseEvent] = event =>
    pressedX = event.getSceneX
    pressedY = event.getSceneY
    scene.requestFocus()

  private val draggedHandler: EventHandler[MouseEvent] = event =>
    val dx = event.getSceneX - pressedX
    val dy = event.getSceneY - pressedY
    val action =
      if event.isPrimaryButtonDown then SurfaceViewerAction.OrbitBy(dx * 0.35, -dy * 0.35)
      else
        SurfaceViewerAction.SetPan(
          currentState.camera.panX + dx * 0.004 / currentState.camera.zoom.value,
          currentState.camera.panY - dy * 0.004 / currentState.camera.zoom.value
        )
    dispatch(action)
    pressedX = event.getSceneX
    pressedY = event.getSceneY

  private val scrollHandler: EventHandler[ScrollEvent] = event =>
    val factor = math.exp(event.getDeltaY * 0.0015)
    CameraZoom.make((currentState.camera.zoom.value * factor).max(0.05).min(50.0))
      .fold(error => record(JavaFxInteractionError.View(error)), zoom => dispatch(SurfaceViewerAction.SetZoom(zoom)))

  private val clickHandler: EventHandler[MouseEvent] = event =>
    if event.getButton == MouseButton.PRIMARY then
      pick(event).fold(record, pick =>
        latestPick = Some(pick)
        dispatch(SurfaceViewerAction.SelectFace(pick.surface, pick.face, pick.vertex))
      )

  private val moveHandler: EventHandler[MouseEvent] = event =>
    val now = System.nanoTime()
    if now - lastHoverNanos >= 33333333L then
      pick(event).foreach(value => latestPick = Some(value))
      lastHoverNanos = now

  private val keyHandler: EventHandler[KeyEvent] = event =>
    val action = keyAction(event.getCode)
    action.foreach(dispatch)
    if action.nonEmpty then event.consume()

  def state: SurfaceViewerState = currentState
  def plan: SurfaceRenderPlan = currentPlan
  def hovered: Option[JavaFxSurfacePick] = latestPick
  def lastError: Option[JavaFxInteractionError] = lastFailure

  def dispatch(action: SurfaceViewerAction): Either[JavaFxInteractionError, JavaFxInterpretReceipt] =
    if isDisposed then Left(JavaFxInteractionError.Disposed)
    else
      val started = System.nanoTime()
      val result =
        for
          nextState <- SurfaceViewer.reduce(model, currentState, action).left.map(JavaFxInteractionError.View.apply)
          nextPlan <- SurfaceCompiler.compile(model, nextState).left.map(JavaFxInteractionError.View.apply)
          interpreted <- backend.render(nextPlan).left.map(JavaFxInteractionError.Backend.apply)
        yield
          currentState = nextState
          currentPlan = nextPlan
          if interpreted.dirty.geometry then meshUploads += nextPlan.meshes.length
          atlasUploads += interpreted.atlasUpdates
          interpreted
      navigationNanos :+= System.nanoTime() - started
      result.left.foreach(record)
      result

  def pick(event: MouseEvent): Either[JavaFxInteractionError, JavaFxSurfacePick] =
    pick(event.getPickResult)

  def pick(result: PickResult): Either[JavaFxInteractionError, JavaFxSurfacePick] =
    val started = System.nanoTime()
    val picked = pickResult(result)
    pickNanos :+= System.nanoTime() - started
    picked.left.foreach(record)
    picked

  def receipt: JavaFxInteractionReceipt =
    JavaFxInteractionReceipt(
      currentState.camera.viewpoint,
      latestPick.map(_.face.index),
      currentState.selection.map(_.vertex.index),
      meshUploads,
      atlasUploads,
      percentileMillis(navigationNanos, 0.50),
      percentileMillis(navigationNanos, 0.95),
      percentileMillis(pickNanos, 0.50),
      percentileMillis(pickNanos, 0.95),
      isDisposed
    )

  def dispose(): Unit =
    if !isDisposed then
      scene.setOnMousePressed(null)
      scene.setOnMouseDragged(null)
      scene.setOnMouseClicked(null)
      scene.setOnMouseMoved(null)
      scene.setOnScroll(null)
      scene.setOnKeyPressed(null)
      isDisposed = true

  private def install(): Unit =
    scene.setFocusTraversable(true)
    scene.setOnMousePressed(pressedHandler)
    scene.setOnMouseDragged(draggedHandler)
    scene.setOnMouseClicked(clickHandler)
    scene.setOnMouseMoved(moveHandler)
    scene.setOnScroll(scrollHandler)
    scene.setOnKeyPressed(keyHandler)

  private def pickResult(result: PickResult): Either[JavaFxInteractionError, JavaFxSurfacePick] =
    if result == null || result.getIntersectedNode == null then Left(JavaFxInteractionError.PickMiss)
    else
      result.getIntersectedNode match
        case view: MeshView =>
          backendChunk(view) match
            case None => Left(JavaFxInteractionError.UnknownPickNode)
            case Some(chunk) =>
              val localFace = result.getIntersectedFace
              if localFace < 0 || localFace >= chunk.faceCount then
                Left(JavaFxInteractionError.InvalidPickedFace(localFace))
              else
                val face = chunk.faceStart + localFace
                val packet = backend.pickingPlan.getOrElse(currentPlan).meshes.find(_.surface == chunk.surface).get
                val offset = face * 3
                val a = packet.indices(offset)
                val b = packet.indices(offset + 1)
                val c = packet.indices(offset + 2)
                val point = result.getIntersectedPoint
                val weights = barycentric(packet, a, b, c, point.getX, point.getY, point.getZ)
                val original = packet.sourceBarycentric(face, weights._1, weights._2, weights._3)
                val vertex = packet.pickedVertex(face, weights._1, weights._2, weights._3)
                Right(JavaFxSurfacePick(
                  chunk.surface,
                  FaceId(packet.sourceFace(face)),
                  VertexId(vertex),
                  original._1,
                  original._2,
                  original._3
                ))
        case _ => Left(JavaFxInteractionError.UnknownPickNode)

  private def backendChunk(view: MeshView): Option[JavaFxSurfaceChunk] =
    backend.chunks.find(chunk => chunk.view eq view)

  private def barycentric(
    packet: SurfaceMeshPacket,
    a: Int,
    b: Int,
    c: Int,
    px: Double,
    py: Double,
    pz: Double
  ): (Double, Double, Double) =
    def coordinate(vertex: Int, axis: Int): Double = packet.positions(vertex * 3 + axis)
    val abx = coordinate(b, 0) - coordinate(a, 0)
    val aby = coordinate(b, 1) - coordinate(a, 1)
    val abz = coordinate(b, 2) - coordinate(a, 2)
    val acx = coordinate(c, 0) - coordinate(a, 0)
    val acy = coordinate(c, 1) - coordinate(a, 1)
    val acz = coordinate(c, 2) - coordinate(a, 2)
    val apx = px - coordinate(a, 0)
    val apy = py - coordinate(a, 1)
    val apz = pz - coordinate(a, 2)
    val d00 = abx * abx + aby * aby + abz * abz
    val d01 = abx * acx + aby * acy + abz * acz
    val d11 = acx * acx + acy * acy + acz * acz
    val d20 = apx * abx + apy * aby + apz * abz
    val d21 = apx * acx + apy * acy + apz * acz
    val denominator = d00 * d11 - d01 * d01
    if denominator == 0.0 then (1.0, 0.0, 0.0)
    else
      val weightB = (d11 * d20 - d01 * d21) / denominator
      val weightC = (d00 * d21 - d01 * d20) / denominator
      (1.0 - weightB - weightC, weightB, weightC)

  private def keyAction(code: KeyCode): Option[SurfaceViewerAction] =
    val hemisphere =
      currentState.selection
        .flatMap(selection => model.surface(selection.surface))
        .map(_.domain.hemisphere)
        .getOrElse(model.surfaces.head.domain.hemisphere)
    code match
      case KeyCode.L => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(hemisphere)))
      case KeyCode.M => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Medial(hemisphere)))
      case KeyCode.A => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Anterior))
      case KeyCode.P => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Posterior))
      case KeyCode.D => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal))
      case KeyCode.V => Some(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Ventral))
      case KeyCode.R => Some(SurfaceViewerAction.ResetCamera)
      case KeyCode.LEFT => Some(SurfaceViewerAction.SetPan(currentState.camera.panX - 0.02, currentState.camera.panY))
      case KeyCode.RIGHT => Some(SurfaceViewerAction.SetPan(currentState.camera.panX + 0.02, currentState.camera.panY))
      case KeyCode.UP => Some(SurfaceViewerAction.SetPan(currentState.camera.panX, currentState.camera.panY + 0.02))
      case KeyCode.DOWN => Some(SurfaceViewerAction.SetPan(currentState.camera.panX, currentState.camera.panY - 0.02))
      case KeyCode.PLUS | KeyCode.EQUALS =>
        CameraZoom.make((currentState.camera.zoom.value * 1.1).min(50.0)).toOption.map(SurfaceViewerAction.SetZoom.apply)
      case KeyCode.MINUS =>
        CameraZoom.make((currentState.camera.zoom.value / 1.1).max(0.05)).toOption.map(SurfaceViewerAction.SetZoom.apply)
      case _ => None

  private def percentileMillis(values: Vector[Long], probability: Double): Double =
    if values.isEmpty then 0.0
    else
      val sorted = values.sorted
      val index = math.min(sorted.length - 1, math.ceil(probability * sorted.length).toInt - 1)
      sorted(index).toDouble / 1e6

  private def record(error: JavaFxInteractionError): Unit =
    lastFailure = Some(error)

object JavaFxSurfaceController:
  def attach(
    model: SurfaceViewerModel,
    initial: SurfaceViewerState,
    backend: JavaFxSurfaceBackend,
    scene: SubScene
  ): Either[JavaFxInteractionError, JavaFxSurfaceController] =
    if !Platform.isFxApplicationThread then
      Left(JavaFxInteractionError.Backend(JavaFxSurfaceError.IncompatiblePlan("controller attachment must run on the Application Thread")))
    else
      for
        plan <- SurfaceCompiler.compile(model, initial).left.map(JavaFxInteractionError.View.apply)
        _ <- backend.render(plan).left.map(JavaFxInteractionError.Backend.apply)
      yield
        val controller = new JavaFxSurfaceController(model, backend, scene, initial, plan)
        controller.install()
        controller
