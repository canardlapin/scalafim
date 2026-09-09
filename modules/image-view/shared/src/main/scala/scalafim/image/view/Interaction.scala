package scalafim.image.view

import intaglio.DeviceContext
import scalafim.image.*

opaque type SliceStep = Double

object SliceStep:
  def make(value: Double): Either[ImageViewError, SliceStep] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ImageViewError.InvalidSliceStep(value))

  def unsafe(value: Double): SliceStep =
    make(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  val One: SliceStep =
    1.0

extension (step: SliceStep)
  def millimeters: Double = step

opaque type ZoomLevel = Double

object ZoomLevel:
  def make(value: Double): Either[ImageViewError, ZoomLevel] =
    if value.isFinite && value >= 1.0 then Right(value)
    else Left(ImageViewError.InvalidZoom(value))

  def unsafe(value: Double): ZoomLevel =
    make(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  val One: ZoomLevel =
    1.0

extension (zoom: ZoomLevel)
  def factor: Double = zoom

final case class PanelView private (
  zoom: ZoomLevel,
  centerX: Double,
  centerY: Double
):
  private[view] def imageLeft: Double =
    0.5 - centerX * zoom.factor

  private[view] def imageBottom: Double =
    0.5 - centerY * zoom.factor

  private[view] def imageToLocal(value: Double, center: Double): Double =
    (value - center) * zoom.factor + 0.5

  private[view] def localToImage(value: Double, center: Double): Double =
    center + (value - 0.5) / zoom.factor

object PanelView:
  val Default: PanelView =
    new PanelView(ZoomLevel.One, 0.5, 0.5)

  def make(
    zoom: ZoomLevel,
    centerX: Double,
    centerY: Double
  ): Either[ImageViewError, PanelView] =
    val halfSpan = 0.5 / zoom.factor
    val valid =
      centerX.isFinite && centerY.isFinite &&
        centerX >= halfSpan && centerX <= 1.0 - halfSpan &&
        centerY >= halfSpan && centerY <= 1.0 - halfSpan
    if valid then Right(new PanelView(zoom, centerX, centerY))
    else Left(ImageViewError.InvalidViewCenter(centerX, centerY, zoom.factor))

  def unsafe(zoom: ZoomLevel, centerX: Double, centerY: Double): PanelView =
    make(zoom, centerX, centerY).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class ViewerPointer private (rootX: Double, rootY: Double)

object ViewerPointer:
  def make(rootX: Double, rootY: Double): Either[ImageViewError, ViewerPointer] =
    if rootX.isFinite && rootY.isFinite then Right(new ViewerPointer(rootX, rootY))
    else Left(ImageViewError.InvalidPointer(rootX, rootY))

  def unsafe(rootX: Double, rootY: Double): ViewerPointer =
    make(rootX, rootY).fold(err => throw new IllegalArgumentException(err.message), identity)

  def fromDevice(
    deviceX: Double,
    deviceY: Double,
    device: DeviceContext
  ): Either[ImageViewError, ViewerPointer] =
    make(deviceX / device.width, 1.0 - deviceY / device.height)

enum ViewerAction:
  case Pick(plane: AnatomicalPlane, pointer: ViewerPointer)
  case Scroll(plane: AnatomicalPlane, steps: Int)
  case SetCursor(cursor: WorldPoint)
  case SetConvention(convention: LeftRightConvention)
  case SetPixelSpacing(spacing: PixelSpacing)
  case SetSliceStep(step: SliceStep)
  case SetPanelView(plane: AnatomicalPlane, view: PanelView)
  case ResetPanelView(plane: AnatomicalPlane)
  case SetWindow(layer: LayerId, window: DisplayWindow)
  case ClearWindow(layer: LayerId)
  case SetThreshold(layer: LayerId, threshold: DisplayThreshold)
  case ClearThreshold(layer: LayerId)
  case SetOpacity(layer: LayerId, opacity: LayerOpacity)
  case SetVisibility(layer: LayerId, visible: Boolean)
  case SetTimepoint(index: Int)
  case Resize(device: DeviceContext)
  case ShowCrosshair(visible: Boolean)
  case ShowOrientationLabels(visible: Boolean)

final case class ViewerSession(
  state: ViewerState,
  device: DeviceContext,
  layout: OrthogonalLayout = OrthogonalLayout.Default
):
  def frame(model: ViewerModel): Either[ImageViewError, ViewerFrame] =
    ViewerCompiler.compile(model, state, device, layout)

  def compileCached(
    model: ViewerModel,
    cache: ViewerCache
  ): Either[ImageViewError, ViewerCompilation] =
    ViewerCompiler.compileCached(model, state, device, cache, layout)

object ViewerReducer:
  def reduce(
    model: ViewerModel,
    session: ViewerSession,
    action: ViewerAction
  ): Either[ImageViewError, ViewerSession] =
    action match
      case ViewerAction.Pick(plane, pointer) =>
        val panel = ViewerCompiler.panels(model.referenceSpace, session.state, session.device, session.layout)(plane)
        panel.worldAtRootNpc(pointer.rootX, pointer.rootY) match
          case Some(cursor) => Right(session.copy(state = session.state.copy(cursor = cursor)))
          case None => Left(ImageViewError.PointerOutsidePanel(plane))
      case ViewerAction.Scroll(plane, steps) =>
        val distance = steps.toDouble * session.state.sliceStep.millimeters
        val cursor = session.state.cursor + plane.positiveNormal.unit.scaled(distance)
        Right(session.copy(state = session.state.copy(cursor = cursor)))
      case ViewerAction.SetCursor(cursor) =>
        Right(session.copy(state = session.state.copy(cursor = cursor)))
      case ViewerAction.SetConvention(convention) =>
        Right(session.copy(state = session.state.copy(convention = convention)))
      case ViewerAction.SetPixelSpacing(spacing) =>
        Right(session.copy(state = session.state.copy(pixelSpacing = spacing)))
      case ViewerAction.SetSliceStep(step) =>
        Right(session.copy(state = session.state.copy(sliceStep = step)))
      case ViewerAction.SetPanelView(plane, view) =>
        Right(
          session.copy(
            state = session.state.copy(panelViews = session.state.panelViews.updated(plane, view))
          )
        )
      case ViewerAction.ResetPanelView(plane) =>
        Right(
          session.copy(
            state = session.state.copy(panelViews = session.state.panelViews.updated(plane, PanelView.Default))
          )
        )
      case ViewerAction.SetWindow(layer, window) =>
        withLayer(model, session, layer) { (sliceLayer, current) =>
          if !sliceLayer.supportsWindow then Left(ImageViewError.WindowUnsupported(layer))
          else Right(current.copy(window = Some(window)))
        }
      case ViewerAction.ClearWindow(layer) =>
        withLayer(model, session, layer) { (_, current) =>
          Right(current.copy(window = None))
        }
      case ViewerAction.SetThreshold(layer, threshold) =>
        withLayer(model, session, layer) { (sliceLayer, current) =>
          if !sliceLayer.supportsThreshold then Left(ImageViewError.ThresholdUnsupported(layer))
          else Right(current.copy(threshold = Some(threshold)))
        }
      case ViewerAction.ClearThreshold(layer) =>
        withLayer(model, session, layer) { (_, current) =>
          Right(current.copy(threshold = None))
        }
      case ViewerAction.SetOpacity(layer, opacity) =>
        withLayer(model, session, layer) { (_, current) =>
          Right(current.copy(opacity = Some(opacity)))
        }
      case ViewerAction.SetVisibility(layer, visible) =>
        withLayer(model, session, layer) { (_, current) =>
          Right(current.copy(visible = visible))
        }
      case ViewerAction.SetTimepoint(index) =>
        if index >= 0 && index < model.timepointCount then
          Right(session.copy(state = session.state.copy(timepoint = index)))
        else Left(ImageViewError.TimepointOutOfBounds(index, model.timepointCount))
      case ViewerAction.Resize(device) =>
        Right(session.copy(device = device))
      case ViewerAction.ShowCrosshair(visible) =>
        Right(session.copy(state = session.state.copy(showCrosshair = visible)))
      case ViewerAction.ShowOrientationLabels(visible) =>
        Right(session.copy(state = session.state.copy(showOrientationLabels = visible)))

  private def withLayer(
    model: ViewerModel,
    session: ViewerSession,
    id: LayerId
  )(
    update: (SliceLayer, LayerPresentation) => Either[ImageViewError, LayerPresentation]
  ): Either[ImageViewError, ViewerSession] =
    model.layer(id) match
      case None => Left(ImageViewError.UnknownLayer(id))
      case Some(layer) =>
        update(layer, session.state.presentation(id)).map { next =>
          val state = session.state.copy(
            layerPresentation = session.state.layerPresentation.updated(id, next)
          )
          session.copy(state = state)
        }

  /** Swap a layer's scalar volume without touching session presentation. */
  def replaceLayerData(
      model: ViewerModel,
      session: ViewerSession,
      layer: LayerId,
      volume: NeuroVol[Double]
  ): Either[ImageViewError, (ViewerModel, ViewerSession)] =
    model.replaceLayerVolume(layer, volume).map(next => (next, session))

object ViewerEvents:
  def pick(
    frame: ViewerFrame,
    deviceX: Double,
    deviceY: Double
  ): Either[ImageViewError, ViewerAction] =
    actionAt(frame, deviceX, deviceY) { (plane, pointer) =>
      ViewerAction.Pick(plane, pointer)
    }

  def scroll(
    frame: ViewerFrame,
    deviceX: Double,
    deviceY: Double,
    steps: Int
  ): Either[ImageViewError, ViewerAction] =
    actionAt(frame, deviceX, deviceY) { (plane, _) =>
      ViewerAction.Scroll(plane, steps)
    }

  private def actionAt(
    frame: ViewerFrame,
    deviceX: Double,
    deviceY: Double
  )(
    action: (AnatomicalPlane, ViewerPointer) => ViewerAction
  ): Either[ImageViewError, ViewerAction] =
    ViewerPointer.fromDevice(deviceX, deviceY, frame.device).flatMap { pointer =>
      frame.panels.all.find(_.rect.contains(pointer.rootX, pointer.rootY)) match
        case Some(panel) => Right(action(panel.anatomicalPlane, pointer))
        case None => Left(ImageViewError.PointerOutsideViewer)
    }
