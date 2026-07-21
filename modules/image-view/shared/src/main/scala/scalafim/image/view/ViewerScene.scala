package scalafim.image.view

import scalafim.graphics.*
import scalafim.image.*
import scala.collection.mutable

final case class ViewerState(
  cursor: WorldPoint,
  pixelSpacing: PixelSpacing,
  sliceStep: SliceStep = SliceStep.One,
  convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft,
  showCrosshair: Boolean = true,
  showOrientationLabels: Boolean = true,
  timepoint: Int = 0,
  layerPresentation: Map[LayerId, LayerPresentation] = Map.empty,
  panelViews: PanelReceipts[PanelView] = PanelReceipts.fill(PanelView.Default)
):
  def presentation(id: LayerId): LayerPresentation =
    layerPresentation.getOrElse(id, LayerPresentation.Default)

object ViewerState:
  def centered(
    referenceSpace: VolumeSpace,
    convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft
  ): ViewerState =
    val shape = referenceSpace.shape
    val center = referenceSpace.voxelToWorld(
      VoxelPoint(
        (shape.x - 1).toDouble / 2.0,
        (shape.y - 1).toDouble / 2.0,
        (shape.z - 1).toDouble / 2.0
      )
    )
    val nativeStep = referenceSpace.affine.voxelSizes.min
    ViewerState(
      cursor = center,
      pixelSpacing = PixelSpacing(nativeStep, nativeStep),
      sliceStep = SliceStep.unsafe(nativeStep),
      convention = convention
    )

final case class LayerPresentation(
  visible: Boolean = true,
  opacity: Option[LayerOpacity] = None,
  window: Option[DisplayWindow] = None,
  threshold: Option[DisplayThreshold] = None
)

object LayerPresentation:
  val Default: LayerPresentation =
    LayerPresentation()

final case class ViewerTheme(
  background: Rgba,
  border: Rgba,
  crosshair: Rgba,
  orientationLabel: Rgba
)

object ViewerTheme:
  val Default: ViewerTheme =
    ViewerTheme(
      background = Rgba.Black,
      border = Rgba.unsafe(72, 72, 72),
      crosshair = Rgba.unsafe(255, 214, 0, 0.85),
      orientationLabel = Rgba.White
    )

final case class PanelRect private (
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
):
  def right: Double =
    left + width

  def top: Double =
    bottom + height

  def contains(rootX: Double, rootY: Double): Boolean =
    rootX >= left && rootX <= right && rootY >= bottom && rootY <= top

object PanelRect:
  private[view] def unsafe(left: Double, bottom: Double, width: Double, height: Double): PanelRect =
    require(left.isFinite && bottom.isFinite, "panel origin must be finite")
    require(width.isFinite && height.isFinite && width > 0.0 && height > 0.0, "panel size must be positive and finite")
    new PanelRect(left, bottom, width, height)

enum OrthogonalArrangement:
  case LShape, SingleRow, SingleColumn

final case class OrthogonalLayout private (
  margin: Double,
  gap: Double,
  arrangement: OrthogonalArrangement
):
  private[view] def cells: PanelReceipts[PanelRect] =
    arrangement match
      case OrthogonalArrangement.LShape =>
        val extent = (1.0 - 2.0 * margin - gap) / 2.0
        val low = margin
        val high = margin + extent + gap
        PanelReceipts(
          sagittal = PanelRect.unsafe(high, high, extent, extent),
          coronal = PanelRect.unsafe(low, low, extent, extent),
          axial = PanelRect.unsafe(low, high, extent, extent)
        )
      case OrthogonalArrangement.SingleRow =>
        val width = (1.0 - 2.0 * margin - 2.0 * gap) / 3.0
        val height = 1.0 - 2.0 * margin
        PanelReceipts(
          sagittal = PanelRect.unsafe(margin, margin, width, height),
          coronal = PanelRect.unsafe(margin + width + gap, margin, width, height),
          axial = PanelRect.unsafe(margin + 2.0 * (width + gap), margin, width, height)
        )
      case OrthogonalArrangement.SingleColumn =>
        val width = 1.0 - 2.0 * margin
        val height = (1.0 - 2.0 * margin - 2.0 * gap) / 3.0
        PanelReceipts(
          sagittal = PanelRect.unsafe(margin, margin + 2.0 * (height + gap), width, height),
          coronal = PanelRect.unsafe(margin, margin + height + gap, width, height),
          axial = PanelRect.unsafe(margin, margin, width, height)
        )

object OrthogonalLayout:
  val Default: OrthogonalLayout =
    new OrthogonalLayout(margin = 0.02, gap = 0.02, OrthogonalArrangement.LShape)

  def make(
    margin: Double,
    gap: Double,
    arrangement: OrthogonalArrangement = OrthogonalArrangement.LShape
  ): Either[ImageViewError, OrthogonalLayout] =
    val gapCount = if arrangement == OrthogonalArrangement.LShape then 1.0 else 2.0
    val valid =
      margin.isFinite && gap.isFinite && margin >= 0.0 && gap >= 0.0 &&
        2.0 * margin + gapCount * gap < 1.0
    if valid then Right(new OrthogonalLayout(margin, gap, arrangement))
    else Left(ImageViewError.InvalidOrthogonalLayout(margin, gap))

final case class PanelReceipt(
  anatomicalPlane: AnatomicalPlane,
  rect: PanelRect,
  grid: SliceGrid,
  view: PanelView
):
  def cursorRootNpc(cursor: WorldPoint): (Double, Double) =
    val projected = grid.project(cursor).pixel
    val localX = (projected.column + 0.5) / grid.dimensions.width
    val localY = 1.0 - (projected.row + 0.5) / grid.dimensions.height
    val viewedX = view.imageToLocal(localX, view.centerX)
    val viewedY = view.imageToLocal(localY, view.centerY)
    rect.left + viewedX * rect.width -> (rect.bottom + viewedY * rect.height)

  def worldAtRootNpc(rootX: Double, rootY: Double): Option[WorldPoint] =
    if !rect.contains(rootX, rootY) then None
    else
      val viewedX = (rootX - rect.left) / rect.width
      val viewedY = (rootY - rect.bottom) / rect.height
      val localX = view.localToImage(viewedX, view.centerX)
      val localY = view.localToImage(viewedY, view.centerY)
      val column = localX * grid.dimensions.width - 0.5
      val row = (1.0 - localY) * grid.dimensions.height - 0.5
      Some(
        grid.topLeftCenter +
          grid.plane.screenRight.scaled(column * grid.spacing.horizontal) +
          grid.plane.screenUp.scaled(-row * grid.spacing.vertical)
      )

final case class PanelReceipts[A](
  sagittal: A,
  coronal: A,
  axial: A
):
  def apply(plane: AnatomicalPlane): A =
    plane match
      case AnatomicalPlane.Sagittal => sagittal
      case AnatomicalPlane.Coronal => coronal
      case AnatomicalPlane.Axial => axial

  def all: Vector[A] =
    Vector(sagittal, coronal, axial)

  def updated(plane: AnatomicalPlane, value: A): PanelReceipts[A] =
    plane match
      case AnatomicalPlane.Sagittal => copy(sagittal = value)
      case AnatomicalPlane.Coronal => copy(coronal = value)
      case AnatomicalPlane.Axial => copy(axial = value)

object PanelReceipts:
  def fill[A](value: A): PanelReceipts[A] =
    PanelReceipts(value, value, value)

final case class LayerReadout(layer: LayerId, value: LayerSampleValue)

final case class PanelReadout(
  anatomicalPlane: AnatomicalPlane,
  world: WorldPoint,
  referenceVoxel: VoxelPoint,
  layers: Vector[LayerReadout]
)

final case class ViewerFrame(
  scene: Scene,
  panels: PanelReceipts[PanelReceipt],
  readouts: PanelReceipts[PanelReadout],
  state: ViewerState,
  device: DeviceContext
)

object ViewerCompiler:
  def compile(
    model: ViewerModel,
    state: ViewerState,
    device: DeviceContext,
    layout: OrthogonalLayout = OrthogonalLayout.Default,
    theme: ViewerTheme = ViewerTheme.Default
  ): Either[ImageViewError, ViewerFrame] =
    compileCached(model, state, device, ViewerCache.Disabled, layout, theme).map(_.frame)

  def compileCached(
    model: ViewerModel,
    state: ViewerState,
    device: DeviceContext,
    cache: ViewerCache,
    layout: OrthogonalLayout = OrthogonalLayout.Default,
    theme: ViewerTheme = ViewerTheme.Default
  ): Either[ImageViewError, ViewerCompilation] =
    if state.timepoint < 0 || state.timepoint >= model.timepointCount then
      Left(ImageViewError.TimepointOutOfBounds(state.timepoint, model.timepointCount))
    else
      val panelReceipts = panels(model.referenceSpace, state, device, layout)
      val grobs = Vector.newBuilder[Grob]
      val readouts = Vector.newBuilder[PanelReadout]
      var currentCache = cache
      var profile = ViewerProfile.Zero
      val frameResolver = new FrameResolver(state.timepoint)
      var error = Option.empty[ImageViewError]
      var index = 0
      val allPanels = panelReceipts.all
      while index < allPanels.length && error.isEmpty do
        panelGrob(model, state, allPanels(index), theme, currentCache, frameResolver) match
          case Left(value) => error = Some(value)
          case Right(compiled) =>
            grobs += compiled.grob
            readouts += compiled.readout
            currentCache = compiled.cache
            profile = profile + compiled.profile
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val panelReadouts = readouts.result()
          val frame = ViewerFrame(
            Scene(grobs.result()),
            panelReceipts,
            PanelReceipts(panelReadouts(0), panelReadouts(1), panelReadouts(2)),
            state,
            device
          )
          Right(ViewerCompilation(frame, currentCache, profile))

  def panels(
    referenceSpace: VolumeSpace,
    state: ViewerState,
    device: DeviceContext,
    layout: OrthogonalLayout = OrthogonalLayout.Default
  ): PanelReceipts[PanelReceipt] =
    val grids = OrthogonalSliceGrids.covering(
      referenceSpace,
      state.cursor,
      state.pixelSpacing,
      state.convention
    )
    val cells = layout.cells
    PanelReceipts(
      sagittal = receipt(AnatomicalPlane.Sagittal, grids.sagittal, cells.sagittal, state.panelViews.sagittal, device),
      coronal = receipt(AnatomicalPlane.Coronal, grids.coronal, cells.coronal, state.panelViews.coronal, device),
      axial = receipt(AnatomicalPlane.Axial, grids.axial, cells.axial, state.panelViews.axial, device)
    )

  private def receipt(
    anatomicalPlane: AnatomicalPlane,
    grid: SliceGrid,
    cell: PanelRect,
    view: PanelView,
    device: DeviceContext
  ): PanelReceipt =
    val physicalWidth = grid.dimensions.width * grid.spacing.horizontal
    val physicalHeight = grid.dimensions.height * grid.spacing.vertical
    val contentAspect = physicalWidth / physicalHeight
    val cellWidthPixels = cell.width * device.width
    val cellHeightPixels = cell.height * device.height
    val cellAspect = cellWidthPixels / cellHeightPixels
    val fitted =
      if contentAspect >= cellAspect then
        val height = cellWidthPixels / contentAspect / device.height
        PanelRect.unsafe(cell.left, cell.bottom + (cell.height - height) / 2.0, cell.width, height)
      else
        val width = cellHeightPixels * contentAspect / device.width
        PanelRect.unsafe(cell.left + (cell.width - width) / 2.0, cell.bottom, width, cell.height)
    PanelReceipt(anatomicalPlane, fitted, grid, view)

  private def panelGrob(
    model: ViewerModel,
    state: ViewerState,
    panel: PanelReceipt,
    theme: ViewerTheme,
    cache: ViewerCache,
    frameResolver: FrameResolver
  ): Either[ImageViewError, PanelCompilation] =
    val viewport = Viewport.unsafe(
      origin = Point.npcUnsafe(panel.rect.left, panel.rect.bottom),
      size = Size.npcUnsafe(panel.rect.width, panel.rect.height)
    )
    val background = Grob.rectUnsafe(
      center = Point.npcUnsafe(0.0, 0.0),
      size = Size.npcUnsafe(1.0, 1.0),
      anchor = Anchor.BottomLeft,
      gp = GraphicParams.unsafe(
        stroke = Some(theme.border),
        fill = Some(theme.background),
        lineWidth = 1.0
      )
    )
    val visibleLayers = model.layers.filter(layer => state.presentation(layer.id).visible)
    val images = Vector.newBuilder[Grob]
    val readouts = Vector.newBuilder[LayerReadout]
    var currentCache = cache
    var profile = ViewerProfile(panelCount = 1, layerRequests = 0, cacheHits = 0, cacheMisses = 0, sampledPixels = 0L)
    var error = Option.empty[ImageViewError]
    var index = 0
    while index < visibleLayers.length && error.isEmpty do
      layerGrob(visibleLayers(index), panel, state, currentCache, frameResolver) match
        case Left(value) => error = Some(value)
        case Right(compiled) =>
          images += compiled.grob
          compiled.readout.foreach(readouts += _)
          currentCache = compiled.cache
          profile = profile + compiled.profile
      index += 1
    error match
      case Some(value) => Left(value)
      case None =>
        decorationGrobs(state, panel, theme).map { overlay =>
          val group = Grob.group(background +: (images.result() ++ overlay), viewport = Some(viewport))
          val readout = PanelReadout(
            panel.anatomicalPlane,
            state.cursor,
            model.referenceSpace.worldToVoxel(state.cursor),
            readouts.result()
          )
          PanelCompilation(group, readout, currentCache, profile)
        }

  private def layerGrob(
    layer: SliceLayer,
    panel: PanelReceipt,
    state: ViewerState,
    cache: ViewerCache,
    frameResolver: FrameResolver
  ): Either[ImageViewError, LayerCompilation] =
    val presentation = state.presentation(layer.id)
    val grid = panel.grid
    val sampleKey = SliceSampleKey.from(layer, grid, state.timepoint)
    val rasterKey = SliceRasterKey(sampleKey, presentation.window, presentation.threshold)
    val (cached, refreshedCache) = cache.lookupRaster(rasterKey)
    val rasterAndProfile: Either[ImageViewError, (RasterImage, Option[LayerReadout], ViewerCache, ViewerProfile)] =
      cached match
        case Some(raster) =>
          val (sample, sampleCache) = refreshedCache.lookupSample(sampleKey)
          Right(
            (
              raster,
              sample.flatMap(readout(layer.id, _, panel, state.cursor)),
              sampleCache,
              ViewerProfile(0, 1, 1, 0, 0L)
            )
          )
        case None =>
          val pixelCount = grid.dimensions.pixelCount.toLong
          val (sampled, sampleCache) = refreshedCache.lookupSample(sampleKey)
          sampled match
            case Some(sample) =>
              val raster = sample.colorize(presentation.window, presentation.threshold)
              Right(
                (
                  raster,
                  readout(layer.id, sample, panel, state.cursor),
                  sampleCache.storeRaster(rasterKey, raster),
                  ViewerProfile(0, 1, 0, 1, 0L, 1, 0, pixelCount)
                )
              )
            case None =>
              frameResolver.resolve(layer).flatMap { case (frame, sourceRead) =>
                frame.sample(grid).map { sample =>
                  val raster = sample.colorize(presentation.window, presentation.threshold)
                  val nextCache = sampleCache
                    .storeSample(sampleKey, sample)
                    .storeRaster(rasterKey, raster)
                  (
                    raster,
                    readout(layer.id, sample, panel, state.cursor),
                    nextCache,
                    ViewerProfile(
                      0,
                      1,
                      0,
                      1,
                      pixelCount,
                      0,
                      1,
                      pixelCount,
                      if sourceRead then 1 else 0
                    )
                  )
                }
              }
    rasterAndProfile.flatMap { case (raster, layerReadout, nextCache, profile) =>
      Grob.image(
        image = raster,
        at = Point.npcUnsafe(panel.view.imageLeft, panel.view.imageBottom),
        size = Size.npcUnsafe(panel.view.zoom.factor, panel.view.zoom.factor),
        anchor = Anchor.BottomLeft,
        interpolation = layer.displayInterpolation,
        alpha = presentation.opacity.getOrElse(layer.opacity).toDouble
      ).left.map(ImageViewError.GraphicsFailure.apply).map { grob =>
        LayerCompilation(grob, layerReadout, nextCache, profile)
      }
    }

  private def readout(
    id: LayerId,
    sample: SampledLayerSlice,
    panel: PanelReceipt,
    cursor: WorldPoint
  ): Option[LayerReadout] =
    val pixel = panel.grid.project(cursor).pixel
    val column = math.round(pixel.column).toInt
    val row = math.round(pixel.row).toInt
    sample.readout(column, row).map(LayerReadout(id, _))

  private final case class LayerCompilation(
    grob: Grob,
    readout: Option[LayerReadout],
    cache: ViewerCache,
    profile: ViewerProfile
  )

  private final case class PanelCompilation(
    grob: Grob,
    readout: PanelReadout,
    cache: ViewerCache,
    profile: ViewerProfile
  )

  private final class FrameResolver(timepoint: Int):
    private val frames = mutable.HashMap.empty[LayerId, ResolvedLayerFrame]

    def resolve(layer: SliceLayer): Either[ImageViewError, (ResolvedLayerFrame, Boolean)] =
      frames.get(layer.id) match
        case Some(frame) => Right(frame -> false)
        case None =>
          layer.resolve(timepoint).map { frame =>
            frames.update(layer.id, frame)
            frame -> true
          }

  private def decorationGrobs(
    state: ViewerState,
    panel: PanelReceipt,
    theme: ViewerTheme
  ): Either[ImageViewError, Vector[Grob]] =
    val crosshair =
      if !state.showCrosshair then Right(Vector.empty)
      else
        val projected = panel.grid.project(state.cursor).pixel
        val x = (projected.column + 0.5) / panel.grid.dimensions.width
        val y = 1.0 - (projected.row + 0.5) / panel.grid.dimensions.height
        val viewedX = panel.view.imageToLocal(x, panel.view.centerX)
        val viewedY = panel.view.imageToLocal(y, panel.view.centerY)
        Grob.segments(
          Vector(
            Point.npcUnsafe(viewedX, 0.0) -> Point.npcUnsafe(viewedX, 1.0),
            Point.npcUnsafe(0.0, viewedY) -> Point.npcUnsafe(1.0, viewedY)
          ),
          gp = GraphicParams.unsafe(stroke = Some(theme.crosshair), lineWidth = 1.0)
        ).left.map(ImageViewError.GraphicsFailure.apply).map(Vector(_))

    val labels =
      if !state.showOrientationLabels then Right(Vector.empty)
      else orientationLabels(panel.grid.plane, theme)

    for
      crosshairGrobs <- crosshair
      labelGrobs <- labels
    yield crosshairGrobs ++ labelGrobs

  private def orientationLabels(
    plane: SlicePlane,
    theme: ViewerTheme
  ): Either[ImageViewError, Vector[Grob]] =
    val gp = GraphicParams.unsafe(
      stroke = None,
      fill = Some(theme.orientationLabel),
      fontSize = Length.pointsUnsafe(11.0)
    )
    val labels = Vector(
      abbreviation(plane.screenRight.opposite) -> Point.npcUnsafe(0.03, 0.5),
      abbreviation(plane.screenRight) -> Point.npcUnsafe(0.97, 0.5),
      abbreviation(plane.screenUp) -> Point.npcUnsafe(0.5, 0.97),
      abbreviation(plane.screenUp.opposite) -> Point.npcUnsafe(0.5, 0.03)
    )
    sequence(labels.map { case (label, at) =>
      Grob.text(label, at, gp = gp).left.map(ImageViewError.GraphicsFailure.apply)
    })

  private def abbreviation(direction: UnitWorldVector): String =
    val values = Vector(math.abs(direction.x), math.abs(direction.y), math.abs(direction.z))
    val axis = values.zipWithIndex.maxBy(_._1)._2
    axis match
      case 0 => if direction.x >= 0.0 then "R" else "L"
      case 1 => if direction.y >= 0.0 then "A" else "P"
      case _ => if direction.z >= 0.0 then "S" else "I"

  private def sequence[A](values: Vector[Either[ImageViewError, A]]): Either[ImageViewError, Vector[A]] =
    val out = Vector.newBuilder[A]
    var index = 0
    var error = Option.empty[ImageViewError]
    while index < values.length && error.isEmpty do
      values(index) match
        case Left(value) => error = Some(value)
        case Right(value) => out += value
      index += 1
    error match
      case Some(value) => Left(value)
      case None => Right(out.result())
