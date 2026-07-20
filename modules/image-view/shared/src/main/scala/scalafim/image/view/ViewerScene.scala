package scalafim.image.view

import scalafim.graphics.*
import scalafim.image.*

final case class ViewerState(
  cursor: WorldPoint,
  pixelSpacing: PixelSpacing,
  sliceStep: SliceStep = SliceStep.One,
  convention: LeftRightConvention = LeftRightConvention.PatientLeftOnLeft,
  showCrosshair: Boolean = true,
  showOrientationLabels: Boolean = true,
  timepoint: Int = 0,
  layerPresentation: Map[LayerId, LayerPresentation] = Map.empty
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
  window: Option[DisplayWindow] = None
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

final case class OrthogonalLayout private (margin: Double, gap: Double):
  private[view] def cells: PanelReceipts[PanelRect] =
    val extent = (1.0 - 2.0 * margin - gap) / 2.0
    val low = margin
    val high = margin + extent + gap
    PanelReceipts(
      sagittal = PanelRect.unsafe(high, high, extent, extent),
      coronal = PanelRect.unsafe(low, low, extent, extent),
      axial = PanelRect.unsafe(low, high, extent, extent)
    )

object OrthogonalLayout:
  val Default: OrthogonalLayout =
    new OrthogonalLayout(margin = 0.02, gap = 0.02)

  def make(margin: Double, gap: Double): Either[ImageViewError, OrthogonalLayout] =
    val valid =
      margin.isFinite && gap.isFinite && margin >= 0.0 && gap >= 0.0 &&
        2.0 * margin + gap < 1.0
    if valid then Right(new OrthogonalLayout(margin, gap))
    else Left(ImageViewError.InvalidOrthogonalLayout(margin, gap))

final case class PanelReceipt(
  anatomicalPlane: AnatomicalPlane,
  rect: PanelRect,
  grid: SliceGrid
):
  def cursorRootNpc(cursor: WorldPoint): (Double, Double) =
    val projected = grid.project(cursor).pixel
    val localX = (projected.column + 0.5) / grid.dimensions.width
    val localY = 1.0 - (projected.row + 0.5) / grid.dimensions.height
    rect.left + localX * rect.width -> (rect.bottom + localY * rect.height)

  def worldAtRootNpc(rootX: Double, rootY: Double): Option[WorldPoint] =
    if !rect.contains(rootX, rootY) then None
    else
      val localX = (rootX - rect.left) / rect.width
      val localY = (rootY - rect.bottom) / rect.height
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

final case class ViewerFrame(
  scene: Scene,
  panels: PanelReceipts[PanelReceipt],
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
      var currentCache = cache
      var profile = ViewerProfile.Zero
      var error = Option.empty[ImageViewError]
      var index = 0
      val allPanels = panelReceipts.all
      while index < allPanels.length && error.isEmpty do
        panelGrob(model, state, allPanels(index), theme, currentCache) match
          case Left(value) => error = Some(value)
          case Right(compiled) =>
            grobs += compiled.grob
            currentCache = compiled.cache
            profile = profile + compiled.profile
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          val frame = ViewerFrame(Scene(grobs.result()), panelReceipts, state, device)
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
      sagittal = receipt(AnatomicalPlane.Sagittal, grids.sagittal, cells.sagittal, device),
      coronal = receipt(AnatomicalPlane.Coronal, grids.coronal, cells.coronal, device),
      axial = receipt(AnatomicalPlane.Axial, grids.axial, cells.axial, device)
    )

  private def receipt(
    anatomicalPlane: AnatomicalPlane,
    grid: SliceGrid,
    cell: PanelRect,
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
    PanelReceipt(anatomicalPlane, fitted, grid)

  private def panelGrob(
    model: ViewerModel,
    state: ViewerState,
    panel: PanelReceipt,
    theme: ViewerTheme,
    cache: ViewerCache
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
    var currentCache = cache
    var profile = ViewerProfile(panelCount = 1, layerRequests = 0, cacheHits = 0, cacheMisses = 0, sampledPixels = 0L)
    var error = Option.empty[ImageViewError]
    var index = 0
    while index < visibleLayers.length && error.isEmpty do
      layerGrob(visibleLayers(index), panel.grid, state, currentCache) match
        case Left(value) => error = Some(value)
        case Right(compiled) =>
          images += compiled.grob
          currentCache = compiled.cache
          profile = profile + compiled.profile
      index += 1
    error match
      case Some(value) => Left(value)
      case None =>
        decorationGrobs(state, panel, theme).map { overlay =>
          val group = Grob.group(background +: (images.result() ++ overlay), viewport = Some(viewport))
          PanelCompilation(group, currentCache, profile)
        }

  private def layerGrob(
    layer: SliceLayer,
    grid: SliceGrid,
    state: ViewerState,
    cache: ViewerCache
  ): Either[ImageViewError, LayerCompilation] =
    val presentation = state.presentation(layer.id)
    val key = SliceCacheKey(layer, grid, state.timepoint, presentation.window)
    val (cached, refreshedCache) = cache.lookup(key)
    val rasterAndProfile =
      cached match
        case Some(raster) =>
          Right(
            (
              raster,
              refreshedCache,
              ViewerProfile(0, 1, 1, 0, 0L)
            )
          )
        case None =>
          layer.raster(grid, state.timepoint, presentation.window).map { raster =>
            (
              raster,
              cache.store(key, raster),
              ViewerProfile(0, 1, 0, 1, grid.dimensions.pixelCount.toLong)
            )
          }
    rasterAndProfile.flatMap { case (raster, nextCache, profile) =>
      Grob.image(
        image = raster,
        at = Point.npcUnsafe(0.0, 0.0),
        size = Size.npcUnsafe(1.0, 1.0),
        anchor = Anchor.BottomLeft,
        interpolation = layer.displayInterpolation,
        alpha = presentation.opacity.getOrElse(layer.opacity).toDouble
      ).left.map(ImageViewError.GraphicsFailure.apply).map { grob =>
        LayerCompilation(grob, nextCache, profile)
      }
    }

  private final case class LayerCompilation(
    grob: Grob,
    cache: ViewerCache,
    profile: ViewerProfile
  )

  private final case class PanelCompilation(
    grob: Grob,
    cache: ViewerCache,
    profile: ViewerProfile
  )

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
        Grob.segments(
          Vector(
            Point.npcUnsafe(x, 0.0) -> Point.npcUnsafe(x, 1.0),
            Point.npcUnsafe(0.0, y) -> Point.npcUnsafe(1.0, y)
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
