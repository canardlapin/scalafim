package scalafim.image.view

import intaglio.*
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
  case LShape, SingleRow, SingleColumn, Automatic
  case SinglePlane(plane: AnatomicalPlane)

/** Units for the outer margin and the gap between layout cells. */
enum OrthogonalLayoutUnits:
  case Normalized, LogicalPixels

final case class OrthogonalLayout private (
  margin: Double,
  gap: Double,
  arrangement: OrthogonalArrangement,
  units: OrthogonalLayoutUnits = OrthogonalLayoutUnits.Normalized
):
  def shows(plane: AnatomicalPlane): Boolean =
    arrangement match
      case OrthogonalArrangement.SinglePlane(selected) => plane == selected
      case _ => true

  private[view] def cells(grids: OrthogonalSliceGrids, device: DeviceContext): PanelReceipts[PanelRect] =
    // At very small sizes, reduce pixel insets together rather than producing negative cells.
    val (mx, my, gx, gy) = units match
      case OrthogonalLayoutUnits.Normalized => (margin, margin, gap, gap)
      case OrthogonalLayoutUnits.LogicalPixels =>
        val scale = math.min(1.0, math.min(device.width, device.height) / (2.0 * margin + 2.0 * gap + 3.0))
        (margin * scale / device.width, margin * scale / device.height,
          gap * scale / device.width, gap * scale / device.height)
    def arranged(kind: OrthogonalArrangement): PanelReceipts[PanelRect] = kind match
      case OrthogonalArrangement.LShape =>
        val width = (1.0 - 2.0 * mx - gx) / 2.0
        val height = (1.0 - 2.0 * my - gy) / 2.0
        PanelReceipts(
          sagittal = PanelRect.unsafe(mx + width + gx, my + height + gy, width, height),
          coronal = PanelRect.unsafe(mx, my, width, height),
          axial = PanelRect.unsafe(mx, my + height + gy, width, height)
        )
      case OrthogonalArrangement.SingleRow =>
        val width = (1.0 - 2.0 * mx - 2.0 * gx) / 3.0
        val height = 1.0 - 2.0 * my
        PanelReceipts(
          sagittal = PanelRect.unsafe(mx, my, width, height),
          coronal = PanelRect.unsafe(mx + width + gx, my, width, height),
          axial = PanelRect.unsafe(mx + 2.0 * (width + gx), my, width, height)
        )
      case OrthogonalArrangement.SingleColumn =>
        val width = 1.0 - 2.0 * mx
        val height = (1.0 - 2.0 * my - 2.0 * gy) / 3.0
        PanelReceipts(
          sagittal = PanelRect.unsafe(mx, my + 2.0 * (height + gy), width, height),
          coronal = PanelRect.unsafe(mx, my + height + gy, width, height),
          axial = PanelRect.unsafe(mx, my, width, height)
        )
      case OrthogonalArrangement.SinglePlane(_) =>
        PanelReceipts.fill(PanelRect.unsafe(mx, my, 1.0 - 2.0 * mx, 1.0 - 2.0 * my))
      case OrthogonalArrangement.Automatic =>
        // Stable ties prefer the familiar L; resize decisions use geometry, never image values.
        val candidates = Vector(OrthogonalArrangement.LShape, OrthogonalArrangement.SingleRow,
          OrthogonalArrangement.SingleColumn).map(arranged)
        def area(cells: PanelReceipts[PanelRect]): Double =
          val fitted = Vector(
            OrthogonalLayout.fit(grids.sagittal, cells.sagittal, device),
            OrthogonalLayout.fit(grids.coronal, cells.coronal, device),
            OrthogonalLayout.fit(grids.axial, cells.axial, device)
          )
          fitted.map(rect => rect.width * rect.height).sum
        candidates.tail.foldLeft(candidates.head) { (best, candidate) =>
          if area(candidate) > area(best) then candidate else best
        }
    arranged(arrangement)

object OrthogonalLayout:
  val Default: OrthogonalLayout =
    new OrthogonalLayout(margin = 0.02, gap = 0.02, OrthogonalArrangement.LShape)

  /** Adaptive fitting preserves slice aspect and chooses the greatest occupied image area among
    * row, column and L arrangements. Logical-pixel insets remain constant on resize, except when
    * the viewport is too small to contain them, when all insets shrink proportionally.
    */
  def adaptive(marginPixels: Double = 12.0, gapPixels: Double = 12.0): Either[ImageViewError, OrthogonalLayout] =
    make(marginPixels, gapPixels, OrthogonalArrangement.Automatic, OrthogonalLayoutUnits.LogicalPixels)

  def make(
    margin: Double,
    gap: Double,
    arrangement: OrthogonalArrangement = OrthogonalArrangement.LShape,
    units: OrthogonalLayoutUnits = OrthogonalLayoutUnits.Normalized
  ): Either[ImageViewError, OrthogonalLayout] =
    val gapCount = arrangement match
      case OrthogonalArrangement.LShape => 1.0
      case OrthogonalArrangement.SinglePlane(_) => 0.0
      case _ => 2.0
    val valid =
      margin.isFinite && gap.isFinite && margin >= 0.0 && gap >= 0.0 &&
        (units == OrthogonalLayoutUnits.LogicalPixels || 2.0 * margin + gapCount * gap < 1.0)
    if valid then Right(new OrthogonalLayout(margin, gap, arrangement, units))
    else Left(ImageViewError.InvalidOrthogonalLayout(margin, gap))

  private[view] def fit(grid: SliceGrid, cell: PanelRect, device: DeviceContext): PanelRect =
    val physicalWidth = grid.dimensions.width * grid.spacing.horizontal
    val physicalHeight = grid.dimensions.height * grid.spacing.vertical
    val contentAspect = physicalWidth / physicalHeight
    val cellWidthPixels = cell.width * device.width
    val cellHeightPixels = cell.height * device.height
    val cellAspect = cellWidthPixels / cellHeightPixels
    if contentAspect >= cellAspect then
      val height = cellWidthPixels / contentAspect / device.height
      PanelRect.unsafe(cell.left, cell.bottom + (cell.height - height) / 2.0, cell.width, height)
    else
      val width = cellHeightPixels * contentAspect / device.width
      PanelRect.unsafe(cell.left + (cell.width - width) / 2.0, cell.bottom, width, cell.height)

final case class PanelReceipt(
  anatomicalPlane: AnatomicalPlane,
  rect: PanelRect,
  grid: SliceGrid,
  view: PanelView,
  visible: Boolean = true
):
  def cursorRootNpc(cursor: WorldPoint): (Double, Double) =
    val projected = grid.project(cursor).pixel
    val localX = (projected.column + 0.5) / grid.dimensions.width
    val localY = 1.0 - (projected.row + 0.5) / grid.dimensions.height
    val viewedX = view.imageToLocal(localX, view.centerX)
    val viewedY = view.imageToLocal(localY, view.centerY)
    rect.left + viewedX * rect.width -> (rect.bottom + viewedY * rect.height)

  def worldAtRootNpc(rootX: Double, rootY: Double): Option[WorldPoint] =
    if !visible || !rect.contains(rootX, rootY) then None
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
):
  /** Hidden planes retain named geometry and view state, but are not rendered or sampled. */
  def visiblePanels: Vector[PanelReceipt] = panels.all.filter(_.visible)

  /** Hidden planes have empty layer readouts; use this for presentation. */
  def visibleReadouts: Vector[PanelReadout] = visiblePanels.map(panel => readouts(panel.anatomicalPlane))

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
        val panel = allPanels(index)
        if panel.visible then
          panelGrob(model, state, panel, theme, currentCache, frameResolver) match
            case Left(value) => error = Some(value)
            case Right(compiled) =>
              grobs += compiled.grob
              readouts += compiled.readout
              currentCache = compiled.cache
              profile = profile + compiled.profile
        else
          readouts += PanelReadout(panel.anatomicalPlane, state.cursor,
            model.referenceSpace.worldToVoxel(state.cursor), Vector.empty)
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
    val cells = layout.cells(grids, device)
    PanelReceipts(
      sagittal = receipt(AnatomicalPlane.Sagittal, grids.sagittal, cells.sagittal, state.panelViews.sagittal, device).copy(visible = layout.shows(AnatomicalPlane.Sagittal)),
      coronal = receipt(AnatomicalPlane.Coronal, grids.coronal, cells.coronal, state.panelViews.coronal, device).copy(visible = layout.shows(AnatomicalPlane.Coronal)),
      axial = receipt(AnatomicalPlane.Axial, grids.axial, cells.axial, state.panelViews.axial, device).copy(visible = layout.shows(AnatomicalPlane.Axial))
    )

  private def receipt(
    anatomicalPlane: AnatomicalPlane,
    grid: SliceGrid,
    cell: PanelRect,
    view: PanelView,
    device: DeviceContext
  ): PanelReceipt =
    PanelReceipt(anatomicalPlane, OrthogonalLayout.fit(grid, cell, device), grid, view)

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
