package scalafim.surface.view

import intaglio.*

/** UV coordinates use a top-left origin, matching RasterImage rows. */
final case class SurfaceDecorationTexture(image: RasterImage, coordinates: FloatBufferView):
  require(coordinates.length % 2 == 0, "texture coordinates must be UV pairs")
  require((0 until coordinates.length).forall(i => coordinates(i).isFinite && coordinates(i) >= 0 && coordinates(i) <= 1),
    "texture coordinates must be finite and in [0,1]")

/** Immutable layout input binding: view-dependent glyphs must be lowered again
  * after camera, instance geometry, placement, clipping or output-size changes.
  */
object SurfaceDecorationBinding:
  def key(plan: SurfaceCompositionPlan, width: Int, height: Int): String =
    def matrix(values: FloatBufferView): String =
      (0 until values.length).map(i => java.lang.Float.floatToIntBits(values(i))).mkString(",")
    s"$width/$height/" + plan.views.map: view =>
      s"${view.id.value}:${view.viewport}:${view.viewportFit}:${view.clipping}:" +
        matrix(view.camera.viewMatrix) + ":" + matrix(view.camera.projectionMatrix) + ":" +
        view.instances.map: instance =>
          s"${instance.id.value}:${instance.surface}:${instance.mesh}:${plan.resources.meshes(instance.mesh).geometryKey}:" +
            s"${instance.translationX},${instance.translationY},${instance.translationZ}"
        .mkString(";")
    .mkString("|")

object SurfaceDecorationValidation:
  def validate(geometry: SurfaceDecorationGeometry, plan: SurfaceCompositionPlan,
      width: Int, height: Int): Either[String, Unit] =
    if width != geometry.canvasWidth || height != geometry.canvasHeight ||
        geometry.binding != SurfaceDecorationBinding.key(plan, width, height) then
      Left("decoration geometry is stale; lower again for this camera, geometry and output size")
    else if geometry.labels.nonEmpty then Left("label packets require raster assets before rendering")
    else if geometry.meshes.exists(_.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest) &&
        plan.views.exists(view => view.surfaceBase.alpha != 255 || view.instances.exists(_.opacity != DisplayOpacity.Opaque)) then
      Left("ordered depth-tested decorations require opaque scientific surfaces")
    else
      var failure: Option[String] = None
      val ids = scala.collection.mutable.HashSet.empty[(SurfaceDisplayOrigin, SurfaceDecorationElementId)]
      var index = 0
      while index < geometry.meshes.length && failure.isEmpty do
        val mesh = geometry.meshes(index)
        val occurrence = plan.views.find(_.id == mesh.origin.view).flatMap(_.instances.find(_.id == mesh.origin.instance))
        val targetSurface = mesh.target match
          case SurfaceDecorationTarget.Anchor(anchor) => SurfaceDecorationAnchor.surface(anchor)
          case SurfaceDecorationTarget.Edge(surface, _, _) => surface
          case SurfaceDecorationTarget.NetworkEdge(source, _) => source.surface
        val networkAnchorsValid = mesh.target match
          case SurfaceDecorationTarget.NetworkEdge(source, target) =>
            def resolves(anchor: SurfaceVertexAnchor, instance: SurfaceCompositionInstance): Boolean =
              plan.resources.meshes.get(instance.mesh).exists: resource =>
                SurfaceDecorationCompiler.resolve(SurfaceDecorationAnchor.Vertex(anchor), resource).isRight
            occurrence.exists(instance => instance.surface == source.surface && resolves(source, instance)) &&
              plan.views.find(_.id == mesh.origin.view).exists: view =>
                view.instances.exists(instance => instance.surface == target.surface && resolves(target, instance))
          case _ => true
        val count = mesh.positions.length / 3
        if !ids.add(mesh.origin -> mesh.id) then failure = Some("duplicate decoration occurrence")
        else if occurrence.forall(_.surface != targetSurface) then failure = Some("decoration target does not belong to its display occurrence")
        else if !networkAnchorsValid then failure = Some("network decoration anchor is absent from its mapped view domain")
        else if count == 0 || mesh.positions.length % 3 != 0 || mesh.normals.length != mesh.positions.length ||
            mesh.indices.length % 3 != 0 || mesh.indices.length == 0 then failure = Some("invalid decoration buffer shape")
        else if !(0 until mesh.positions.length).forall(i => mesh.positions(i).isFinite && mesh.normals(i).isFinite) then
          failure = Some("non-finite decoration coordinates")
        else if !(0 until mesh.indices.length).forall(i => mesh.indices(i) >= 0 && mesh.indices(i) < count) then
          failure = Some("decoration index outside vertex buffer")
        else if mesh.texture.nonEmpty && mesh.pickable then failure = Some("textured decorations must pass through picking")
        else if mesh.texture.nonEmpty && mesh.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest then
          failure = Some("ordered depth-tested decorations require solid colors")
        else if mesh.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest &&
            !mesh.roundedStroke.exists(_.accepts(mesh, geometry.binding)) then
          failure = Some("ordered depth testing requires unchanged rounded contour geometry from the builder")
        else if mesh.texture.isEmpty && mesh.color.alpha != 255 && mesh.depthPolicy != SurfaceDecorationDepthPolicy.OrderedDepthTest then
          failure = Some("solid decorations require opaque color unless using ordered depth testing")
        else if mesh.texture.exists(_.coordinates.length != count * 2) then failure = Some("texture coordinates do not match decoration vertices")
        index += 1
      failure.toLeft(())

/** A platform text renderer supplies an explicitly measured transparent image.
  * Pixels in the image are at rasterScale pixels per logical pixel. The anchor
  * is measured from its top-left corner; fontEm is in logical pixels.
  */
final case class SurfaceLabelAsset(image: RasterImage, fontEm: Double,
    anchorX: Double, anchorY: Double, rasterScale: Double = 1.0):
  require(fontEm.isFinite && fontEm > 0 && rasterScale.isFinite && rasterScale > 0)
  require(anchorX.isFinite && anchorY.isFinite)

trait SurfaceLabelRasterizer:
  def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset]

object SurfaceLabelLayout:
  /** Uses the same platform-supplied glyph pixels for live 3D and export. Labels
    * are camera-facing, unlit, and pass through picking to scientific surfaces.
    */
  def resolve(geometry: SurfaceDecorationGeometry, plan: SurfaceCompositionPlan,
      rasterizer: SurfaceLabelRasterizer): Either[String, SurfaceDecorationGeometry] =
    if geometry.binding != SurfaceDecorationBinding.key(plan, geometry.canvasWidth, geometry.canvasHeight) then
      Left("label geometry is stale")
    else
      val output = Vector.newBuilder[SurfaceDecorationMesh]
      var error: Option[String] = None
      geometry.labels.foreach: label =>
        if error.isEmpty then rasterizer.rasterize(label.text, label.color) match
          case Left(value) => error = Some(value)
          case Right(asset) =>
            plan.views.find(_.id == label.origin.view) match
              case None => error = Some("label view is absent")
              case Some(view) => output += quad(label, asset, plan, view, geometry.canvasWidth, geometry.canvasHeight)
      error.toLeft(geometry.copy(meshes = geometry.meshes ++ output.result(), labels = Vector.empty))

  private def quad(label: SurfaceLabelPacket, asset: SurfaceLabelAsset, plan: SurfaceCompositionPlan,
      view: SurfaceCompositionView, width: Int, height: Int): SurfaceDecorationMesh =
    val point = label.point
    val camera = view.camera
    val m = camera.viewMatrix
    val p = camera.projectionMatrix
    val eyeZ = m(8) * point.x + m(9) * point.y + m(10) * point.z + m(11)
    val w = math.abs(p(14) * eyeZ + p(15))
    val box = plan.fittedViewports(width, height).find(_._1.id == view.id).get._2
    val (sx, sy) = label.size.unit match
      case SurfaceDecorationUnit.World => (label.size.value / asset.fontEm, label.size.value / asset.fontEm)
      case SurfaceDecorationUnit.Pixels =>
        (label.size.value / asset.fontEm * 2 * w / (math.abs(p(0)) * box.width * width),
          label.size.value / asset.fontEm * 2 * w / (math.abs(p(5)) * box.height * height))
    val left = -asset.anchorX * sx
    val right = (asset.image.width / asset.rasterScale - asset.anchorX) * sx
    val top = asset.anchorY * sy
    val bottom = (asset.anchorY - asset.image.height / asset.rasterScale) * sy
    val positions = new Array[Float](12)
    val normals = new Array[Float](12)
    Vector((left, top), (right, top), (left, bottom), (right, bottom)).zipWithIndex.foreach: (xy, index) =>
      positions(index * 3) = (point.x + m(0) * xy._1 + m(4) * xy._2).toFloat
      positions(index * 3 + 1) = (point.y + m(1) * xy._1 + m(5) * xy._2).toFloat
      positions(index * 3 + 2) = (point.z + m(2) * xy._1 + m(6) * xy._2).toFloat
      normals(index * 3) = m(8)
      normals(index * 3 + 1) = m(9)
      normals(index * 3 + 2) = m(10)
    SurfaceDecorationMesh(label.id, label.origin, SurfaceDecorationTarget.Anchor(label.anchor),
      new FloatBufferView(positions), new FloatBufferView(normals), new IntBufferView(Array(0, 2, 1, 1, 2, 3)),
      Rgba32.unsafe(255, 255, 255), label.depthPolicy,
      Some(SurfaceDecorationTexture(asset.image, new FloatBufferView(Array(0f, 0f, 1f, 0f, 0f, 1f, 1f, 1f)))), pickable = false)
