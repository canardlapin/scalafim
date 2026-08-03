package scalafim.examples.surfaceview

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

final case class SurfaceViewerExample(
  model: SurfaceViewerModel,
  state: SurfaceViewerState,
  plan: SurfaceRenderPlan
)

final case class SurfaceViewerExampleReceipt(
  schema: String,
  source: String,
  vertices: Int,
  faces: Int,
  layerOrder: Vector[String],
  slotOrder: Vector[String],
  cameraDirection: Vector[Double],
  selectedSurface: String,
  selectedVertex: Int,
  imageHash: Int,
  shadedPixels: Int,
  pickedSurface: String,
  pickedVertex: Int
):
  def canonicalJson: String =
    def strings(values: Vector[String]): String = values.map(value => s"\"$value\"").mkString("[", ",", "]")
    def doubles(values: Vector[Double]): String = values.mkString("[", ",", "]")
    s"{" +
      s"\"schema\":\"$schema\"," +
      s"\"source\":\"$source\"," +
      s"\"vertices\":$vertices," +
      s"\"faces\":$faces," +
      s"\"layerOrder\":${strings(layerOrder)}," +
      s"\"slotOrder\":${strings(slotOrder)}," +
      s"\"cameraDirection\":${doubles(cameraDirection)}," +
      s"\"selectedSurface\":\"$selectedSurface\"," +
      s"\"selectedVertex\":$selectedVertex," +
      s"\"imageHash\":$imageHash," +
      s"\"shadedPixels\":$shadedPixels," +
      s"\"pickedSurface\":\"$pickedSurface\"," +
      s"\"pickedVertex\":$pickedVertex}"

object SurfaceViewerExample:
  val LeftSurface: SurfaceId = SurfaceId.unsafe("gifti-left")
  val RightSurface: SurfaceId = SurfaceId.unsafe("gifti-right")
  val Activation: SurfaceLayerId = SurfaceLayerId.unsafe("activation")
  val Parcels: SurfaceLayerId = SurfaceLayerId.unsafe("parcels")

  /** Portable decoding of the checked `tetra_lh_midthickness.surf.gii`
    * fixture. The JVM example verifies these values against the production
    * GIFTI reader; Scala.js starts from the same decoded scientific payload.
    */
  def portableGiftiGeometry: SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 1.0)
        ),
        Vector((0, 1, 2), (0, 1, 3))
      ),
      Hemisphere.Left,
      SurfaceKind.Midthickness,
      DMat.fromRows(Vector(
        Vector(1.0, 0.0, 0.0, 10.0),
        Vector(0.0, 1.0, 0.0, 20.0),
        Vector(0.0, 0.0, 1.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      ))
    )

  def fromGifti(left: SurfaceGeometry): Either[SurfaceViewError, SurfaceViewerExample] =
    val right = SurfaceGeometry(left.mesh, Hemisphere.Right, left.kind, left.surfaceToWorld)
    val activation = SurfaceLayer.scalar(
      Activation,
      LeftSurface,
      left,
      Array(-2.0, -0.25, 0.5, 2.5, 2.5, 0.5, -0.25, -2.0),
      ScalarColorizer(DisplayWindow.unsafe(-2.5, 2.5), ColorRamp.Heat),
      frameCount = 2,
      opacity = DisplayOpacity.unsafe(0.85)
    )
    val parcels = SurfaceLayer.labels(
      Parcels,
      RightSurface,
      right,
      Array(1, 1, 2, 3, 3, 2, 1, 1),
      LabelColorizer(Map(
        1 -> Rgba32.unsafe(38, 139, 210),
        2 -> Rgba32.unsafe(220, 80, 64),
        3 -> Rgba32.unsafe(92, 184, 92)
      )),
      frameCount = 2
    )
    for
      activationLayer <- activation
      parcelLayer <- parcels
      model <- SurfaceViewerModel.make(
        Vector(
          SurfaceAsset.make(LeftSurface, left).toOption.get,
          SurfaceAsset.make(RightSurface, right).toOption.get
        ),
        Vector(activationLayer, parcelLayer)
      )
      state <- actions.foldLeft[Either[SurfaceViewError, SurfaceViewerState]](
        Right(SurfaceViewerState.initial(model))
      ) { (current, action) =>
        current.flatMap(state => SurfaceViewer.reduce(model, state, action))
      }
      plan <- SurfaceCompiler.compile(model, state)
    yield SurfaceViewerExample(model, state, plan)

  def portable: SurfaceViewerExample =
    fromGifti(portableGiftiGeometry).fold(error => throw new IllegalStateException(error.message), identity)

  def semanticReceipt(
    example: SurfaceViewerExample = portable,
    dimensions: RasterDimensions = RasterDimensions.unsafe(320, 180)
  ): SurfaceViewerExampleReceipt =
    val rendered = SurfaceRasterizer.render(
      example.plan,
      dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).fold(error => throw new IllegalStateException(error.message), identity)
    val pick = firstPick(rendered)
    val selection = example.state.selection.get
    SurfaceViewerExampleReceipt(
      "scalafim.surface-view-example.v1",
      "tetra_lh_midthickness.surf.gii",
      example.plan.profile.verticesPacked,
      example.plan.profile.facesPacked,
      example.state.layerOrder.map(_.value),
      example.plan.slots.map(_.surface.value),
      Vector(example.plan.camera.directionX, example.plan.camera.directionY, example.plan.camera.directionZ),
      selection.surface.value,
      selection.vertex.index,
      imageHash(rendered.image),
      rendered.receipt.shadedPixels,
      pick.surface.value,
      pick.vertex
    )

  private val actions = Vector(
    SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(
      LeftSurface,
      RightSurface,
      BilateralOrder.LeftThenRight
    )),
    // The fixture's GIFTI transform places the tetrahedron at (10, 20, 30).
    // Keep that scientific transform intact. The compiler centers the camera
    // on the transformed world bounds; a dorsal view then preserves winding
    // while framing the asymmetric vertices and bilateral order tightly.
    SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
    SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(0.75))),
    SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
    SurfaceViewerAction.SetTimepoint(1),
    SurfaceViewerAction.SetLayerThreshold(
      Activation,
      DisplayThreshold.transparentBand(-0.4, 0.4).toOption.get
    ),
    SurfaceViewerAction.Select(LeftSurface, VertexId(2))
  )

  private def firstPick(result: SurfaceRasterResult): SurfacePick =
    var y = 0
    while y < result.image.height do
      var x = 0
      while x < result.image.width do
        result.pick(x, y).toOption.flatten match
          case Some(value) => return value
          case None => ()
        x += 1
      y += 1
    throw new IllegalStateException("example surface produced no pickable pixel")

  private def imageHash(image: RasterImage): Int =
    var hash = 1
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        hash = 31 * hash + image.pixelUnsafe(x, y).toPackedInt
        x += 1
      y += 1
    hash

object SurfaceViewerReceiptExample:
  def main(args: Array[String]): Unit =
    println(SurfaceViewerExample.semanticReceipt().canonicalJson)
