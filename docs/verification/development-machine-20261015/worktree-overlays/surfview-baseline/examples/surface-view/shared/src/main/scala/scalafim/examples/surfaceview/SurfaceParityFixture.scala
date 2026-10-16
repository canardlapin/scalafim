package scalafim.examples.surfaceview

import image4s.geometry.{Affine, D3, Frame, Point}
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
/** Frozen, deliberately asymmetric input declaration for the first display
  * parity captures. The JS adapters use the same topology and scalar values;
  * this declaration keeps the scientific inputs inspectable without coupling
  * the independent comparison oracle to a renderer implementation.
  */
object SurfaceParityFixture:
  val Schema = "scalafim.surface-parity-fixture.v1"
  val CaseIds = Vector("C01", "C02", "C03")
  val Vertices = Vector(
    0.0, 0.0, 0.0,
    1.4, 0.1, 0.2,
    -0.2, 1.1, 0.35,
    0.15, -0.35, 1.3
  )
  val Faces = Vector(0, 1, 2, 0, 3, 1, 0, 2, 3, 1, 3, 2)
  // Endpoint-only values deliberately exercise a shared two-entry LUT without
  // pretending that the two renderers share interpolation implementation.
  val Scalars = Vector(-2.5, -2.5, 2.5, 2.5)
  val Curvature = Vector(-0.8, 0.25, 0.7, -0.15)

  def contract(caseId: String): String =
    if !CaseIds.contains(caseId) then
      throw new IllegalArgumentException(s"unknown surface parity case '$caseId'")
    s"{\"schema\":\"$Schema\",\"case\":\"$caseId\",\"vertices\":4,\"faces\":4," +
      "\"camera\":{\"projection\":\"perspective\",\"viewpoint\":\"dorsal\"}," +
      "\"material\":\"observed-default-lit\",\"viewport\":[512,512]}"

  def example(caseId: String): SurfaceViewerExample =
    val surface = SurfaceGeometry(
      TriangleMesh.fromRows(
        Vertices.grouped(3).map(_.toVector).toVector,
        Faces.grouped(3).map(values => (values(0), values(1), values(2))).toVector
      ),
      Hemisphere.Left,
      SurfaceKind.Midthickness,
      Affine.fromRowMajor[D3](Vector(
        Vector(1.0, 0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0), Vector(0.0, 0.0, 0.0, 1.0)
      ).flatten).toOption.get
    )
    val id = SurfaceId.unsafe("parity-left")
    val curvatureId = SurfaceLayerId.unsafe("curvature")
    val scalarId = SurfaceLayerId.unsafe("scalar")
    val secondId = SurfaceLayerId.unsafe("scalar-additive")
    val curvature = SurfaceLayer.curvatureUnderlay(
      curvatureId, id, SurfaceField.full(surface, Curvature, "analytic curvature"), surface
    ).toOption.get
    val scalar = SurfaceLayer.scalar(
      scalarId, id, surface, Scalars.toArray,
      ScalarColorizer(DisplayWindow.unsafe(-2.5, 2.5), ColorRamp(Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 0, 0))),
      opacity = DisplayOpacity.unsafe(0.85)
    ).toOption.get
    val additive = SurfaceLayer.scalar(
      secondId, id, surface, Array(1.0, 1.0, -1.0, -1.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 1.0), ColorRamp(Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 0, 0))),
      opacity = DisplayOpacity.unsafe(0.35), blendMode = DisplayBlendMode.Additive
    ).toOption.get
    val layers = caseId match
      case "C01" => Vector(curvature)
      case "C02" => Vector(scalar)
      case "C03" => Vector(curvature, scalar, additive)
      case other => throw new IllegalArgumentException(s"unknown surface parity case '$other'")
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, surface).toOption.get), layers).toOption.get
    val actions = Vector(
      SurfaceViewerAction.SetLayout(SurfaceLayout.Single(id)),
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
      SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35.0))),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Default)
    ) ++ (if caseId == "C02" || caseId == "C03" then Vector(
      SurfaceViewerAction.SetLayerThreshold(scalarId, DisplayThreshold.transparentBand(-0.35, 0.35).toOption.get)
    ) else Vector.empty)
    val state = actions.foldLeft[Either[SurfaceViewError, SurfaceViewerState]](Right(SurfaceViewerState.initial(model)))
      ((current, action) => current.flatMap(SurfaceViewer.reduce(model, _, action))).toOption.get
    val frame = Frame.named[D3]("analytic surface parity display").toOption.get
    val display = SurfaceDisplayFrame.declare(model, id, frame).toOption.get
    val target = Point.in(frame)(0.6, 0.375, 0.65).toOption.get
    val eye = Point.in(frame)(0.6, 0.375, 4.65).toOption.get
    val pose = SurfaceCameraPose.make(target, eye).toOption.get
    SurfaceViewerExample(model, state, SurfaceCompiler.compile(model, state, display, pose).toOption.get)
