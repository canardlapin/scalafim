package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import javafx.animation.AnimationTimer
import javafx.application.{Application, Platform}
import javafx.scene.{Group, ParallelCamera, Scene, SceneAntialiasing, SnapshotParameters, SubScene}
import javafx.scene.image.{Image, WritableImage}
import javafx.scene.layout.HBox
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{CullFace, MeshView, TriangleMesh as FxMesh}
import javafx.stage.Stage
import intaglio.Rgba32
import scalafim.surface.*
import scalafim.surface.view.*
import scala.jdk.CollectionConverters.*

/** Public-API native colour reproducer. Arguments: fresh output directory.
  * Optional --unit, --lateral and --orbit isolate geometry and camera effects.
  * Keeps CPU atlas, native mesh/material metadata and initial/updated snapshots.
  * This is a diagnostic, not admission of a different renderer or lighting policy.
  */
object JavaFxEmissionReproducer:
  def main(args: Array[String]): Unit = Application.launch(classOf[JavaFxEmissionReproducerApp], args*)

final class JavaFxEmissionReproducerApp extends Application:
  override def start(stage: Stage): Unit =
    val out = Path.of(getParameters.getRaw.asScala.head)
    require(!Files.exists(out), s"Refusing to overwrite $out")
    Files.createDirectories(out): Unit
    val flags = getParameters.getRaw.asScala.toSet
    val varying = flags.contains("--varying")
    val unitGeometry = flags.contains("--unit")
    val lateral = flags.contains("--lateral")
    val orbit = flags.contains("--orbit")
    val id = SurfaceId.unsafe("tetrahedron")
    val layerId = SurfaceLayerId.unsafe("constant")
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      if unitGeometry then Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0))
      else Vector(Vector(-1.0, -1.0, 0.0), Vector(1.0, -1.0, 0.0),
        Vector(-1.0, 1.0, 0.0), Vector(-1.0, -1.0, 2.0)),
      if unitGeometry then Vector((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2))
      else Vector((0, 1, 2), (0, 3, 1), (0, 2, 3), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Inflated)
    def plan(red: Int, green: Int, blue: Int): SurfaceRenderPlan =
      val asset = SurfaceAsset.make(id, geometry).toOption.get
      val colors = if !varying then Vector.fill(4)(Rgba32.unsafe(red, green, blue))
        else Vector(Rgba32.unsafe(red, green, blue), Rgba32.unsafe(blue, red, green),
          Rgba32.unsafe(green, blue, red), Rgba32.unsafe((red + blue) / 2, green, (green + blue) / 2))
      val layer = SurfaceLayer.packedRgba(layerId, id, geometry, colors).toOption.get
      val model = SurfaceViewerModel.make(Vector(asset), Vector(layer)).toOption.get
      val state = Vector(SurfaceViewerAction.SetViewpoint(if lateral then SurfaceViewpoint.Lateral(CorticalHemisphere.Left) else SurfaceViewpoint.Dorsal),
        SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.Default)),
        SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
        SurfaceViewerAction.OrbitBy(if orbit then 17 else 0, if orbit then -9 else 0), SurfaceViewerAction.FitCamera)
        .foldLeft(SurfaceViewerState.initial(model))((s, action) => SurfaceViewer.reduce(model, s, action).toOption.get)
      SurfaceCompiler.compile(model, state).toOption.get
    val first = plan(225, 70, 20)
    val next = plan(30, 70, 220)
    val configs = Vector("retained" -> JavaFxAtlasEncoding.RetainedAffineOpaque,
      "legacy" -> JavaFxAtlasEncoding.LegacyTriangle)
    val providers = configs.map { (name, encoding) =>
      val config = JavaFxAtlasConfig.make(encoding = encoding).toOption.get
      val probe = JavaFxSurfaceProbe.compile(first, JavaFxMaterialMode.Unlit, config).toOption.get
      val scene = probe.newSubScene(320, 320)
      (name, probe, scene)
    }
    val plainImage = new WritableImage(64, 64)
    def paint(red: Int, green: Int, blue: Int): Unit =
      for y <- 0 until 64; x <- 0 until 64 do plainImage.getPixelWriter.setColor(x, y, Color.rgb(red, green, blue))
    paint(225, 70, 20)
    val plainMesh = new FxMesh()
    plainMesh.getPoints.setAll(Array[Float](40, 40, 0, 280, 40, 0, 160, 280, 0), 0, 9)
    plainMesh.getTexCoords.setAll(Array[Float](0, 0, 1, 0, 0.5f, 1), 0, 6)
    plainMesh.getFaces.setAll(Array[Int](0, 0, 1, 1, 2, 2), 0, 6)
    val material = new PhongMaterial(Color.BLACK)
    material.setSpecularColor(Color.TRANSPARENT)
    material.setSelfIlluminationMap(plainImage)
    val meshView = new MeshView(plainMesh)
    meshView.setCullFace(CullFace.NONE)
    meshView.setMaterial(material)
    val plain = new SubScene(new Group(meshView), 320, 320, false, SceneAntialiasing.BALANCED)
    plain.setCamera(new ParallelCamera())
    plain.setFill(Color.WHITE)
    val root = new HBox(8, (providers.map(_._3) :+ plain)*)
    stage.setScene(new Scene(root))
    stage.setTitle("ScalaFIM public emission control")
    stage.show()
    val observations = scala.collection.mutable.ArrayBuffer.empty[ujson.Obj]
    def save(name: String, image: Image): ujson.Obj =
      val width = image.getWidth.toInt
      val height = image.getHeight.toInt
      val pixels = Array.tabulate(width * height)(i => image.getPixelReader.getArgb(i % width, i / width))
      val buffered = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      buffered.setRGB(0, 0, width, height, pixels, 0, width)
      javax.imageio.ImageIO.write(buffered, "png", out.resolve(name + ".png").toFile): Unit
      ujson.Obj("file" -> (name + ".png"), "width" -> width, "height" -> height,
        "coloredPixels" -> pixels.count(p => (p & 0xffffff) != 0 && (p & 0xffffff) != 0xffffff),
        "uniqueArgb" -> ujson.Arr.from(pixels.distinct.take(16).map(p => f"$p%08x")))
    def snapshot(name: String, scene: SubScene): ujson.Obj =
      val params = new SnapshotParameters()
      params.setFill(Color.WHITE)
      save(name, scene.snapshot(params, null))
    def capture(phase: String): Unit =
      root.applyCss()
      root.layout()
      providers.foreach { (name, probe, scene) =>
        val chunks = probe.chunks.zipWithIndex.map { (chunk, index) =>
          val uv = new Array[Float](chunk.mesh.getTexCoords.size())
          chunk.mesh.getTexCoords.toArray(uv): Unit
          ujson.Obj("atlas" -> save(s"$phase-$name-atlas-$index", chunk.atlas.image),
            "selfIlluminationIsAtlas" -> (chunk.material.getSelfIlluminationMap eq chunk.atlas.image),
            "diffuseMapNull" -> (chunk.material.getDiffuseMap == null),
            "diffuseColor" -> chunk.material.getDiffuseColor.toString,
            "uv" -> ujson.Arr.from(uv.map(_.toDouble)),
            "normals" -> ujson.Arr.from(chunk.mesh.getNormals.toArray(new Array[Float](chunk.mesh.getNormals.size())).map(_.toDouble)),
            "points" -> ujson.Arr.from(chunk.mesh.getPoints.toArray(new Array[Float](chunk.mesh.getPoints.size())).map(_.toDouble)),
            "faces" -> ujson.Arr.from(chunk.mesh.getFaces.toArray(new Array[Int](chunk.mesh.getFaces.size()))),
            "worldTransform" -> ujson.Arr.from(chunk.view.getLocalToSceneTransform.toArray(_root_.javafx.scene.transform.MatrixType.MT_3D_3x4)))
        }
        observations += ujson.Obj("phase" -> phase, "case" -> name, "chunks" -> ujson.Arr.from(chunks),
          "native" -> snapshot(s"$phase-$name-native", scene))
      }
      observations += ujson.Obj("phase" -> phase, "case" -> "plain-javafx-emission",
        "image" -> save(s"$phase-plain-image", plainImage), "native" -> snapshot(s"$phase-plain-native", plain))
      Files.writeString(out.resolve("receipt.json"), ujson.write(ujson.Obj(
        "schema" -> "scalafim.emission-reproducer.v1", "lighting" -> "Unlit", "varyingColors" -> varying, "unitGeometry" -> unitGeometry, "lateral" -> lateral, "orbit" -> orbit,
        "affineProperty" -> java.lang.Boolean.getBoolean("scalafim.javafx.affineAtlas"),
        "observations" -> ujson.Arr.from(observations)), indent = 2)): Unit
    var pulses = 0
    val timer = new AnimationTimer:
      override def handle(now: Long): Unit =
        pulses += 1
        try
          if pulses == 3 then
            capture("initial")
            providers.foreach { (_, probe, _) => probe.updateColors(next, commit = true).fold(
              error => throw IllegalArgumentException(error.message), _ => ()) }
            paint(30, 70, 220)
          else if pulses == 6 then
            capture("updated")
            stop()
            stage.close()
            Platform.exit()
            println(s"RECORDED: public surface emission control at $out")
        catch case error: Throwable =>
          error.printStackTrace()
          stop()
          stage.close()
          Platform.exit()
          System.exit(1)
    timer.start()
