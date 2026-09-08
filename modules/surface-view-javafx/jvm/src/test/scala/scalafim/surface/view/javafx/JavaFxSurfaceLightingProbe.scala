package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.animation.AnimationTimer
import javafx.geometry.Point3D
import javafx.scene.{Group, Scene, SubScene}
import javafx.stage.Stage
import java.nio.file.{Files, Path}
import java.util.concurrent.CountDownLatch
import scalafim.surface.*
import scalafim.surface.view.*
import intaglio.Rgba32

/** Measured rendered pixels versus a closed-form world-normal Lambert oracle. */
object JavaFxSurfaceLightingProbe:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "Supply fresh output directory")
    val out = Path.of(args(0))
    require(!Files.exists(out))
    Files.createDirectories(out)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      val stage = new Stage()
      val backend = JavaFxSurfaceBackend.create().toOption.get
      val id = SurfaceId.unsafe("MNI-mm-lighting-square")
      val cases = Vector(
        ("unlit", SurfaceLighting.Unlit, 1.0),
        ("ambient", SurfaceLighting.directional(.25,0,0,0,1).toOption.get, .25),
        ("facing", SurfaceLighting.directional(.25,.75,0,0,1).toOption.get, 1.0),
        ("opposed", SurfaceLighting.directional(.25,.75,0,0,-1).toOption.get, .25),
        ("tangent", SurfaceLighting.directional(.25,.75,1,0,0).toOption.get, .25),
        ("default", SurfaceLighting.Default, .35+.65*.7681145747868608),
        ("unlit-restored", SurfaceLighting.Unlit, 1.0))
      val jobs = for scale <- Vector(1.0,25.0); entry <- cases yield (scale,entry)
      var mismatches = Vector.empty[String]
      var mounted: Option[SubScene] = None
      def finish(error: Option[Throwable] = None): Unit =
        failure = error.orNull
        backend.dispose().toOption.get
        stage.close()
        done.countDown()
      def run(index: Int): Unit =
        try
          if index == jobs.size then
            if mismatches.nonEmpty then finish(Some(IllegalArgumentException(mismatches.mkString("; "))))
            else
              println(s"PASS: ${jobs.size} native lighting pixel comparisons, two physical mesh scales and retained lighting updates")
              finish()
          else
            val (scale,(label,lighting,factor)) = jobs(index)
            val geometry = SurfaceGeometry(TriangleMesh.fromRows(
              Seq(Seq(-40-scale,-20-scale,30.0),Seq(-40+scale,-20-scale,30.0),Seq(-40+scale,-20+scale,30.0),Seq(-40-scale,-20+scale,30.0)),
              Seq((0,1,2),(0,2,3))), Hemisphere.Left, SurfaceKind.Pial)
            val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("constant"),id,geometry,Vector.fill(4)(Rgba32.unsafe(200,100,50))).toOption.get
            val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id,geometry).toOption.get),Vector(layer)).toOption.get
            val state = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),SurfaceViewerAction.SetLighting(lighting))
              .foldLeft(SurfaceViewerState.initial(model))((state,action) => SurfaceViewer.reduce(model,state,action).toOption.get)
            backend.render(SurfaceCompiler.compile(model,state).toOption.get).toOption.get
            val scene = mounted.getOrElse {
              val fresh = backend.newSubScene(JavaFxSnapshotConfig.make(640,480).toOption.get).toOption.get
              mounted = Some(fresh)
              stage.setScene(new Scene(new Group(fresh),640,480))
              stage.show()
              fresh
            }
            stage.getScene.getRoot.applyCss()
            stage.getScene.getRoot.layout()
            val timer = new AnimationTimer:
              private var frames = 0
              def handle(now: Long): Unit =
                frames += 1
                if frames == 3 then
                  this.stop()
                  try
                    val image = scene.snapshot(null,null)
                    val screen = backend.chunks.head.view.localToScreen(new Point3D(-40,-20,30))
                    val pixel = scene.screenToLocal(screen)
                    val color = image.getPixelReader.getColor(pixel.getX.toInt,pixel.getY.toInt)
                    val actual = Vector(color.getRed,color.getGreen,color.getBlue).map(_*255)
                    val expected = Vector(200.0,100.0,50.0).map(_*factor)
                    val error = actual.zip(expected).map((a,b) => math.abs(a-b)).max
                    val row = s"scale=$scale case=$label actual=${actual.mkString(",")} expected=${expected.mkString(",")} maxChannelError=$error"
                    println(row)
                    Files.writeString(out.resolve(s"$scale-$label.txt"),row+"\n")
                    val width = image.getWidth.toInt
                    val height = image.getHeight.toInt
                    val pixels = new Array[Int](width*height)
                    image.getPixelReader.getPixels(0,0,width,height,_root_.javafx.scene.image.PixelFormat.getIntArgbInstance(),pixels,0,width)
                    val png = new java.awt.image.BufferedImage(width,height,java.awt.image.BufferedImage.TYPE_INT_ARGB)
                    png.setRGB(0,0,width,height,pixels,0,width)
                    javax.imageio.ImageIO.write(png,"png",out.resolve(s"$scale-$label.png").toFile)
                    if error > 2.0 then mismatches :+= row
                    run(index+1)
                  catch case error: Throwable => finish(Some(error))
            timer.start()
        catch case error: Throwable => finish(Some(error))
      run(0)
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn
