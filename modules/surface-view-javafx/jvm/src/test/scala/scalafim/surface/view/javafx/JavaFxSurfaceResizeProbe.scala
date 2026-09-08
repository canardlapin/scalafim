package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.Platform
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

object JavaFxSurfaceResizeProbe:
  def main(args: Array[String]): Unit =
    require(args.isEmpty)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      val resized = JavaFxSurfaceBackend.create().toOption.get
      val fresh = JavaFxSurfaceBackend.create().toOption.get
      try
        val left = SurfaceId.unsafe("left")
        val right = SurfaceId.unsafe("right")
        def asset(id: SurfaceId, hemisphere: Hemisphere, offset: Double): SurfaceAsset =
          SurfaceAsset.make(id,SurfaceGeometry(TriangleMesh.fromRows(
            Seq(Seq(offset,-1.0,-1.0),Seq(offset,1.0,-1.0),Seq(offset,0.0,1.0)),Seq((0,1,2))),
            hemisphere,SurfaceKind.Inflated)).toOption.get
        val model = SurfaceViewerModel.make(Vector(asset(left,Hemisphere.Left,0),asset(right,Hemisphere.Right,100)),Vector.empty).toOption.get
        val initial = SurfaceViewer.reduce(model,SurfaceViewerState.initial(model),
          SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left,right))).toOption.get
        val plan = SurfaceCompiler.compile(model,initial).toOption.get
        resized.render(plan).toOption.get
        fresh.render(plan).toOption.get
        val scene = resized.newSubScene(JavaFxSnapshotConfig.make(1,380).toOption.get).toOption.get
        val reference = fresh.newSubScene(JavaFxSnapshotConfig.make(1006,240).toOption.get).toOption.get
        scene.setWidth(1006)
        scene.setHeight(240)
        def transforms(backend: JavaFxSurfaceBackend): Vector[Vector[Double]] =
          backend.chunks.map: chunk =>
            val t = chunk.view.getParent.getLocalToParentTransform
            Vector(t.getMxx,t.getMxy,t.getMxz,t.getTx,t.getMyx,t.getMyy,t.getMyz,t.getTy,t.getMzx,t.getMzy,t.getMzz,t.getTz)
        require(transforms(resized).nonEmpty)
        def compare(): Unit =
          transforms(resized).flatten.zip(transforms(fresh).flatten).foreach: (a,b) =>
            require(math.abs(a-b) <= 1e-10*math.max(1,math.abs(b)),s"resized transform $a differs from fresh $b")
        compare()
        scene.setWidth(720)
        scene.setHeight(480)
        reference.setWidth(720)
        reference.setHeight(480)
        compare()
        val beforeRebuildCamera = scene.getCamera
        val replacementLeft = SurfaceId.unsafe("replacement-left")
        val replacementRight = SurfaceId.unsafe("replacement-right")
        val changedModel = SurfaceViewerModel.make(Vector(asset(replacementLeft,Hemisphere.Left,4),asset(replacementRight,Hemisphere.Right,110)),Vector.empty).toOption.get
        val changedState = SurfaceViewer.reduce(changedModel,SurfaceViewerState.initial(changedModel),
          SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(replacementLeft,replacementRight))).toOption.get
        val changedPlan = SurfaceCompiler.compile(changedModel,changedState).toOption.get
        resized.render(changedPlan).toOption.get
        fresh.render(changedPlan).toOption.get
        require(scene.getCamera ne beforeRebuildCamera, "rebuilt view retained the old camera")
        scene.setWidth(900)
        reference.setWidth(900)
        compare()
        val beforeDispose = transforms(resized)
        resized.dispose().toOption.get
        scene.setWidth(99)
        scene.setHeight(99)
        require(resized.resourceKeys.isEmpty && resized.viewportCameras.isEmpty && scene.getCamera == null)
        require(beforeDispose.nonEmpty)
        println("PASS: resized native viewport equals final-size construction and disposes cleanly")
      catch case error: Throwable => failure = error
      finally
        resized.dispose()
        fresh.dispose()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn
