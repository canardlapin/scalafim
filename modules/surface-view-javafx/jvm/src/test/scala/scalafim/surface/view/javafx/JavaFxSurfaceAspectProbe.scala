package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.geometry.Point3D
import javafx.scene.{Group, Scene}
import javafx.stage.Stage
import java.util.concurrent.CountDownLatch
import scalafim.surface.*
import scalafim.surface.view.*

/** Native projected-pixel oracle: a front-facing MNI-mm square must stay square.
  * Uses JavaFX's actual projection to screen, not the backend layout formula.
  */
object JavaFxSurfaceAspectProbe:
  def main(args: Array[String]): Unit =
    require(args.isEmpty)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      val stage = new Stage()
      try
        var checks = 0
        for
          bilateral <- Vector(false, true)
          projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default), CameraProjection.Orthographic(OrthographicScale.unsafe(4)))
          (width, height) <- Vector((600,600), (1200,300), (300,900))
          zoom <- Vector(0.7, 1.0)
          shapeAspect <- Vector(0.5, 1.0, 2.0)
          fitted <- Vector(false, true)
        do
          val backend = JavaFxSurfaceBackend.create().toOption.get
          try
            val ids = Vector(SurfaceId.unsafe("mni-left"), SurfaceId.unsafe("mni-right"))
            val offsets = Vector(-40.0, 40.0)
            val assets = ids.zipWithIndex.take(if bilateral then 2 else 1).map: (id, index) =>
              val x = offsets(index)
              val geometry = SurfaceGeometry(TriangleMesh.fromRows(
                Seq(Seq(x-shapeAspect,-21.0,30.0),Seq(x+shapeAspect,-21.0,30.0),Seq(x+shapeAspect,-19.0,30.0),Seq(x-shapeAspect,-19.0,30.0)),
                Seq((0,1,2),(0,2,3))), if index == 0 then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Pial)
              SurfaceAsset.make(id, geometry).toOption.get
            val model = SurfaceViewerModel.make(assets, Vector.empty).toOption.get
            var state = SurfaceViewerState.initial(model)
            val actions = Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal), SurfaceViewerAction.SetProjection(projection), SurfaceViewerAction.SetZoom(CameraZoom.unsafe(zoom))) ++
              (if bilateral then Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(ids(0),ids(1)))) else Vector.empty)
            actions.foreach(action => state = SurfaceViewer.reduce(model,state,action).toOption.get)
            if fitted then state = SurfaceViewer.reduce(model,state,SurfaceViewerAction.FitCamera).toOption.get
            backend.render(SurfaceCompiler.compile(model,state).toOption.get).toOption.get
            val subscene = backend.newSubScene(JavaFxSnapshotConfig.make(width,height).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(subscene),width,height))
            stage.show()
            stage.getScene.getRoot.applyCss()
            stage.getScene.getRoot.layout()
            backend.chunks.foreach: chunk =>
              val index = ids.indexOf(chunk.surface)
              val x = offsets(index)
              val a = chunk.view.localToScreen(new Point3D(x-shapeAspect,-21,30))
              val b = chunk.view.localToScreen(new Point3D(x+shapeAspect,-21,30))
              val c = chunk.view.localToScreen(new Point3D(x+shapeAspect,-19,30))
              require(a != null && b != null && c != null)
              val ratio = a.distance(b) / b.distance(c)
              require(math.abs(ratio-shapeAspect) < 1e-5, s"Anatomical aspect distorted: bilateral=$bilateral projection=$projection size=${width}x$height ratio=$ratio")
              val origin = subscene.localToScreen(0,0)
              val center = chunk.view.localToScreen(new Point3D(x, -20.0, 30.0))
              val columns = if bilateral then 2 else 1
              val side = math.min(width.toDouble / columns, height.toDouble * state.camera.aspectRatio.value)
              val expectedX = origin.getX + (width - columns * side) * 0.5 + (index + 0.5) * side
              require(math.abs(center.getX - expectedX) < 1e-5 && math.abs(center.getY - origin.getY - height * 0.5) < 1e-5,
                s"Surface group is not compact and centered: size=${width}x$height center=$center expectedX=$expectedX")
              val slots = if bilateral then 2 else 1
              val left = origin.getX + index*width.toDouble/slots
              val right = left + width.toDouble/slots
              Vector(a,b,c).foreach: point =>
                require(point.getX > left && point.getX < right && point.getY > origin.getY && point.getY < origin.getY+height,
                  "Initial anatomical square clipped outside its viewport slot")
              checks += 1
          finally backend.dispose().toOption.get
        println(s"PASS: $checks independent native pixel-scale and initial framing checks")
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn
