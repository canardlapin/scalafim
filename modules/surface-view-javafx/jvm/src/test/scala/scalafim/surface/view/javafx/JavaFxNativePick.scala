package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.scene.SubScene
import javafx.scene.input.PickResult

/** Test-only access to JavaFX's camera ray, scene traversal, and mesh intersection.
  * Verified against OpenJFX 21.0.5 on the classpath. This does not simulate OS events.
  */
private[javafx] object JavaFxNativePick:
  private val pickRoot = classOf[SubScene].getDeclaredMethod(
    "pickRootSG", java.lang.Double.TYPE, java.lang.Double.TYPE)
  pickRoot.setAccessible(true)

  def at(scene: SubScene, x: Double, y: Double): Option[PickResult] =
    require(Platform.isFxApplicationThread, "native picking requires the JavaFX thread")
    Option(pickRoot.invoke(scene, Double.box(x), Double.box(y)).asInstanceOf[PickResult])
