package scalafim.surface.view.javafx

import javafx.application.{ConditionalFeature, Platform}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{CountDownLatch, TimeUnit}

/** Forked native qualification probe. Prism exposes its selected pipeline only through the supported verbose startup
  * log, so the probe captures that log around toolkit startup and passes the observed class to the public receipt
  * parser. It does not link to or reflect over private Prism classes.
  */
object JavaFxRuntimeCapabilityProbe:
  def main(args: Array[String]): Unit =
    require(args.length == 3, "expected JavaFX runtime version, javafx-base SHA-256, and javafx-graphics SHA-256")
    val expectedJavaFxRuntimeVersion = args(0)
    val expectedArtifactSha256 = Map("javafx-base" -> args(1), "javafx-graphics" -> args(2))
    val originalOutput = System.out
    val originalError = System.err
    val capturedOutput = new ByteArrayOutputStream()
    val capturedError = new ByteArrayOutputStream()
    val outputStream = new PrintStream(capturedOutput, true, StandardCharsets.UTF_8)
    val errorStream = new PrintStream(capturedError, true, StandardCharsets.UTF_8)
    val started = new CountDownLatch(1)

    try
      try
        System.setOut(outputStream)
        System.setErr(errorStream)
        Platform.startup(() => started.countDown())
        require(started.await(30, TimeUnit.SECONDS), "JavaFX toolkit startup timed out")
        require(Platform.isSupported(ConditionalFeature.SCENE3D), "JavaFX Scene3D is unavailable")
      finally
        outputStream.flush()
        errorStream.flush()
        System.setOut(originalOutput)
        System.setErr(originalError)
        originalOutput.print(capturedOutput.toString(StandardCharsets.UTF_8))
        originalError.print(capturedError.toString(StandardCharsets.UTF_8))

      val verboseLog =
        capturedOutput.toString(StandardCharsets.UTF_8) + "\n" +
          capturedError.toString(StandardCharsets.UTF_8)
      val receipt = JavaFxRuntimeCapabilityReceipt
        .current(verboseLog)
        .fold(
          error => throw new IllegalStateException(error),
          identity
        )
      val failures = receipt.stockMetalFailures(expectedJavaFxRuntimeVersion, expectedArtifactSha256)
      println(s"javafx_runtime_capability=${receipt.toJson}")
      if failures.nonEmpty then println(s"javafx_runtime_capability_failures=${failures.mkString(" | ")}")
      require(failures.isEmpty, failures.mkString("stock Metal qualification failed: ", "; ", ""))
    finally Platform.exit()
