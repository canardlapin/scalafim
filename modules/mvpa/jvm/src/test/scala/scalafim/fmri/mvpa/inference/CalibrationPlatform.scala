package scalafim.fmri.mvpa.inference

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.security.MessageDigest

private[inference] object CalibrationPlatform:
  def sha256Utf8(text: String): String =
    MessageDigest.getInstance("SHA-256").digest(text.getBytes(StandardCharsets.UTF_8))
      .map(value => f"$value%02x").mkString
  def environment(name: String): Option[String] = Option(System.getenv(name))
  def readText(path: String): String = Files.readString(Paths.get(path), StandardCharsets.UTF_8)
  def appendText(path: String, text: String): Unit =
    Files.writeString(Paths.get(path), text, StandardCharsets.UTF_8,
      StandardOpenOption.CREATE, StandardOpenOption.APPEND): Unit
