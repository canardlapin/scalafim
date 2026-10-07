package scalafim.fmri.mvpa.inference

import scala.scalajs.js

/** Test-only Node seam. No browser or production crypto/I/O capability is added. */
private[inference] object CalibrationPlatform:
  def sha256Utf8(text: String): String =
    js.Dynamic.global.require("crypto").createHash("sha256")
      .update(text, "utf8").digest("hex").asInstanceOf[String]
  def environment(name: String): Option[String] =
    val value = js.Dynamic.global.process.env.selectDynamic(name)
    if js.typeOf(value) == "string" then Some(value.asInstanceOf[String]) else None
  def readText(path: String): String =
    js.Dynamic.global.require("fs").readFileSync(path, "utf8").asInstanceOf[String]
  def appendText(path: String, text: String): Unit =
    js.Dynamic.global.require("fs").appendFileSync(path, text, "utf8"): Unit
