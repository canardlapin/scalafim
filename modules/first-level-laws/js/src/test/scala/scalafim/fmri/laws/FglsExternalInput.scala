package scalafim.fmri.laws

import scala.scalajs.js

private[laws] object FglsExternalInput:
  def text(path: String): String = js.Dynamic.global.require("fs").readFileSync(path, "utf8").asInstanceOf[String]
  def doubles(path: String): Array[Double] =
    val buffer = js.Dynamic.global.require("fs").readFileSync(path)
    val length = buffer.length.asInstanceOf[Int]
    require(length % 8 == 0)
    Array.tabulate(length / 8)(i => buffer.readDoubleLE(i * 8).asInstanceOf[Double])
