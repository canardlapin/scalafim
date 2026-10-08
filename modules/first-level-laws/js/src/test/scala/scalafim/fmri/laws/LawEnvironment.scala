package scalafim.fmri.laws

import scala.scalajs.js

/** Node's environment is explicit; Scala.js System.getenv does not supply it. */
private[laws] object LawEnvironment:
  def get(name: String): Option[String] =
    val value = js.Dynamic.global.process.env.selectDynamic(name)
    if js.isUndefined(value) then None else Some(value.asInstanceOf[String])
