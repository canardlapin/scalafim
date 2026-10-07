import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*
ThisBuild / scalaVersion := "3.7.4"
lazy val hrf = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full).in(file("hrf"))
  .settings(
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xmax-inlines:64", "-release:17", "-Werror", "-Wunused:all", "-Wvalue-discard"),
    Test / fork := false,
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.12.0",
      "org.typelevel" %%% "spire" % "0.18.0",
      "org.scalameta" %%% "munit" % "1.2.1" % Test,
      "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test
    )
  )
  .jvmSettings(libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.1")
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
  )
lazy val hrfJVM = hrf.jvm
lazy val hrfJS = hrf.js
