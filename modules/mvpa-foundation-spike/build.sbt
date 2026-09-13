import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

// An unpublished consumer spike, not a production dependency or alternate
// provider implementation. The runner extracts exact Git trees into an owned
// temporary sibling layout required by Alder's development source build.
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / organization := "scalafim.spike"
ThisBuild / version := "0.0.0-unpublished"
ThisBuild / publish / skip := true

val providers = file(sys.props("umvpa.providers"))
def provider(name: String, project: String) =
  ProjectRef((providers / name).toURI, project)

lazy val spike = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "mvpa-foundation-spike",
    publish / skip := true,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Werror"),
    Test / fork := false,
    // Actual, unchanged response/locus main sources are snapshotted by the
    // runner. No dependency on the unrelated production aggregate build.
    Test / unmanagedSourceDirectories += file(sys.props("umvpa.adapters")),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit" % "1.3.4" % Test,
      "org.typelevel" %%% "cats-core" % "2.12.0" % Test,
      "org.typelevel" %%% "cats-effect" % "3.5.4" % Test
    )
  )
  .jvmConfigure(_.dependsOn(
    provider("multivar", "coreJVM"),
    provider("alder", "kernelJVM"),
    provider("alder", "dataJVM"),
    provider("resample4s", "coreJVM"),
    provider("resample4s", "designsJVM"),
    provider("ravel", "coreJVM"),
    provider("locus4s", "locus4s-coreJVM"),
    provider("locus4s", "locus4s-dataJVM")
  ))
  .jsConfigure(_.dependsOn(
    provider("multivar", "coreJS"),
    provider("alder", "kernelJS"),
    provider("alder", "dataJS"),
    provider("resample4s", "coreJS"),
    provider("resample4s", "designsJS"),
    provider("ravel", "coreJS"),
    provider("locus4s", "locus4s-coreJS"),
    provider("locus4s", "locus4s-dataJS")
  ))
  .jsSettings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
  )

lazy val spikeJVM = spike.jvm
lazy val spikeJS = spike.js
