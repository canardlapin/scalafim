import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "org.example.frame4s-rehearsal"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xmax-inlines:64"),
  Test / fork := false,
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.1" % Test
)

lazy val frame =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/frame"))
    .settings(commonSettings)
    .settings(name := "frame4s-core")
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
    )

lazy val frameFs2 =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/frame-fs2"))
    .dependsOn(frame)
    .settings(commonSettings)
    .settings(
      name := "frame4s-fs2",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-effect" % "3.7.0",
        "co.fs2" %%% "fs2-core" % "3.13.0"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.apache.arrow" % "arrow-vector" % "19.0.0",
        "org.apache.arrow" % "arrow-memory-unsafe" % "19.0.0"
      ),
      Test / fork := true,
      Test / javaOptions += "--add-opens=java.base/java.nio=ALL-UNNAMED"
    )
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
      Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
    )

lazy val frameJVM = frame.jvm
lazy val frameJS = frame.js
lazy val frameFs2JVM = frameFs2.jvm
lazy val frameFs2JS = frameFs2.js

lazy val root =
  project
    .in(file("."))
    .aggregate(frameJVM, frameJS, frameFs2JVM, frameFs2JS)
    .settings(publish / skip := true)
