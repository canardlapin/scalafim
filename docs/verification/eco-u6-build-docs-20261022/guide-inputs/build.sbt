ThisBuild / scalaVersion := "3.7.4"

lazy val images = uri("https://github.com/canardlapin/neuroim4s.git#4f75c2bf141449d032f9fe3c558ef13a22a4d59f")
lazy val atlases = uri("https://github.com/canardlapin/neuroatlas4s.git#4df875d8a1cd20afd3b83ed49446422bc507e5ec")
lazy val surfaces = uri("https://github.com/canardlapin/neurosurf4s.git#73a244fbe5d7b4db2f0c946d98fa2a3c00101ab7")
lazy val analyses = uri("https://github.com/canardlapin/mvpa4s.git#c27469484bf760c34cd896dabe4fe1c384a83b45")

lazy val volume = project.in(file("volume"))
  .dependsOn(ProjectRef(images, "coreJVM"))
lazy val atlas = project.in(file("atlas"))
  .dependsOn(ProjectRef(atlases, "coreJVM"))
lazy val surface = project.in(file("surface"))
  .dependsOn(ProjectRef(surfaces, "coreJVM"))
lazy val analysis = project.in(file("analysis"))
  .dependsOn(ProjectRef(analyses, "coreJVM"))
lazy val root = project.in(file("."))
  .aggregate(volume, atlas, surface, analysis)
