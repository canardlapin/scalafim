ThisBuild / scalaVersion := "3.7.4"

lazy val neuroimRevision = "4f75c2bf141449d032f9fe3c558ef13a22a4d59f"
lazy val neuroimBuild = uri(s"https://github.com/canardlapin/neuroim4s.git#$neuroimRevision")

lazy val ioJVM = ProjectRef(neuroimBuild, "ioJVM")
lazy val transformJVM = ProjectRef(neuroimBuild, "transformJVM")
lazy val transformIoJVM = ProjectRef(neuroimBuild, "transformIoJVM")
lazy val viewJava2dJVM = ProjectRef(neuroimBuild, "viewJava2dJVM")

lazy val root = project
  .in(file("."))
  .settings(
    name := "neuroim-composition-consumer",
    Compile / run / fork := true,
    libraryDependencies := Nil
  )
  .dependsOn(ioJVM, transformJVM, transformIoJVM, viewJava2dJVM)
