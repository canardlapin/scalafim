// Outer adapters depend on atlas and connectivity without reversing their edges.
lazy val atlasWorkflows =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/atlas-workflows"))
    .jvmConfigure(_.dependsOn(LocalProject("atlasJVM"), LocalProject("connectivityJVM")))
    .jsConfigure(_.dependsOn(LocalProject("atlasJS"), LocalProject("connectivityJS")))
    .settings(
      name := "scalafim-atlas-workflows",
      scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xmax-inlines:64"),
      Test / fork := false,
      libraryDependencies += "org.scalameta" %%% "munit" % "1.2.1" % Test
    )
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.CommonJSModule)),
      Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
    )

lazy val atlasWorkflowsJS = atlasWorkflows.js
lazy val atlasWorkflowsJVM = atlasWorkflows.jvm

addCommandAlias("atlasWorkflowsCompile", ";atlasWorkflowsJVM/compile;atlasWorkflowsJS/compile")
addCommandAlias("atlasWorkflowsTest", ";atlasWorkflowsJVM/test;atlasWorkflowsJS/test")
