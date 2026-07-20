import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "scalafim"
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// Immutable source dependency: sbt clones this exact Gale commit into its
// staging area, so a clean checkout never depends on publishLocal or a sibling
// developer checkout.
lazy val galeRevision = "0207c653eb643cc07fdfa018989a4c3578d40aa7"
lazy val galeBuild =
  uri(s"https://github.com/bbuchsbaum/gale.git#$galeRevision")
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xmax-inlines:64"
  ),
  Test / fork := false,
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.1" % Test
)

lazy val jsSettingsBase = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
)

lazy val graph =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graph"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-graph"
    )
    .jsSettings(jsSettingsBase)

lazy val graphJS  = graph.js
lazy val graphJVM = graph.jvm

lazy val linalg =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/linalg"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-linalg"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val linalgJS  = linalg.js
lazy val linalgJVM = linalg.jvm

lazy val linalgBreeze =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/linalg-breeze"))
    .dependsOn(linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-linalg-breeze",
      libraryDependencies += "org.scalanlp" %% "breeze" % "2.1.0"
    )

lazy val linalgBreezeJVM = linalgBreeze.jvm

lazy val graphLinalg =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graph-linalg"))
    .dependsOn(graph, linalg, multivar % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-graph-linalg"
    )
    .jsSettings(jsSettingsBase)

lazy val graphLinalgJS  = graphLinalg.js
lazy val graphLinalgJVM = graphLinalg.jvm

lazy val pipeline =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/pipeline"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-pipeline"
    )
    .jsSettings(jsSettingsBase)

lazy val pipelineJS  = pipeline.js
lazy val pipelineJVM = pipeline.jvm

lazy val graphics =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graphics"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-graphics"
    )
    .jsSettings(jsSettingsBase)

lazy val graphicsJS  = graphics.js
lazy val graphicsJVM = graphics.jvm

lazy val graphicsSvg =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graphics-svg"))
    .dependsOn(graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-graphics-svg"
    )
    .jsSettings(jsSettingsBase)

lazy val graphicsSvgJS  = graphicsSvg.js
lazy val graphicsSvgJVM = graphicsSvg.jvm

lazy val graphicsCanvas =
  crossProject(JSPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graphics-canvas"))
    .dependsOn(graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-graphics-canvas"
    )
    .jsSettings(jsSettingsBase)

lazy val graphicsCanvasJS = graphicsCanvas.js

lazy val graphicsJava2d =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graphics-java2d"))
    .dependsOn(graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-graphics-java2d"
    )

lazy val graphicsJava2dJVM = graphicsJava2d.jvm

// OpenJFX publishes platform-specific artifacts by classifier; resolve the one
// matching the build machine so the JavaFX backend compiles and tests locally.
lazy val javafxPlatformClassifier: String = {
  val os = sys.props.getOrElse("os.name", "").toLowerCase
  val arch = sys.props.getOrElse("os.arch", "").toLowerCase
  val base =
    if (os.contains("mac")) "mac"
    else if (os.contains("win")) "win"
    else "linux"
  if (arch.contains("aarch64") && base != "win") base + "-aarch64" else base
}

lazy val graphicsJavafx =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graphics-javafx"))
    .dependsOn(graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-graphics-javafx",
      libraryDependencies ++= Seq(
        "org.openjfx" % "javafx-base" % "21.0.5" % Provided classifier javafxPlatformClassifier,
        "org.openjfx" % "javafx-graphics" % "21.0.5" % Provided classifier javafxPlatformClassifier
      )
    )

lazy val graphicsJavafxJVM = graphicsJavafx.jvm

lazy val latent =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/latent"))
    .dependsOn(linalg, archive)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-latent"
    )
    .jsSettings(jsSettingsBase)

lazy val latentJS  = latent.js
lazy val latentJVM = latent.jvm

lazy val ar =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/ar"))
    .dependsOn(linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-ar"
    )
    .jsSettings(jsSettingsBase)

lazy val arJS  = ar.js
lazy val arJVM = ar.jvm

lazy val hrf =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/hrf"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-hrf",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.12.0",
        "org.typelevel" %%% "spire"     % "0.18.0"
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.github.wendykierp" % "JTransforms" % "3.1"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val hrfJS  = hrf.js
lazy val hrfJVM = hrf.jvm

lazy val design =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/design"))
    .dependsOn(hrf, linalg, graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-design",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.12.0",
        "org.typelevel" %%% "spire"     % "0.18.0"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val designJS  = design.js
lazy val designJVM = design.jvm

lazy val image =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-image",
      libraryDependencies ++= Seq(
        "ai.dragonfly" %%% "narr"        % "1.0.1",
        "ai.dragonfly" %%% "slash"       % "0.4.1",
        "org.typelevel" %%% "cats-core"   % "2.12.0",
        "org.typelevel" %%% "cats-effect" % "3.5.4",
        "org.typelevel" %%% "spire"       % "0.18.0"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val imageJS  = image.js
lazy val imageJVM = image.jvm

lazy val imageView =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view"))
    .dependsOn(image, graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view"
    )
    .jsSettings(jsSettingsBase)

lazy val imageViewJS  = imageView.js
lazy val imageViewJVM = imageView.jvm

lazy val threshold =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/threshold"))
    .dependsOn(image, linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-threshold"
    )
    .jsSettings(jsSettingsBase)

lazy val thresholdJS  = threshold.js
lazy val thresholdJVM = threshold.jvm

lazy val motion =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/motion"))
    .dependsOn(image, linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-motion"
    )
    .jsSettings(jsSettingsBase)

lazy val motionJS  = motion.js
lazy val motionJVM = motion.jvm.dependsOn(bidsJVM)

lazy val surface =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface"))
    .dependsOn(image, graph)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface",
      libraryDependencies ++= Seq(
        "ai.dragonfly" %%% "narr" % "1.0.1"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceJS  = surface.js
lazy val surfaceJVM = surface.jvm

lazy val surfaceExamplesJVM =
  project
    .in(file("examples/surface-jvm"))
    .dependsOn(surfaceJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-surface-jvm",
      publish / skip := true
    )

lazy val spatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/spatial"))
    .dependsOn(linalg, image, surface)
    .settings(commonSettings)
    .settings(
      name := "scalafim-spatial"
    )
    .jsSettings(jsSettingsBase)

lazy val spatialJS  = spatial.js
lazy val spatialJVM = spatial.jvm

lazy val atlas =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/atlas"))
    .dependsOn(image, surface, graph)
    .settings(commonSettings)
    .settings(
      name := "scalafim-atlas"
    )
    .jsSettings(jsSettingsBase)

lazy val atlasJS  = atlas.js
lazy val atlasJVM = atlas.jvm

lazy val atlasExamplesJVM =
  project
    .in(file("examples/atlas-jvm"))
    .dependsOn(atlasJVM, mvpaSpatialJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-atlas-jvm",
      publish / skip := true
    )

lazy val workflowExamplesJVM =
  project
    .in(file("examples/workflows-jvm"))
    .dependsOn(atlasJVM, mvpaSpatialJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-workflows-jvm",
      publish / skip := true
    )

lazy val archive =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive"))
    .dependsOn(image)
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive"
    )
    .jvmSettings(
      libraryDependencies += "io.jhdf" % "jhdf" % "0.12.0"
    )
    .jsSettings(jsSettingsBase)

lazy val archiveJS  = archive.js
lazy val archiveJVM = archive.jvm

lazy val dataset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/dataset"))
    .dependsOn(image, hrf, archive, latent, bids)
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset",
      libraryDependencies ++= Seq(
        "ai.dragonfly" %%% "narr" % "1.0.1"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val datasetJS  = dataset.js
lazy val datasetJVM = dataset.jvm

lazy val bids =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/bids"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-bids"
    )
    .jsSettings(jsSettingsBase)

lazy val bidsJS  = bids.js
lazy val bidsJVM = bids.jvm

lazy val model =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/model"))
    .dependsOn(design, dataset, linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-model"
    )
    .jsSettings(jsSettingsBase)

lazy val modelJS  = model.js
lazy val modelJVM = model.jvm

lazy val fit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/fit"))
    .dependsOn(linalg, model, ar)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-fit"
    )
    .jsSettings(jsSettingsBase)

lazy val fitJS  = fit.js
lazy val fitJVM = fit.jvm

lazy val mvpa =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa"))
    .dependsOn(linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa"
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaJS  = mvpa.js
lazy val mvpaJVM = mvpa.jvm

lazy val multivar =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/multivar"))
    .dependsOn(linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-multivar"
    )
    .jsSettings(jsSettingsBase)

lazy val multivarJS  = multivar.js
lazy val multivarJVM = multivar.jvm

lazy val multivarIr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/multivar-ir"))
    .dependsOn(multivar)
    .settings(commonSettings)
    .settings(
      name := "scalafim-multivar-ir"
    )
    .jsSettings(jsSettingsBase)

lazy val multivarIrJS  = multivarIr.js
lazy val multivarIrJVM = multivarIr.jvm

lazy val inference =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/inference"))
    .dependsOn(multivar, linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-inference"
    )
    .jsSettings(jsSettingsBase)

lazy val inferenceJS  = inference.js
lazy val inferenceJVM = inference.jvm

lazy val connectivity =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/connectivity"))
    .dependsOn(graph, linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-connectivity"
    )
    .jsSettings(jsSettingsBase)

lazy val connectivityJS  = connectivity.js
lazy val connectivityJVM = connectivity.jvm

lazy val mvpaDataset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-dataset"))
    .dependsOn(mvpa, dataset)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-dataset"
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaDatasetJS  = mvpaDataset.js
lazy val mvpaDatasetJVM = mvpaDataset.jvm

lazy val mvpaSpatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-spatial"))
    .dependsOn(mvpa, image, surface, atlas)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-spatial"
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaSpatialJS  = mvpaSpatial.js
lazy val mvpaSpatialJVM = mvpaSpatial.jvm

lazy val group =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/group"))
    .dependsOn(linalg, image, dataset, design, fit)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-group"
    )
    .jsSettings(jsSettingsBase)

lazy val groupJS  = group.js
lazy val groupJVM = group.jvm

lazy val root =
  project
    .in(file("."))
    .aggregate(
      graphJS,
      graphJVM,
      graphLinalgJS,
      graphLinalgJVM,
      linalgJS,
      linalgJVM,
      linalgBreezeJVM,
      pipelineJS,
      pipelineJVM,
      graphicsJS,
      graphicsJVM,
      graphicsSvgJS,
      graphicsSvgJVM,
      graphicsCanvasJS,
      graphicsJava2dJVM,
      graphicsJavafxJVM,
      latentJS,
      latentJVM,
      arJS,
      arJVM,
      hrfJS,
      hrfJVM,
      designJS,
      designJVM,
      imageJS,
      imageJVM,
      imageViewJS,
      imageViewJVM,
      thresholdJS,
      thresholdJVM,
      motionJS,
      motionJVM,
      surfaceJS,
      surfaceJVM,
      spatialJS,
      spatialJVM,
      atlasJS,
      atlasJVM,
      archiveJS,
      archiveJVM,
      bidsJS,
      bidsJVM,
      datasetJS,
      datasetJVM,
      modelJS,
      modelJVM,
      fitJS,
      fitJVM,
      mvpaJS,
      mvpaJVM,
      multivarJS,
      multivarJVM,
      multivarIrJS,
      multivarIrJVM,
      inferenceJS,
      inferenceJVM,
      connectivityJS,
      connectivityJVM,
      mvpaDatasetJS,
      mvpaDatasetJVM,
      mvpaSpatialJS,
      mvpaSpatialJVM,
      surfaceExamplesJVM,
      atlasExamplesJVM,
      workflowExamplesJVM,
      groupJS,
      groupJVM
    )
    .settings(
      name := "scalafim",
      publish / skip := true
    )

addCommandAlias("compileAll", ";linalgJVM/compile;linalgJS/compile;pipelineJVM/compile;pipelineJS/compile;graphicsJVM/compile;graphicsJS/compile;graphicsSvgJVM/compile;graphicsSvgJS/compile;graphicsCanvasJS/compile;graphicsJava2dJVM/compile;graphicsJavafxJVM/compile;latentJVM/compile;latentJS/compile;arJVM/compile;arJS/compile;hrfJVM/compile;hrfJS/compile;designJVM/compile;designJS/compile;imageJVM/compile;imageJS/compile;thresholdJVM/compile;thresholdJS/compile;motionJVM/compile;motionJS/compile;surfaceJVM/compile;surfaceJS/compile;spatialJVM/compile;spatialJS/compile;atlasJVM/compile;atlasJS/compile;archiveJVM/compile;archiveJS/compile;bidsJVM/compile;bidsJS/compile;datasetJVM/compile;datasetJS/compile;modelJVM/compile;modelJS/compile;fitJVM/compile;fitJS/compile;mvpaJVM/compile;mvpaJS/compile;mvpaDatasetJVM/compile;mvpaDatasetJS/compile;mvpaSpatialJVM/compile;mvpaSpatialJS/compile;groupJVM/compile;groupJS/compile")
addCommandAlias("testAll", ";linalgJVM/test;linalgJS/test;pipelineJVM/test;pipelineJS/test;graphicsJVM/test;graphicsJS/test;graphicsSvgJVM/test;graphicsSvgJS/test;graphicsCanvasJS/test;graphicsJava2dJVM/test;graphicsJavafxJVM/test;latentJVM/test;latentJS/test;arJVM/test;arJS/test;hrfJVM/test;hrfJS/test;designJVM/test;designJS/test;imageJVM/test;imageJS/test;thresholdJVM/test;thresholdJS/test;motionJVM/test;motionJS/test;surfaceJVM/test;surfaceJS/test;spatialJVM/test;spatialJS/test;atlasJVM/test;atlasJS/test;archiveJVM/test;archiveJS/test;bidsJVM/test;bidsJS/test;datasetJVM/test;datasetJS/test;modelJVM/test;modelJS/test;fitJVM/test;fitJS/test;mvpaJVM/test;mvpaJS/test;mvpaDatasetJVM/test;mvpaDatasetJS/test;mvpaSpatialJVM/test;mvpaSpatialJS/test;groupJVM/test;groupJS/test")
addCommandAlias("examplesCompile", ";surfaceExamplesJVM/compile;atlasExamplesJVM/compile;workflowExamplesJVM/compile")
addCommandAlias("examplesTest", ";surfaceExamplesJVM/test;atlasExamplesJVM/test;workflowExamplesJVM/test")
addCommandAlias("atlasCoverage", ";set atlasJVM / coverageEnabled := true;atlasJVM/test;atlasJVM/coverageReport;set atlasJVM / coverageEnabled := false")
