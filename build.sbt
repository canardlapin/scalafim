import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "scalafim"
ThisBuild / scalaVersion := "3.4.2"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// Immutable source dependency: sbt clones this exact Gale commit into its
// staging area, so a clean checkout never depends on publishLocal or a sibling
// developer checkout.
lazy val galeRevision = "0e6e9934b358b569231ee6a39d43edde50c8fbc5"
lazy val galeBuild =
  uri(s"https://github.com/canardlapin/gale.git#$galeRevision")
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")
lazy val jhdfVersion = "0.12.0"

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

lazy val locusKernel =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/locus-kernel"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-locus-kernel"
    )
    .jsSettings(jsSettingsBase)

lazy val locusKernelJS  = locusKernel.js
lazy val locusKernelJVM = locusKernel.jvm

lazy val locusData =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/locus-data"))
    .dependsOn(locusKernel)
    .settings(commonSettings)
    .settings(
      name := "scalafim-locus-data",
      libraryDependencies += "org.typelevel" %%% "cats-kernel" % "2.12.0"
    )
    .jsSettings(jsSettingsBase)

lazy val locusDataJS  = locusData.js
lazy val locusDataJVM = locusData.jvm

lazy val locusLaws =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/locus-laws"))
    .dependsOn(locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-locus-laws",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.19.0" % Test
    )
    .jsSettings(jsSettingsBase)

lazy val locusLawsJS  = locusLaws.js
lazy val locusLawsJVM = locusLaws.jvm

lazy val graph =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graph"))
    .dependsOn(locusKernel)
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

lazy val graphLinalg =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/graph-linalg"))
    .dependsOn(graph, multivar % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-graph-linalg"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val graphLinalgJS  = graphLinalg.js
lazy val graphLinalgJVM = graphLinalg.jvm

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

lazy val pipeline =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/pipeline"))
    .dependsOn(graph)
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
    .dependsOn(archive)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-latent"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val latentJS  = latent.js
lazy val latentJVM = latent.jvm

lazy val ar =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/ar"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-ar"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
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
    .dependsOn(hrf, graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-design",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.12.0",
        "org.typelevel" %%% "spire"     % "0.18.0"
      )
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val designJS  = design.js
lazy val designJVM = design.jvm

lazy val image =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image"))
    .dependsOn(locusData, locusLaws % "test->compile")
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

lazy val registration =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/registration"))
    .dependsOn(image)
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .settings(commonSettings)
    .settings(
      name := "scalafim-registration"
    )
    .jsSettings(jsSettingsBase)

lazy val registrationJS  = registration.js
lazy val registrationJVM = registration.jvm

lazy val imageBenchJVM =
  project
    .in(file("benchmarks/image-jvm"))
    .dependsOn(imageJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-benchmarks",
      publish / skip := true
    )

lazy val galeBenchJVM =
  project
    .in(file("benchmarks/gale-jvm"))
    .dependsOn(linalgJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(
      name := "scalafim-gale-stress-benchmarks",
      publish / skip := true
    )

lazy val registrationBenchJVM =
  project
    .in(file("benchmarks/registration-jvm"))
    .dependsOn(registrationJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(
      name := "scalafim-registration-benchmarks",
      publish / skip := true
    )

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

lazy val imageViewCanvas =
  crossProject(JSPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view-canvas"))
    .dependsOn(imageView, graphicsCanvas)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view-canvas"
    )
    .jsSettings(jsSettingsBase)

lazy val imageViewCanvasJS = imageViewCanvas.js

lazy val imageViewJava2d =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view-java2d"))
    .dependsOn(imageView, graphicsJava2d)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view-java2d"
    )

lazy val imageViewJava2dJVM = imageViewJava2d.jvm

lazy val imageViewJavafx =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view-javafx"))
    .dependsOn(imageView, graphicsJavafx)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view-javafx",
      libraryDependencies ++= Seq(
        "org.openjfx" % "javafx-base" % "21.0.5" % Provided classifier javafxPlatformClassifier,
        "org.openjfx" % "javafx-graphics" % "21.0.5" % Provided classifier javafxPlatformClassifier
      )
    )

lazy val imageViewJavafxJVM = imageViewJavafx.jvm

lazy val threshold =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/threshold"))
    .dependsOn(image)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-threshold"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val thresholdJS  = threshold.js
lazy val thresholdJVM = threshold.jvm

lazy val motion =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/motion"))
    .dependsOn(image)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-motion"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val motionJS  = motion.js
lazy val motionJVM = motion.jvm.dependsOn(bidsJVM)

lazy val surface =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface"))
    .dependsOn(image, graph, locusData, locusLaws % "test->compile")
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

lazy val surfaceView =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view"))
    .dependsOn(surface, graphics)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view"
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewJS  = surfaceView.js
lazy val surfaceViewJVM = surfaceView.jvm

lazy val surfaceViewRaster =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view-raster"))
    .dependsOn(surfaceView)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view-raster"
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewRasterJS  = surfaceViewRaster.js
lazy val surfaceViewRasterJVM = surfaceViewRaster.jvm

lazy val surfaceViewJavafx =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view-javafx"))
    .dependsOn(surfaceView, graphicsJavafx, surfaceViewRaster % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view-javafx",
      Test / run / fork := true,
      libraryDependencies ++= Seq(
        "org.openjfx" % "javafx-base" % "21.0.5" % Provided classifier javafxPlatformClassifier,
        "org.openjfx" % "javafx-graphics" % "21.0.5" % Provided classifier javafxPlatformClassifier
      )
    )

lazy val surfaceViewJavafxJVM = surfaceViewJavafx.jvm

lazy val surfaceViewThree =
  crossProject(JSPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view-three"))
    .dependsOn(surfaceView)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view-three"
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewThreeJS = surfaceViewThree.js

lazy val surfaceViewExamples =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("examples/surface-view"))
    .dependsOn(surfaceView, surfaceViewRaster)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-surface-view",
      publish / skip := true
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewExamplesJS = surfaceViewExamples.js.dependsOn(surfaceViewThreeJS)
lazy val surfaceViewExamplesJVM = surfaceViewExamples.jvm
  .dependsOn(surfaceViewJavafxJVM)
  .settings(
    Test / run / fork := true,
    libraryDependencies ++= Seq(
      "org.openjfx" % "javafx-base" % "21.0.5" classifier javafxPlatformClassifier,
      "org.openjfx" % "javafx-graphics" % "21.0.5" classifier javafxPlatformClassifier
    )
  )

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
    .jvmSettings(
      libraryDependencies ++= Seq(
        "io.jhdf" % "jhdf" % jhdfVersion,
        "org.slf4j" % "slf4j-nop" % "2.0.18" % Test
      )
    )
    .jsSettings(jsSettingsBase)

lazy val spatialJS  = spatial.js
lazy val spatialJVM = spatial.jvm

lazy val atlas =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/atlas"))
    .dependsOn(image, surface, graph, locusData, locusLaws % "test->compile")
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
      libraryDependencies += "io.jhdf" % "jhdf" % jhdfVersion
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
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val datasetJS  = dataset.js
lazy val datasetJVM = dataset.jvm

lazy val bids =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/bids"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-bids",
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0"
    )
    .jvmSettings(
      libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.4"
    )
    .jsSettings(jsSettingsBase)

lazy val bidsJS  = bids.js
lazy val bidsJVM = bids.jvm

lazy val model =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/model"))
    .dependsOn(design, dataset)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-model"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val modelJS  = model.js
lazy val modelJVM = model.jvm

lazy val fit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/fit"))
    .dependsOn(model, ar, pipeline % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-fit"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val fitJS  = fit.js
lazy val fitJVM = fit.jvm

lazy val mvpa =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val mvpaJS  = mvpa.js
lazy val mvpaJVM = mvpa.jvm

lazy val mvpaFit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-fit"))
    .dependsOn(fit, mvpa, multivar % "compile->compile;test->test")
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-fit"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val mvpaFitJS  = mvpaFit.js
lazy val mvpaFitJVM = mvpaFit.jvm

lazy val multivar =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/multivar"))
    .dependsOn(linalg)
    .settings(commonSettings)
    .settings(
      name := "scalafim-multivar"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
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
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val multivarIrJS  = multivarIr.js
lazy val multivarIrJVM = multivarIr.jvm

lazy val inference =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/inference"))
    .dependsOn(multivar)
    .settings(commonSettings)
    .settings(
      name := "scalafim-inference"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val inferenceJS  = inference.js
lazy val inferenceJVM = inference.jvm

lazy val connectivity =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/connectivity"))
    .dependsOn(graph)
    .settings(commonSettings)
    .settings(
      name := "scalafim-connectivity"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val connectivityJS  = connectivity.js
lazy val connectivityJVM = connectivity.jvm

lazy val surfaceViewConnectivity =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view-connectivity"))
    .dependsOn(surfaceView, connectivity)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view-connectivity"
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewConnectivityJS  = surfaceViewConnectivity.js
lazy val surfaceViewConnectivityJVM = surfaceViewConnectivity.jvm

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
    .dependsOn(image, dataset, design, fit)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-group"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val groupJS  = group.js
lazy val groupJVM = group.jvm

lazy val fmriWorkflow =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/fmri-workflow"))
    .dependsOn(bids, dataset, model, fit, group)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-workflow"
    )
    .jsSettings(jsSettingsBase)

lazy val fmriWorkflowJS  = fmriWorkflow.js
lazy val fmriWorkflowJVM = fmriWorkflow.jvm

lazy val zarr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/zarr"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-zarr"
    )
    .jsSettings(jsSettingsBase)

lazy val zarrJS  = zarr.js
lazy val zarrJVM = zarr.jvm

lazy val zarrCodecBloscZstd =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/zarr-codec-blosc-zstd"))
    .dependsOn(zarr)
    .settings(commonSettings)
    .settings(
      name := "scalafim-zarr-codec-blosc-zstd"
    )
    .jvmSettings(
      libraryDependencies += "com.scalableminds" % "blosc-java" % "0.3-1.21.6"
    )
    .jsSettings(jsSettingsBase)
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule))
    )

lazy val zarrCodecBloscZstdJS  = zarrCodecBloscZstd.js
lazy val zarrCodecBloscZstdJVM = zarrCodecBloscZstd.jvm

lazy val archiveZarr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive-zarr"))
    .dependsOn(archive, zarr)
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive-zarr"
    )
    .jsSettings(jsSettingsBase)

lazy val archiveZarrJS  = archiveZarr.js
lazy val archiveZarrJVM = archiveZarr.jvm

lazy val datasetZarr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/dataset-zarr"))
    .dependsOn(dataset, archiveZarr, image, bids, fit % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset-zarr"
    )
    .jsSettings(jsSettingsBase)

lazy val datasetZarrJS  = datasetZarr.js
lazy val datasetZarrJVM = datasetZarr.jvm

lazy val root =
  project
    .in(file("."))
    .aggregate(
      locusKernelJS,
      locusKernelJVM,
      locusDataJS,
      locusDataJVM,
      locusLawsJS,
      locusLawsJVM,
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
      registrationJS,
      registrationJVM,
      imageViewJS,
      imageViewJVM,
      imageViewCanvasJS,
      imageViewJava2dJVM,
      imageViewJavafxJVM,
      thresholdJS,
      thresholdJVM,
      motionJS,
      motionJVM,
      surfaceJS,
      surfaceJVM,
      surfaceViewJS,
      surfaceViewJVM,
      surfaceViewRasterJS,
      surfaceViewRasterJVM,
      surfaceViewJavafxJVM,
      surfaceViewThreeJS,
      surfaceViewExamplesJS,
      surfaceViewExamplesJVM,
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
      mvpaFitJS,
      mvpaFitJVM,
      multivarJS,
      multivarJVM,
      multivarIrJS,
      multivarIrJVM,
      inferenceJS,
      inferenceJVM,
      connectivityJS,
      connectivityJVM,
      surfaceViewConnectivityJS,
      surfaceViewConnectivityJVM,
      mvpaDatasetJS,
      mvpaDatasetJVM,
      mvpaSpatialJS,
      mvpaSpatialJVM,
      surfaceExamplesJVM,
      atlasExamplesJVM,
      workflowExamplesJVM,
      groupJS,
      groupJVM,
      fmriWorkflowJS,
      fmriWorkflowJVM,
      zarrJS,
      zarrJVM,
      zarrCodecBloscZstdJS,
      zarrCodecBloscZstdJVM,
      archiveZarrJS,
      archiveZarrJVM,
      datasetZarrJS,
      datasetZarrJVM
    )
    .settings(
      name := "scalafim",
      publish / skip := true
    )

addCommandAlias("compileAll", ";locusKernelJVM/compile;locusKernelJS/compile;locusDataJVM/compile;locusDataJS/compile;locusLawsJVM/compile;locusLawsJS/compile;graphJVM/compile;graphJS/compile;graphLinalgJVM/compile;graphLinalgJS/compile;linalgJVM/compile;linalgJS/compile;linalgBreezeJVM/compile;pipelineJVM/compile;pipelineJS/compile;graphicsJVM/compile;graphicsJS/compile;graphicsSvgJVM/compile;graphicsSvgJS/compile;graphicsCanvasJS/compile;graphicsJava2dJVM/compile;graphicsJavafxJVM/compile;latentJVM/compile;latentJS/compile;arJVM/compile;arJS/compile;hrfJVM/compile;hrfJS/compile;designJVM/compile;designJS/compile;imageJVM/compile;imageJS/compile;registrationJVM/compile;registrationJS/compile;imageViewJVM/compile;imageViewJS/compile;imageViewCanvasJS/compile;imageViewJava2dJVM/compile;imageViewJavafxJVM/compile;thresholdJVM/compile;thresholdJS/compile;motionJVM/compile;motionJS/compile;surfaceJVM/compile;surfaceJS/compile;surfaceViewJVM/compile;surfaceViewJS/compile;surfaceViewRasterJVM/compile;surfaceViewRasterJS/compile;surfaceViewJavafxJVM/compile;surfaceViewThreeJS/compile;surfaceViewConnectivityJVM/compile;surfaceViewConnectivityJS/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;spatialJVM/compile;spatialJS/compile;atlasJVM/compile;atlasJS/compile;archiveJVM/compile;archiveJS/compile;bidsJVM/compile;bidsJS/compile;datasetJVM/compile;datasetJS/compile;modelJVM/compile;modelJS/compile;fitJVM/compile;fitJS/compile;mvpaJVM/compile;mvpaJS/compile;mvpaFitJVM/compile;mvpaFitJS/compile;multivarJVM/compile;multivarJS/compile;multivarIrJVM/compile;multivarIrJS/compile;inferenceJVM/compile;inferenceJS/compile;connectivityJVM/compile;connectivityJS/compile;mvpaDatasetJVM/compile;mvpaDatasetJS/compile;mvpaSpatialJVM/compile;mvpaSpatialJS/compile;groupJVM/compile;groupJS/compile;fmriWorkflowJVM/compile;fmriWorkflowJS/compile;zarrJVM/compile;zarrJS/compile;zarrCodecBloscZstdJVM/compile;zarrCodecBloscZstdJS/compile;archiveZarrJVM/compile;archiveZarrJS/compile;datasetZarrJVM/compile;datasetZarrJS/compile")
addCommandAlias("testAll", ";locusKernelJVM/test;locusKernelJS/test;locusDataJVM/test;locusDataJS/test;locusLawsJVM/test;locusLawsJS/test;graphJVM/test;graphJS/test;graphLinalgJVM/test;graphLinalgJS/test;linalgJVM/test;linalgJS/test;linalgBreezeJVM/test;pipelineJVM/test;pipelineJS/test;graphicsJVM/test;graphicsJS/test;graphicsSvgJVM/test;graphicsSvgJS/test;graphicsCanvasJS/test;graphicsJava2dJVM/test;graphicsJavafxJVM/test;latentJVM/test;latentJS/test;arJVM/test;arJS/test;hrfJVM/test;hrfJS/test;designJVM/test;designJS/test;imageJVM/test;imageJS/test;registrationJVM/test;registrationJS/test;imageViewJVM/test;imageViewJS/test;imageViewCanvasJS/test;imageViewJava2dJVM/test;imageViewJavafxJVM/test;thresholdJVM/test;thresholdJS/test;motionJVM/test;motionJS/test;surfaceJVM/test;surfaceJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;spatialJVM/test;spatialJS/test;atlasJVM/test;atlasJS/test;archiveJVM/test;archiveJS/test;bidsJVM/test;bidsJS/test;datasetJVM/test;datasetJS/test;modelJVM/test;modelJS/test;fitJVM/test;fitJS/test;mvpaJVM/test;mvpaJS/test;mvpaFitJVM/test;mvpaFitJS/test;multivarJVM/test;multivarJS/test;multivarIrJVM/test;multivarIrJS/test;inferenceJVM/test;inferenceJS/test;connectivityJVM/test;connectivityJS/test;mvpaDatasetJVM/test;mvpaDatasetJS/test;mvpaSpatialJVM/test;mvpaSpatialJS/test;groupJVM/test;groupJS/test;fmriWorkflowJVM/test;fmriWorkflowJS/test;zarrJVM/test;zarrJS/test;zarrCodecBloscZstdJVM/test;zarrCodecBloscZstdJS/test;archiveZarrJVM/test;archiveZarrJS/test;datasetZarrJVM/test;datasetZarrJS/test")
addCommandAlias("examplesCompile", ";surfaceExamplesJVM/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;atlasExamplesJVM/compile;workflowExamplesJVM/compile")
addCommandAlias("examplesTest", ";surfaceExamplesJVM/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;atlasExamplesJVM/test;workflowExamplesJVM/test")
addCommandAlias("surfaceViewConformance", ";surfaceJVM/test;surfaceJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test")
addCommandAlias("surfaceViewAdmissionJVM", ";surfaceViewRasterJVM/runMain scalafim.surface.view.raster.SurfaceRasterAdmissionBenchmark;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceAdmissionBenchmark")
addCommandAlias("surfaceViewVisualQaJVM", ";surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxGiftiParityProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceCorrectnessProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceInteractionProbe")
addCommandAlias("atlasCoverage", ";set atlasJVM / coverageEnabled := true;atlasJVM/test;atlasJVM/coverageReport;set atlasJVM / coverageEnabled := false")
