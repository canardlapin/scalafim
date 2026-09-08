import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

ThisBuild / organization := "scalafim"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// Ravel is the single dense-array substrate shared by response and image
// semantics. The local override coordinates cross-repository development;
// ordinary builds remain pinned to an immutable source revision.
lazy val ravelRevision = "f804ba51242aae3a1442b3855a20bd896ffa8b64"
lazy val ravelBuild =
  sys.props
    .get("scalafim.ravel.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/ravel.git#$ravelRevision"))
lazy val ravelCoreJVM = ProjectRef(ravelBuild, "coreJVM")
lazy val ravelCoreJS  = ProjectRef(ravelBuild, "coreJS")

// Immutable source dependency: sbt clones this exact Gale commit into its
// staging area, so a clean checkout never depends on publishLocal or a sibling
// developer checkout.
lazy val galeRevision = "83cac90a678d1b8a31c590e0c1b8fc8bf3427161"
lazy val galeBuild =
  uri(s"https://github.com/canardlapin/gale.git#$galeRevision")
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")

// locus4s is independently owned. Ordinary builds clone the exact reviewed
// revision; the property is an explicit sibling-checkout override for
// coordinated development.
lazy val locus4sRevision = "58c9739be51345ad9adc4bc9c9e7335023254ec9"
lazy val locus4sBuild =
  sys.props
    .get("scalafim.locus4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/locus4s.git#$locus4sRevision"))
lazy val locus4sCoreJVM = ProjectRef(locus4sBuild, "locus4s-coreJVM")
lazy val locus4sCoreJS  = ProjectRef(locus4sBuild, "locus4s-coreJS")
lazy val locus4sDataJVM = ProjectRef(locus4sBuild, "locus4s-dataJVM")
lazy val locus4sDataJS  = ProjectRef(locus4sBuild, "locus4s-dataJS")

// image4s is independently owned. Ordinary builds use its immutable source
// revision; coordinated development can select a sibling checkout explicitly.
lazy val image4sRevision = "497bfd164ad514ff3d1944699550c78caa57e85d"
lazy val image4sBuild = {
  sys.props
    .get("scalafim.locus4s.build")
    .foreach(System.setProperty("image4s.locus4s.build", _))
  sys.props
    .get("scalafim.image4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/image4s.git#$image4sRevision"))
}
lazy val image4sCoreJVM = ProjectRef(image4sBuild, "image4s-coreJVM")
lazy val image4sCoreJS  = ProjectRef(image4sBuild, "image4s-coreJS")

// graph4s is an independently owned topology and algorithms library. Ordinary
// builds clone the exact reviewed revision; the property is an explicit local
// source override for coordinated development.
lazy val graph4sRevision = "ea5d2d762f85f5a0f97ee188deb5fac0ef2bcbaf"
lazy val graph4sBuild =
  sys.props
    .get("scalafim.graph4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/graph4s.git#$graph4sRevision"))
lazy val graph4sCoreJVM       = ProjectRef(graph4sBuild, "coreJVM")
lazy val graph4sCoreJS        = ProjectRef(graph4sBuild, "coreJS")
lazy val graph4sAlgorithmsJVM = ProjectRef(graph4sBuild, "algorithmsJVM")
lazy val graph4sAlgorithmsJS  = ProjectRef(graph4sBuild, "algorithmsJS")

// General multivariate analysis is developed independently. The optional
// system property is an explicit local-development override; ordinary builds
// clone the exact committed source revision.
lazy val multivarRevision = "b0a16e0764bc4d86a95dfe02a043f8fe240b7e27"
lazy val multivarBuild =
  uri(
    sys.props.getOrElse(
      "scalafim.multivar.build",
      s"https://github.com/canardlapin/multivar.git#$multivarRevision"
    )
  )
lazy val multivarJVM   = ProjectRef(multivarBuild, "coreJVM")
lazy val multivarJS    = ProjectRef(multivarBuild, "coreJS")
lazy val multivarIrJVM = ProjectRef(multivarBuild, "irJVM")
lazy val multivarIrJS  = ProjectRef(multivarBuild, "irJS")

// Finite reindexing and resampling semantics are independently owned by
// resample4s. The property supports coordinated local development while the
// default remains the exact reviewed source revision.
lazy val resample4sRevision = "6bc4172a966c92f1b06811eac64ac2bada9fef9b"
lazy val resample4sBuild =
  uri(
    sys.props.getOrElse(
      "scalafim.resample4s.build",
      s"https://github.com/canardlapin/resample4s.git#$resample4sRevision"
    )
  )
lazy val resample4sCoreJVM = ProjectRef(resample4sBuild, "coreJVM")
lazy val resample4sCoreJS  = ProjectRef(resample4sBuild, "coreJS")
lazy val resample4sDesignsJVM = ProjectRef(resample4sBuild, "designsJVM")
lazy val resample4sDesignsJS  = ProjectRef(resample4sBuild, "designsJS")

// Predictive fitting discipline is independently owned by Alder. Keep this
// dependency at the MVPA predictive boundary: the identified-evidence and
// relational cores must not expose Alder types. The override supports a clean
// sibling composite rehearsal without weakening the immutable default pin.
lazy val alderRevision = "aa998f88ac96c655aa6f4093ffb28492b9e31087"
lazy val alderBuild =
  uri(
    sys.props.getOrElse(
      "scalafim.alder.build",
      s"https://github.com/canardlapin/alder.git#$alderRevision"
    )
  )
lazy val alderApplicationJVM = ProjectRef(alderBuild, "applicationJVM")
lazy val alderApplicationJS  = ProjectRef(alderBuild, "applicationJS")
lazy val alderPreprocessJVM  = ProjectRef(alderBuild, "preprocessJVM")
lazy val alderPreprocessJS   = ProjectRef(alderBuild, "preprocessJS")
lazy val alderTuneJVM        = ProjectRef(alderBuild, "tuneJVM")
lazy val alderTuneJS         = ProjectRef(alderBuild, "tuneJS")

// Renderer-neutral graphics and platform backends are developed independently.
// Ordinary builds clone the exact public revision; the system property is an
// explicit local-development override.
lazy val intaglioRevision = "55658abfbbfed0c9a36ab612b38bd8f0677bc158"
lazy val intaglioBuild =
  uri(
    sys.props.getOrElse(
      "scalafim.intaglio.build",
      s"https://github.com/canardlapin/intaglio.git#$intaglioRevision"
    )
  )
lazy val intaglioCoreJVM   = ProjectRef(intaglioBuild, "coreJVM")
lazy val intaglioCoreJS    = ProjectRef(intaglioBuild, "coreJS")
lazy val intaglioSvgJVM    = ProjectRef(intaglioBuild, "svgJVM")
lazy val intaglioCanvasJS  = ProjectRef(intaglioBuild, "canvasJS")
lazy val intaglioJava2dJVM = ProjectRef(intaglioBuild, "java2dJVM")
lazy val intaglioJavafxJVM = ProjectRef(intaglioBuild, "javafxJVM")

// Reusable BIDS semantics are developed independently. Ordinary builds clone
// the exact reviewed revision; the property is an explicit local-development
// override for downstream rehearsal.
lazy val bids4sRevision = "a33678390614a91fadbdef13f22970e78c26e091"
lazy val bids4sBuild =
  uri(
    sys.props.getOrElse(
      "scalafim.bids4s.build",
      s"https://github.com/canardlapin/bids4s.git#$bids4sRevision"
    )
  )
lazy val bids4sJVM = ProjectRef(bids4sBuild, "coreJVM")
lazy val bids4sJS  = ProjectRef(bids4sBuild, "coreJS")

// Generic Zarr mechanics are independently owned by zarr4s. Ordinary builds
// clone the exact reviewed revision; the property is an explicit local source
// override for coordinated development.
lazy val zarr4sRevision = "2a5ba963b151b62c739d1bf5a19d49202bb6ff29"
lazy val zarr4sBuild =
  sys.props
    .get("scalafim.zarr4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/zarr4s.git#$zarr4sRevision"))
lazy val zarr4sCoreJVM = ProjectRef(zarr4sBuild, "coreJVM")
lazy val zarr4sCoreJS  = ProjectRef(zarr4sBuild, "coreJS")

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

// The first-level scientific stack is the initial strict-warning court. Keep
// these options scoped until the remaining modules have been migrated rather
// than weakening the signal with repository-wide exclusions.
lazy val strictFirstLevelCompilerSettings = Seq(
  scalacOptions ++= Seq(
    "-Werror",
    "-Wunused:all",
    "-Wvalue-discard"
  )
)

// MVPA is a closed strict-warning court on both JVM and Scala.js. Keep this
// scoped to the unified module so warning debt elsewhere cannot dilute its
// compiler contract.
lazy val strictMvpaCompilerSettings = Seq(
  scalacOptions ++= Seq(
    "-Werror",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wnonunit-statement"
  )
)

def scientificCoverageSettings(statementMinimum: Double, branchMinimum: Double) = Seq(
  coverageMinimumStmtTotal := statementMinimum,
  coverageMinimumBranchTotal := branchMinimum,
  coverageFailOnMinimum := true
)

lazy val jsSettingsBase = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  Test / jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv()
)

lazy val locusData =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/locus-data"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-locus-data",
      libraryDependencies += "org.typelevel" %%% "cats-kernel" % "2.12.0"
    )
    .jvmConfigure(_.dependsOn(locus4sCoreJVM, locus4sDataJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS, locus4sDataJS))
    .jsSettings(jsSettingsBase)

lazy val locusDataJS  = locusData.js
lazy val locusDataJVM = locusData.jvm

lazy val pipeline =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/pipeline"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-pipeline",
      // graph4s is a source ProjectRef. Re-declare its runtime collection
      // dependency at this public boundary so second-order ScalaFIM consumers
      // such as fit receive it on both JVM and Scala.js classpaths.
      libraryDependencies +=
        "org.typelevel" %%% "cats-collections-core" % "0.9.10"
    )
    .jvmConfigure(_.dependsOn(graph4sAlgorithmsJVM))
    .jsConfigure(_.dependsOn(graph4sAlgorithmsJS))
    .jsSettings(jsSettingsBase)

lazy val pipelineJS  = pipeline.js
lazy val pipelineJVM = pipeline.jvm

// OpenJFX publishes platform-specific artifacts by classifier; resolve the one
// matching the build machine so ScalaFIM's JavaFX hosts compile and test locally.
lazy val javafxPlatformClassifier: String = {
  val os = sys.props.getOrElse("os.name", "").toLowerCase
  val arch = sys.props.getOrElse("os.arch", "").toLowerCase
  val base =
    if (os.contains("mac")) "mac"
    else if (os.contains("win")) "win"
    else "linux"
  if (arch.contains("aarch64") && base != "win") base + "-aarch64" else base
}

lazy val response =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/response"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-response",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core"   % "2.12.0",
        "org.typelevel" %%% "cats-effect" % "3.5.4"
      )
    )
    .jvmConfigure(_.dependsOn(ravelCoreJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS))
    .jsSettings(jsSettingsBase)

lazy val responseJS  = response.js
lazy val responseJVM = response.jvm

lazy val responseLaws =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/response-laws"))
    .dependsOn(response)
    .settings(commonSettings)
    .settings(
      name := "scalafim-response-laws"
    )
    .jsSettings(jsSettingsBase)

lazy val responseLawsJS  = responseLaws.js
lazy val responseLawsJVM = responseLaws.jvm

lazy val latent =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/latent"))
    .dependsOn(
      response,
      responseLaws % "test->compile",
      image,
      locusData
    )
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-latent",
      Test / unmanagedSources ~= (_.filterNot(
        _.getName == "ResponseArchiveMigrationBaselineSuite.scala"
      ))
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
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-ar"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jvmSettings(scientificCoverageSettings(statementMinimum = 75.0, branchMinimum = 62.0))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val arJS  = ar.js
lazy val arJVM = ar.jvm

// Cross-built scenario verdict/policy core. It is test-support only: design
// and fit depend on its main classes from test scope, so published production
// artifacts do not acquire MUnit or scenario-testkit dependencies.
lazy val scenarioTestkit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/scenario-testkit"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-scenario-testkit",
      publish / skip := true
    )
    .jsSettings(jsSettingsBase)

lazy val scenarioTestkitJS  = scenarioTestkit.js
lazy val scenarioTestkitJVM = scenarioTestkit.jvm

lazy val hrf =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/hrf"))
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-hrf",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.12.0",
        "org.typelevel" %%% "spire"     % "0.18.0",
        "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test
      )
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "com.github.wendykierp" % "JTransforms" % "3.1"
      )
    )
    .jvmSettings(scientificCoverageSettings(statementMinimum = 70.0, branchMinimum = 60.0))
    .jsSettings(jsSettingsBase)

lazy val hrfJS  = hrf.js
lazy val hrfJVM = hrf.jvm

lazy val hrfLaws =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/hrf-laws"))
    .dependsOn(hrf)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-hrf-laws",
      libraryDependencies += "org.scalameta" %%% "munit" % "1.2.1"
    )
    .jsSettings(jsSettingsBase)

lazy val hrfLawsJS  = hrfLaws.js
lazy val hrfLawsJVM = hrfLaws.jvm

lazy val design =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/design"))
    .dependsOn(hrf, scenarioTestkit % "test->compile")
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-design",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core" % "2.12.0",
        "org.typelevel" %%% "spire"     % "0.18.0"
      )
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, intaglioCoreJVM, intaglioSvgJVM % "test->compile"))
    .jvmSettings(scientificCoverageSettings(statementMinimum = 70.0, branchMinimum = 58.0))
    .jsConfigure(_.dependsOn(galeCoreJS, intaglioCoreJS))
    .jsSettings(jsSettingsBase)

lazy val designJS  = design.js
lazy val designJVM = design.jvm

lazy val image =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image"))
    .dependsOn(locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-image",
      libraryDependencies ++= Seq(
        "ai.dragonfly" %%% "slash"       % "0.4.1",
        "org.typelevel" %%% "cats-core"   % "2.12.0",
        "org.typelevel" %%% "cats-effect" % "3.5.4",
        "org.typelevel" %%% "spire"       % "0.18.0"
      )
    )
    .jvmConfigure(_.dependsOn(image4sCoreJVM))
    .jsConfigure(_.dependsOn(image4sCoreJS))
    .jsSettings(jsSettingsBase)

lazy val imageJS  = image.js
lazy val imageJVM = image.jvm

lazy val hrfBenchJVM =
  project
    .in(file("benchmarks/hrf-jvm"))
    .dependsOn(hrfJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-hrf-benchmarks",
      publish / skip := true
    )

lazy val imageView =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view"))
    .dependsOn(image)
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))
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
    .dependsOn(imageView)
    .jsConfigure(_.dependsOn(intaglioCanvasJS))
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
    .dependsOn(imageView)
    .jvmConfigure(_.dependsOn(intaglioJava2dJVM))
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view-java2d"
    )

lazy val imageViewJava2dJVM = imageViewJava2d.jvm

lazy val imageViewJavafx =
  crossProject(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/image-view-javafx"))
    .dependsOn(imageView)
    .jvmConfigure(_.dependsOn(intaglioJavafxJVM))
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
lazy val motionJVM = motion.jvm.dependsOn(bids4sJVM)

lazy val surface =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface"))
    .dependsOn(image, locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface",
      libraryDependencies ++= Seq(
        "org.scala-lang.modules" %%% "scala-xml" % "2.4.0"
      )
    )
    .jvmConfigure(_.dependsOn(graph4sAlgorithmsJVM))
    .jsConfigure(_.dependsOn(graph4sAlgorithmsJS))
    .jsSettings(jsSettingsBase)

lazy val surfaceJS  = surface.js
lazy val surfaceJVM = surface.jvm

lazy val surfaceView =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view"))
    .dependsOn(surface)
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))
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
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))
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
    .dependsOn(surfaceView, surfaceViewRaster % "test->compile")
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
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
    .jsConfigure(_.dependsOn(intaglioCoreJS))
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
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))
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
    .dependsOn(image, surface, locusData)
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
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val spatialJS  = spatial.js
lazy val spatialJVM = spatial.jvm

lazy val atlas =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/atlas"))
    .dependsOn(image, surface, locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-atlas"
    )
    .jvmConfigure(_.dependsOn(graph4sAlgorithmsJVM))
    .jsConfigure(_.dependsOn(graph4sAlgorithmsJS))
    .jsSettings(jsSettingsBase)

lazy val atlasJS  = atlas.js
lazy val atlasJVM = atlas.jvm

lazy val atlasExamplesJVM =
  project
    .in(file("examples/atlas-jvm"))
    .dependsOn(atlasJVM, mvpaJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-atlas-jvm",
      publish / skip := true
    )

lazy val workflowExamplesJVM =
  project
    .in(file("examples/workflows-jvm"))
    .dependsOn(atlasJVM, mvpaJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-workflows-jvm",
      publish / skip := true
    )

lazy val archive =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core"   % "2.12.0",
        "org.typelevel" %%% "cats-effect" % "3.5.4"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val archiveJS  = archive.js
lazy val archiveJVM = archive.jvm

lazy val archiveLna =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive-lna"))
    .dependsOn(archive, image)
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive-lna"
    )
    .jvmSettings(
      libraryDependencies += "io.jhdf" % "jhdf" % jhdfVersion
    )
    .jsSettings(jsSettingsBase)

lazy val archiveLnaJS  = archiveLna.js
lazy val archiveLnaJVM = archiveLna.jvm

lazy val archivedResponseInterop =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/interop-archived-response"))
    .dependsOn(
      response,
      responseLaws % "test->compile",
      latent % "compile->compile;test->test",
      archive,
      archiveLna % "compile->compile;test->test",
      archiveZarr % "compile->compile;test->test",
      dataset % "compile->compile;test->test"
    )
    .settings(commonSettings)
    .settings(
      name := "scalafim-interop-archived-response",
      Test / unmanagedSources += file(
        "modules/latent/shared/src/test/scala/scalafim/latent/" +
          "ResponseArchiveMigrationBaselineSuite.scala"
      )
    )
    .jvmConfigure(_.dependsOn(bids4sJVM, zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(zarr4sCoreJS))
    .jsSettings(jsSettingsBase)

lazy val archivedResponseInteropJS  = archivedResponseInterop.js
lazy val archivedResponseInteropJVM = archivedResponseInterop.jvm

lazy val dataset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/dataset"))
    .dependsOn(
      response,
      responseLaws % "test->compile",
      image,
      hrf,
      locusData
    )
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset",
      libraryDependencies ++= Seq(
      )
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val datasetJS  = dataset.js
lazy val datasetJVM = dataset.jvm

lazy val model =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/model"))
    .dependsOn(design, dataset)
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-model"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jvmSettings(scientificCoverageSettings(statementMinimum = 58.0, branchMinimum = 52.0))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val modelJS  = model.js
lazy val modelJVM = model.jvm

lazy val fit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/fit"))
    .dependsOn(model, ar, pipeline % "test->compile", scenarioTestkit % "test->compile")
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-fit"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jvmSettings(scientificCoverageSettings(statementMinimum = 75.0, branchMinimum = 62.0))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val fitJS  = fit.js
lazy val fitJVM = fit.jvm

lazy val fitBenchJVM =
  project
    .in(file("benchmarks/fit-jvm"))
    .dependsOn(fitJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fit-benchmarks",
      publish / skip := true
    )

// Cross-built generated-law court for the complete public first-level stack.
// It is deliberately non-published: generated evidence belongs beside the
// production modules, without making property-testing part of their API.
lazy val firstLevelLaws =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/first-level-laws"))
    .dependsOn(fit, hrfLaws)
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-fmri-first-level-laws",
      publish / skip := true,
      libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test
    )
    .jsSettings(jsSettingsBase)

lazy val firstLevelLawsJS  = firstLevelLaws.js
lazy val firstLevelLawsJVM = firstLevelLaws.jvm

lazy val mvpa =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa"))
    .dependsOn(fit, dataset, image, surface, atlas, locusData)
    .settings(commonSettings)
    .settings(strictMvpaCompilerSettings)
    .settings(
      name := "scalafim-fmri-mvpa",
      libraryDependencies += "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test
    )
    .jvmConfigure(
      _.dependsOn(
        galeCoreJVM,
        multivarJVM,
        resample4sCoreJVM,
        resample4sDesignsJVM,
        alderApplicationJVM,
        alderPreprocessJVM,
        alderTuneJVM
      )
    )
    .jsConfigure(
      _.dependsOn(
        galeCoreJS,
        multivarJS,
        resample4sCoreJS,
        resample4sDesignsJS,
        alderApplicationJS,
        alderPreprocessJS,
        alderTuneJS
      )
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaJS  = mvpa.js
lazy val mvpaJVM = mvpa.jvm

lazy val mvpaBenchJVM =
  project
    .in(file("benchmarks/mvpa-jvm"))
    .dependsOn(mvpaJVM)
    .enablePlugins(JmhPlugin)
    .settings(commonSettings)
    .settings(strictMvpaCompilerSettings)
    .settings(
      name := "scalafim-mvpa-benchmarks",
      publish / skip := true
    )

lazy val connectivity =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/connectivity"))
    .dependsOn(locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-connectivity"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, graph4sCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS, graph4sCoreJS))
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
    .dependsOn(dataset, model, fit, group)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-workflow"
    )
    .jvmConfigure(_.dependsOn(bids4sJVM))
    .jsConfigure(_.dependsOn(bids4sJS))
    .jsSettings(jsSettingsBase)

lazy val fmriWorkflowJS  = fmriWorkflow.js
lazy val fmriWorkflowJVM = fmriWorkflow.jvm

lazy val archiveZarr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive-zarr"))
    .dependsOn(archive)
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive-zarr",
      libraryDependencies ++= Seq(
        "org.typelevel" %%% "cats-core"   % "2.12.0",
        "org.typelevel" %%% "cats-effect" % "3.5.4"
      )
    )
    .jvmConfigure(_.dependsOn(zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(zarr4sCoreJS))
    .jsSettings(jsSettingsBase)

lazy val archiveZarrJS  = archiveZarr.js
lazy val archiveZarrJVM = archiveZarr.jvm

lazy val datasetZarr =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/dataset-zarr"))
    .dependsOn(dataset, archiveZarr, image, fit % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset-zarr"
    )
    .jvmConfigure(_.dependsOn(bids4sJVM, zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(bids4sJS, zarr4sCoreJS))
    .jsSettings(jsSettingsBase)

lazy val datasetZarrJS  = datasetZarr.js
lazy val datasetZarrJVM = datasetZarr.jvm

lazy val root =
  project
    .in(file("."))
    .aggregate(
      locusDataJS,
      locusDataJVM,
      pipelineJS,
      pipelineJVM,
      responseJS,
      responseJVM,
      responseLawsJS,
      responseLawsJVM,
      latentJS,
      latentJVM,
      arJS,
      arJVM,
      hrfJS,
      hrfJVM,
      hrfLawsJS,
      hrfLawsJVM,
      scenarioTestkitJS,
      scenarioTestkitJVM,
      designJS,
      designJVM,
      imageJS,
      imageJVM,
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
      archiveLnaJS,
      archiveLnaJVM,
      archivedResponseInteropJS,
      archivedResponseInteropJVM,
      datasetJS,
      datasetJVM,
      modelJS,
      modelJVM,
      fitJS,
      fitJVM,
      firstLevelLawsJS,
      firstLevelLawsJVM,
      mvpaJS,
      mvpaJVM,
      connectivityJS,
      connectivityJVM,
      surfaceViewConnectivityJS,
      surfaceViewConnectivityJVM,
      surfaceExamplesJVM,
      atlasExamplesJVM,
      workflowExamplesJVM,
      groupJS,
      groupJVM,
      fmriWorkflowJS,
      fmriWorkflowJVM,
      archiveZarrJS,
      archiveZarrJVM,
      datasetZarrJS,
      datasetZarrJVM
    )
    .settings(
      name := "scalafim",
      publish / skip := true
    )

addCommandAlias("compileAll", ";locusDataJVM/compile;locusDataJS/compile;pipelineJVM/compile;pipelineJS/compile;responseJVM/compile;responseJS/compile;responseLawsJVM/compile;responseLawsJS/compile;latentJVM/compile;latentJS/compile;arJVM/compile;arJS/compile;hrfJVM/compile;hrfJS/compile;hrfLawsJVM/compile;hrfLawsJS/compile;designJVM/compile;designJS/compile;imageJVM/compile;imageJS/compile;imageViewJVM/compile;imageViewJS/compile;imageViewCanvasJS/compile;imageViewJava2dJVM/compile;imageViewJavafxJVM/compile;thresholdJVM/compile;thresholdJS/compile;motionJVM/compile;motionJS/compile;surfaceJVM/compile;surfaceJS/compile;surfaceViewJVM/compile;surfaceViewJS/compile;surfaceViewRasterJVM/compile;surfaceViewRasterJS/compile;surfaceViewJavafxJVM/compile;surfaceViewThreeJS/compile;surfaceViewConnectivityJVM/compile;surfaceViewConnectivityJS/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;spatialJVM/compile;spatialJS/compile;atlasJVM/compile;atlasJS/compile;archiveJVM/compile;archiveJS/compile;archiveLnaJVM/compile;archiveLnaJS/compile;archivedResponseInteropJVM/compile;archivedResponseInteropJS/compile;datasetJVM/compile;datasetJS/compile;modelJVM/compile;modelJS/compile;fitJVM/compile;fitJS/compile;firstLevelLawsJVM/compile;firstLevelLawsJS/compile;mvpaJVM/compile;mvpaJS/compile;connectivityJVM/compile;connectivityJS/compile;groupJVM/compile;groupJS/compile;fmriWorkflowJVM/compile;fmriWorkflowJS/compile;archiveZarrJVM/compile;archiveZarrJS/compile;datasetZarrJVM/compile;datasetZarrJS/compile")
addCommandAlias("testAll", ";locusDataJVM/test;locusDataJS/test;pipelineJVM/test;pipelineJS/test;responseJVM/test;responseJS/test;responseLawsJVM/test;responseLawsJS/test;latentJVM/test;latentJS/test;arJVM/test;arJS/test;hrfJVM/test;hrfJS/test;hrfLawsJVM/test;hrfLawsJS/test;designJVM/test;designJS/test;imageJVM/test;imageJS/test;imageViewJVM/test;imageViewJS/test;imageViewCanvasJS/test;imageViewJava2dJVM/test;imageViewJavafxJVM/test;thresholdJVM/test;thresholdJS/test;motionJVM/test;motionJS/test;surfaceJVM/test;surfaceJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;spatialJVM/test;spatialJS/test;atlasJVM/test;atlasJS/test;archiveJVM/test;archiveJS/test;archiveLnaJVM/test;archiveLnaJS/test;archivedResponseInteropJVM/test;archivedResponseInteropJS/test;datasetJVM/test;datasetJS/test;modelJVM/test;modelJS/test;fitJVM/test;fitJS/test;firstLevelLawsJVM/test;firstLevelLawsJS/test;mvpaJVM/test;mvpaJS/test;connectivityJVM/test;connectivityJS/test;groupJVM/test;groupJS/test;fmriWorkflowJVM/test;fmriWorkflowJS/test;archiveZarrJVM/test;archiveZarrJS/test;datasetZarrJVM/test;datasetZarrJS/test")
addCommandAlias("examplesCompile", ";surfaceExamplesJVM/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;atlasExamplesJVM/compile;workflowExamplesJVM/compile")
addCommandAlias("examplesTest", ";surfaceExamplesJVM/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;atlasExamplesJVM/test;workflowExamplesJVM/test")
addCommandAlias("surfaceViewConformance", ";surfaceJVM/test;surfaceJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test")
addCommandAlias("surfaceViewAdmissionJVM", ";surfaceViewRasterJVM/runMain scalafim.surface.view.raster.SurfaceRasterAdmissionBenchmark;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceAdmissionBenchmark")
addCommandAlias("surfaceViewVisualQaJVM", ";surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxGiftiParityProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceCorrectnessProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceInteractionProbe")
addCommandAlias("atlasCoverage", ";set atlasJVM / coverageEnabled := true;atlasJVM/test;atlasJVM/coverageReport;set atlasJVM / coverageEnabled := false")
