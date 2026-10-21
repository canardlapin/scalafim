import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*



ThisBuild / organization := "scalafim"
ThisBuild / scalaVersion := "3.7.4"
ThisBuild / version      := "0.1.0-SNAPSHOT"

// Ravel is the single dense-array substrate shared by response and image
// semantics. The local override coordinates cross-repository development;
// ordinary builds remain pinned to an immutable source revision.
lazy val ravelRevision = "195d71027423d5bda1c53ef1baa17e07670fe290"
lazy val ravelBuild =
  sys.props
    .get("scalafim.ravel.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/ravel.git#$ravelRevision"))
lazy val ravelCoreJVM = ProjectRef(ravelBuild, "coreJVM")
lazy val ravelCoreJS  = ProjectRef(ravelBuild, "coreJS")

// Immutable source dependency: sbt clones this exact Gale commit into its
// staging area. Multivar also needs its revision-tagged artifact; prepare it
// with tools/prepare-pinned-dependencies.sh. `scalafim.gale.build` is the explicit
// sibling-checkout override for coordinated upstream development (numerical
// capabilities such as banded factors are written in Gale, then pinned here).
lazy val galeRevision = "779ed3d8169cdd9f4eb9ab10b1ca9defadc27715"
lazy val galeBuild =
  sys.props
    .get("scalafim.gale.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/gale.git#$galeRevision"))
lazy val galeCoreJVM = ProjectRef(galeBuild, "coreJVM")
lazy val galeCoreJS  = ProjectRef(galeBuild, "coreJS")

// Resample4s owns typed reindexing and resampling designs. The source pin is
// also consumed by Alder's admitted integration module below.
lazy val resample4sRevision =
  "6bc4172a966c92f1b06811eac64ac2bada9fef9b"
lazy val resample4sBuild =
  sys.props
    .get("scalafim.resample4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(
      uri(
        s"https://github.com/canardlapin/resample4s.git#$resample4sRevision"
      )
    )
lazy val resample4sCoreJVM    = ProjectRef(resample4sBuild, "coreJVM")
lazy val resample4sCoreJS     = ProjectRef(resample4sBuild, "coreJS")
lazy val resample4sDesignsJVM = ProjectRef(resample4sBuild, "designsJVM")
lazy val resample4sDesignsJS  = ProjectRef(resample4sBuild, "designsJS")

// Alder owns fit lifecycles, stage-aware cross-fitting, and fitted artifacts.
// Keep its coordinated provider overrides explicit and process-local.
lazy val alderRevision = "7618c715002b3912f856c4d44468ae173fce4dec"
lazy val alderBuild = {
  sys.props
    .get("scalafim.resample4s.build")
    .foreach(System.setProperty("alder.resample4s.build", _))
  sys.props
    .get("scalafim.gale.build")
    .foreach(System.setProperty("alder.gale.build", _))
  sys.props
    .get("scalafim.linop4s.build")
    .foreach(System.setProperty("alder.linop4s.build", _))
  sys.props
    .get("scalafim.alder.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/alder.git#$alderRevision"))
}
lazy val alderKernelJVM = ProjectRef(alderBuild, "kernelJVM")
lazy val alderKernelJS  = ProjectRef(alderBuild, "kernelJS")
lazy val alderDataJVM   = ProjectRef(alderBuild, "dataJVM")
lazy val alderDataJS    = ProjectRef(alderBuild, "dataJS")

// locus4s is independently owned. Ordinary builds clone the exact reviewed
// revision; the property is an explicit sibling-checkout override for
// coordinated development.
lazy val locus4sRevision = "e32b0aeba5c4d19c457b2329272b95b26c061943"
lazy val locus4sBuild =
  sys.props
    .get("scalafim.locus4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/locus4s.git#$locus4sRevision"))
lazy val locus4sCoreJVM = ProjectRef(locus4sBuild, "locus4s-coreJVM")
lazy val locus4sCoreJS  = ProjectRef(locus4sBuild, "locus4s-coreJS")
lazy val locus4sDataJVM = ProjectRef(locus4sBuild, "locus4s-dataJVM")
lazy val locus4sDataJS  = ProjectRef(locus4sBuild, "locus4s-dataJS")
lazy val locus4sProvenanceJVM = ProjectRef(locus4sBuild, "locus4s-provenanceJVM")
lazy val locus4sProvenanceJS  = ProjectRef(locus4sBuild, "locus4s-provenanceJS")

// mvpa4s owns identified MVPA evidence and analyses. Ordinary builds clone the
// exact reviewed revision. Coordinated overrides are forwarded so mvpa4s
// resolves the same provider builds as ScalaFIM.
lazy val mvpa4sRevision = "b8259ab7eb00b4ad48cf26e3f5da49f398eae58a"
lazy val mvpa4sBuild = {
  Seq("alder", "gale", "locus4s", "multivar", "resample4s").foreach { provider =>
    sys.props
      .get(s"scalafim.$provider.build")
      .foreach(System.setProperty(s"mvpa4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.mvpa4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/mvpa4s.git#$mvpa4sRevision"))
}
lazy val mvpa4sCoreJVM    = ProjectRef(mvpa4sBuild, "coreJVM")
lazy val mvpa4sCoreJS     = ProjectRef(mvpa4sBuild, "coreJS")
lazy val mvpa4sTestkitJVM = ProjectRef(mvpa4sBuild, "testkitJVM")
lazy val mvpa4sTestkitJS  = ProjectRef(mvpa4sBuild, "testkitJS")
lazy val mvpa4sPredictiveJVM = ProjectRef(mvpa4sBuild, "predictiveJVM")
lazy val mvpa4sPredictiveJS  = ProjectRef(mvpa4sBuild, "predictiveJS")

// image4s is independently owned. Ordinary builds use its immutable source
// revision; coordinated development can select a sibling checkout explicitly.
lazy val image4sRevision = "aef5e223169f0831c942694c4b109ab20d152217"
lazy val image4sBuild = {
  Seq("locus4s", "spatial4s").foreach { provider =>
    sys.props
      .get(s"scalafim.$provider.build")
      .foreach(System.setProperty(s"image4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.image4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/image4s.git#$image4sRevision"))
}
lazy val image4sCoreJVM = ProjectRef(image4sBuild, "image4s-coreJVM")
lazy val image4sCoreJS  = ProjectRef(image4sBuild, "image4s-coreJS")
lazy val image4sGeometryJVM = ProjectRef(image4sBuild, "image4s-geometryJVM")
lazy val image4sGeometryJS  = ProjectRef(image4sBuild, "image4s-geometryJS")
lazy val image4sLocusJVM = ProjectRef(image4sBuild, "image4s-locusJVM")
lazy val image4sLocusJS  = ProjectRef(image4sBuild, "image4s-locusJS")
lazy val image4sFilterJVM = ProjectRef(image4sBuild, "image4s-filterJVM")
lazy val image4sFilterJS  = ProjectRef(image4sBuild, "image4s-filterJS")
lazy val image4sNiftiJVM  = ProjectRef(image4sBuild, "image4s-niftiJVM")

// reframe4s owns generic spatial maps and resampling execution. ScalaFIM
// retains neuroimaging policy and delegates affine kernels to this exact
// reviewed source revision. A ScalaFIM image4s checkout override is forwarded
// so reframe4s resolves the same image4s build; reframe4s pins the same image4s
// as ScalaFIM and the same spatial4s as image4s.
lazy val reframe4sRevision = "7ec7feff96d4057a86196b5e86d592688b7c7f84"
lazy val reframe4sBuild = {
  Seq("image4s", "spatial4s").foreach { provider =>
    sys.props
      .get(s"scalafim.$provider.build")
      .foreach(System.setProperty(s"reframe4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.reframe4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/reframe4s.git#$reframe4sRevision"))
}
lazy val reframe4sLieJVM      = ProjectRef(reframe4sBuild, "reframe4s-lieJVM")
lazy val reframe4sLieJS       = ProjectRef(reframe4sBuild, "reframe4s-lieJS")
lazy val reframe4sFieldJVM    = ProjectRef(reframe4sBuild, "reframe4s-fieldJVM")
lazy val reframe4sFieldJS     = ProjectRef(reframe4sBuild, "reframe4s-fieldJS")
lazy val reframe4sResampleJVM = ProjectRef(reframe4sBuild, "reframe4s-resampleJVM")
lazy val reframe4sResampleJS  = ProjectRef(reframe4sBuild, "reframe4s-resampleJS")

// neuroim4s-spaces owns the anatomical world identities (template, subject
// scanner-RAS and tkRAS, declared and unresolved worlds) and their frames. It
// pins the same spatial4s revision as image4s and reframe4s.
lazy val neuroim4sRevision = "6d20f52e6701f10126f36a4fc97bf97eeb99fa24"
lazy val neuroim4sBuild = {
  Seq("spatial4s", "image4s", "gale", "ravel", "locus4s", "reframe4s", "graph4s").foreach { provider =>
    sys.props
      .get(s"scalafim.$provider.build")
      .foreach(System.setProperty(s"neuroim4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.neuroim4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/neuroim4s.git#$neuroim4sRevision"))
}
lazy val neuroim4sSpacesJVM = ProjectRef(neuroim4sBuild, "spacesJVM")
lazy val neuroim4sSpacesJS  = ProjectRef(neuroim4sBuild, "spacesJS")
lazy val neuroim4sCoreJVM = ProjectRef(neuroim4sBuild, "coreJVM")
lazy val neuroim4sCoreJS  = ProjectRef(neuroim4sBuild, "coreJS")
lazy val neuroim4sIoJVM = ProjectRef(neuroim4sBuild, "ioJVM")
lazy val neuroim4sTransformJVM = ProjectRef(neuroim4sBuild, "transformJVM")
lazy val neuroim4sTransformJS = ProjectRef(neuroim4sBuild, "transformJS")
lazy val neuroim4sTransformIoJVM = ProjectRef(neuroim4sBuild, "transformIoJVM")
lazy val neuroim4sRoutingIoJVM = ProjectRef(neuroim4sBuild, "routingIoJVM")
lazy val neuroim4sRoutingJVM = ProjectRef(neuroim4sBuild, "routingJVM")
lazy val neuroim4sRoutingJS = ProjectRef(neuroim4sBuild, "routingJS")

// neuroatlas4s owns atlas metadata/publication in core and volume realizations,
// queries, reductions, template policy and loaders in optional volume/IO modules.
// Forward explicit overrides so each provider has one owner across the graph.
lazy val neuroatlas4sRevision = "f03287dd870bb65394914e69be0ecfcc4b9334d9"
lazy val neuroatlas4sBuild = {
  Seq("locus4s", "neuroim4s", "image4s", "spatial4s", "reframe4s", "gale", "graph4s").foreach { provider =>
    sys.props.get(s"scalafim.$provider.build").foreach(System.setProperty(s"neuroatlas4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.neuroatlas4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/neuroatlas4s.git#$neuroatlas4sRevision"))
}
lazy val neuroatlas4sCoreJVM = ProjectRef(neuroatlas4sBuild, "coreJVM")
lazy val neuroatlas4sCoreJS  = ProjectRef(neuroatlas4sBuild, "coreJS")

lazy val neuroatlas4sVolumeJVM = ProjectRef(neuroatlas4sBuild, "volumeJVM")
lazy val neuroatlas4sVolumeJS = ProjectRef(neuroatlas4sBuild, "volumeJS")
lazy val neuroatlas4sVolumeIoJVM = ProjectRef(neuroatlas4sBuild, "volumeIoJVM")

// mesh4s owns triangle topology, surface realizations and point fields. It
// pins the same locus4s, spatial4s and graph4s revisions as ScalaFIM's other
// providers; ScalaFIM overrides for those are forwarded.
lazy val mesh4sRevision = "5ed45de9ab39deb22865438d65abcd50f9f8eb4a"
lazy val mesh4sBuild = {
  Seq("locus4s", "spatial4s", "graph4s").foreach { provider =>
    sys.props
      .get(s"scalafim.$provider.build")
      .foreach(System.setProperty(s"mesh4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.mesh4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/mesh4s.git#$mesh4sRevision"))
}
lazy val mesh4sGeometryJVM = ProjectRef(mesh4sBuild, "mesh4s-geometryJVM")
lazy val mesh4sGeometryJS  = ProjectRef(mesh4sBuild, "mesh4s-geometryJS")
lazy val mesh4sGraph4sJVM = ProjectRef(mesh4sBuild, "mesh4s-graph4sJVM")
lazy val mesh4sGraph4sJS = ProjectRef(mesh4sBuild, "mesh4s-graph4sJS")

// graph4s is an independently owned topology and algorithms library. Ordinary
// builds clone the exact reviewed revision; the property is an explicit local
// source override for coordinated development.
lazy val graph4sRevision = "613371f3bd0b12ce99352db0c637645eb62c6283"
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
lazy val multivarRevision = "7afdeea290fdcc80b81a4126aa303f93a5348c0e"
lazy val multivarBuild = {
  // Inference must share the same resample4s classes as native MVPA adapters.
  System.setProperty("multivar.resample4s.build.uri", resample4sBuild.toString)
  sys.props
    .get("scalafim.multivar.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(
      uri(
        s"https://github.com/canardlapin/multivar.git#$multivarRevision"
      )
    )
}
lazy val multivarJVM   = ProjectRef(multivarBuild, "coreJVM")
lazy val multivarJS    = ProjectRef(multivarBuild, "coreJS")

// Renderer-neutral graphics and platform backends are developed independently.
// Ordinary builds clone the exact public revision; the system property is an
// explicit local-development override.
lazy val intaglioRevision = "d553c327824f9916b1c0a055226c6c5e8f77bc59"
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
    "-Xmax-inlines:64",
    // CI pins JDK 17; restrict the JDK API surface so newer-JDK calls
    // (e.g. Thread.threadId, JDK 19) fail locally on any newer host JDK.
    "-release:17"
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
    .settings(name := "scalafim-locus-data")
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
    .jvmConfigure(_.dependsOn(ravelCoreJVM, locus4sProvenanceJVM))
    .jsConfigure(_.dependsOn(ravelCoreJS, locus4sProvenanceJS))
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
    .dependsOn(response, responseLaws % "test->compile", locusData)
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
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

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

// Cross-built scenario verdict/policy core. It is test-support only: design,
// fit, spatial and surface depend on its main classes from test scope, so
// published production artifacts do not acquire MUnit or scenario-testkit
// dependencies.
lazy val scenarioTestkit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/scenario-testkit"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-scenario-testkit",
      publish / skip := true
    )
    .jvmSettings(libraryDependencies += "org.scalameta" %% "munit" % "1.2.1")
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
        "org.scalameta" %%% "munit-scalacheck" % "1.1.0" % Test
      )
    )
    .jvmSettings(scientificCoverageSettings(statementMinimum = 70.0, branchMinimum = 60.0))
    .jsSettings(jsSettingsBase)

lazy val hrfJS  = hrf.js
lazy val hrfJVM = hrf.jvm

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
        "com.lihaoyi" %%% "upickle" % "4.1.0"
      )
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, intaglioCoreJVM, intaglioSvgJVM % "test->compile"))
    .jvmSettings(scientificCoverageSettings(statementMinimum = 70.0, branchMinimum = 58.0))
    .jsConfigure(_.dependsOn(galeCoreJS, intaglioCoreJS))
    .jsSettings(jsSettingsBase)

lazy val designJS  = design.js
lazy val designJVM = design.jvm

// Unpublished cross-platform compile contract proving that ScalaFIM consumers
// can compose the admitted provider APIs directly. Main sources live outside
// package scalafim and define no adapter algebra. Strict test-only toolkit scenario
// courts compose neuroim4s with the existing ScenarioHarness; neither depends on ScalaFIM upstream.
lazy val providerSpike =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/provider-spike"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-provider-spike",
      publish / skip := true,
      Test / scalacOptions ++= Seq("-Werror", "-Wunused:all", "-Wvalue-discard")
    )
    .jvmConfigure(
      _.dependsOn(
        ravelCoreJVM,
        galeCoreJVM,
        locus4sCoreJVM,
        image4sCoreJVM,
        image4sGeometryJVM,
        image4sLocusJVM,
        reframe4sLieJVM,
        reframe4sResampleJVM
      )
    )
    .jsConfigure(
      _.dependsOn(
        ravelCoreJS,
        galeCoreJS,
        locus4sCoreJS,
        image4sCoreJS,
        image4sGeometryJS,
        image4sLocusJS,
        reframe4sLieJS,
        reframe4sResampleJS
      )
    )
    .jsSettings(jsSettingsBase)

    .jvmConfigure(_.dependsOn(
      neuroim4sTransformJVM % "test->compile;test->test",
      neuroim4sTransformIoJVM % "test->compile",
      scenarioTestkit.jvm % "test->compile"
    ))
    .jsConfigure(_.dependsOn(
      neuroim4sTransformJS % "test->compile;test->test",
      scenarioTestkit.js % "test->compile"
    ))

lazy val providerSpikeJS  = providerSpike.js
lazy val providerSpikeJVM = providerSpike.jvm

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
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .jsConfigure(_.dependsOn(intaglioCoreJS))
    .settings(commonSettings)
    .settings(
      name := "scalafim-image-view"
    )
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

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
    .dependsOn(scenarioTestkit % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-threshold"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

lazy val thresholdJS  = threshold.js
lazy val thresholdJVM = threshold.jvm

lazy val motion =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/motion"))

    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-motion"
    )
    .jvmConfigure(_.dependsOn(image4sGeometryJVM))
    .jsConfigure(_.dependsOn(image4sGeometryJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

lazy val motionJS  = motion.js
lazy val motionJVM = motion.jvm.dependsOn(bids4sJVM)

lazy val surface =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface"))
    .dependsOn(locusData, scenarioTestkit % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface",
      libraryDependencies ++= Seq(
        "com.lihaoyi" %%% "upickle" % "4.1.0",
        "org.scala-lang.modules" %%% "scala-xml" % "2.4.0"
      )
    )
    .jvmConfigure(_.dependsOn(mesh4sGeometryJVM, mesh4sGraph4sJVM, image4sGeometryJVM, graph4sAlgorithmsJVM, zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(mesh4sGeometryJS, mesh4sGraph4sJS, image4sGeometryJS, graph4sAlgorithmsJS, zarr4sCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))
    .jvmConfigure(_.dependsOn(neuroim4sTransformJVM))
    .jsConfigure(_.dependsOn(neuroim4sTransformJS))

lazy val surfaceJS  = surface.js
lazy val surfaceJVM = surface.jvm

// Unpublished JVM fixture support and its qualification courts. Tests that use these fixtures live here,
// keeping the project graph acyclic: fixtures -> surface, atlas Test -> fixtures Compile.
lazy val surfaceTestFixtures =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-test-fixtures"))
    .dependsOn(surface, scenarioTestkit)
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-test-fixtures",
      publish / skip := true
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceTestFixturesJS = surfaceTestFixtures.js
lazy val surfaceTestFixturesJVM = surfaceTestFixtures.jvm

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
    .dependsOn(surfaceView, surfaceViewRaster)
    .jvmConfigure(_.dependsOn(intaglioCoreJVM))
    .settings(commonSettings)
    .settings(
      name := "scalafim-surface-view-javafx",
      // Native JavaFX libraries cannot be reloaded across warm sbt test classloaders.
      Test / fork := true,
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
    .dependsOn(surfaceView, surfaceViewRaster)
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
      publish / skip := true,
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.4.0"
    )
    .jsSettings(jsSettingsBase)

lazy val surfaceViewExamplesJS = surfaceViewExamples.js.dependsOn(surfaceViewThreeJS)
lazy val surfaceViewExamplesJVM = surfaceViewExamples.jvm.dependsOn(intaglioJava2dJVM % "test->compile")
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
      publish / skip := true,
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0"
    )

lazy val spatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/spatial"))
    .dependsOn(surface, locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-spatial"
    )
    .jvmSettings(
      // HDF5 transform files are read by transform's jHDF container; tests silence jHDF's logging.
      libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.18" % Test
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))
    .jvmConfigure(_.dependsOn(neuroim4sTransformJVM, neuroim4sTransformIoJVM, neuroim4sRoutingJVM, neuroim4sRoutingIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sTransformJS, neuroim4sRoutingJS))

lazy val spatialJS  = spatial.js
lazy val spatialJVM = spatial.jvm

lazy val atlas =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/atlas"))
    .dependsOn(surface, locusData, spatial)
    .settings(commonSettings)
    .settings(
      name := "scalafim-atlas"
    )
    .jvmSettings(
      libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
      // Full-resolution MNI inversion retains large fields and residual arrays. Give this
      // native oracle a fresh heap on every gate, independent of the resident sbt server.
      Test / parallelExecution := false,
      Test / testForkedParallel := false,
      Test / testGrouping := {
        val (nativeOracle, ordinary) = (Test / definedTests).value.partition(
          _.name == "scalafim.atlas.io.MniTemplateBridgeFilesSuite"
        )
        val inherited = (Test / forkOptions).value
        val nativeOptions = inherited.withRunJVMOptions(
          inherited.runJVMOptions.filterNot(option => option.startsWith("-Xmx") || option.startsWith("-Xms")) ++
            Vector("-Xmx3g", "-XX:ActiveProcessorCount=2")
        )
        Seq(
          Tests.Group("atlas", ordinary, Tests.InProcess),
          Tests.Group("atlas-mni-native-oracle", nativeOracle, Tests.SubProcess(nativeOptions))
        ).filter(_.tests.nonEmpty)
      }
    )
    // Test-only: the real-asset fsLR fixtures (locked asset gate, SimpleITK inverse oracle, qualification inputs)
    // live in an unpublished fixture module; atlas drives the public standard-route entry point.
    .jvmConfigure(_.dependsOn(neuroatlas4sCoreJVM, neuroatlas4sVolumeJVM, neuroatlas4sVolumeIoJVM, image4sGeometryJVM, graph4sAlgorithmsJVM, surfaceTestFixturesJVM % "test->compile"))
    .jsConfigure(_.dependsOn(neuroatlas4sCoreJS, neuroatlas4sVolumeJS, image4sGeometryJS, graph4sAlgorithmsJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))
    .jvmConfigure(_.dependsOn(neuroim4sTransformJVM, neuroim4sTransformIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sTransformJS))

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
    .dependsOn(atlasJVM, mvpaSpatialJVM, mvpaDatasetJVM, fitJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-workflows-jvm",
      publish / skip := true
    )

// The in-memory atlas workflow is portable: run the same source and suite on JS.
lazy val workflowExamplesJS =
  project
    .enablePlugins(ScalaJSPlugin)
    .in(file("examples/workflows-js"))
    .dependsOn(atlasJS, mvpaSpatialJS, mvpaDatasetJS)
    .settings(commonSettings)
    .settings(jsSettingsBase)
    .settings(
      name := "scalafim-examples-workflows-js",
      publish / skip := true,
      Compile / unmanagedSources += baseDirectory.value.getParentFile / "workflows-jvm/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala",
      Test / unmanagedSources += baseDirectory.value.getParentFile / "workflows-jvm/src/test/scala/scalafim/examples/workflows/AtlasMvpaWorkflowSuite.scala"
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

lazy val estimates =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/estimates"))
    .dependsOn(archive)
    .settings(commonSettings)
    .settings(name := "scalafim-estimates")
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

lazy val estimatesJS = estimates.js
lazy val estimatesJVM = estimates.jvm

lazy val estimatesIo =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/estimates-io"))
    .dependsOn(estimates)
    .settings(commonSettings)
    .settings(
      name := "scalafim-estimates-io",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.1.0"
    )
    .jvmConfigure(_.dependsOn(image4sNiftiJVM))
    .jsSettings(jsSettingsBase)

lazy val estimatesIoJS = estimatesIo.js
lazy val estimatesIoJVM = estimatesIo.jvm

lazy val archiveLna =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/archive-lna"))
    .dependsOn(archive)
    .settings(commonSettings)
    .settings(
      name := "scalafim-archive-lna"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jvmSettings(
      libraryDependencies += "io.jhdf" % "jhdf" % jhdfVersion
    )
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

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
    .jvmConfigure(_.dependsOn(galeCoreJVM, bids4sJVM, zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS, zarr4sCoreJS))
    .jsSettings(jsSettingsBase)

lazy val archivedResponseInteropJS  = archivedResponseInterop.js
lazy val archivedResponseInteropJVM = archivedResponseInterop.jvm

lazy val dataset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/dataset"))
    .dependsOn(response, responseLaws % "test->compile", hrf, locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset",
      libraryDependencies ++= Seq(
      )
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

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

// Unpublished comparative evaluation: portable preparation/scoring, JVM custody and runners.
lazy val phrfComparison =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/phrf-comparison"))
    .dependsOn(fit)
    .settings(commonSettings)
    .settings(strictFirstLevelCompilerSettings)
    .settings(
      name := "scalafim-phrf-comparison",
      libraryDependencies += "com.lihaoyi" %%% "upickle" % "4.1.0",
      publish / skip := true
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)

lazy val phrfComparisonJS  = phrfComparison.js
lazy val phrfComparisonJVM = phrfComparison.jvm

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
    .dependsOn(fit, scenarioTestkit % "test->compile")
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

lazy val mvpaPipeline =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-pipeline"))
    .dependsOn(pipeline)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS))
    .settings(commonSettings)
    .settings(name := "scalafim-mvpa-pipeline")
    .jsSettings(jsSettingsBase)

lazy val mvpaPipelineJS  = mvpaPipeline.js
lazy val mvpaPipelineJVM = mvpaPipeline.jvm

lazy val mvpaFit =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-fit"))
    .dependsOn(fit)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-fit"
    )
    .jvmConfigure(
      _.dependsOn(
        galeCoreJVM,
        multivarJVM % "compile->compile;test->test"
      )
    )
    .jsConfigure(
      _.dependsOn(
        galeCoreJS,
        multivarJS % "compile->compile;test->test"
      )
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaFitJS  = mvpaFit.js
lazy val mvpaFitJVM = mvpaFit.jvm

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

lazy val mvpaDataset =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-dataset"))
    .dependsOn(mvpaFit, dataset)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM, mvpa4sTestkitJVM % Test))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS, mvpa4sTestkitJS % Test))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-dataset"
    )
    .jvmConfigure(_.dependsOn(mvpa4sPredictiveJVM, alderKernelJVM, alderDataJVM))
    .jsConfigure(_.dependsOn(mvpa4sPredictiveJS, alderKernelJS, alderDataJS))
    .jsSettings(jsSettingsBase)

lazy val mvpaDatasetJS  = mvpaDataset.js
lazy val mvpaDatasetJVM = mvpaDataset.jvm

// Fitter-independent bounded pattern profiles over estimate types and archive IO.
lazy val mvpaArtifacts =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-artifacts"))
    .dependsOn(estimates, archive)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-artifacts",
      libraryDependencies += "com.lihaoyi" %%% "ujson" % "4.1.0"
    )
    .jvmSettings(Test / fork := true)
    .jsSettings(jsSettingsBase)

lazy val mvpaArtifactsJS = mvpaArtifacts.js
lazy val mvpaArtifactsJVM = mvpaArtifacts.jvm

lazy val mvpaSpatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-spatial"))
    .dependsOn(surface, threshold)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM, mvpa4sTestkitJVM % Test, mvpa4sPredictiveJVM % "test->compile"))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS, mvpa4sTestkitJS % Test, mvpa4sPredictiveJS % "test->compile"))
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-mvpa-spatial"
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaSpatialJS  = mvpaSpatial.js
lazy val mvpaSpatialJVM = mvpaSpatial.jvm

/** Unpublished M0 admission court for the provider graph and typed-axis laws.
  * Production APIs begin in M1; this module prevents the admitted provider
  * closure from drifting while those laws migrate to their permanent homes.
  */
lazy val mvpaFoundationAdmission =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-foundation-spike"))
    .dependsOn(response, locusData)
    .settings(commonSettings)
    .settings(
      name := "scalafim-mvpa-foundation-admission",
      publish / skip := true,
      scalacOptions += "-Werror",
      libraryDependencies ++= Seq(
        "org.scalameta" %%% "munit"       % "1.3.4" % Test,
        "org.typelevel" %%% "cats-core"   % "2.12.0" % Test,
        "org.typelevel" %%% "cats-effect" % "3.5.4" % Test
      )
    )
    .jvmConfigure(
      _.dependsOn(
        multivarJVM,
        alderKernelJVM,
        alderDataJVM,
        resample4sCoreJVM,
        resample4sDesignsJVM
      )
    )
    .jsConfigure(
      _.dependsOn(
        multivarJS,
        alderKernelJS,
        alderDataJS,
        resample4sCoreJS,
        resample4sDesignsJS
      )
    )
    .jsSettings(jsSettingsBase)

lazy val mvpaFoundationAdmissionJS  = mvpaFoundationAdmission.js
lazy val mvpaFoundationAdmissionJVM = mvpaFoundationAdmission.jvm

lazy val group =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/group"))
    .dependsOn(dataset, design, estimates)
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-group"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

lazy val groupJS  = group.js
lazy val groupJVM = group.jvm


/** Consumer-owned subject uncertainty bridge; lower modules remain independent. */
lazy val mvpaGroup =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/mvpa-group"))
    .dependsOn(group)
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM, mvpa4sTestkitJVM % Test))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS, mvpa4sTestkitJS % Test))
    .settings(commonSettings)
    .settings(name := "scalafim-fmri-mvpa-group")
    .jsSettings(jsSettingsBase)

lazy val mvpaGroupJS  = mvpaGroup.js
lazy val mvpaGroupJVM = mvpaGroup.jvm

lazy val fmriWorkflow =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/fmri-workflow"))
    .dependsOn(fit, group % "compile->compile;test->test", estimatesIo % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-fmri-workflow"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, bids4sJVM))
    .jsConfigure(_.dependsOn(galeCoreJS, bids4sJS))
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
    .dependsOn(dataset, archiveZarr, fit % "test->compile")
    .settings(commonSettings)
    .settings(
      name := "scalafim-dataset-zarr"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, bids4sJVM, zarr4sCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS, bids4sJS, zarr4sCoreJS))
    .jsSettings(jsSettingsBase)
    .jvmConfigure(_.dependsOn(neuroim4sCoreJVM, neuroim4sIoJVM))
    .jsConfigure(_.dependsOn(neuroim4sCoreJS))

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
      scenarioTestkitJS,
      scenarioTestkitJVM,
      designJS,
      designJVM,
      providerSpikeJS,
      providerSpikeJVM,
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
      surfaceTestFixturesJVM,
      surfaceTestFixturesJS,
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
      estimatesJS,
      estimatesJVM,
      estimatesIoJS,
      estimatesIoJVM,
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
      phrfComparisonJS,
      phrfComparisonJVM,
      firstLevelLawsJS,
      firstLevelLawsJVM,
      mvpaPipelineJS,
      mvpaPipelineJVM,
      mvpaFitJS,
      mvpaFitJVM,
      connectivityJS,
      connectivityJVM,
      surfaceViewConnectivityJS,
      surfaceViewConnectivityJVM,
      mvpaDatasetJS,
      mvpaDatasetJVM,
      mvpaArtifactsJS,
      mvpaArtifactsJVM,
      mvpaSpatialJS,
      mvpaSpatialJVM,
      mvpaFoundationAdmissionJS,
      mvpaFoundationAdmissionJVM,
      surfaceExamplesJVM,
      atlasExamplesJVM,
      workflowExamplesJVM,
      workflowExamplesJS,
      groupJS,
      groupJVM,
      mvpaGroupJS,
      mvpaGroupJVM,
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

addCommandAlias("scalafimCompileAll", ";locusDataJVM/compile;locusDataJS/compile;pipelineJVM/compile;pipelineJS/compile;responseJVM/compile;responseJS/compile;responseLawsJVM/compile;responseLawsJS/compile;latentJVM/compile;latentJS/compile;arJVM/compile;arJS/compile;hrfJVM/compile;hrfJS/compile;scenarioTestkitJVM/compile;scenarioTestkitJS/compile;designJVM/compile;designJS/compile;providerSpikeJVM/compile;providerSpikeJS/compile;imageViewJVM/compile;imageViewJS/compile;imageViewCanvasJS/compile;imageViewJava2dJVM/compile;imageViewJavafxJVM/compile;thresholdJVM/compile;thresholdJS/compile;motionJVM/compile;motionJS/compile;surfaceJVM/compile;surfaceJS/compile;surfaceTestFixturesJVM/compile;surfaceTestFixturesJS/compile;surfaceViewJVM/compile;surfaceViewJS/compile;surfaceViewRasterJVM/compile;surfaceViewRasterJS/compile;surfaceViewJavafxJVM/compile;surfaceViewThreeJS/compile;surfaceViewConnectivityJVM/compile;surfaceViewConnectivityJS/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;spatialJVM/compile;spatialJS/compile;atlasJVM/compile;atlasJS/compile;archiveJVM/compile;archiveJS/compile;estimatesJVM/compile;estimatesJS/compile;estimatesIoJVM/compile;estimatesIoJS/compile;archiveLnaJVM/compile;archiveLnaJS/compile;archivedResponseInteropJVM/compile;archivedResponseInteropJS/compile;datasetJVM/compile;datasetJS/compile;modelJVM/compile;modelJS/compile;fitJVM/compile;fitJS/compile;phrfComparisonJVM/compile;phrfComparisonJS/compile;firstLevelLawsJVM/compile;firstLevelLawsJS/compile;mvpaPipelineJVM/compile;mvpaPipelineJS/compile;mvpaFitJVM/compile;mvpaFitJS/compile;connectivityJVM/compile;connectivityJS/compile;mvpaDatasetJVM/compile;mvpaDatasetJS/compile;mvpaArtifactsJVM/compile;mvpaArtifactsJS/compile;mvpaSpatialJVM/compile;mvpaSpatialJS/compile;mvpaFoundationAdmissionJVM/compile;mvpaFoundationAdmissionJS/compile;groupJVM/compile;groupJS/compile;mvpaGroupJVM/compile;mvpaGroupJS/compile;fmriWorkflowJVM/compile;fmriWorkflowJS/compile;archiveZarrJVM/compile;archiveZarrJS/compile;datasetZarrJVM/compile;datasetZarrJS/compile")
addCommandAlias("scalafimTestAll", ";locusDataJVM/test;locusDataJS/test;pipelineJVM/test;pipelineJS/test;responseJVM/test;responseJS/test;responseLawsJVM/test;responseLawsJS/test;latentJVM/test;latentJS/test;arJVM/test;arJS/test;hrfJVM/test;hrfJS/test;scenarioTestkitJVM/test;scenarioTestkitJS/test;designJVM/test;designJS/test;neuroimTestAll;neuroimTransformTestAll;neuroimRoutingTestAll;providerSpikeJVM/test;providerSpikeJS/test;imageViewJVM/test;imageViewJS/test;imageViewCanvasJS/test;imageViewJava2dJVM/test;imageViewJavafxJVM/test;thresholdJVM/test;thresholdJS/test;motionJVM/test;motionJS/test;surfaceJVM/test;surfaceJS/test;surfaceTestFixturesJVM/test;surfaceTestFixturesJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;spatialJVM/test;spatialJS/test;atlasJVM/test;atlasJS/test;archiveJVM/test;archiveJS/test;estimatesJVM/test;estimatesJS/test;estimatesIoJVM/test;estimatesIoJS/test;archiveLnaJVM/test;archiveLnaJS/test;archivedResponseInteropJVM/test;archivedResponseInteropJS/test;datasetJVM/test;datasetJS/test;modelJVM/test;modelJS/test;fitJVM/test;fitJS/test;phrfComparisonJVM/test;phrfComparisonJS/test;firstLevelLawsJVM/test;firstLevelLawsJS/test;mvpaPipelineJVM/test;mvpaPipelineJS/test;mvpaFitJVM/test;mvpaFitJS/test;connectivityJVM/test;connectivityJS/test;mvpaDatasetJVM/test;mvpaDatasetJS/test;mvpaArtifactsJVM/test;mvpaArtifactsJS/test;mvpaSpatialJVM/test;mvpaSpatialJS/test;mvpaFoundationAdmissionJVM/test;mvpaFoundationAdmissionJS/test;groupJVM/test;groupJS/test;mvpaGroupJVM/test;mvpaGroupJS/test;fmriWorkflowJVM/test;fmriWorkflowJS/test;archiveZarrJVM/test;archiveZarrJS/test;datasetZarrJVM/test;datasetZarrJS/test")
addCommandAlias("examplesCompile", ";surfaceExamplesJVM/compile;surfaceViewExamplesJVM/compile;surfaceViewExamplesJS/compile;atlasExamplesJVM/compile;workflowExamplesJVM/compile;workflowExamplesJS/compile")
addCommandAlias("examplesTest", ";surfaceExamplesJVM/test;surfaceViewExamplesJVM/test;surfaceViewExamplesJS/test;atlasExamplesJVM/test;workflowExamplesJVM/test;workflowExamplesJS/test")
addCommandAlias("surfaceViewConformance", ";surfaceJVM/test;surfaceJS/test;surfaceTestFixturesJVM/test;surfaceTestFixturesJS/test;surfaceViewJVM/test;surfaceViewJS/test;surfaceViewRasterJVM/test;surfaceViewRasterJS/test;surfaceViewJavafxJVM/test;surfaceViewThreeJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test")
addCommandAlias("surfaceViewAdmissionJVM", ";surfaceViewRasterJVM/runMain scalafim.surface.view.raster.SurfaceRasterAdmissionBenchmark;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceAdmissionBenchmark")
addCommandAlias("surfaceViewVisualQaJVM", ";surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxGiftiParityProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceCorrectnessProbe;surfaceViewJavafxJVM/Test/runMain scalafim.surface.view.javafx.JavaFxSurfaceInteractionProbe")
addCommandAlias("atlasCoverage", ";set atlasJVM / coverageEnabled := true;atlasJVM/test;atlasJVM/coverageReport;set atlasJVM / coverageEnabled := false")

addCommandAlias("neuroimTestAll", s";{${neuroim4sBuild}}spacesJVM/test;{${neuroim4sBuild}}spacesJS/test;{${neuroim4sBuild}}coreJVM/test;{${neuroim4sBuild}}coreJS/test;{${neuroim4sBuild}}ioJVM/test")

addCommandAlias("neuroimRoutingTestAll", s";{${neuroim4sBuild}}routingJVM/test;{${neuroim4sBuild}}routingJS/test;{${neuroim4sBuild}}routingIoJVM/test")

addCommandAlias("neuroimTransformTestAll", s";{${neuroim4sBuild}}transformJVM/test;{${neuroim4sBuild}}transformJS/test;{${neuroim4sBuild}}transformIoJVM/test")

addCommandAlias("neuroatlasVolumeTestAll", ";" + Seq("coreJVM", "coreJS", "volumeJVM", "volumeJS", "volumeIoJVM").map(p => s"{${neuroatlas4sBuild}}$p/test").mkString(";"))
