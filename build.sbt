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
lazy val mvpa4sRevision = "c27469484bf760c34cd896dabe4fe1c384a83b45"
lazy val mvpa4sBuild = {
  Seq("alder", "gale", "locus4s", "multivar", "resample4s", "neuroatlas4s", "neurosurf4s",
    "neuroim4s", "mesh4s", "image4s", "spatial4s", "reframe4s", "graph4s", "zarr4s", "ravel", "intaglio").foreach { provider =>
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
lazy val mvpa4sLocusJVM = ProjectRef(mvpa4sBuild, "locusJVM")
lazy val mvpa4sLocusJS = ProjectRef(mvpa4sBuild, "locusJS")
lazy val mvpa4sPredictiveJVM = ProjectRef(mvpa4sBuild, "predictiveJVM")
lazy val mvpa4sPredictiveJS  = ProjectRef(mvpa4sBuild, "predictiveJS")
lazy val mvpa4sSurfaceJVM = ProjectRef(mvpa4sBuild, "surfaceJVM")
lazy val mvpa4sSurfaceJS = ProjectRef(mvpa4sBuild, "surfaceJS")
lazy val mvpa4sAtlasJVM = ProjectRef(mvpa4sBuild, "atlasJVM")
lazy val mvpa4sAtlasJS = ProjectRef(mvpa4sBuild, "atlasJS")

// image4s is independently owned. Ordinary builds use its immutable source
// revision; coordinated development can select a sibling checkout explicitly.
lazy val image4sRevision = "855a3a61029f78e975b43cd1736de1c94180c004"
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
lazy val reframe4sRevision = "622ab31f97bbe30a9bc6436464f3124643856f47"
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
lazy val neuroim4sRevision = "4f75c2bf141449d032f9fe3c558ef13a22a4d59f"
lazy val neuroim4sBuild = {
  Seq("spatial4s", "image4s", "gale", "ravel", "locus4s", "reframe4s", "graph4s", "intaglio").foreach { provider =>
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
// Optional volume display belongs to neuroim4s; there are no local bridge artifacts.
lazy val neuroim4sViewJVM = ProjectRef(neuroim4sBuild, "viewJVM")
lazy val neuroim4sViewJS = ProjectRef(neuroim4sBuild, "viewJS")
lazy val neuroim4sViewCanvasJS = ProjectRef(neuroim4sBuild, "viewCanvasJS")
lazy val neuroim4sViewJava2dJVM = ProjectRef(neuroim4sBuild, "viewJava2dJVM")
lazy val neuroim4sViewJavafxJVM = ProjectRef(neuroim4sBuild, "viewJavafxJVM")

// neuroatlas4s owns catalog/publication, optional volume/surface realizations,
// loaders and mixed template route policy. ScalaFIM composes its public APIs.
// Forward explicit overrides so each provider has one owner across the graph.
lazy val neuroatlas4sRevision = "4df875d8a1cd20afd3b83ed49446422bc507e5ec"
lazy val neuroatlas4sBuild = {
  Seq("locus4s", "neuroim4s", "neurosurf4s", "mesh4s", "zarr4s", "image4s", "spatial4s", "reframe4s", "gale", "ravel", "graph4s", "intaglio").foreach { provider =>
    sys.props.get(s"scalafim.$provider.build").foreach(System.setProperty(s"neuroatlas4s.$provider.build", _))
  }
  sys.props
    .get("scalafim.neuroatlas4s.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/neuroatlas4s.git#$neuroatlas4sRevision"))
}

lazy val neuroatlas4sCoreJVM = ProjectRef(neuroatlas4sBuild, "coreJVM")
lazy val neuroatlas4sCoreJS = ProjectRef(neuroatlas4sBuild, "coreJS")
lazy val neuroatlas4sVolumeJS = ProjectRef(neuroatlas4sBuild, "volumeJS")
lazy val neuroatlas4sVolumeIoJVM = ProjectRef(neuroatlas4sBuild, "volumeIoJVM")

// Surface anatomy, formats, scenes and backends have one independent provider.
lazy val neurosurf4sRevision = "73a244fbe5d7b4db2f0c946d98fa2a3c00101ab7"
lazy val neurosurf4sBuild = {
  Seq("spatial4s", "locus4s", "mesh4s", "image4s", "graph4s", "zarr4s", "neuroim4s", "reframe4s", "gale", "ravel", "intaglio").foreach { provider =>
    sys.props.get(s"scalafim.$provider.build").foreach(System.setProperty(s"neurosurf4s.$provider.build", _))
  }
  sys.props.get("scalafim.neurosurf4s.build").map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/neurosurf4s.git#$neurosurf4sRevision"))
}
lazy val neurosurf4sCoreJVM = ProjectRef(neurosurf4sBuild, "coreJVM")
lazy val neurosurf4sCoreJS = ProjectRef(neurosurf4sBuild, "coreJS")
lazy val neurosurf4sIoJVM = ProjectRef(neurosurf4sBuild, "ioJVM")
lazy val neurosurf4sIoJS = ProjectRef(neurosurf4sBuild, "ioJS")
lazy val neurosurf4sSpatialJVM = ProjectRef(neurosurf4sBuild, "spatialJVM")
lazy val neurosurf4sSpatialJS = ProjectRef(neurosurf4sBuild, "spatialJS")
lazy val neurosurf4sSpatialIoJVM = ProjectRef(neurosurf4sBuild, "spatialIoJVM")
lazy val neurosurf4sVolumeJVM = ProjectRef(neurosurf4sBuild, "volumeJVM")
lazy val neurosurf4sVolumeJS = ProjectRef(neurosurf4sBuild, "volumeJS")
lazy val neurosurf4sVolumeIoJVM = ProjectRef(neurosurf4sBuild, "volumeIoJVM")
lazy val neurosurf4sRoutingJVM = ProjectRef(neurosurf4sBuild, "routingJVM")
lazy val neurosurf4sRoutingJS = ProjectRef(neurosurf4sBuild, "routingJS")
lazy val neurosurf4sRoutingIoJVM = ProjectRef(neurosurf4sBuild, "routingIoJVM")
lazy val neurosurf4sFixturesJVM = ProjectRef(neurosurf4sBuild, "fixturesJVM")
lazy val neurosurf4sFixturesJS = ProjectRef(neurosurf4sBuild, "fixturesJS")
lazy val neurosurf4sViewJVM = ProjectRef(neurosurf4sBuild, "viewJVM")
lazy val neurosurf4sViewJS = ProjectRef(neurosurf4sBuild, "viewJS")

// mesh4s owns triangle topology, surface realizations and point fields. It
// pins the same locus4s, spatial4s and graph4s revisions as ScalaFIM's other
// providers; ScalaFIM overrides for those are forwarded.
lazy val mesh4sRevision = "fd8b93ea42879ec5414342a084f05b831a4bbfc7"
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
  sys.props
    .get("scalafim.intaglio.build")
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"https://github.com/canardlapin/intaglio.git#$intaglioRevision"))
lazy val intaglioCoreJVM   = ProjectRef(intaglioBuild, "coreJVM")
lazy val intaglioCoreJS    = ProjectRef(intaglioBuild, "coreJS")
lazy val intaglioSvgJVM    = ProjectRef(intaglioBuild, "svgJVM")
lazy val intaglioCanvasJS  = ProjectRef(intaglioBuild, "canvasJS")
lazy val intaglioJava2dJVM = ProjectRef(intaglioBuild, "java2dJVM")
lazy val intaglioJavafxJVM = ProjectRef(intaglioBuild, "javafxJVM")

// Reusable BIDS semantics are developed independently. Ordinary builds clone
// the exact reviewed revision; the property is an explicit local-development
// override for downstream rehearsal.
lazy val bids4sRevision = "5dafc800499b2a4642a19ff47fa5924516b2c024"
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
    .dependsOn(response, responseLaws % "test->compile")
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

    .jvmConfigure(_.dependsOn(locus4sCoreJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS))

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

    .jvmConfigure(_.dependsOn(locus4sDataJVM % Test, multivarJVM % Test, alderKernelJVM % Test, alderDataJVM % Test,
      resample4sCoreJVM % Test, resample4sDesignsJVM % Test))
    .jsConfigure(_.dependsOn(locus4sDataJS % Test, multivarJS % Test, alderKernelJS % Test, alderDataJS % Test,
      resample4sCoreJS % Test, resample4sDesignsJS % Test))
    .settings(libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0" % Test)

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

lazy val spatial =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/spatial"))
    .settings(commonSettings)
    .settings(name := "scalafim-spatial")
    .jvmConfigure(_.dependsOn(neurosurf4sRoutingJVM % "compile->compile;test->test", neuroim4sRoutingIoJVM,
      neurosurf4sRoutingIoJVM % "test->compile"))
    .jsConfigure(_.dependsOn(neurosurf4sRoutingJS % "compile->compile;test->test"))
    .jsSettings(jsSettingsBase)

lazy val spatialJS  = spatial.js
lazy val spatialJVM = spatial.jvm

lazy val atlasExamplesJVM =
  project
    .in(file("examples/atlas-jvm"))
    .dependsOn(neuroatlas4sVolumeIoJVM, mvpaSpatialJVM)
    .settings(commonSettings)
    .settings(
      name := "scalafim-examples-atlas-jvm",
      publish / skip := true
    )

lazy val workflowExamplesJVM =
  project
    .in(file("examples/workflows-jvm"))
    .dependsOn(neuroatlas4sVolumeIoJVM, mvpa4sAtlasJVM, mvpaSpatialJVM, mvpaDatasetJVM, fitJVM)
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
    .dependsOn(neuroatlas4sVolumeJS, mvpa4sAtlasJS, mvpaSpatialJS, mvpaDatasetJS)
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
    .dependsOn(response, responseLaws % "test->compile", hrf)
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

    .jvmConfigure(_.dependsOn(locus4sCoreJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS))

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
    .dependsOn(fit, scenarioTestkit % "test->compile")
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
    .settings(commonSettings)
    .settings(
      name := "scalafim-connectivity"
    )
    .jvmConfigure(_.dependsOn(galeCoreJVM, graph4sCoreJVM))
    .jsConfigure(_.dependsOn(galeCoreJS, graph4sCoreJS))
    .jsSettings(jsSettingsBase)

    .jvmConfigure(_.dependsOn(locus4sCoreJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS))

lazy val connectivityJS  = connectivity.js
lazy val connectivityJVM = connectivity.jvm

lazy val surfaceViewConnectivity =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("modules/surface-view-connectivity"))
    .dependsOn(connectivity)
    .jvmConfigure(_.dependsOn(neurosurf4sViewJVM))
    .jsConfigure(_.dependsOn(neurosurf4sViewJS))
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
    .dependsOn(mvpaFit, dataset, scenarioTestkit % "test->compile",
      mvpaGroup % "test->compile;test->test", mvpaArtifacts % "test->compile")
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
    .dependsOn(estimates, archive, scenarioTestkit % "test->compile")
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
    .dependsOn(threshold)
    .jvmConfigure(_.dependsOn(neurosurf4sCoreJVM % Test))
    .jsConfigure(_.dependsOn(neurosurf4sCoreJS % Test))
    .jvmConfigure(_.dependsOn(mvpa4sCoreJVM, mvpa4sLocusJVM % Test, mvpa4sTestkitJVM % Test, mvpa4sPredictiveJVM % "test->compile"))
    .jsConfigure(_.dependsOn(mvpa4sCoreJS, mvpa4sLocusJS % Test, mvpa4sTestkitJS % Test, mvpa4sPredictiveJS % "test->compile"))
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
      neuroim4sViewJS,
      neuroim4sViewJVM,
      neuroim4sViewCanvasJS,
      neuroim4sViewJava2dJVM,
      neuroim4sViewJavafxJVM,
      thresholdJS,
      thresholdJVM,
      motionJS,
      motionJVM,
      spatialJS,
      spatialJVM,
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

// Resolve the real transitive Compile closures on both platforms; source-import
// checks alone cannot detect an accidental runtime, rendering or Test backedge.
lazy val ecosystemBoundaries = taskKey[Unit]("Check ecosystem ownership and resolved provider Compile boundaries")
ecosystemBoundaries / aggregate := false
ecosystemBoundaries := {
  val log = streams.value.log
  val repo = baseDirectory.value
  val output = target.value / "ecosystem-boundaries"
  val closures = Seq(
    "mvpa-predictive-JVM" -> (mvpa4sPredictiveJVM / Compile / fullClasspath).value,
    "mvpa-predictive-JS" -> (mvpa4sPredictiveJS / Compile / fullClasspath).value,
    "mvpa-core-JVM" -> (mvpa4sCoreJVM / Compile / fullClasspath).value,
    "mvpa-core-JS" -> (mvpa4sCoreJS / Compile / fullClasspath).value,
    "mvpa-locus-JVM" -> (mvpa4sLocusJVM / Compile / fullClasspath).value,
    "mvpa-locus-JS" -> (mvpa4sLocusJS / Compile / fullClasspath).value,
    "mvpa-atlas-JVM" -> (mvpa4sAtlasJVM / Compile / fullClasspath).value,
    "mvpa-atlas-JS" -> (mvpa4sAtlasJS / Compile / fullClasspath).value,
    "mvpa-surface-JVM" -> (mvpa4sSurfaceJVM / Compile / fullClasspath).value,
    "mvpa-surface-JS" -> (mvpa4sSurfaceJS / Compile / fullClasspath).value,
    "atlas-core-JVM" -> (neuroatlas4sCoreJVM / Compile / fullClasspath).value,
    "atlas-core-JS" -> (neuroatlas4sCoreJS / Compile / fullClasspath).value,
    "surface-core-JVM" -> (neurosurf4sCoreJVM / Compile / fullClasspath).value,
    "surface-core-JS" -> (neurosurf4sCoreJS / Compile / fullClasspath).value,
    "volume-core-JVM" -> (neuroim4sCoreJVM / Compile / fullClasspath).value,
    "volume-core-JS" -> (neuroim4sCoreJS / Compile / fullClasspath).value,
    "spaces-JVM" -> (neuroim4sSpacesJVM / Compile / fullClasspath).value,
    "spaces-JS" -> (neuroim4sSpacesJS / Compile / fullClasspath).value,
    "locus-core-JVM" -> (locus4sCoreJVM / Compile / fullClasspath).value,
    "locus-core-JS" -> (locus4sCoreJS / Compile / fullClasspath).value,
    "locus-data-JVM" -> (locus4sDataJVM / Compile / fullClasspath).value,
    "locus-data-JS" -> (locus4sDataJS / Compile / fullClasspath).value,
    "locus-provenance-JVM" -> (locus4sProvenanceJVM / Compile / fullClasspath).value,
    "locus-provenance-JS" -> (locus4sProvenanceJS / Compile / fullClasspath).value
  )
  val extracted = Project.extract(state.value)
  val structure = extracted.structure
  val owners = Map(structure.root -> "scalafim", mvpa4sBuild -> "mvpa4s",
    neuroim4sBuild -> "neuroim4s", neurosurf4sBuild -> "neurosurf4s",
    neuroatlas4sBuild -> "neuroatlas4s", image4sBuild -> "image4s",
    reframe4sBuild -> "reframe4s",
    mesh4sBuild -> "mesh4s", intaglioBuild -> "intaglio") ++
    sys.props.get("scalafim.spatial4s.build").map(path => file(path).getCanonicalFile.toURI -> "spatial4s").toSeq
  val builds = structure.units.keys.toSeq.map(_.toString).sorted
  val projects = structure.allProjectRefs.map { ref =>
    val owner = owners.getOrElse(ref.build,
      if (extracted.get(ref / organization) == "scalafim") "scalafim"
      else """https://github.com/canardlapin/([^/]+)\.git#[0-9a-f]{40}""".r
        .findFirstMatchIn(ref.build.toString).map(_.group(1)).getOrElse(""))
    Seq("@project", owner, ref.project, extracted.get(ref / baseDirectory).getCanonicalPath).mkString("\t")
  }
  val artifacts = closures.flatMap { case (_, paths) =>
    paths.flatMap { entry =>
      entry.get(moduleID.key).map { module =>
        Seq("@artifact", entry.data.getCanonicalPath, module.organization, module.name).mkString("\t")
      }
    }
  }.distinct
  val rows = builds.map("@build\t" + _) ++ projects ++ artifacts ++ closures.flatMap { case (label, paths) =>
    paths.map(entry => label + "\t" + entry.data.getCanonicalPath)
  }
  IO.createDirectory(output)
  val manifest = output / "compile-classpaths.tsv"
  IO.writeLines(manifest, rows)
  val command = Seq("python3", (repo / "tools/ci/check_ecosystem_boundaries.py").getPath,
    "--root", repo.getPath, "--classpath-manifest", manifest.getPath,
    "--receipt", (output / "receipt.json").getPath)
  val exit = scala.sys.process.Process(command, repo).!(scala.sys.process.ProcessLogger(
    line => log.info(line), line => log.error(line)))
  if (exit != 0) sys.error("Ecosystem ownership or Compile boundary check failed")
}

addCommandAlias("scalafimCompileAll", ";ecosystemBoundaries;mvpa4sImagingCompileAll;neuroatlasCompileAll;neurosurfCompileAll;neuroimViewCompileAll;pipelineJVM/compile;pipelineJS/compile;responseJVM/compile;responseJS/compile;responseLawsJVM/compile;responseLawsJS/compile;latentJVM/compile;latentJS/compile;arJVM/compile;arJS/compile;hrfJVM/compile;hrfJS/compile;scenarioTestkitJVM/compile;scenarioTestkitJS/compile;designJVM/compile;designJS/compile;providerSpikeJVM/compile;providerSpikeJS/compile;thresholdJVM/compile;thresholdJS/compile;motionJVM/compile;motionJS/compile;surfaceViewConnectivityJVM/compile;surfaceViewConnectivityJS/compile;spatialJVM/compile;spatialJS/compile;archiveJVM/compile;archiveJS/compile;estimatesJVM/compile;estimatesJS/compile;estimatesIoJVM/compile;estimatesIoJS/compile;archiveLnaJVM/compile;archiveLnaJS/compile;archivedResponseInteropJVM/compile;archivedResponseInteropJS/compile;datasetJVM/compile;datasetJS/compile;modelJVM/compile;modelJS/compile;fitJVM/compile;fitJS/compile;phrfComparisonJVM/compile;phrfComparisonJS/compile;firstLevelLawsJVM/compile;firstLevelLawsJS/compile;mvpaPipelineJVM/compile;mvpaPipelineJS/compile;mvpaFitJVM/compile;mvpaFitJS/compile;connectivityJVM/compile;connectivityJS/compile;mvpaDatasetJVM/compile;mvpaDatasetJS/compile;mvpaArtifactsJVM/compile;mvpaArtifactsJS/compile;mvpaSpatialJVM/compile;mvpaSpatialJS/compile;groupJVM/compile;groupJS/compile;mvpaGroupJVM/compile;mvpaGroupJS/compile;fmriWorkflowJVM/compile;fmriWorkflowJS/compile;archiveZarrJVM/compile;archiveZarrJS/compile;datasetZarrJVM/compile;datasetZarrJS/compile")
addCommandAlias("scalafimTestAll", ";neurosurfCoreTestAll;neurosurfSpatialTestAll;neurosurfFixtureTestAll;neurosurfViewJvmTestAll;neurosurfViewJsTestAll;neurosurfBrowserTestAll;neurosurfExamplesJvmTestAll;neurosurfNativeTestAll;mvpa4sCoreTestAll;mvpa4sPredictiveTestAll;mvpa4sLocusTestAll;mvpa4sImagingTestAll;pipelineJVM/test;pipelineJS/test;responseJVM/test;responseJS/test;responseLawsJVM/test;responseLawsJS/test;latentJVM/test;latentJS/test;arJVM/test;arJS/test;hrfJVM/test;hrfJS/test;scenarioTestkitJVM/test;scenarioTestkitJS/test;designJVM/test;designJS/test;neuroimTestAll;neuroimTransformTestAll;neuroimRoutingTestAll;neuroatlasCoreTestAll;neuroatlasVolumeTestAll;neuroatlasSurfaceTestAll;neuroatlasRoutingTestAll;neuroimViewJvmTestAll;neuroimViewJsTestAll;providerSpikeJVM/test;providerSpikeJS/test;thresholdJVM/test;thresholdJS/test;motionJVM/test;motionJS/test;surfaceViewConnectivityJVM/test;surfaceViewConnectivityJS/test;spatialJVM/test;spatialJS/test;archiveJVM/test;archiveJS/test;estimatesJVM/test;estimatesJS/test;estimatesIoJVM/test;estimatesIoJS/test;archiveLnaJVM/test;archiveLnaJS/test;archivedResponseInteropJVM/test;archivedResponseInteropJS/test;datasetJVM/test;datasetJS/test;modelJVM/test;modelJS/test;fitJVM/test;fitJS/test;phrfComparisonJVM/test;phrfComparisonJS/test;firstLevelLawsJVM/test;firstLevelLawsJS/test;mvpaPipelineJVM/test;mvpaPipelineJS/test;mvpaFitJVM/test;mvpaFitJS/test;connectivityJVM/test;connectivityJS/test;mvpaDatasetJVM/test;mvpaDatasetJS/test;mvpaArtifactsJVM/test;mvpaArtifactsJS/test;mvpaSpatialJVM/test;mvpaSpatialJS/test;groupJVM/test;groupJS/test;mvpaGroupJVM/test;mvpaGroupJS/test;fmriWorkflowJVM/test;fmriWorkflowJS/test;archiveZarrJVM/test;archiveZarrJS/test;datasetZarrJVM/test;datasetZarrJS/test")
addCommandAlias("examplesCompile", ";atlasExamplesJVM/compile;workflowExamplesJVM/compile;workflowExamplesJS/compile")
addCommandAlias("examplesTest", ";atlasExamplesJVM/test;workflowExamplesJVM/test;workflowExamplesJS/test")

addCommandAlias("neuroimTestAll", s";{${neuroim4sBuild}}spacesJVM/test;{${neuroim4sBuild}}spacesJS/test;{${neuroim4sBuild}}coreJVM/test;{${neuroim4sBuild}}coreJS/test;{${neuroim4sBuild}}ioJVM/test")

addCommandAlias("neuroimRoutingTestAll", s";{${neuroim4sBuild}}routingJVM/test;{${neuroim4sBuild}}routingJS/test;{${neuroim4sBuild}}routingIoJVM/test")

addCommandAlias("neuroimTransformTestAll", s";{${neuroim4sBuild}}transformJVM/test;{${neuroim4sBuild}}transformJS/test;{${neuroim4sBuild}}transformIoJVM/test")

addCommandAlias("neuroatlasVolumeTestAll", ";" + Seq("volumeJVM", "volumeJS", "volumeIoJVM").map(p => s"{${neuroatlas4sBuild}}$p/test").mkString(";"))

// Provider-qualified commands remain explicit in the bounded test inventory.
addCommandAlias("neuroimViewCompileAll", Seq("viewJVM", "viewJS", "viewCanvasJS", "viewJava2dJVM", "viewJavafxJVM")
  .flatMap(project => Seq(s"{${neuroim4sBuild}}$project/compile", s"{${neuroim4sBuild}}$project/Test/compile")).mkString(";", ";", ""))
addCommandAlias("neuroimViewJvmTestAll", Seq("viewJVM", "viewJava2dJVM", "viewJavafxJVM")
  .map(project => s"{${neuroim4sBuild}}$project/test").mkString(";", ";", ""))
addCommandAlias("neuroimViewJsTestAll", Seq("viewJS", "viewCanvasJS")
  .map(project => s"{${neuroim4sBuild}}$project/test").mkString(";", ";", ""))

// Optional providers retain their owning behavior tests after extraction.
addCommandAlias("mvpa4sCoreTestAll", ";" + Seq("coreJVM", "coreJS", "testkitJVM", "testkitJS").map(p => s"{${mvpa4sBuild}}$p/test").mkString(";"))
addCommandAlias("mvpa4sPredictiveTestAll", ";" + Seq("predictiveJVM", "predictiveJS", "consumerJVM", "consumerJS").map(p => s"{${mvpa4sBuild}}$p/test").mkString(";"))
addCommandAlias("mvpa4sLocusTestAll", s";{${mvpa4sBuild}}locusJVM/test;{${mvpa4sBuild}}locusJS/test")

addCommandAlias("neurosurfCoreTestAll", ";" + Seq("coreJVM", "coreJS", "ioJVM", "ioJS").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfSpatialTestAll", ";" + Seq("spatialJVM", "spatialJS", "spatialIoJVM", "volumeJVM", "volumeJS", "volumeIoJVM", "routingJVM", "routingJS", "routingIoJVM").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfFixtureTestAll", ";" + Seq("fixturesJVM", "fixturesJS").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfViewJvmTestAll", ";" + Seq("viewJVM", "viewLinkJVM", "viewVolumeJVM", "viewRasterJVM").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfViewJsTestAll", ";" + Seq("viewJS", "viewLinkJS", "viewVolumeJS", "viewRasterJS").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfBrowserTestAll", ";" + Seq("viewThreeJS", "viewThreeVolumeJS", "viewExamplesJS").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfExamplesJvmTestAll", ";" + Seq("viewExamplesJVM", "surfaceExamplesJVM").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfNativeTestAll", ";" + Seq("viewJavafxJVM").map(p => s"{${neurosurf4sBuild}}$p/test").mkString(";"))

addCommandAlias("neurosurfCompileAll", ";" + Seq("coreJVM", "coreJS", "ioJVM", "ioJS", "spatialJVM", "spatialJS", "spatialIoJVM", "volumeJVM", "volumeJS", "volumeIoJVM", "routingJVM", "routingJS", "routingIoJVM", "fixturesJVM", "fixturesJS", "viewJVM", "viewLinkJVM", "viewVolumeJVM", "viewRasterJVM", "viewJS", "viewLinkJS", "viewVolumeJS", "viewRasterJS", "viewThreeJS", "viewThreeVolumeJS", "viewExamplesJS", "viewExamplesJVM", "surfaceExamplesJVM", "viewJavafxJVM").map(p => s"{${neurosurf4sBuild}}$p/compile").mkString(";"))

addCommandAlias("neuroatlasCoreTestAll", ";" + Seq("coreJVM", "coreJS", "ioJVM").map(p => s"{${neuroatlas4sBuild}}$p/test").mkString(";"))

addCommandAlias("neuroatlasSurfaceTestAll", ";" + Seq("surfaceJVM", "surfaceJS", "surfaceIoJVM").map(p => s"{${neuroatlas4sBuild}}$p/test").mkString(";"))

addCommandAlias("neuroatlasRoutingTestAll", ";" + Seq("routingJVM", "routingJS", "routingIoJVM").map(p => s"{${neuroatlas4sBuild}}$p/test").mkString(";"))

addCommandAlias("neuroatlasCompileAll", ";" + Seq("coreJVM", "coreJS", "ioJVM", "volumeJVM", "volumeJS", "volumeIoJVM", "surfaceJVM", "surfaceJS", "surfaceIoJVM", "routingJVM", "routingJS", "routingIoJVM").map(p => s"{${neuroatlas4sBuild}}$p/compile").mkString(";"))

addCommandAlias("mvpa4sImagingCompileAll", ";" + Seq("surfaceJVM", "surfaceJS", "atlasJVM", "atlasJS", "imagingConsumerJVM", "imagingConsumerJS").map(p => s"{${mvpa4sBuild}}$p/compile").mkString(";"))

addCommandAlias("mvpa4sImagingTestAll", ";" + Seq("surfaceJVM", "surfaceJS", "atlasJVM", "atlasJS", "imagingConsumerJVM", "imagingConsumerJS").map(p => s"{${mvpa4sBuild}}$p/test").mkString(";"))
