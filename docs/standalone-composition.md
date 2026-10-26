# Direct library composition

Choose the smallest provider module that owns your data. None of these libraries
needs ScalaFIM to compile or run. ScalaFIM adds functional datasets, response
interpretation, fitting and workflow integration when an application needs them.
These are development source pins, not published artifact releases.

## Select a module

This minimal `build.sbt` selects four independent JVM consumers. Keep only the
consumer you need; the shared providers resolve to the same immutable revisions.
Use Scala 3.7.4 and sbt 1.11.7.

```scala
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
```

MVPA's Multivar dependency currently needs revision-tagged Gale and Resample4s
artifacts prepared from their declared sources. On a development machine,
ScalaFIM's `tools/prepare-pinned-dependencies.sh` validates the shared default
graph before running those upstream publishers. That is build preparation, not
a ScalaFIM runtime dependency. Its `--check-only` option validates without
publishing. Clean acceptance uses a separately inventoried private artifact
store; a successful build against an arbitrary local cache is weaker evidence.

For Scala.js, use `sbt-scalajs` 1.22.0 and `sbt-scalajs-crossproject` 1.3.2,
create a JS `crossProject`, and select the corresponding `coreJS` ProjectRef.
Portable behavior must run on both JVM and Node.js; JavaFX is JVM-only and
Three.js is JS-only.

## Compose public values

The complete examples below are executable suites at the selected immutable
provider sources. Read their public API calls; do not depend on provider Test
configurations or import their test helpers into application code.

| Library | Direct composition and checked refusal | Complete example |
| --- | --- | --- |
| neuroim4s | Build image4s `Grid`/`SampleSpace`/`Sampled` values over Ravel, admit with `NeuroSeries.fromSampled`, register the exact grid through `image4s.locus.GridDomain` from image4s-locus, gather an ordered `Selection`, then produce dense output. The admitted series retains the sampled value; a Channel axis is refused where Time is required. | [ImageWorkflowSuite](https://github.com/canardlapin/neuroim4s/blob/4f75c2bf141449d032f9fe3c558ef13a22a4d59f/modules/neuroim4s-core/shared/src/test/scala/external/neuroim/consumer/ImageWorkflowSuite.scala) |
| neuroatlas4s | Compose an `AtlasRealization` from one locus4s `PartialSurjection`, parcel-indexed keys/metadata and an explicit display selection. Region/network support derives from membership; equal cardinality does not authorize alignment with another parcel owner. | [AtlasRealizationContractSuite](https://github.com/canardlapin/neuroatlas4s/blob/4df875d8a1cd20afd3b83ed49446422bc507e5ec/modules/neuroatlas4s-core/shared/src/test/scala/neuroatlas4s/AtlasRealizationContractSuite.scala) |
| neurosurf4s | `TriangleMesh.fromArraysEither` returns `Either[MeshAdmissionError, TriangleMesh]`, retaining mesh4s topology and point storage. `sharingTopology` checks a second realization against an existing owner; raw triples do not certify a world frame. Topological admission does not imply nonzero geometric area. | [TriangleMesh](https://github.com/canardlapin/neurosurf4s/blob/73a244fbe5d7b4db2f0c946d98fa2a3c00101ab7/modules/neurosurf4s-core/shared/src/main/scala/neurosurf4s/surface/TriangleMesh.scala), [public frame-boundary control](https://github.com/canardlapin/neurosurf4s/blob/73a244fbe5d7b4db2f0c946d98fa2a3c00101ab7/modules/neurosurf4s-core/shared/src/test/scala/consumer/MeshStorageBoundaryCompileSuite.scala) |
| mvpa4s | Build `AxisRef` from complete stable keys and use native `Observations`. Optional `mvpa4s.atlas.AtlasMeasurementFrames.parcels` requires an explicit certified atlas-sample → source-locus bijection. It validates owners/keys before reads; background remains unvisited and display order stays separate from traversal identity. | [AtlasMeasurementFramesSuite](https://github.com/canardlapin/mvpa4s/blob/c27469484bf760c34cd896dabe4fe1c384a83b45/modules/mvpa4s-atlas/shared/src/test/scala/mvpa4s/atlas/AtlasMeasurementFramesSuite.scala), [independent consumer receipt](verification/eco-u6-20261022/adapter-qualification.json) |

Use the constructor's `Either` at an admission boundary: propagate its typed
error or explicitly interpret its `message`. Do not replace owner evidence with
size checks, rebuild parallel containers, or cast between domain types.

## Add optional capabilities

| Need | Provider ProjectRefs, replacing or supplementing the core reference |
| --- | --- |
| Shared anatomical worlds without images | neuroim4s `spacesJVM` / `spacesJS` |
| NIfTI anatomical admission | neuroim4s `ioJVM` |
| Toolkit transforms and container IO | neuroim4s `transformJVM` / `transformJS`, optional `transformIoJVM` |
| Slice scenes and hosts | neuroim4s `viewJVM` / `viewJS`; optional `viewJava2dJVM`, `viewJavafxJVM`, `viewCanvasJS` |
| Surface formats, transport or volume projection | neurosurf4s `ioJVM` / `ioJS`, `spatialJVM` / `spatialJS`, or `volumeJVM` / `volumeJS`; JVM file adapters are separate |
| Surface display | neurosurf4s `viewJVM` / `viewJS`; optional raster, JavaFX or Three.js backends |
| Atlas geometry and file loading | neuroatlas4s `volumeJVM` / `volumeJS` or `surfaceJVM` / `surfaceJS`; optional `volumeIoJVM` / `surfaceIoJVM` |
| Locus, surface or atlas measurements | mvpa4s `locusJVM` / `locusJS`, `surfaceJVM` / `surfaceJS`, `atlasJVM` / `atlasJS` |
| Predictive fitting | mvpa4s `predictiveJVM` / `predictiveJS` over Alder; no imaging dependency |
| Functional fitting/datasets/workflows | ScalaFIM's fit, dataset and `mvpa-*` integration modules |

Surface core consumes neutral world declarations and image geometry, not the
volume or rendering stack. Atlas core uses locus4s without either geometry
runtime. MVPA core/predictive/locus need neither anatomical library. Optional
surface/atlas measurement modules consume only their respective minimal cores.

## Check a ScalaFIM integration

From ScalaFIM, run focused JVM/JS suites through `tools/build/sbt-warm`.
`ecosystemBoundaries` checks 24 actual transitive Compile closures, production
source ownership and immutable Git-pin coherence. `scalafimCompileAll` includes
that preflight; `bash tools/ci/full-repository-tests.sh` uses fresh bounded
batches. Native display qualification remains separate.

Explicit `-Dscalafim.<provider>.build=/absolute/path` properties select coordinated
local checkouts and are forwarded through the graph. Removing them restores
immutable defaults. A local override is not evidence that its checkout is clean
or matches the admitted source revision. Final independent-consumer acceptance
uses public source pins and isolated artifacts, without sibling overrides.

The [ownership ledger](verification/eco-u6-cleanup-20261022/ownership.md) records
retired APIs and preserved test custody. Persisted schema tags and digest framing
remain separate from package names; legacy affine v1 fingerprints still have a
documented JVM/JS difference. Extraction does not expand numerical, native or
performance qualification, and final epic acceptance remains a separate gate.
