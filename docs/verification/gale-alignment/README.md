# Gale and image4s alignment — 10 September 2026

ScalaFIM now uses one Gale revision, `83cac90a678d1b8a31c590e0c1b8fc8bf3427161`,
through its direct dependency, image4s, graph4s, reframe4s and Multivar.
The canonical image4s merge includes the incremental NIfTI writer and checked
sample-space refinement. No temporary provider or `-D` override is required.

## Published providers

| Provider | Revision | Local validation |
|---|---|---|
| image4s | `ec56b34806c22e26c28ecbd366ef2e323195fc88` | 354 core/NIfTI/locus tests, JVM + JS |
| graph4s | `c343e0876a29d0cb73f67799e78b4d25ecd3eb4b` | All-platform compile; 334 affected JVM + JS tests |
| Multivar | `aeb75a30a846d5f68369302e344a613da9c5c21f` | compileAll/testAll; 1,174 core/IR tests |
| reframe4s | `fa015c38a1b481096646d2c857191c327fc7a9e5` | root/compile and root/test; 437 tests |
| Gale | `83cac90a678d1b8a31c590e0c1b8fc8bf3427161` | 50 QR, builder and syntax tests, JVM + JS |

These commits were checked against live remote main refs (Gale's selected
revision is an older reachable commit). This is local source qualification,
not a release, remote CI or binary-compatibility certificate.

## Why more than a pin change was needed

- `accea04` diverged before canonical image4s acquired its newer axis and native
  storage contracts. The merge retains both histories and preserves all 12
  files from the temporary estimate-output provider byte for byte. Their
  checksums are in [temporary-provider-preservation.json](temporary-provider-preservation.json).
- Graph4s's Gale pin existed only in its dirty adapter work. The published
  candidate includes that adapter and its typed indexed-data prerequisites.
- Multivar's pin and builder rename were uncommitted. Its published update is
  a two-line change atop the previous remote main; unrelated local inference
  work remains local.
- Reframe4s introduced another older Gale/image4s dependency path. Both of its
  pins are now aligned.
- Source Gale called itself `1.0.0-SNAPSHOT`, while Multivar declared
  `1.0.0-83cac90a678d`. ScalaFIM gives source Gale the same exact coordinates,
  allowing sbt to resolve one provider without weakening eviction checks.
  Both MVPA fitting classpaths contain one Gale source directory and no Gale
  artifact jar; see [classpath.json](classpath.json).

The Gale delta also includes QR multi-RHS optimization and Ravel interop
additions. ScalaFIM's required call-site change is `updateRowMajor` to
`writeLinear`: eleven committed files plus the same rename in an existing,
uncommitted HRF spike file.

## ScalaFIM qualification

The isolated candidate starts at `a2d6f95f2a7703b015c156262699138db1d8b70c`
and includes only the alignment, builder renames, bootstrap helper and receipts.

- `scalafimCompileAll` passed on both platforms.
- All 2,661 affected tests passed: 1,382 JVM and 1,279 Scala.js.
- Covered modules: latent, design, image, spatial, dataset, fit, MVPA,
  MVPA fitting, connectivity and archived-response interop.
- The actual dirty working tree also passed `scalafimCompileAll` without
  provider overrides. Its pre-existing estimate-set and HRF work is preserved.

The working-tree image and estimate modules passed a further 623 tests
(325 JVM / 298 JS), and the in-flight HRF tests compiled on both
platforms. This does not qualify the uncommitted HRF experiment scientifically.

[gates.json](gates.json) records the exact bounded test batches.
[logs.json](logs.json) indexes compressed logs with checksums.
[pins.json](pins.json) records provider revisions. All runs used the checked-in
build toolchains and local Java 25.0.1. Reframe4s's generic compileAll alias
collides with an upstream alias, so its root tasks were invoked explicitly;
its successful full run used a 4 GiB heap.

## Reproduce

```sh
./tools/prepare-pinned-dependencies.sh
sbt scalafimCompileAll
sbt latentJVM/test designJVM/test imageJVM/test spatialJVM/test datasetJVM/test fitJVM/test mvpaJVM/test mvpaFitJVM/test connectivityJVM/test archivedResponseInteropJVM/test
sbt latentJS/test designJS/test imageJS/test
sbt spatialJS/test datasetJS/test fitJS/test
sbt mvpaJS/test mvpaFitJS/test connectivityJS/test archivedResponseInteropJS/test
```

The bootstrap checks out pinned Multivar and runs its exact Gale artifact
installer. It was exercised successfully with the revision already present
in the local artifact cache; the installer emitted non-SNAPSHOT overwrite
warnings. This run is not an empty-cache or published-artifact consumer proof.
The full compile's build-reload advisory came from a comment-only edit while
it ran; the subsequent test batches loaded the final build file.
