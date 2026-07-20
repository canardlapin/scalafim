# scalafim-fmri-workflow

Cross-compiled JVM/Scala.js composition layer for study-scale fMRI analysis.

Package root:

```scala
import scalafim.fmri.workflow.*
```

This module owns immutable study recipes and persistent references, not image
payloads or numerical implementations. A `StudyAnalysisSpec` selects a BIDS
project, a first-level model and contrast workflow, optional group workflows,
and an output policy. A `StudyCatalog` records header-derived first-level units
with exact subject/session/task/run, TR, space, shape, mask, and artifact
identity. `AnalysisPlan.compile` performs exhaustive structural preflight and
only constructs an executable-looking plan when every error is resolved.

`BidsStudyCompiler` is the pure ingest boundary. It joins selected BOLD files
to inherited TR metadata, events, optional confounds, derivative masks, image
header descriptors, and participant covariates. It preserves acquisition,
echo, resolution, derivative pipeline, session, run, exact affine geometry,
and mask policy in deterministic units. Missing and ambiguous companions are
returned together in `CatalogCompileReport`; no voxel payload is needed.
`BidsStudyCompilerJvm` supplies real NIfTI headers from a loaded `BidsProject`
and restricts reads to selected BOLD images and candidate masks.

Mask policy is explicit. `Explicit` uses exactly the declared artifact without
derivative lookup. `RequireCommonDerivative` requires one run-agnostic mask for
the complete analysis unit. `IntersectRunMasks` requires one derivative mask
per run and records their intersection plan. Confound lookup is performed only
when the dataset recipe requests confounds; unrelated or ambiguous confound
tables cannot invalidate a confound-free recipe.

The resulting plan fans out into deterministic `SubjectJob` values and
`GroupJob` values. Jobs contain recipes and `ResultBundleRef`s; they do not
capture readers, functions, matrices, thread pools, or filesystem handles.
Execution capabilities are supplied by interpreters. The current foundation
exposes subject jobs as the existing scheduler-neutral `ChunkProgram`. The
future orchestration slice will add the `scalafim-pipeline` dependency when it
actually lowers these descriptors into graphs without placing voxel payloads in
artifact tables.

Module ownership remains one-way:

```text
bids + dataset + model + fit + group
                 |
                 v
           fmri-workflow
```

Lower modules never depend on `fmri-workflow`. JVM BIDS discovery, NIfTI
staging/reads, result writers, and future scheduler adapters remain platform
interpreters around these shared values.

At execution time, `FirstLevelUnitSource` resolves local file references,
materializes the declared single/intersection mask, opens one positional
`NiftiResponseBlockSource` per run, and returns a
`CompositeResponseBlockSource`. The catalog therefore remains portable data;
open files and decompression caches exist only inside the JVM interpreter. Each
run source uses the dataset module's bounded bulk-window reader, so execution
cost scales with requested windows rather than one file read per voxel.

Output formats are identified by the open `OutputFormatId`, so a new backend
does not expand a closed workflow enum. `bids-nifti` is the reference format;
`ResultMapLayout` selects bundled maps, individual named maps, or a backend
default. A `ResultBundleRef` names the eventual manifest boundary rather than
embedding a concrete writer.

Run the shared contract tests on both platforms:

```sh
sbt fmriWorkflowJVM/test
sbt fmriWorkflowJS/test
```
