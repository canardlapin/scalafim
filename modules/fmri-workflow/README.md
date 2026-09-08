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

`FirstLevelTables.bind` converts already selected per-run BIDS tables into
`DatasetEvents` and optional named `NuisanceRegressors`. It keeps the declared
run order and text identities (including leading zeros), checks scan counts,
and retains each BIDS4s `ConfoundSelection` with its diagnostics. Confound
selection and cleaning use BIDS4s. Missing event values and non-finite confounds
after selection are explicit binding errors; this initial binder does not
impute events or silently drop scans. The configured run column must agree
with any existing event values.

On the JVM, `FirstLevelUnitDataset.open(unit, datasetId, confounds = ...)` reads
only the selected events and requested confound artifacts, then uses
`FirstLevelUnitSource` and `FmriDataset.open` for response loading. The returned
`OpenedFirstLevelDataset` retains the catalog unit and binding receipts.
`opened.buildPlan(modelSpec)` binds the run column and nuisance matrices before
calling `FmriModelBuilder`; conflicting model configuration is rejected.
Execute the resulting plan with `FitPlanExecutor.fit(opened.dataset, plan)`.
A confound-free request does not read optional confound files. The current
interpreter supports local file URIs; other transports require an interpreter.

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

## First-level sampling reference

Declare the model's within-volume reference when opening a first-level unit:

```scala
val opened = FirstLevelUnitDataset.open(unit, datasetId,
  samplingReference = SamplingReference.VolumeOnset)
```

`SamplingReference.VolumeMidpoint` remains the default for existing callers.
`Uniform(SamplingFraction.unsafe(0.25))` selects one fraction of each run's TR;
`PerRun(Map(runId -> fraction, ...))` supports differing run references. Fractions
must be finite and in `[0, 1]`, including volume onset, midpoint and end. The
per-run map must cover exactly the selected run identities; `01` and `1` are
different identities here. Invalid declarations fail before companion or image
IO. Sampling-grid construction uses the existing native `SamplingFrame` contract.

`opened.sampling` records the declaration, ordered runs, fractions, TRs, first
and last run-local sample times, and the exact frame supplied to the dataset.
For 300 samples at TR 2 seconds, onset sampling uses 0 through 598 seconds;
midpoint sampling uses 1 through 599. This changes response support and design
values, so it is part of the model specification, not a display-only offset.
Building and fitting the plan uses this frame; event rows are never shifted.

BIDS event onsets retain their source origin: zero is the beginning of the first
stored data point, including when earlier dummy volumes have been discarded.
See the [BIDS event timing specification](https://bids-specification.readthedocs.io/en/v1.10.0/modality-specific-files/task-events.html).
The sampling fraction is explicitly a fraction of **TR**; it is not inferred
from a NIfTI time offset, a `SliceTiming` array, or another tool's command line.

`RunInput.timing.sliceTimingCorrected` separately preserves the selected BOLD's
BIDS4s-resolved `SliceTimingCorrected` boolean. Missing metadata remains unknown;
present non-boolean values are errors. The compiler does not borrow this claim
from a matched raw BOLD companion. Neither true nor false selects a model
reference. A single volume-wide reference does not correct slice-dependent
acquisition times in uncorrected data. Choosing an appropriate model reference
requires the preprocessing provenance; this API does not perform slice-timing
correction or implement slice-specific designs.


## Common spatial support across a study

`StudySpatialSupport.compile(catalog, limits)(openMask)` compiles a strict
intersection across the catalog's original masks. `StudySpatialLimits.make`
requires positive maximum grid size and maximum voxels per read. All units
must declare the same named space and exact spatial geometry; intersection
mask references must cover exactly their unit's acquired run IDs. These checks
precede mask IO. The compiler does not resample, infer registration, replace
mask files, or fill unavailable response values.

The shared compiler accepts a `ResponseBlockSource` opener. Each mask must expose
one volume and the full spatial domain. Finite nonzero values, including negative
values, indicate available voxels; zero and nonfinite values exclude voxels.
Masks are scanned one at a time in bounded chunks. Empty common support is an
error. The result retains canonical absolute voxel identities, original units,
per-source selected/nonfinite counts, per-unit intersection counts, and actual
read counts and maximum request width. Repeated run IDs across participants stay
qualified by their unit. A single unit mask is assigned to all its runs.

`support.selectionFor(unit)` returns a checked dataset voxel selection only for
an unchanged unit captured in the support catalog. Use that selection in native
dataset reads and first-level fitting. The result captures references and catalog
identity, not file-content hashes; callers must enforce any required source
revision or content-integrity policy around compilation and subsequent use.

On the JVM, `StudySpatialSupportJvm.compile(catalog, limits, staging)` uses native
NIfTI block sources, checks mask headers before opening the full domain, and
requires an explicit `NiftiStagingCache` for compressed masks. It never opens BOLD
payloads. Cancellation interrupts propagate between source opens and read blocks.
The shared compiler holds two grid-sized Boolean arrays and a bounded response
block while scanning, plus output indices and coverage receipts. Request limits
are not total heap limits: sources retain their own indexing, decoder and staging
storage. Header geometry equality does not establish anatomical registration.

## Inspect before loading responses

Pass `responseAccess = FirstLevelResponseAccess.OnFirstRead` to
`FirstLevelUnitDataset.open` to bind sampling, masks and tables and compile the
same condition/FIR model without opening BOLD storage. The default remains
`Immediate`. Deferred access validates the actual response source at the first
bounded read; compressed images need an explicit staging cache at that point.
A metadata preview is not a digest-verified snapshot of BOLD content. Consumers
must verify source revisions at execution.

`FirstLevelCompanionReviewJvm.inspect(root, unit)` discovers optional QC tables
through BIDS matching independently of the fitted nuisance request. Its byte
budget is enforced during reads. It checks original row count, preserves
missing samples, reports invalid numeric columns, and uses only declared source
units. Absent companions are explicit; ambiguity or invalid table shape fails
review. Results carry artifact locations and SHA-256 digests, and selection and
metadata are checked again before returning. No BOLD payload is read. Shared
`FirstLevelCompanionReview.bind` supports JVM and Scala.js table consumers.
