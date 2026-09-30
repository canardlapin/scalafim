# Estimate sets: native implementation progress

The `estimates`, `estimates-io` and `fit-estimates` modules implement a first
vertical slice of the [native implementation plan](plans/estimate-set-implementation.md).
They are **not a claim of full estimate-set V0 conformance**.
The scientific target is the [canonical ScalaFIM V0 0.2.0 specification](estimate-set-v0-spec.md).
New locally published JSON/TSV/NIfTI units use the versioned
`scalafim-estimates-core-nifti-1` wire discriminator and `WireVersion` `1.0.0`.
`ProfileVersion` `0.2.0` names the scientific target separately. The reader also
accepts `scalafim-estimates-development-1` for historical bundles; a direct
metadata encode without TSV references remains a development document. The
Core-NIfTI-1 remains the default writer layout. An explicit
`CovarianceLayout.SharedNormalizedTable()` opts a unit into Core-NIfTI-2,
wire `2.0.0`, for bounded shared normalized covariance. Catalogs, TSV projections,
collections and pointers remain Core-NIfTI-1. The checks below qualify this bounded
storage/producer/consumer slice; HDF5, pooled persistence and workflow adoption
remain separate gates.

## Implemented boundary

- Shared catalogs use model-scoped estimand IDs; display labels may repeat.
  Unit bindings retain ordered column IDs and exact numerical operators.
- Products separate effects and hypotheses/statistics, validity, precision,
  physical response coordinates, uncertainty descriptors and requested outcomes.
  Covariance uses an explicit pair axis with absolute or variance-scaled
  interpretation. Stored pair values and reconstructed absolute matrices are
  distinct APIs.
- The shared-OLS producer streams selected effects and optional marginal standard
  errors/residual variance or joint normalized covariance. It retains actual
  readout weights, selected run-local scan rows, full-rank QR tolerance/method
  evidence and residual df. A scalar-only sink is refused before executing a
  joint request unless it advertises the required pair-only shared capability.
  The default NIfTI encoding repeats shared U over samples. An opted-in compact
  sink receives each observation's U once from the existing prepared matrix;
  it does not refit or compute another inverse. Effects, residual variance and
  marginal standard errors keep their existing per-sample costs.
- The JVM scalar NIfTI sink writes caller-ordered blocks into exclusive staging
  files. Values never become dense image maps in memory. A disk-backed coverage
  ledger rejects duplicate deliveries and prevents incomplete sealing. Invalid
  samples remain explicit per-estimand uint8 validity values; zero is data.
- The reader verifies manifest, catalog and each required payload SHA256/length
  before using that payload. Reads preserve observation/estimand/sample order and
  validate destination capacity and the declared block budget. Cancellation makes
  the destination discardable scratch. Closing is serialized with owned reads.
- Archive-owned local primitives stream bytes or accept an owned prewritten file,
  fsync files/directories and publish immutable names without overwrite. Discovery
  updates use a filesystem lock and compare-and-swap; stale writers must reread
  and explicitly merge compatible additions. No last-writer-wins update exists.
  A same-revision retry may reuse an already published object only when its exact
  length and SHA256 match the staged bytes; changed bytes conflict without overwrite.

## Current restrictions

The local writer emits uncompressed NIfTI-1 scalar Float64 and exactly
representable declared Float32 products. The reader accepts `.nii` and `.nii.gz`,
physical Float32/Float64 with separately declared logical precision and NIfTI
scaling applied once, matching UInt8 validity, singleton 3D or ordered 4D axes,
RAS world coordinates in millimetres, and the explicitly qualified scanner sform.
It compares manifest/header grid corners within 0.001 mm before publication and
on opening. A coded qform with opposite handedness is refused. A differing
same-handed qform needs an explicit alternate-frame declaration matching its
NIfTI transform code (`aligned-anatomical`, `talairach`, or `mni-152`). It supports
arbitrary stored voxel orientation within that binding. Other selected frame
bindings are not yet qualified. Without inference evidence the reader and writer
cap an open unit at 32 actual NIfTI product/observation pairs. The reader owns 64
data/validity handles; the writer additionally owns one coverage ledger per pair
(96 total handles). Both check before touching payloads. The evidence route's
aggregate handle accounting and reduced pair cap are described below. Shared code does not pretend to implement browser IO.

Compact storage accepts Float64 normalized covariance declared invariant across
samples. One digest-pinned JSON upper triangle per product/observation records
ordered estimand IDs, finite values and pair validity. Validity broadcasts over
unit support; outside support remains `OutsideSupport`. Missing or invalid pairs
never become zeros. Observation invariance, when declared, is checked across both
shared tables and NIfTI arms. Readers return raw U; `CovarianceAccess` applies the
declared residual scale once and checks U before a zero scale can conceal
indefiniteness. NIfTI slope/intercept remains a separate decoding step.

Shared tables have cumulative defaults of 4,096 pairs and 1 MiB declared table
bytes per open unit, checked before payload reads. The sink checks pair count and
a conservative UTF-8 serialization reservation before allocating pair coverage;
it checks actual serialized bytes again at seal. Thus a writer can refuse a byte
budget that an existing smaller table satisfies. `maximumCells` independently
bounds each caller block. Shared values, validity and coverage occupy O(P)
space, with no sample-by-pair payload or coverage ledger. These bounds coexist
with the existing NIfTI handle and gzip staging caps.

The [compact covariance receipt](verification/estimate-set-compact-covariance-2026-09-30/README.md)
records actual JVM/JS tests, an independently authored complete physical bundle,
native OLS block-size parity, fitter-free relocated covariance/group reads,
publication interruptions and two sample counts under `-Xmx64m`. For K=64,
compact U stays at 200,714 bytes and 2,080 coverage entries for both N=2,048 and
N=8,192. The synthetic probe keeps two scalar products: their payload is
2,360,704/9,438,592 bytes and scalar coverage has 262,144/1,048,576 entries.
Support metadata still grows with N. These are bounded resource observations,
not a speed comparison, a whole-job constant-memory claim or a power-loss guarantee.

The metadata embeds ordered support indices. Metadata documents are
limited to 16 MiB and exact JSON byte counts through 2^53-1. Core units require `estimands.tsv` and
`observations.tsv` with zero-based `index` columns; the unit manifest pins their
digests and the reader verifies that their ordered IDs agree exactly with the
JSON catalog and unit. JSON is the scientific authority, and these TSV files are
checked projections rather than editable alternative definitions. Core unit
representations must cover every product/observation pair and declare physical
stored dtype separately from decoded product precision. Development documents
predating the tables remain readable. Legacy bare statistic effect/SE IDs decode
as explicitly unknown hypothesis correspondence; only a complete named mapping
can establish a known link. Support and catalog metadata
occupy memory proportional to their size; payload streaming alone is not a
general whole-job peak-memory certificate. The
initial [heap/relocation probe](verification/estimate-set-increment/heap-and-relocation.json)
wrote 16,777,216 Float64 cells (128 MiB numerical payload) under `-Xmx64m` in 3.10 s
on this machine, then reopened in a separate 64 MiB JVM after relocation with no
fitter on its classpath. Independent Python checked every value and validity byte.
That one workload does not qualify the full performance/access matrix.
Current sparse reads are bounded but use individual scalar file reads. Gzip
payloads are decompressed into owned temporary seekable files using a 64 KiB
buffer, with a cumulative `ReadLimits.maximumStagingBytes` cap (1 GiB default)
checked during expansion; temporary files are removed on close or failed open.
The independent 3D big-endian/scaled fixture stages exactly 714 bytes at the
boundary and refuses 713 bytes. A separate `-Xmx64m` JVM staged 75,498,176
bytes under that exact cap and measured 38,172,664 bytes peak heap; it refused
one byte less and removed its staging files on close. See the
[Core-NIfTI receipt](verification/estimate-set-core-nifti-2026-09-29/README.md).
Broader access performance remains open. Directory fsync and exclusive
hard links must be supported by the actual destination filesystem. The tests
exercise failures and pointer conflicts, not power-loss/crash durability at every
publication boundary. Process interruption can leave unpublished staging files
for explicit recovery.

`CovarianceAccess.matrix` reconstructs one selected principal covariance matrix
in caller order, applies a declared variance scale once, and uses Gale to check
positive semidefiniteness under an explicit consumer tolerance/order budget.
It checks normalized U before multiplying: zero scale cannot hide an indefinite U.
Unavailable entries reject this operation. The call does not certify estimability,
all matrices in a file, spatial covariance, or inference admission. The single
bounded eigensolve is not interruptible; cancellation is checked before and after.

`group.EstimateGroup` now accepts pinned unit references without importing `fit`.
Preparation requires explicit `GroupEstimateAdmission` for scientific suitability
and common sample-grid alignment; successful admission returns retained geometry
evidence whose world frame must match every unit. It rejects repeated participant
rows and incompatible catalogs, pooling scopes or uncertainty axes. Bounded blocks
preserve pinned input receipts, participant/estimand/sample order and validity;
unavailable cells refuse the block rather than silently reduce the cohort. One unit
is opened, fully verified and closed at a time. Repeated integrity scans are a
documented cost, not a qualified cohort-performance policy. The old eager bridge
moved to `fit-estimates.FitGroupAdapter`, with its tests.

Variance and standard-error products may bind a
`MarginalUncertaintyDescriptor` to their effect. The origin is explicitly known,
estimated with scalar or sample-dependent df, approximate effective df, or unknown.
`GroupData` retains the resolved subject/contrast/sample receipt, including
estimator, serial-noise, nuisance, run-combination, pooling, and geometry evidence.
Raw variance matrices acquire an explicit unknown receipt; no df is inferred from
their presence. The receipt is provenance, not a first-level calibration result or
automatic second-level inference admission.

Pooled selected execution is available in `fit`, but its persistence adapter,
HDF5 schema/fixtures, workflow and
PLS Neuro adoption remain outstanding. `ResultManifestWriter` remains until its
replacement covers and tests its useful scientific extraction. The older eager
canonical archive writer remains; the new local streaming primitives do not yet
replace every existing canonical/BOLD publication path.

## Writing and reopening

The producer takes an already prepared native `FirstLevelEstimatePlan`, persistent
publication identity and a model-level catalog/output-ID mapping. Allocate unit
revision IDs before running; scientific changes require a new revision. Supply
the original response units separately from estimand output units.

```scala
import scalafim.estimates.*
import scalafim.estimates.io.LocalEstimateStore
import scalafim.fmri.fit.estimates.FitEstimateProducer

// prepared, identity, catalog and outputIds describe the actual scientific job.
val saved = for
  producer <- FitEstimateProducer.shared(prepared, identity, catalog, outputIds, "scanner")
  store <- LocalEstimateStore.open(destinationRoot)
  sink <- store.newSink(producer.unit, maximumBlockCells = prepared.maxBlockVoxels)
  reference <- producer.write(datasetReader, sink, cancelled)
yield reference
```

`reference` is immutable and includes the manifest digest/length. A reader imports
only `estimates` and `estimates-io`, opens that reference under a `ReadLimits` policy,
and closes the returned source. Collection publication and `discover` are separate
operations. A collection retains failed/missing intended units rather than silently
shrinking a cohort. Its `unitsPublished` flag is not scientific admission or complete
requested-product coverage.

Executable examples and regression evidence live in
[`FitEstimateProducerSuite`](../modules/fit-estimates/shared/src/test/scala/scalafim/fmri/fit/estimates/FitEstimateProducerSuite.scala),
[`FitEstimateReadbackSuite`](../modules/fit-estimates/jvm/src/test/scala/scalafim/fmri/fit/estimates/FitEstimateReadbackSuite.scala), and
[`LocalEstimateStoreSuite`](../modules/estimates-io/jvm/src/test/scala/scalafim/estimates/io/LocalEstimateStoreSuite.scala).

## Provider prerequisite

The selected-executor forward-port is in this working tree and uses canonical
image/Gale types. The incremental image4s forward-port is based on exact main
`18ffdce67fd05f5bcd28336650edcae891da6265`; its retained patch is
[image4s-canonical-output.patch](verification/estimate-set-increment/image4s-canonical-output.patch).
The published `accea049...` branch is divergent: it lacks canonical selected-image
changes. It must not be adopted as a replacement for main.

The prerequisite is now published on image4s main at
`ec56b34806c22e26c28ecbd366ef2e323195fc88`. It incorporates the retained patch
exactly alongside canonical image changes. ScalaFIM commit
`316a15758686c4693f97c5ac84376a3382a37b95` already pins it. Both live remote
refs were verified on continuation; no duplicate provider branch was needed.
Ordinary builds now use this immutable published pin without a local override.

The earlier approval rejection and override-only evidence are historical in the
first-increment report. Current qualification is recorded in
[continuation evidence](verification/estimate-set-increment/continuation.md).
Source reconciliation, remote publication, physical conformance and downstream
admission remain separately reported gates.

## Optional typed inference evidence

`EstimateUnit.inferenceEvidence` adds small coefficient-scope declarations and
ordered fit/hypothesis planes without introducing a fit dependency or numerical
product kind. `ScientificFact.Unknown` preserves unavailable conditioning and
`InferenceStatusCode.Unrecorded` preserves missing native status. Numerical
validity continues to control numerical use; explanatory status grants no
validity or known reference law.

Use `LocalEstimateStore.newInferenceSink(unit, maximumBlockCells)` explicitly for
an evidence-bearing PairNifti unit. The returned `InferenceEvidenceSink` accepts
borrowed bytes through `writeInferenceStatus(InferenceStatusSelection(...), codes)`.
Deliver every supported sample for each declared plane before sealing. Old sink
factories and old metadata encoders refuse evidence-bearing units. Sources expose
`InferenceEvidenceSource.readInferenceStatus`; an old unit retains `None` and
status reads return Unsupported.

The unit manifest uses Core-NIfTI-3, wire `3.0.0`, one digest-pinned identity-scaled
UInt8 status stack and tagged numeric NIfTI representations. Catalogs, TSV,
collections and pointers stay Core-1. Compact shared covariance with status is
prospectively refused. There are at most 32 planes, an 8 GiB status payload budget checked before staging,
bounded block allocations,
disk-backed coverage, and explicit aggregate handle caps: writer 96 (three per
numeric pair plus two for status), reader 64 (two per numeric pair plus one for
status). Evidence therefore permits at most 31 numeric pairs. Selected reads
preserve sparse physical permutations. Every status code, support hole and
geometry/digest/length declaration is verified before source ownership transfers.

The independent literal fixture and failure/lifetime checks are recorded in
[the inference-evidence verification record](verification/estimate-set-inference-evidence-2026-09-30.md).
This prerequisite does not qualify a native producer, remove the legacy exporter,
qualify HDF5, establish statistical reference laws or establish performance.
