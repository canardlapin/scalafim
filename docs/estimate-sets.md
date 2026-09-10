# Estimate sets: native implementation progress

The `estimates`, `estimates-io` and `fit-estimates` modules implement a first
vertical slice of the [native implementation plan](plans/estimate-set-implementation.md).
They are **development APIs, not a claim of full estimate-set V0 conformance**.
The scientific target remains [V0 0.2.0](../../plsneuro/docs/first-level-artifact-spec.md).
The wire discriminator is `scalafim-estimates-development-1`; `ProfileVersion`
records the scientific target. Do not treat this development layout as the final
interchange schema or BIDS-shaped directory layout.

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
  joint request. The current NIfTI encoding repeats shared U over samples;
  compact shared-matrix JSON/TSV storage remains a future encoding improvement.
- The JVM scalar NIfTI sink writes caller-ordered blocks into exclusive staging
  files. Values never become dense image maps in memory. A disk-backed coverage
  ledger rejects duplicate deliveries and prevents incomplete sealing. Invalid
  samples remain explicit per-estimand uint8 validity values; zero is data.
- The reader verifies manifest, catalog and complete required payload SHA256/length
  before opening handles. Reads preserve observation/estimand/sample order and
  validate destination capacity and the declared block budget. Cancellation makes
  the destination discardable scratch. Closing is serialized with owned reads.
- Archive-owned local primitives stream bytes or accept an owned prewritten file,
  fsync files/directories and publish immutable names without overwrite. Discovery
  updates use a filesystem lock and compare-and-swap; stale writers must reread
  and explicitly merge compatible additions. No last-writer-wins update exists.

## Current restrictions

The local physical implementation supports uncompressed NIfTI-1 scalar Float64
and exactly representable declared Float32 products, matching UInt8 validity,
RAS world coordinates in millimetres, and the explicitly qualified scanner sform.
It compares manifest/header grid corners within 0.001 mm before publication and
on opening. It supports arbitrary stored voxel orientation within that binding.
It refuses other frame bindings and more than 32 open
product/observation files. Shared code does not pretend to implement browser IO.

The development metadata embeds ordered support indices. Metadata documents are
limited to 16 MiB and exact JSON byte counts through 2^53-1. This is not the final
TSV/mask/profile schema. Support and catalog metadata occupy memory proportional
to their size; payload streaming alone is not a general whole-job peak-memory certificate. The
initial [heap/relocation probe](verification/estimate-set-increment/heap-and-relocation.json)
wrote 16,777,216 Float64 cells (128 MiB numerical payload) under `-Xmx64m` in 3.10 s
on this machine, then reopened in a separate 64 MiB JVM after relocation with no
fitter on its classpath. Independent Python checked every value and validity byte.
That one workload does not qualify the full performance/access matrix.
Current sparse reads are bounded but use individual scalar file reads. Access
performance and gzip staging are not qualified. Directory fsync and exclusive
hard links must be supported by the actual destination filesystem. The tests
exercise failures and pointer conflicts, not power-loss/crash durability at every
publication boundary. Failed staging remains available for explicit recovery.

`CovarianceAccess.matrix` reconstructs one selected principal covariance matrix
in caller order, applies a declared variance scale once, and uses Gale to check
positive semidefiniteness under an explicit consumer tolerance/order budget.
It checks normalized U before multiplying: zero scale cannot hide an indefinite U.
Unavailable entries reject this operation. The call does not certify estimability,
all matrices in a file, spatial covariance, or inference admission. The single
bounded eigensolve is not interruptible; cancellation is checked before and after.

`group.EstimateGroup` now accepts pinned unit references without importing `fit`.
Preparation requires explicit `GroupEstimateAdmission` for scientific suitability
and common sample-grid alignment. It rejects repeated participant rows and
incompatible catalogs, pooling scopes or uncertainty axes. Bounded blocks preserve
pinned input receipts, participant/estimand/sample order and validity; unavailable
cells refuse the block rather than silently reduce the cohort. One unit is opened,
fully verified and closed at a time. Repeated integrity scans are a documented
cost, not a qualified cohort-performance policy. The old eager bridge moved to
`fit-estimates.FitGroupAdapter`, with its tests.

Pooled selected execution is available in `fit`, but its persistence adapter,
compact shared-matrix encoding, final wire schemas/fixtures, HDF5, workflow and
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
