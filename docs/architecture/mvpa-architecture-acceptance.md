# MVPA architecture acceptance

Status: accepted and cut over; sole forward MVPA architecture

Date: 2026-08-25

Mote gate: `bd-01M0SV5BRF9GMX20AZWJY78E5K`

This report records why the identified-evidence architecture is the sole
ScalaFIM MVPA architecture. The two unlike vertical slices share one public
waist, and the retired ontology and split artifacts have been removed rather
than retained behind compatibility adapters.

## Shared shell

| Stage | Predictive kernel | Relational kernel | Shared definition |
|---|---|---|---|
| Source | `CategoricalObservationSource` | `PartitionedRelations` | `ScientificSource`, `AxisRef`, `EvidenceTable` |
| Design | `ValidationDesign` / `CrossFitDesign` | `PairingDesign` | `EvidenceDesign`, exact `DesignAxisReference`s |
| Localization | region, global, searchlight | region, global, searchlight | `Measurement`, `MeasurementFrame` |
| Question | classification workflow | identity distance, crossnobis, RSA | open `Estimand` plus `Compile` capability |
| Admission | predictive capability errors | estimability/model/certificate errors | `ScientificSpecification`, `BoundScientificPlan.bind` |
| Execution | Alder-backed measurement task | sufficient-statistic measurement task | `ExecutionPlan`, `AnalysisExecution` |
| Result | `OutOfFoldClassification` | `MeasuredRelationalRdm`, `MeasuredRankRsa` | `AnalysisResult[A, Rejection, Failure, Rendition]` |
| Provenance | fold/preparation/fit evidence | pairing/projection/precision evidence | `ScientificPlanIdentity`, `ExecutionReceipt` |
| Inspection | method-specific estimand remains visible | method-specific estimand remains visible | `ScientificPlanInspection`, `Mvpa.inspect` |

There is no second frame, traversal, local-failure, provenance, streaming, or
result shell in either vertical slice. Method errors stay precise type members
of their estimands and travel through the common `MeasurementOutcome`.

## Constitutional laws and executable evidence

Every suite below is in shared test sources and therefore runs on both the JVM
and Scala.js.

| Law | Primary executable evidence |
|---|---|
| Equal dimensions do not establish axis compatibility; ordered coordinates and full descriptors do. | `AxisIdentitySuite`, `AxisRefSuite`, `ScientificIdentitySuite` |
| Reindexing identity is neutral; exact kind, order, multiplicity, and composition survive. | `ReindexingLegSuite`, `MvpaGeneratedLawsSuite`, `DesignLawsSuite` |
| Evidence and axis-bound columns reindex together without confusing semantic keys and ordinals. | `AxisRefSuite`, `ReindexingLegSuite`, `EvidenceTableSuite`, `ColumnSuite` |
| Sparse selections, weighted regions, fixed projections, and global identity are one measurement algebra; learned projections are withheld until fit scope is compiler-owned. | `MeasurementSuite`, `RelationalArchitectureCourtSuite` |
| Measurement identity includes exact support and leg order; rendition is descriptive. | `MeasurementSuite`, `RelationalFrameExecutionSuite`, `SpatialFramesSuite` |
| Dense, matrix-free, materialized, and sufficient-statistic paths agree where admitted. | `EvidenceTableSuite`, `PredictiveFrameExecutionSuite`, `RelationalCompilerSuite`, `RelationalArchitectureCourtSuite` |
| Materialization and fallback can never be silent. | `ExecutionPlanSuite`, `ExecutionReceiptSuite`, both frame execution suites |
| Exact-once validation reconstructs one authoritative output per `SampleId`; weaker coverage is distinct. | `PredictiveDesignSuite`, `OutOfFoldClassificationSuite`, `BoundScheduleSuite` |
| Predictive preparation is training-scoped and held-out targets/patterns cannot affect a fitted fold. | `ClassificationCompilerSuite`, `PredictiveArchitectureCourtSuite` |
| Ordered partition edges remain ordered; reversal transposes the relational form. | `RelationalDesignSuite`, `RelationalArchitectureCourtSuite` |
| A direct relational query equals contraction of the complete effect form. | `RelationalCompilerSuite`, `RelationalArchitectureCourtSuite` |
| Lawful axis reorder and measurement composition preserve the keyed relational estimand. | `RelationalArchitectureCourtSuite` |
| Independent orthogonal partition errors yield zero while overlapping errors expose positive bias. | `RelationalArchitectureCourtSuite` |
| Noise precision requires an axis/value-bound symmetry and positive-definiteness certificate and matches an independent bilinear oracle. | `DistanceEstimandsSuite`, `RelationalArchitectureCourtSuite` |
| One compatible relation fit answers RDM, distance, and RSA without reopening evidence. | `RelationalFitSuite`, `RelationalRsaSuite` |
| Scientific identity excludes backend and scheduling; execution identity and receipts include them. | `ExecutionPlanSuite`, `ExecutionReceiptSuite`, `ScientificIdentitySuite` |
| A downstream estimand adds its own result and errors without editing a central registry. | `EstimandExtensibilitySuite`, `MvpaFacadeSuite` |
| A downstream categorical learner supplies its own configuration, fitted value, error, prediction, and compiler without editing ScalaFIM. | `PredictiveLearnerExtensionSuite`, `MvpaGeneratedLawsSuite` |
| Residual moments and precision certificates are bound to the exact neural axis and value identity they certify. | `RelationSuite`, `DistanceEstimandsSuite`, `MvpaGeneratedLawsSuite` |
| Invalid public values remain in total error channels; external callers cannot forge admitted plans, receipts, or certified capabilities. | `PublicConstructionSuite`, `AdmissionSpoofSuite`, `MvpaGeneratedLawsSuite` |
| Region, global, and searchlight execution retain typed local outcomes and scatter only from rendition metadata. | `PredictiveFrameExecutionSuite`, `RelationalFrameExecutionSuite`, `PredictiveSpatialExecutionSuite`, `RelationalSpatialExecutionSuite` |

## Vertical-slice verdict

The predictive slice proves exact leave-one-group-out classification,
train-only standardization, authoritative out-of-fold reconstruction,
dense/operator input parity, local failure, global/region/searchlight frames,
and complete receipts through the common shell.

The relational slice proves operator-native identity distance and certified
crossnobis, ordered pairing, direct/form/materialized parity, pair reversal,
RDM/RSA fit reuse, independent precision and bias oracles, local failure, and
global/region/searchlight frames through that same shell.

On 2026-08-25 the final current-tree gate passed 300 tests on the JVM and the
same 300 tests on Scala.js. It also passed all four compile/test formatting
scopes, JMH compilation, optimized Scala.js linking, three executable workflow
tests, eight hostile oracle-manifest tests, and freshness checks for all six
versioned base-R oracle families. The portable downstream court contributes
five real predictive, relational, bind-rejection, execution-context, and
capability tests on each platform.

The deterministic work court separately proves exact materialized-cell,
operator-application, convergence-iteration, and
planned/visited/completed-measurement accounting. Bounded JVM JMH diagnostics
keep fixture setup outside timed execution and compare equal relational
estimands; they are allocation observations, not timing-superiority or
Scala.js-performance claims. All evidence in this report is local; it is not a
claim about remote CI or publication.

## Prototype and duplication audit

- Canonical production code lives directly in `scalafim.fmri.mvpa`, with only
  the intentional `predictive` lifecycle boundary beneath it.
- No `next`, `prototype`, `compat`, `compatibility`, `legacy`, or `adapter`
  namespace exists.
- `BoundScientificPlan`, `AnalysisResult`, execution receipts, and certified
  relation capabilities have no public bypass constructor. The cutover
  removed every public `unsafe` factory in the MVPA API; trusted constructors
  are confined to the package boundary that validates them.
- The vertical slices introduce no conversion to the retired source, split,
  feature-set, per-region, payload, or result models.
- Predictive learning is open through
  `CategoricalLearnerCompiler[C, F, E, P]`; there is no classifier registry or
  central match.
- Relational reuse has one public waist, `RelationalFit.query`, parameterized
  by a typed `RelationalFitQuery`; direct compiler entry points and duplicate
  fit/result shells are private or absent.
- `tools/verify-mvpa-legacy-surface.sh --final` rejects reintroduction of the
  retired ontology, split modules, duplicate policy, and historical generator
  surfaces.

New method work must provide an estimand or typed fit query, compiler
capability, typed result, and laws on this identified-evidence path. It must
not create a second source, frame, traversal, receipt, or result ontology.
