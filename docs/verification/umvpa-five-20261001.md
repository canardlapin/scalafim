# Five UMVPA implementation packets, 2026-10-01

This isolated worktree starts from accepted prerequisites at
`43a6525ae4f1009f7c61babaa1b0375f745ffb39` on `land/umvpa-20261001`.
Implementation branch: `work/umvpa-five-20261001`.
The shared dirty main checkout is not the verification tree. No push, merge,
release or publication is performed by these packets.

Provider pins are Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`,
Multivar `f74d631720d65147c51496dcbdd37c01912de1cb`,
Alder `e555bad92307af1c2cbc104aef398cb9d9de88f0` and
resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

## Implemented contracts

- **M1.10** (`bd-01M2BNF7YCCH46K478DGXX7KT5`): correlation centroid and fixed
  ridge LDA fit through actual Alder completion. Correlation retains row-offset
  normalization; it remains distinct from Swift and supplies no calibration
  claim. Cross-domain correlation, Swift and ridge-LDA heads fit source rows
  and evaluate actual target rows with a precommitted Test receipt. Native
  feature identity is checked. Operator ridge and SoftLda retain operator
  products, hard/simplex targets, role-derived restrictions and numerical fit
  receipts. SoftLda uses one extracted single-fit kernel shared with its frozen
  reference route, including training-only nuisance restriction. FeatureModel
  retains both directions, sample-SD standardization, fitted coefficients,
  named columns, repeated exact item averaging, all metrics and optional output.
  The exact 32-path caller map is `umvpa-m1-10-caller-map-20261001.tsv`.
- **M2.04** (`bd-01M2BNFP6QZMG5RCK7RGF2Z30W`): typed fixed-identity signed
  operator RDM uses every directed distinct partition pair at unit weight,
  weighted-mean reduction, frozen column-major condensed effect order and
  optional division by neural feature count. Negative values remain negative.
  Effect-local non-estimability remains visible. All endpoint origins survive.
- **M2.05** (`bd-01M2BNFRH41EPFJWS0CJYM11B3`): separately admitted residual
  precision uses the existing regularized diagonal-plus-low-rank covariance.
  Actual bound precision/source/df are checked; conditional versus descriptive
  claims are assessed separately from the identity baseline. Missing residuals,
  foreign covariance and excessive forward/adjoint widths refuse without a
  covariance or identity substitution.
- **M3.04** (`bd-01M2BNG9V3MV63PXSJ4D4QMY8N`): the conditional convex support
  update enforces `norm(A_v) <= g_v`, support sparsity and weighted graph TV.
  Signed-loading smoothing has an independent penalty defaulting to zero.
  Surface graphs use mesh triangles; volume graphs use face adjacency with
  explicit physical weighting. Gale supplies generic iterations/stopping.
  Workspace refusal precedes solver allocation; iteration-limit state remains
  explicit. Analytic cone, two-node fusion and signed-smoothing oracles plus
  checkerboard and disconnected-bank fixtures anchor behavior.
- **M3.11** (`bd-01M2BNGRSCG831C9RKMKBNACDJ`): `mvpa-artifacts` provides a finite
  completed PatternArtifact v1 profile through existing immutable archive IO.
  Matrix leaves are hash-pinned little-endian Float64; signed zero survives.
  All declared shapes, aggregate budget, metadata and leaf integrity are checked
  before numerical allocation. A fresh child JVM writes and a second child
  reopens the artifact without constructing or invoking a fitter. Exact Long
  seeds, ordered coverage, axes, policy, binding, lineage, diagnostics and
  experimental interpretation survive. Checkpoints and unavailable profiles
  explicitly refuse. No automatic upload or registry is introduced.

## Numerical and review evidence

The RDM suite includes an independent nested-loop four-effect, three-partition
oracle and the frozen legacy condensed output. Residual precision agrees with
the analytic inverse of `[[3,2],[2,7]]`. FeatureModel has a scalar independent
ridge coefficient oracle (`3/(3+lambda)` for four sample-standardized training
rows) plus frozen multiresponse predictions in both directions. Predictive
operator tests compare hard/soft coefficients and score columns by class
identity, and perturb held-out patterns, targets and nuisance rows.

Independent review found and prompted repairs for operator-ridge class-column
mislabeling, SoftLda prediction feature-axis rebinding and unchecked native
cross-domain feature declarations. The regression suites exercise all three.
The relational review found no remaining implementation defect and emphasized
that residual training scope remains declared, not independently proven.
The predictive reviewer subsequently passed correction closure against the
frozen implementation manifest. This is a bounded defect-review verdict, not
the separate M1.11 scientific qualification packet.

## Owning gates

All commands use the worktree-local `tools/build/sbt-warm` cache and retained
raw logs with actual exit-status metadata under
`/private/tmp/scalafim-umvpa-five-evidence-20261001`.

| Module | JVM | JS |
| --- | --- | --- |
| mvpa | 270 passed | 270 passed |
| mvpa-fit | 46 passed | 46 passed |
| mvpa-dataset | 71 passed | 71 passed |
| mvpa-spatial | 30 passed | 29 passed |
| mvpa-artifacts | 10 passed | 4 passed |
| estimates-io | 10 passed | 4 passed |

The six owning modules pass 437 JVM tests and 424 JS tests (861 total).
`scalafimCompileAll` passes for both platforms with no warnings or errors in
`final-compile-jvm-21.log`. Final raw gate records are
`predictive-draft-jvm-18.log`, `owning-draft-jvm-15.log` (archive gate),
`owning-js-a-19.log`, `owning-js-b-20.log` and `final-compile-jvm-21.log`,
each with retained `.meta.json` exit records.
The implementation manifest SHA256 is
`1f22de55b53f23c9f7d4cde71156b4d3cbaf3eb6f454e3fa7986ff96df86cf67`;
it binds 30 changed Scala/build files before the final gates.
The same manifest is checked in as `umvpa-five-implementation-20261001.json`.

## Scope limits and downstream packets

Declared operator source fingerprints do not independently verify payloads or
preprocessing history. A materialized feature correspondence and SourceOnly
preparation scope remain declarations; available native feature descriptors
are checked. Residual training-axis/receipt declarations are retained because
the bound residual capability has no separate typed sample-scope witness.
Conditional assumptions are never promoted to externally proven unbiasedness.

The heads use identified exact validation; FeatureModel also supports repeated
exact passes. Partial assessment families are not upgraded to complete evidence.
M1.12 remains physical legacy engine/example cutover and deletion; M1.11,
M2.08 and M3.14 remain their separate independent qualification packets.
The archive profile supports JVM filesystem persistence and the shared JS
metadata contract; JS filesystem persistence and optimizer resume are explicitly
unsupported. The persisted artifact remains `ExperimentalFitOnly` with an
unfixed coordinate gauge. These packets make no global-fit, unique-map,
inferential calibration, hosted release or downstream qualification claim.
