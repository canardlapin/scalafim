# Unified MVPA foundation: consumer evidence and admission gaps

2026-09-12. Lead-owned implementation record for M0.02
(`bd-01M2BNEA7C953AXD5FJ8YDNW9R`) and M0.03
(`bd-01M2BNEC4NBXSN193PFS0Q5BCR`).

Status: the ownership/deletion constitution is closed, the provider-stage
semantic gap is fixed, and the exact provider closure is admitted through
ScalaFIM's real root build. Alder `c56a6b17989e77bdab8d57220fe3299fe9348e30`
and Multivar `f74d631720d65147c51496dcbdd37c01912de1cb` are published on their
canonical `main` branches. The root admission court and the independent
source-manifest court both pass on JVM and Scala.js. M0.02 is complete; the
prototype remains test-only and does not freeze M1 production signatures.

## Exact inputs and adoption boundary

The production change was prepared from clean ScalaFIM remote base
`528c302e454697055bc9af31c9a6eca684f019e3`, independently of the dirty primary
checkout, and published as ScalaFIM implementation revision
`37dba04c4e6eb68ec9695c01bfb2ded5f7298826`. The root build now owns exact
source pins for Alder and Resample4s, updates Multivar to the mutually
compatible landed closure, and retains the already-qualified Gale revision. The unpublished
`mvpaFoundationAdmissionJVM/JS` module directly consumes those pins plus the
production response and locus adapters.

The standalone runner qualifies this exact candidate source closure:

| Provider | Exact Git revision | Consumer use |
| --- | --- | --- |
| Multivar | `f74d631720d65147c51496dcbdd37c01912de1cb` | `coreJVM/coreJS`: nominal witnesses, typed `Lin`/`Table`, adjoints, structural value lineage. |
| Alder | `c56a6b17989e77bdab8d57220fe3299fe9348e30` | `kernelJVM/kernelJS`, `dataJVM/dataJS`: public fit lifecycle, row views, composition-safe cross-fitting, fitted-artifact focus. |
| resample4s | `6bc4172a966c92f1b06811eac64ac2bada9fef9b` | `coreJVM/coreJS`, `designsJVM/designsJS`: distinct reindexing kinds and exact-once grouped plans. |
| Gale | `099832ff15c8a4a8fcf3398c7b779fb4bbc12434` | Actual matrices and operators; exact dynver artifact `0.1.0+99-099832ff-SNAPSHOT` required by admitted Multivar. |
| locus4s | `58c9739be51345ad9adc4bc9c9e7335023254ec9` | Existing production-pin core/data capabilities used by actual ScalaFIM locus adapters. |
| Ravel | `9c5669399ab8e2a11402e71973dd5f1e2f2c13f4` | Existing production-pin `coreJVM/coreJS` used by actual response sources. |
| linop4s | `fec77db060b130b3c609a43d07ff0bfb31088aae` | Required only to load Alder's unmodified sibling build; not a numerical consumer dependency here. |

Alder ordinary builds now resolve the three exact source revisions above.
Sibling checkouts are selected only by explicit `alder.*.build` or propagated
`scalafim.*.build` overrides. The standalone runner reconstructs the exact
trees from Git archives with verified digests and points those overrides at
its owned workspace; the root court independently proves ordinary production
resolution. Multivar and ScalaFIM agree on Gale `099832ff`.

The runner snapshots **actual current** ScalaFIM `response` and `locus-data`
shared main sources, without modification, and records their exact hashes and
working-tree status. This is an adapter consumer build, not a root-aggregate
or whole-repository qualification. A first attempt loading the whole root
encountered an unrelated cached zarr4s build-class loading failure; no shared
cache was deleted. Narrow adapter sourcing removes that unrelated load.

## Capability matrix

| Boundary | Independent evidence | Admission meaning |
| --- | --- | --- |
| Typed table orientation | Compute `R X Lᵀ` against explicitly indexed weighted values; wrong primal/dual and foreign-space compositions fail compiler checks. | Candidate API supports the intended algebra. |
| Runtime metadata | Equal-size reordered coordinates, units, basis, scale and derivation mismatches reject before poison source access. | Full metadata needs a checked ScalaFIM adapter; current `MvSpace` itself is only ID/role/dimension. |
| Dense/operator realization | Coordinate descriptors and structural value identity agree; numeric outputs agree within `1e-12`. | The entire `LinDescriptor` is **not** scientific identity: it also includes representation. |
| Reindexing | Ordered selections, arbitrary injections, draws and permutations use actual resample4s constructors; independent expected parent keys and nested occurrence addresses. | No ordinal-only sample identity; source membership survives repetition. |
| Existing response/locus bridge | Real response `DomainId`/`OrderedIndices` and locus ephemeral domain APIs are used, not replicated. | No new global identity registry; further production alignment/read adapters belong to M1. |
| Heterogeneous measurements | One- and two-dimensional local spaces packaged with dependent `Table[S, local.Id]`; independent weighted values. | No `Any`, casts, erased orientation or universal payload enum needed. |
| Multiresponse target | Actual `Table[Samples, Features]`; row restrictions preserve the feature space and match direct indexing. | A universal `Response` ADT is unnecessary. |
| Metadata-only access | Binding, bounded inspection and lazy composition leave poison-read count at zero; explicit evaluation trips it. | Unknown moments stay unknown; declared content is not verified content. No statistical fitting is performed by this prototype. |
| Grouped exact-once design | Every independent expected ordinal assessed once; train/test disjoint; each held-out fold contains one group; receipt repeatability. | Reuse resample4s coverage and receipts, not a second fold engine. |
| Batching and keys | Provider `foreachBatch` retains the same matrix row-view objects, semantic keys and provider-assigned `RowId`s. | No brain-row copy in this fixture. Does not qualify end-to-end streaming. |
| Own-target exclusion | Root and composed cross-fit encoder records every assessed key as absent from its fitting targets; full-data serving state supplies the positive control. | Public `fromDesign` binds at the actual stage; strict prebound plans still reject incompatible seeds. |
| Rich artifacts | Public target-blind preparation plus terminal learner retains typed model-specific fields via `terminalModel`. | A fitted result need not be reduced to `predict`; artifacts remain in transformed coordinates. |
| Fit roles | External-package compile controls reject fitting `Use.Test`, reading `Prepared.rows` and fitting another preprocessing stage after OOF preparation. | Consumer does not use private protocol factories or reproduce leakage machinery. |

## Stage-aware exact-plan binding candidate

**Admission status:** the bounded semantic change is commit `ac06b2d6dceb30458576721299667b161f853ebf`;
published Alder closure commit `c56a6b17989e77bdab8d57220fe3299fe9348e30`
adds immutable ordinary-build provider pins and explicit local overrides. The
ScalaFIM root and archive courts consume that closure without source overlays.

The provider adds `Resample4sResampler.fromDesign`, accepting only
`Design[Split[Selection], Coverage.ExactOnce]`. It fixes design identity at
construction, then compiles and receipts the design from the actual Alder
population and normalized stage seed at `split`. `complete` and
`fromCompiled` remain strict prebound routes and continue to reject seed,
population-size and fingerprint mismatches. Compile-negative tests retain the
boundary against incomplete, draw-based and repeated-exact designs. No
`Prepared.rows`, `Use` or private seed API was exposed.

The fresh Alder provider court passed the full affected modules after
formatting: data 64/64 and application 39/39 on JVM, data 64/64 and
application 38/38 on both Scala.js and Scala Native. The focused adapter court
contains 11 tests per platform, including own-target exclusion, generated
exact-coverage laws, deterministic receipts, typed compile failure,
target-blind prefixes and parenthesization-stable assignments. The canonical
external ScalaFIM consumer then passed all 18 tests on JVM and Scala.js,
including 8 provider/lifecycle tests. The composed positive case retains a
negative control proving `fromCompiled` still rejects the wrong composed-stage
seed. Exact provider facts are in
[`unified-mvpa-alder-stage-binding-candidate.json`](../audits/unified-mvpa-alder-stage-binding-candidate.json).

Upstream acceptance:

1. External-package consumer compiles and runs root and composed cross-fitting
   followed by a learner using only public APIs.
2. Adding a target-blind prefix, nested composition and parenthesization keep
   the documented normalized stage/assignment laws; differing scientific
   plan/seed/population binds invalidate as declared.
3. Own-target-free checks, stable row keys, exact coverage and rich terminal
   artifacts survive; deliberate incompatible precompiled plans still fail.
4. Exact immutable revision and JVM/JS evidence are consumed here. The
   canonical witness is positive and contains no `ignore`/`assume`.

The M0 packaging prerequisite is satisfied by exact source revisions and a
root-build consumer. `tools/prepare-pinned-dependencies.sh` checks out the exact
Multivar revision and bootstraps its matching Gale and Resample4s artifacts;
ordinary source dependencies do not discover siblings. This is provider build
integration, not a second local lifecycle or numerical implementation. Stable
Maven Central publication, remote provider checks, and the sparse-PCA plugin
release remain owned by broader Alder release ticket
`bd-01KYDTPZZHRP1AKP8HSVBXD581`; M0 does not misstate those as complete.

## Prototype limits and handoff

The test-only `Axis` uses a bounded, full, injective descriptor encoding rather
than pretending a short hash proves identity. It intentionally admits only
1–64 coordinates and tiny dense restriction matrices. Source revisions are
separate from coordinate identity; their payload verification remains
the typed `PayloadVerification.CallerDeclared` state (declared, not verified).
Production compact signatures, capabilities,
training-scope proofs and lazy full-brain frames still need M1 design and
tests. The prototype is not a public API to preserve.

Alder currently materializes vectors of row references and per-fold
selections. Row-view batching avoids duplicating brain arrays in the tiny
fixture; it does not establish bounded allocations for the full cross-fit
lifecycle. Resolve that limit with the M1 matrix adapter/resource contract,
not a claimed zero-allocation pipeline.

No classification performance, Haufe identity, rank inference, localization,
group analysis, whole-brain resource envelope or searchlight superiority is
qualified by these foundation tests. Those retain their own epic packets.

M0.03's implemented prototype is ready for independent M0.07 review now that
M0.02 is admitted. M1 owners should migrate these laws into the real API
and delete the temporary prototype, not maintain another identity layer.

## Execution evidence

The durable executable source is
[the standalone spike](../../modules/mvpa-foundation-spike/README.md) and
[runner](../../tools/run-umvpa-foundation-spike.mjs). The runner retains exact
inputs and logs in its temporary workspace. The checked-in
[compact evidence receipt](../audits/unified-mvpa-foundation-spike-evidence.json)
records exact source hashes, commands, log digests, prior failed attempts and
the final cross-platform counts. The root build is the production dependency
court; the standalone runner remains the independent source-manifest court.

| Root-build gate | JVM | Scala.js |
| --- | ---: | ---: |
| `mvpaFoundationAdmission` | 18 passed | 18 passed |
| Existing `mvpa` regression | 114 passed | 114 passed |
| Existing `mvpaFit` regression | 38 passed | 38 passed |
| `scalafimCompileAll` | passed | passed |

| Gate | JVM | Scala.js |
| --- | ---: | ---: |
| External consumer: provider/lifecycle suite | 8 passed | 8 passed |
| External consumer: runtime identity/inspection suite | 10 passed | 10 passed |
| Gale `LinearOperatorSuite` | 8 passed | 8 passed |
| Multivar `SemanticAlgebraSuite` | 9 passed | 9 passed |
| resample4s `AlgebraSuite` + `BackingAndPlanSuite` | 17 passed | 17 passed |
| resample4s `GroupedOracleSuite` | 5 passed | 5 passed |
| Alder `Resample4sResamplerSuite` + `CrossFittedSuite` | 16 passed | 16 passed |

Thus **18 consumer and 55 upstream tests per platform** passed. The consumer
includes the positive composed exact-plan law and strict incompatible
precompiled-plan control. No tests were ignored. Type review added
bounded source metadata/occurrence-depth checks, an explicit caller-declared
verification state and typed locus-index access; the final consumer run
includes those changes.

Executed from this repository:

```sh
./tools/prepare-pinned-dependencies.sh
sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaFoundationAdmissionJVM/test
sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaFoundationAdmissionJS/test
sbt -J-Xmx6G -Dsbt.task.cpus=2 -Dsbt.supershell=false scalafimCompileAll
sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaJVM/test mvpaFitJVM/test
sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaJS/test mvpaFitJS/test
node tools/run-umvpa-foundation-spike.mjs --siblings /private/tmp/umvpa-provider-links.JBiaij --platform both --upstream
```

The successful standalone run owns workspace
`/private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/umvpa-foundation-spike-cLyJot`;
its receipt SHA-256 is
`a7a1ec646a8e543b9b006d2c9be172f6e6d1deeca23ec2ee713404f835e0da12`.
An initial v3 setup stopped before compilation because the selected local Gale
checkout did not contain the pinned commit. Repointing the temporary link to
sbt's exact pinned staging checkout produced the green run without changing
sources. The older v2 sandbox boot-lock failure remains recorded as historical
diagnostic evidence.

Observed toolchain: macOS arm64, Java 25.0.1, Node 26.7.0; consumer Scala 3.7.4
and sbt 1.11.7. resample4s builds at its own Scala 3.3.8; standalone Multivar
uses its own sbt 1.10.5. This does not claim a separate JDK 21/Node 22 CI run.
The consumer compiles with `-Werror`; unchanged provider build logs retain
linop4s task-linter warnings, Gale Scaladoc duplicate-classpath warnings and
Git metadata probe messages from archived trees without `.git`. No warnings
were suppressed or provider sources patched to pass.

The three owned Scala sources were formatted and passed the targeted admission
format check before the final JVM/Scala.js rerun. Repository-wide
`scalafmtCheckAll` is not green on remote base `528c302`: it reports hundreds
of existing files and two existing Scala 3 parse errors outside this slice.
`scalafmtSbtCheck` likewise wants to rewrite 85 pre-existing lines in
`build.sbt`; that whole-file rewrite was discarded and is not claimed here.

Runner syntax checking and four negative CLI guards (bad platform, injected
suite command, unowned workspace, unknown option) passed in the earlier court;
the updated runner passed `node --check`. Native Mote graph readback previously
verified 75 nodes/214 blocking edges and no cycles. This admits the production
root provider graph only. No future implementation audit, scientific-method
claim, statistical calibration, or whole-brain resource envelope is claimed.
