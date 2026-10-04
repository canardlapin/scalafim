# Profile trial conditional readout, Stage 2 (2026-09-30)

Mote: `bd-01M24MSJQZH70BTAFYTXD3MPZZ`. This is a bounded public contract
slice over the required TrialBanded backend. It is not a PHRF-11 completion or
an adaptive estimator derivative.

## Contract

`ProfileTrialAxis.make` binds caller-ordered `TrialId` and `ConditionId`, trial
membership, explicitly declared nuisance `ColumnId`, selected `ScanIndex`, the
exact `ExpandedTrialDesign` source and the exact `TrialBandedPreparation` owner.
The caller supplies physical IDs because the existing expanded design does not
store them. The axis checks uniqueness, dimensions and membership. Its source
retains caller-to-canonical event row maps; no query reorders the design.

`ProfileTrialReadout.freeze` binds the actual selected chart coordinates, a
declared reference node, the native lambda, normalization and an explicit
`CorrectedReference` or `ExactShape` mode. A worker has separate mutable
scratch. Response values carry the ordered axis and physical selected rows.
Foreign or reordered axes and rows are refused even when their lengths match.
The frozen operator is linear conditional on these coordinates. The decoder
which selects them may be response dependent; its adaptive Jacobian is a
different object.

`OutputRequest.TrialQueries` takes signed `ProfileTrialSignedQuery` values
bound to the trial axis. The existing condition `SignedQuery` remains the
condition-query type. The TrialBanded backend can emit either axis at the same
shape; the condition-only backend still refuses trial requests. A query-only
readout retains J query results and no N-element trial-amplitude output. Its
wrapper always keeps separate N+F coefficient and T adjoint-row buffers and
traverses the normal operator; these scoped counts exclude other worker
storage, as detailed below. Nuisance coefficients are in native
units. Trial amplitudes and arithmetic condition means are divided by the
actual-shape kernel scale `s`; the equivalent normalized penalty is
`lambda* = s² lambda_native`.

Corrected mode uses the prepared reference inverse three times:

```
w0 = R B0 y
w1 = R (B1 y - G1 w0)
p  = w0 + w1
w  = p + R (Btheta y - Gtheta p)
```

The one residual correction is mandatory in this mode. Transpose reverses the
whole composition, including that correction. The operator returns selected
whitened-response dual coordinates, or applies the reviewed AR `Wᵀ` for
original-response coordinates. It never uses inverse whitening. ExactShape
explicitly factors at the actual coordinates and charges the factor attempt;
it is not an automatic fallback.

`PreparedBasisResidual` is the observed Euclidean residual of the compiled
basis normal equations. It is not a uniform original-family error bound. A
request for `CertifiedOriginalEquations` receives a typed
`CertificateUnavailable` refusal before output. Float32 conversion audit is
per query and reports rounding alone; query tolerance does not certify solver,
basis or original-family error. ML determinant jets and criterion selection
remain unsupported by this slice.

## Scoped storage repair

Independent review of historical candidate
`bab9a99f971a2c81acf8f0ab80f6c8aeadfb5d7b` requested this accounting repair.
The previous `workerCoefficientScratchValues` field reported only wrapper
coefficients on evaluate and added adjoint rows on transpose. Both arrays
are eagerly retained by the same worker, so that name and operation-dependent
count did not describe retained worker storage accurately.

`ProfileTrialReadoutWork` now reports `wrapperCoefficientBufferValues = N+F`
and `alwaysRetainedAdjointRowValues = T` on both operations, including evaluate
before any transpose. These count the two wrapper arrays only.
`retainedTrialAmplitudeValues` counts the optional N-value result vector and
is zero for query-only evaluation. `retainedAdjointOutputValues` counts the
separate T-value transpose result vector and is zero on evaluate. A retained
result can outlive and coexist with worker reuse; it is not worker scratch.
Condition means, nuisance coefficients and J query result values are separate
outputs excluded from these scoped counts.

The wrapper counts exclude all `TrialConditionalSolve` storage: ten N+F
Double arrays, d reference coordinates, (1+d)m reference coefficients, m
actual and m directional coefficients, `jetComponents*fineCount` kernel
scratch, C Double condition sums and C Int condition counts. Thus its retained
Double arrays contain `10*(N+F) + d + (d+3)*m + jetComponents*fineCount + C`
values, separately from those C integers. The receipt also excludes the
underlying objective and its reduction/output workspace, encoded response
(N*m trial-basis and F nuisance Double values), shared preparation/bank/basis,
caller response storage, transient whitening/build/result allocations, VM
headers and collection storage. These counts are not total worker memory or
peak memory, and are not a memory benchmark.

`TrialBandedPreparation.source` keeps the caller's exact `ExpandedTrialDesign`
referenced for identity and row-adjoint readout. The source and basis remain
reachable while the preparation, a bank, a worker or an axis retains them;
they are shared without copying, and dropping the caller's own reference does
not release them. New `retainedSourceDesignDataValues` uses the actual
`source.term.data.data.length`; `retainedSourceDesignDataBytes` is eight times
that length. The dense data normally contains T*N*m doubles. This is a scoped
data count, excluding the source's basis, membership, event row maps,
convolved-term metadata and object/collection overhead, not total source
storage.

The existing preparation receipt and `estimatedSharedBytes` cover their
packed/sparse preparation and node-reference estimates. They exclude this
retained dense source and basis; its dense data count can be added once per
shared source, not once per worker. `estimatedWorkerBytes` covers the listed
objective arrays and excludes reduction workspace, encoded responses and the
conditional solver/wrapper. `estimatedEngineBytes` adds that scoped shared
estimate to one scoped objective estimate. The API documents the remaining
grid, membership, whitening, transient, caller/result and overhead exclusions.
None is a total retained-memory bound or evidence of a bounded engine peak.

The new shared reuse regression evaluates a query, transposes it to original
response rows, then evaluates again on the same worker, with F=0 and F=2.
It checks invariant N+F and T retained buffer counts, zero trial-amplitude
output on both evaluations, a separate T-value adjoint output, unchanged query
values, the adjoint dot product and source-data counts against actual fixture
array lengths. Existing whitened-transpose and query controls also check the
separate storage fields. Numerical solve/transpose/normalization behavior and
the global 22-field attempted-work schema are unchanged.

## Reproducible evidence

The shared `ProfileTrialReadoutSuite` assembles the original `(a, gamma)`
time-row normal equations and the within-condition penalty projector using
Gale's dense factor only in the oracle. Its HRF coefficients come from the same
compiled basis as production, so these tests establish prepared-basis behavior
and do not measure family-to-basis approximation. The fixtures cover off-node
Gaussian and Cascade shapes, nuisance present and absent, singleton and
unequal condition membership, coincident trials, lambda `1e-6` and `1e4`,
two-run AR with distinct coefficients and nonunit first scales, IID, signed
trial and condition queries, near cancellation, and exact-factor parity. An
independently assembled dense normal operator with centrally differenced
shape direction checks selected transpose rows; dot products and frozen
linearity check both response domains. The established Stage 1 suite checks
roughly cubic local corrected error before its floating-point floor.

The sample consumer sequence compiled and ran in this shared suite on both
platforms. In the suite's synthetic fixture, `s`, `actual`, `rule`, and `query`
are the declared preparation, selected coordinates, normalization and bound
query:

```scala
val frozen = ProfileTrialReadout.freeze(s.objective, s.axis, actual,
  s.referenceNode, rule, ProfileTrialReadoutMode.CorrectedReference)
  .fold(error => fail(error.message), identity)
val response = ProfileTrialResponse.make(s.axis, s.axis.selectedResponseRows,
  ProfileTrialResponseDomain.Original, s.rawResponse)
  .fold(error => fail(error.message), identity)
val result = frozen.newWorker().evaluate(response,
  OutputRequest.TrialQueries(Vector(query), rule))
  .fold(error => fail(error.message), identity)
assertEquals(result.axis.trialIds, s.axis.trialIds)
assertEquals(result.work.retainedTrialAmplitudeValues, 0)
assert(result.evidence.preparedBasisNormalResidualNorm.isFinite)
```

This example uses no restricted response data.

The following gates and hashes describe the historical candidate before the
storage repair; they are retained for provenance. Current repair evidence is
recorded separately below.

| Historical gate | Result | Log and metadata |
| --- | --- | --- |
| Initial JVM public/core/output suites | 14 passed | `ROOT/logs/trial-public-stage2-jvm-r2.log`, exit 0 |
| JVM public/core/output and existing banded laws | 15 + 9 passed | `ROOT/logs/trial-public-stage2-jvm-r3.log`, exit 0, raw SHA256 `5c5bd64407f9c8826a4ecfc23ede7cca845ab1eedad0d10e5e3b0745240a7cb8` |
| JS public/core/output and existing banded laws | 15 + 9 passed | `ROOT/logs/trial-public-stage2-js-r1.log`, exit 0, raw SHA256 `b38b602326d7a429182119b283182f627d63551dd35521ff262c5b249c9a54d7` |
| Final JVM public suite after exact-transpose test addition and `scalafimCompileAll` | 7 passed, full compile passed without warnings | `ROOT/logs/trial-public-stage2-jvm-compile-r4.log`, exit 0, raw SHA256 `dbd7e032c2678ef5da1f188adcc6641cee8ed0ce4101e3cd87b4eba338d4e8df` |

`ROOT` is `/private/tmp/scalafim-execution-20260929`. Each raw log has the
same basename plus `.meta.json` with the command, elapsed time, actual exit
status, and full log path. No `[warn]` or `[error]` lines appear in the three
final logs. The 7-test JVM rerun covers the only change after the 24-test JVM
gate, which was an addition to the new suite. The final JS run covers that
addition too. No benchmark, throughput, original-family certification, ML,
or decoder-selection accuracy is inferred from these tests.

The initial sbt launch without authorized boot-cache access failed before
compilation with `sbt.boot.lock: Operation not permitted` in
`trial-public-stage2-jvm-r1.log`; it is an environment failure, not test
evidence. The tested source and accepted prerequisite identities follow.

The historical Stage 2 code and suite bytes at its final gate were:

| Path under `modules/fit/shared` | SHA256 |
| --- | --- |
| `src/main/scala/scalafim/fmri/fit/profile/TrialBanded.scala` | `12ef968c08477a1d7f778852244fad36fb553f0a5e584e3ebd8e4134493bc43b` |
| `src/main/scala/scalafim/fmri/fit/profile/TrialConditionalSolve.scala` | `27b8833ca9f4474eed2833fcbe9b4b0db57c63598438400eed6d593a19eb8584` |
| `src/main/scala/scalafim/fmri/fit/profile/ProfileTrialReadout.scala` | `b9e70ee2c525ae71ce4cb46a5bc2f931c7ac8cbe49cc8c058c3a6820115598e9` |
| `src/main/scala/scalafim/fmri/fit/profile/OutputRequest.scala` | `090265aea06b15566b1f8247cb83f726fa3614bf9170334d48e05d3b09983a90` |
| `src/test/scala/scalafim/fmri/fit/profile/ProfileTrialReadoutSuite.scala` | `5404dbc7ce02f971cfb841272b1d06dedb4a9a7c46b413bfbc5b41c6b80fe810` |

The six additional modified files in this worktree are accepted prerequisites
copied without alteration. Their exact paths and hashes are in
`ROOT/accepted-prerequisite-local-commits.json`: AR transpose local commit
`d797a0dd128a14831909b4db9df007feb345ab50` and decoder diagnostics local
commit `041514a635aa85766aa15cf4d79a9aa024cf53d8`. They are not included
in the Stage 2 commit. The source checkout is based on `498021e3`; those
prerequisite bytes are uncommitted in this worktree and must be included when
reproducing its tests. Parent integration uses its separately committed
prerequisite base.

## Current storage repair evidence

The repaired source was frozen before queueing the gates in
`ROOT/trial-public-storage-repair-freeze.json` (SHA256
`5b0cd4de0af039d096937437839c085c11c165a4b7393dd9e23a32b0898afa42`).
All 15 source/test/prerequisite file hashes stayed unchanged through both
runs. The historical candidate manifest remained unchanged. The four-path
local repair is based on historical candidate `bab9a99f971a2c81acf8f0ab80f6c8aeadfb5d7b`;
its accepted prerequisites remain uncommitted copies in this worktree as
disclosed above, and are included in the freeze and repair manifest.

| Repair gate | Result | Raw SHA256 | Metadata SHA256 |
| --- | --- | --- | --- |
| `trial-public-storage-jvm-r1.log` | 18 fit tests + 9 banded laws passed; child and wrapper exit 0 | `1e7664abc1eb93cd7c57941e7150c46d782a6f2c70b8bec4ec3a7c34d71648b6` | `cbf7a53424f22505f787a998f08f66f6a31850dc16fa1e122cecc49212095356` |
| `trial-public-storage-js-compile-r1.log` | 18 fit tests + 9 banded laws passed; warning-clean `scalafimCompileAll` passed; child and wrapper exit 0 | `3c379f48f45b5fd820b98bfaa35e1b6d05817b207f7abffc90b83039bf1e91bb` | `fccfe4fce78078c6c4bdd18c610a5a6c2decf62e16a7ba3b3fbbc452222b1bd1` |

The fit selection comprises ProfileTrialReadoutSuite (8),
TrialConditionalSolveSuite (5), OutputRequestSuite (3) and
TrialBandedWorkReceiptSuite (2), on each platform. Both complete raw logs
contain no warning or error markers; exact argv, durations and exits are in
their `.meta.json` sidecars. The source and suite bytes changed by this repair
are:

| Path under `modules/fit/shared` | SHA256 |
| --- | --- |
| `src/main/scala/scalafim/fmri/fit/profile/ProfileTrialReadout.scala` | `673c34c211a15cb30238062b25c58a67d815b2719cca7ac4f91840235eadf3e8` |
| `src/main/scala/scalafim/fmri/fit/profile/TrialBanded.scala` | `6ae7b1f62419a884cdb50e344a581f3e97296ae0c642ce1a752f68e7a507d986` |
| `src/test/scala/scalafim/fmri/fit/profile/ProfileTrialReadoutSuite.scala` | `ce5424ffe239451f037b7e131cd98bcea38945c00e45072538d7f4bb396049c0` |

Unchanged conditional solver, output contract, prerequisite and other gate
source/test hashes are in the freeze and
`ROOT/trial-public-storage-repair-candidate.json`. The verification document
was finalized with these receipts after the successful frozen-source runs;
its final hash is in that candidate manifest. Independent review and parent
integration remain separate. ML, original-family certification, full PHRF-11
acceptance and performance qualification remain open.
