# UMVPA foundation orchestration — 2026-09-29

Seven child agents were used in waves (two active children maximum): measurement core, compilation core, progress page, independent compiler review, spatial adapters, compiler repair, and spatial recovery. One spatial session was interrupted and replaced. The parent recovered missing prerequisite source, integrated the changes, independently checked arithmetic and lifecycle behavior, and ran serialized qualification gates.

## Qualified candidate

- Local repository branch: `umvpa/foundation-20260929`.
- Commit: `e57997c26fb749b5cf7d7ef15c2dbc2c0643476f`.
- Tree: `11e9ca9eba9214e5fda4bdb65ee03b6069ba75ea`.
- Base: `79a8b7d1fbe07c383bb8929a69973729c22ce64b`.
- Candidate checkout: `/private/tmp/scalafim-umvpa-20260929/candidate` (clean).
- Source imported into the primary Git object store and local branch. It is **not merged or pushed**.

M1.03 supplies typed measurement legs, dependent lazy frame traversal, spatial region/searchlight/identity adapters, and explicit scattering outcomes. M1.04 supplies open estimands with dependent result types, separate scientific/numerical/execution descriptors, capability admission, immutable receipts, and explicit product/reduction states.

The independent reviewer accepted this exact commit and tree after checking source and log hashes. Counterexamples drove repairs to structural identities, native replay admission, result/receipt construction, exact reduction totals, duplicate traversal, spatial owner typing, and nonfinite scatter arithmetic. See [independent review](umvpa-20260929/independent-review.md).

## Verification

| Gate | Result | Full log |
| --- | --- | --- |
| `mvpaJVM/test`; `mvpaSpatialJVM/test` | 173 + 27 passed | [JVM](umvpa-20260929/final-jvm.log) |
| `mvpaJS/test` | 173 passed | [MVPA JS](umvpa-20260929/mvpa-js-01.log) |
| `mvpaSpatialJS/test` | 26 passed | [Spatial JS](umvpa-20260929/spatial-js-02.log) |
| `mvpaSpatialJVM/test`; `scalafimCompileAll` | 27 passed again; full JVM/JS compilation passed | [Compile](umvpa-20260929/spatial-jvm-compile-03.log) |

Unique owning-suite executions total **200 JVM + 199 Scala.js = 399**. The extra spatial JVM rerun is not counted twice. Every process exited zero; all four logs have no compiler warning/error lines. Runs were bounded, serial within an owned slot, with isolated sbt staging, a 3 GiB heap and two processors. Other sessions' processes and caches were preserved.

[Qualification receipt](umvpa-20260929/qualification.json) records full log SHA-256 digests and exact provider pins. [Source manifest](umvpa-20260929/source-manifest.json) records selected path hashes.

## Recovery and shared checkout

The shared checkout started and remains at `8d0dd730b60a40b093fae0f7ff61996fa071a811`; its index was empty at preservation checks. No unrelated source edits were changed or staged. Our primary checkout writes are this report and its evidence directory, plus the isolated local Git branch.

Closed M1.01/M1.02/M1.E1 source was absent from the shared checkout, and recorded historical commits were unavailable. The foundation was recovered from historical patches and complete readbacks, then requalified in the current owning suites. This does not recover the missing historical commit objects. [Recovery receipt](umvpa-20260929/recovery-receipt.json) preserves that distinction.

The candidate base/provider pins differ from current shared main. Future integration must reconcile that difference explicitly; copying the entire candidate build onto main would overwrite unrelated evolution. The local branch is a qualified candidate, not proof of compatibility with the divergent current-main build.

## Scope and next frontier

Mote epic: `bd-01M2BMGRP4MSM0RRRKNTHM0H4K`. The epic and milestone acceptance gates remain open. M1.03 (`bd-01M2BNETNF8TD1NQDTW692WE9S`) and M1.04 (`bd-01M2BNEWJMN7SYF95W45SVM6X6`) are closed after bounded acceptance. [Completion records](umvpa-20260929/packet-completion.json) retain their notes. Candidate `cand-5SSQMJHFVVXBD329CMCJDKXQDB` has the worker proposal and parent integration approval, with the independent expert verdict attributed separately. [Candidate record](umvpa-20260929/candidate-record.json) remains pending solely for absent landing authorization; no authorization was requested or recorded.

Native replay remains uncertified and is rejected where required. Spatial construction materializes support metadata; lazy traversal does not prove constant-memory construction. Weighted arithmetic overflow yields a typed rejection rather than a stabilized mean. Legacy cutover, full predictive workflows, independent scientific calibration, performance, and release admission belong to later packets.

The next dependency frontier includes M1.05 bounded diagnostics, M1.06 Alder integration, M2.01 partitioned relations, and M3.10 operational work units. All four are ready in the final [Mote readiness snapshot](umvpa-20260929/next-ready.json). Check current Mote readiness and path ownership before claiming. M1.06 has a provider seam requiring qualification: repeat-callable traversal is not a replay certificate, the easy in-memory constructor materializes rows with ordinal identities, and ridge materialization reads the complete matrix. See [next-frontier evidence](umvpa-20260929/next-frontier.md); no downstream consumer admission is claimed.
