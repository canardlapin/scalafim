# UMVPA next frontier — 30 September 2026

Three packets are qualified on the standalone core candidate: M1.05 bounded
diagnostics, M2.01 typed partitioned relations with scoped readout ownership,
and M3.10 operational work units. M1.06 remains open with a separately tested,
explicitly materialized local Alder integration.

## Immutable candidates

| Candidate | Exact revision | Local review branch |
|---|---|---|
| Core | `4d2463b82c3546fa44bbddcb3b83f4c037e32b1b` | `umvpa/frontier-core-20260930` |
| Predictive consumer | `723268a8516518801e9c98c9ac9f354551c36532` | `umvpa/frontier-local-alder-20260930` |
| Local Alder provider | `e555bad92307af1c2cbc104aef398cb9d9de88f0` | `umvpa/native-fixed-roles-20260930` in the isolated provider checkout |

Core base is the previously qualified foundation
`e57997c26fb749b5cf7d7ef15c2dbc2c0643476f`. Provider base is pinned Alder
`c56a6b17989e77bdab8d57220fe3299fe9348e30`. Predictive source review covered
`326019976a5eb08e97abb3ab228cce19c6e25049`; its later commit corrects only a
documentation test count. The review branches are present in the primary object
store. Shared main, its index, existing provider pins and unrelated agents' edits
were preserved. These candidates have not been pushed, merged or landed.

## Qualification

The standalone core was built with unchanged canonical provider pins:

```
sbt scalafimCompileAll mvpaJVM/test mvpaFitJVM/test mvpaDatasetJVM/test
sbt mvpaJS/test mvpaFitJS/test mvpaDatasetJS/test
```

Commands ran in separate bounded processes with 3 GiB heap, two processors and
an isolated staging cache. Both exited zero. All-module compilation was
warning-clean. Owning test totals are 198 MVPA, 45 readout-adapter and 7 dataset
on each platform: **500 passing tests**. This is owning-module testing plus
all-module compilation, not the full repository test alias.

The local predictive branch uses
`-Dscalafim.alder.build=/private/tmp/scalafim-umvpa-20260929/alder-fixed-rows`.
Its owning modules pass 198/45/16 tests on each platform (518 total), including
nine new predictive scenarios on each. Provider data suites pass 70 JVM and 70
JS tests. These repeated common tests are not additional distinct core coverage.
Full raw logs, actual exit metadata, source manifests and artifact hashes are in
[the evidence folder](umvpa-frontier-20260930/qualification.json).

## Correctness evidence

Independent read-only expert review approved all three source revisions after
repairs to captured metadata identity, scoped cleanup ownership, escaped reads,
retry failure preservation and population binding. Details are in
[the review](umvpa-frontier-20260930/frontier-independent-review.md).

Diagnostics return bounded metadata and blocker pages without neural reads or
payload hashing. They distinguish required capability identity from source
availability and require renewed admission after relevant changes. Pagination
bounds returned output, not all metadata workspace.

Relations compose B = A Y through Multivar without eager trial images. They retain
nominal axes, raw declared revisions and selected estimability. Reader scopes
close exactly their own acquisitions, guard direct/restricted forward and adjoint
reads, preserve all nonfatal cleanup faults and cannot be closed by rejected nested
scopes. Foreign residual sources are refused before callbacks.

Execution includes full plan/measurement identities in domain-separated seeds,
canonical reducer/OOF order, exact immutable duplicate comparison, terminal close
faults and retry conflicts, typed reduction failures and explicit incomplete
coverage. Cancellation cannot erase known failures. Durable resume is refused.

Alder's new factory preserves native IDs/order and disjoint typed roles. A deferred
resampler checks declared training identity and ordered IDs before fitting. A
recording terminal learner independently verifies actual ordered OOF tuples and
own-target exclusion; serving uses distinct all-row state. Public target-blind
Transform and test-role fitting boundaries are checked at compilation.

## Remaining boundary and handoff

M1.06 remains open. Default Alder lacks the new APIs, and the local bridge admits
already materialized matrices only. Direct operator tables and matrix-native Ridge
are explicitly refused. Upstream publication, immutable pinning, canonical consumer
admission and complete native read policy remain necessary. The narrow provider
change is available as [a reviewable patch](umvpa-frontier-20260930/alder-provider.patch)
and a bundle requiring the pinned base. No foreign store or upstream branch was
modified.

Core Mote candidate: `cand-5611573MF23VAWY3GVB5FDH48R`, proposed by actual
contributor `umvpa-diagnostics-20260930`; reviewer `umvpa-frontier-20260930`;
human authorizer `bbuchsbaum`. Candidate landing remains separate from qualified
packet completion, and the earlier foundation candidate is still pending.

Six internal agents contributed in bounded waves; a separate Claude peer supplied
an isolated build diagnosis of the value-ID and DigestError failures, both reproduced
and repaired by the parent. The independent expert source review did not run builds;
all claimed final command/exit evidence is parent-produced. Additional requested
Claude diagnostics review is optional and is not counted as a completed gate.

Immutable contribution payloads, honest declared scientific source identities and
failed-acquire self-cleanup remain caller contracts. Fatal/interruption cleanup and
distributed execution are not qualified. Next ready relational packets can be
selected from Mote after the three qualified packets are completed; M1.07 remains
behind M1.06. Evidence/state authority is the Mote tracker, not this report.

Final Mote state: M1.05, M2.01 and M3.10 are closed after qualification and
integration review. M1.06 is open with its blocker note. Core candidate review
is satisfied; landing reason codes are `ancestor_pending` and
`authorization_absent`. Current ready UMVPA item is M2.03
(`bd-01M2BNFM8MNBHSAV0MBG18J2NG`). These are snapshots, not an instruction
to mutate unrelated work or authorize landing.
