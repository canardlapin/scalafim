# U1: optional MVPA pipeline interpretation

2026-10-16. Mote: `eco-u1-pipeline`. Baseline commit: `1412c16c2`
([U0 behavioral baseline](../ecosystem-unification-u0/README.md)).
The tested implementation files are bound by [SHA-256 hashes](source-sha256.json).

MVPA no longer depends on or imports ScalaFIM pipeline. `LocalExecution` owns
one evaluation implementation, including acquisition, compute, cleanup and
pre-commit checks. `UnitInterpreter.direct` runs it directly. The optional
`mvpa-pipeline` module wraps the supplied evaluation in the existing
`LocalPipelineRunner`; it does not duplicate scheduling or cleanup.
`PreparedVoxelFamily.run` forwards the same optional interpreter choice.

`UnitError.Pipeline` becomes `UnitError.Interpreter`; this is a deliberate
source API change. No scientific identity/digest framing or persisted schema
was changed. The core still depends on response and the locus facade until
the separate U1 domain/provenance tasks complete; this is not the mvpa4s
repository extraction.

## Verification

```sh
python3 tools/build/sbt-warm reload \
  mvpaJVM/test mvpaPipelineJVM/test mvpaJS/test mvpaPipelineJS/test
# After adding the throwing-interpreter boundary check:
python3 tools/build/sbt-warm \
  mvpaJVM/test mvpaPipelineJVM/test mvpaJS/test mvpaPipelineJS/test
# Rebuild generated JS after the incremental-link failure, without source edits:
python3 tools/build/sbt-warm mvpaJS/clean mvpaJS/test mvpaPipelineJS/test
```

| Final-source gate | Result | Receipt |
| --- | --- | --- |
| MVPA JVM | 474 passed, 5 skipped; task exit 0 | `final-tests.log.gz` |
| Pipeline adapter JVM | 6 passed; task exit 0 | `final-tests.log.gz` |
| MVPA JS, clean rebuild | 474 passed, 5 skipped; task exit 0 | `clean-js-tests.log.gz` |
| Pipeline adapter JS | 6 passed; task exit 0 | `clean-js-tests.log.gz` |

960 passing test executions, ten opt-in skips. The initial successful run
(`tests.log.gz`, exit 0) preceded the last fault-injection check and is bound
by `initial-source-sha256.json`. The next run (`final-tests.log.gz`, exit 1)
passed the two JVM tasks but failed the new JS case: generated JS bypassed
the supplied interpreter and called the direct evaluator. The retained
[generated-code excerpt](failed-generated-js.txt) shows that discrepancy.
A clean MVPA JS rebuild with **unchanged source** passed the full core and
adapter suites (`clean-js-tests.log.gz`, exit 0). This establishes that the
failure depended on generated build state; no compiler/linker fix is claimed.
All three full logs and their corresponding `.meta.json` command/timing/exit
receipts are retained. Read a log with `gzip -dc NAME.log.gz`.

The existing 16 execution cases and 12 voxel-family cases still pass. The new
core compile test requires `scalafim.pipeline.RunContext` to be unavailable,
so an accidental transitive pipeline dependency fails this boundary check.
A second new core test checks an interpreter that throws after evaluation:
cleanup still runs and already committed coverage survives as a typed failure.
An external-package adapter suite uses only public evidence/plan constructors
and checks direct/pipeline equality, deterministic seeds and ordering, exactly
once evaluation, retries, pre-commit refusal, cancellation, compute/close error
preservation and typed interpreter failures.

The five skipped cases per platform are opt-in rank diagnostics:
`RankPortabilityTraceSuite`, `RankDiagnosticReplaySuite`,
`RankConfirmationSuite`, `RankPairedPilotSuite`, `RankV2ResourceProbeSuite`.
Their fixture/environment guards are unchanged. These skips do not become
scientific qualification. Existing Intaglio build-load warnings about 18
unused documentation `tags` settings remain; changed Scala sources compiled
without warnings.

`git diff --check` and `bash -n tools/ci/full-repository-tests.sh` pass. The
root aggregate, compile/test aliases, bounded CI batches, README and module
map include the optional module. Inspection found no other production caller
relying on MVPA's former transitive pipeline dependency; fit's pipeline tests
already declare their own test dependency. No native display, benchmark,
full-repository release, provider publication or downstream application gate
is claimed by this packet.

Next ready implementation paths are `eco-u1-domains`, `eco-u1-provenance` and
`eco-u2-coordinates`. They retain the U0 ownership decisions and exact-order,
identity, resource and deletion requirements.

Mote `fsck --json` exited 0 after completion: 25,520 operations checked,
no filename/hash/JSON/operation-shape errors. The [receipt](mote-fsck.json)
is retained. U0, its two child tasks, and `eco-u1-pipeline` are closed.

Native `mote authority status --json` also exited 0 with the original enabled
authority/version/genesis ([receipt](mote-authority.json)). The commit carries
the complete shared admission prefix, including concurrent warm-salvage
coordination operations; that agent's audit files are left untouched.
