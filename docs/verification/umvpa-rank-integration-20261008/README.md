# UMVPA published rank integration and historical timing

This continues the [implementation packet](../umvpa-rank-v2-20261008/README.md). The user authorized publication after reviewing that packet. Multivar commit `edb05de01401ec0b3aea4dc1190dd3100e70ee51` is now public on `work/umvpa-rank-v2-20261008`; [draft PR #2](https://github.com/canardlapin/multivar/pull/2) proposes it against upstream main. The PR is not merged, and publication does not admit rank inference.

## Public dependency integration

ScalaFIM now pins that exact public revision. A fresh sbt launch without a local-provider property resolved it from GitHub; the resolved checkout's Git revision was verified directly. MVPA tests pass on both JVM and Scala.js: **468 passed, three opt-in skips per platform**, with no source compiler warnings. Commands, timings, the original log and raw test reports are retained in this packet. The previous `scalafimCompileAll` result used the same exact provider source through its local override; this continuation verifies ordinary public-pin resolution and the owning-module gates.

[Upstream CI](https://github.com/canardlapin/multivar/actions/runs/37716593488) also passed its full JVM/Scala.js compile/test, API checks, binary-compatibility gate, docs/guides and published-local consumer smoke. [upstream-ci.json](upstream-ci.json) and [upstream-pr.json](upstream-pr.json) retain the observed remote state.

The required `github-canardlapin` SSH alias rejected its public key. The verified `canardlapin` CLI profile supplied a command-scoped HTTPS credential helper for the approved push. No key, token, shared account setting or repository credential configuration was changed. [publication.json](publication.json) records the transport and authorization.

## Historical comparison boundary

The historical comparator uses ScalaFIM baseline `edad5bcd65f7ba004458a6dd08c5fb3a31a3e6d3` with its original public Multivar pin `ab811e257dd67f77e8c3b70cb1ea600f274429a3`, in an isolated checkout. Production code and its existing test adapter remain unchanged. The additional harness only accepts the new fixture namespace, checks assignments, invokes the old public adapter, and retains timings and results. It implements no numerical method and is not added as a legacy production path.

Replay uses the exact eight archived input files from the preceding probe, with their hashes verified before use. It generates no new dataset and consumes no pilot or confirmation stream. Historical and repaired permutation methods use the same declared child stream; the Gaussian reference uses its separate method child. All three evaluate the same matrices, but these resource fixtures support no rate or power conclusion.

The historical harness passed JVM/JS gates (**465 passed, three opt-in skips per platform**) and was committed before replay: source freeze `db97fc1c`, validation receipt `cff792a0`, retained results `6dd0d031`. All eight evaluations completed without failure in **59.68 seconds including startup**, with **3.40 GiB sampled peak RSS**. Source and input hashes remained unchanged. [The replay receipt](historical/probe/process-receipt.json), [source manifest](historical/historical-manifest.json), readable [harness patch](historical-harness.patch) and verified [Git bundle](historical-replay.bundle) retain the historical implementation separately from current production.

Runtime limits remained one worker, four configured processors, 3 GiB heap, 4 GiB sampled sbt RSS and 900 worker seconds. Early startup and thin-client memory are outside the inherited RSS sampler's scope. An evidence-archiving attempt assumed an incorrect passing-test count and stopped after archiving JVM reports; the partial archive and its explanation are retained alongside the complete unchanged JVM/JS reports. No test or fixture was rerun to address that bookkeeping error.

## Cost decision for the paired pilot

Across the two probes, **24 method evaluations on eight identical datasets** completed without failure. [resource-summary.json](resource-summary.json) records the aggregation and explicit extrapolation assumptions.

| n | Historical permutation median | Repaired permutation median | Gaussian reference median |
| ---: | ---: | ---: | ---: |
| 80 | 54.1 ms | 24.1 ms | 72.4 ms |
| 640 | 267.9 ms | 67.9 ms | 253.5 ms |

These are four-fixture medians, with fixed order and substantial JIT/GC variation; they support no speedup guarantee. The historical and current providers retain their published dependency versions, including different Gale Maven prerequisites. These measurements cover complete public adapters, not isolated changes to one kernel.

Using n640 measurements provisionally for the unmeasured n160/n320 shapes gives about **100 minutes at observed means or 169 minutes at observed maxima**, excluding orchestration, generation and longer-run GC. The original proposed one-hour aggregate limit is unsupported. The [resource proposal](paired-resource-proposal.md) retains all 64 cells and 12,800 paired datasets, and proposes a four-hour aggregate cap with unchanged worker/memory limits. This is a prospective budget proposal, not an executable campaign manifest or a statistical qualification. No pilot/confirmation streams have been assigned or consumed.

Rank admission remains `PendingFrozenProtocol`. Prior qualification criteria, receipts and artifacts remain immutable; dated statements that publication was pending describe their original execution state.
