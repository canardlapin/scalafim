# UMVPA rank implementation and resource probe, 2026-10-08

This packet implements Steps 1 and 2 of the [prospective plan](../../plans/unified-mvpa-rank-method-plan-v2.md) and completes the eight resource fixtures for the two new methods. No pilot or confirmation streams have been consumed by this packet. Rank admission remains `PendingFrozenProtocol`.

## Method and scope

Multivar commit `edb05de01401ec0b3aea4dc1190dd3100e70ee51`, based on public commit `790e1bad390ccd95d15d9ed033ceecaed56fa307`, contains:

- Score-orthogonal canonical completion, retaining every observed root and explicitly refusing permutation cuts through tied roots. The exhaustive H2 counterexample is now 642/720 in both feature coordinates; the historical method gave 642/720 versus 672/720.
- A separately identified Gaussian interlacing reference. Under rank at most s, compare the observed unscaled Wilks tail with complete-null Gaussian blocks of dimensions `(a-s,b)` and `m=n-rank(Z)` zero-mean rows. Independent reference streams, inclusive plus-one counting and prefix-maximum closure implement the conservative argument. Reducing both block dimensions is not justified.
- A written proof/assumption record in `modules/inference/CANONICAL-RANK.md`, independent Python/base-R numerical fixtures, one-column beta-law checks and both-platform tests. Floating-point and pseudorandom execution are validated numerically; they are not a machine-verified probability theorem.

ScalaFIM defaults its candidate arithmetic to the Gaussian route and exposes the repaired permutation as an explicit research comparator. Its evidence/exposure/axis/dependence checks remain in force. `admittedDetectableRank` remains unavailable. The permutation geometry repair alone establishes no composite-null validity; the Gaussian argument alone promises no useful power.

## Integration and reproducibility

The Multivar commit is local and is preserved in [multivar-edb05de.bundle](multivar-edb05de.bundle). The bundle requires public parent `790e1bad390ccd95d15d9ed033ceecaed56fa307`; `git bundle verify` succeeded. A [draft PR description](multivar-pr.md) is prepared. Publication approval is pending because M4.07 explicitly excludes Git push/release publication from ticket authorization.

ScalaFIM's public Multivar pin is still `ab811e257dd67f77e8c3b70cb1ea600f274429a3`. These consumer changes require the tested local provider override until the new provider commit is published and the pin updated. Do not land this consumer branch as a default-build-ready change with that old pin.

Run bounded consumer gates from this worktree:

```sh
python3 docs/verification/umvpa-rank-v2-20261008/run_consumer.py \
  /private/tmp/multivar-umvpa-rank-v2-20261008 new-gate-label \
  mvpaJVM/test mvpaJS/test
```

The helper sets `JAVA_TOOL_OPTIONS=-Dscalafim.multivar.build=...` on every startup, a 3 GiB heap and four configured processors. Provider and consumer builds must run serially because a source override shares the provider's targets. Multivar's exact Gale prerequisite `ce84e51c2123abd3cddc41a5f1c9540f79199fa0` was installed locally with its own upstream publisher; ScalaFIM retains its newer `d03eb99bde389ce9bce21b8e0fc59bec9ac9b4aa` source pin.

## Validation and retained attempts

Complete Multivar compilation, its JVM/JS suites, published-local consumer smoke and API/package-boundary checks pass. The suite inventory is [multivar-tests.json](multivar-tests.json): **783 JVM and 780 JS tests**, with no failures, errors or skips. Raw test XML is archived in `multivar-tests.tar.gz`.

ScalaFIM `scalafimCompileAll` passed on both platforms using the new provider in 476.58 seconds. The final MVPA integration run passed **468 tests with three opt-in skips per platform** in 107.82 seconds. Counts and raw XML are retained in [consumer-tests.json](consumer-tests.json) and `consumer-tests.tar.gz`. This is not a full-repository test-all claim.

All attempts are retained, including the missing local Gale dependency, a stopped overlapping consumer build, a corrected consumer type annotation, and a cold sbt restart that lost an invocation-only provider override. The latter compiled against the old public pin and failed; the committed startup helper fixes it. Earlier log text claiming that a later process would inherit the running server's override was incorrect. Existing Gale/Multivar Scaladoc warnings are distinguished from Scala source compiler warnings; the consumer compile-all gate was warning-clean.

## Resource fixture boundary

`prepare_probe.py` freezes eight assignments without drawing data. `run_probe.py` requires the manifest to be committed before generation, verifies provider and source locks, checks current host headroom, then runs exactly one assigned fixture for each n80/n640, (4,6)/(6,4), intercept/three-column combination. Each fixture uses both new methods at B199 through the complete public adapter. The generator uses a fresh `scalafim/umvpa/rank-method-comparison/v2` fixture namespace.

The run is limited to one worker, four configured processors, 3 GiB heap, 4 GiB sampled resident sbt memory and 900 worker seconds. The R generator has a separate 120-second limit. Every failure is retained and there are no replacement seeds or retries. This measures implementation resources for the two new methods; it does not estimate error rates, power or historical-method timing. A live `probe/host-before.json` supersedes the earlier planning snapshot.

The manifest and 146 source hashes were committed at `72847e147efd8cae0e619e8e2fecd09ab5b4852c` before generating the fixtures. All **16 method evaluations completed**, with 199 draws per hypothesis and 796 compact fits per method/dataset. The worker took **38.47 seconds including startup**; its observed peak resident memory was **3.44 GiB**. Source hashes remained unchanged. See [process-receipt.json](probe/process-receipt.json), [probe-summary.json](probe-summary.json) and retained exact inputs, records and logs.

| n | Score-permutation median | Gaussian-reference median |
| ---: | ---: | ---: |
| 80 | 24.1 ms | 72.4 ms |
| 640 | 67.9 ms | 253.5 ms |

Each median uses only four fixtures; fixed ordering and JIT effects prevent a performance guarantee. Memory sampling begins when the sbt server socket is available and excludes early startup, the thin client and R generator. The measured peak leaves only about 0.56 GiB below the proposed process cap.

A planning sensitivity assigns measured n640 pair costs to all as-yet-unmeasured n160/n320 cases: the two new methods alone would take about **54 minutes using observed means, or 75 minutes using observed maxima**, before historical-method and orchestration costs. This is an explicit extrapolation assumption, not a runtime bound. It does **not** admit the proposed whole campaign under a one-hour ceiling.

## Next decision

Next, bind and time the historical comparator on these same eight retained fixtures without drawing replacements. Build the complete cost inventory, then either fit the proposed 64-cell study within its declared cap or make a prospective resource-plan revision before opening its pilot streams. The proposed 12,800 exploratory datasets are not yet an admitted campaign. Their results must precede procedure/sample-size selection and a separately reviewed v2 confirmation protocol. Preserve v1 as unsuccessful qualification; do not reinterpret its criteria or promote any current result to admitted rank inference.

The [prior-evidence check](prior-evidence-check.json) verifies all 632 retained artifact hashes and each earlier packet's source hashes against its recorded Git source revision. The pilot, diagnosis and mathematical-review receipts remain byte-identical. M4.07 and M4.09 stay open.
