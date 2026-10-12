# PR #51 Gale compatibility repair

The [initial integration run](https://github.com/canardlapin/scalafim/actions/runs/38151200434/job/114504855591)
failed in `mvpaFitJS / Test / fastLinkJS`, after the JVM consumer tests passed.
Multivar `edb05de01401ec0b3aea4dc1190dd3100e70ee51` compiled against the Gale
`ce84e51c` Maven artifact, while ScalaFIM linked its `6f7a1f95` source pin.
The newer Gale API adds defaulted parameters to `FirstOrderConfig.from`,
`linearCompositePrimalDual` and `smoothCompositePrimalDual`. Recompilation is
necessary: source compatibility does not preserve those binary method signatures.

[Multivar PR #3](https://github.com/canardlapin/multivar/pull/3), commit
`42f6e52be18847677fdc906b8f958bc65e5877eb`, aligns its revision-tagged Maven
dependency with exactly the Gale source revision already used here:
`6f7a1f9509a07c2b3f5b956332d39a1bc5b64c3c`, version
`0.1.0+143-6f7a1f95-SNAPSHOT`. It also handles the additional Gale stopping
states exhaustively and conservatively. Stagnation, evaluation exhaustion,
cancellation and line-search failure do not acquire convergence claims; split
failure reasons retain the exact stop name. No Gale kernel or model definition
was changed.

ScalaFIM pins that compatibility commit. Its dependency bootstrap now refuses
different or missing Gale revisions before publishing provider artifacts.
Four isolated Python tests exercise matching, mismatched, missing and truncated
pins without using the network or sbt. They run in the focused CI gate.

## Validation

Upstream `sbt compileAll testAll smokeCheck` passed: core 599 tests per platform,
IR 48 JVM / 46 JS, inference 138 JVM / 137 JS, **1,567 passes total**. The
publishedLocal consumer compiled. Both supported API snapshots are unchanged.
Source compilation has no warnings; publication retains the existing Scaladoc
classpath warning. See [upstream gates](multivar-gates.log.gz),
[API snapshots](api-surfaces.txt) and [exact Gale publication](gale-publication.log.gz).

The ordinary immutable ScalaFIM build, without sibling overrides, passed:

| Target | Passed | Explicit skips |
| --- | ---: | ---: |
| `mvpaFitJVM/test` | 45 | 0 |
| `mvpaFitJS/test` | 45 | 0 |
| `mvpaJVM/test` | 472 | 4 |
| `mvpaJS/test` | 472 | 4 |
| `mvpaFoundationAdmissionJVM/test` | 18 | 0 |
| `mvpaFoundationAdmissionJS/test` | 18 | 0 |

These six commands ran in order through `python3 tools/build/sbt-warm`, with
**1,070 passes, eight existing opt-in skips and no failures**; the original
JavaScript linking failure is resolved. See [consumer gates](consumer-gates.log.gz).
`python3 -S -m unittest tools/ci/test_pinned_dependencies.py` passed four tests;
shell syntax and whitespace checks passed, and the bounded CI inventory remains
99 supported targets in 16 batches.

The local gates do not substitute for GitHub's three required checks on the
final integration head. Scientific qualification boundaries remain unchanged:
the FGLS confirmation is still failed, the paired rank pilot remains unopened,
and the PHRF evidence remains a synthetic rehearsal.
