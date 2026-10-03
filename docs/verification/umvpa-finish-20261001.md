# UMVPA predictive qualification and query reuse, 2026-10-01

Local candidate over `48453314884f0109c03721fa4ffcab5412a1ccdd`, branch `work/umvpa-finish-20261001`. This report qualifies M1.11 and M2.06 only. Epic completion and landing are separate gates.

Provider pins: Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`; Multivar `f74d631720d65147c51496dcbdd37c01912de1cb`; Alder `e555bad92307af1c2cbc104aef398cb9d9de88f0`; resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

## M1.11

Independent source review found native-origin receipts discarded from feature-model and cross-decoding results. Results now retain the actual preparation receipts. New shared scenarios cover both feature directions, all three cross-domain heads, reordered/foreign grouping, unequal held-out groups, composed repeated draws, duplicate occurrence-key refusal, and volume/surface frame → native prediction → scatter. A scalar training-only standardization/centroid/softmax oracle checks selected feature values and predictions. A real missing-class failure preserves later successes and is excluded from overlap denominators; resources close exactly once. Existing frozen PyMVPA, poisoned-target, training-scope, operator/dense and metadata-only zero-read suites remain in the owning gates.

The mvpa-spatial dependency on mvpa-dataset is test-scoped (`test->compile`). Production dependencies are unchanged. The independent reviewer approved this bounded acceptance slice after the integration repairs, contingent on the actual final gates below.

## M2.06

Query reuse validates exact declared dependencies and both retained/current metadata budgets before callbacks. Changed value identities, metrics, targets, folds, ROI scopes, parameters, randomness, fidelity and implementations propagate causal invalidation; unspecified evidence remains unknown. Explanation paths are bounded. Typed rectangular low-rank contractions use scalar column reads and compensated traces. All-distinct products use compensated statistics with one global ordered-pair fallback under coordinate or scalar cancellation. Independent reviewer counterexamples exposed and repaired trace, budget and cross-coordinate cancellation failures; exact fixtures, permutations and swapped endpoints are tested.

The reviewer approved final QueryReuse source/tests after the last two fixes (explanation accounting and global cancellation). This establishes the tested numerical and declaration contract, not provider payload authentication, universal floating-point accuracy, process-memory bounds or statistical unbiasedness.

## Runtime evidence

All commands used the warm runner from the main checkout, with this isolated worktree as cwd. The final owning gates passed 789 tests: MVPA 285 JVM + 285 JS, dataset 77 JVM + 77 JS, spatial 33 JVM + 32 JS. The spatial difference is the existing JVM-only platform suite. Full `scalafimCompileAll` passed on both platforms. Complete successful raw logs contain no `[warn]` or `[error]` lines.

| Raw evidence | Exit | Totals | SHA-256 |
|---|---:|---|---|
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/predictive-query-jvm-06.log` | 0 | 283/77/33 JVM; final MVPA superseded by08 | `bbb6b513a5ec467eca7938700c9cd4656fc30b04f405e6ca6ba5609d015a306c` |
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/mvpa-final-jvm-js-08.log` | 0 | 285 JVM,285 JS | `1680d89404c421760599f7be80b4180887e94450efe8d3cc26d0f14b0b11b953` |
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/predictive-spatial-js-09.log` | 0 | 77 dataset JS,32 spatial JS | `14e65a9e0ed48d220d45bbd46d88486bf93743c9bc5ff09e3c7c81d5ed8f4545` |
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/compile-all-10.log` | 0 | whole repository JVM+JS compile | `324d3fb299b1c0d7c330ca063da1298686c38d932399f3c905132f4054fc495c` |

Earlier failed runs are preserved in the same evidence directory: rejected incomplete optimizer draft compile03, invalid test fixture04, erroneous test expectation05, and heap-thrashing JS07 stopped on this session’s owned server. They are not passing evidence. Restarting this worktree’s server with6GB heap resolved the build limit.

## Source identity

SHA-256 values bind the reviewed source; the report is included in the local commit.

| Path | SHA-256 |
|---|---|
| `build.sbt` | `121b8384cc47cb46e81a0cdbec110ebdd2ecfbafd5128f204078d7dd936353df` |
| `modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderCrossDecoding.scala` | `3172a8035d442a833fa8c89b9f9da68bd2f9f4ef15e6c0456827da1180988e47` |
| `modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderFeatureModel.scala` | `de18209d32b43f20fd70176c21c502219246c7fe7e8727b492f46bdd7515a62c` |
| `modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/GroupedPredictiveAcceptanceSuite.scala` | `fcb6ae36e6f386e7575cf81fc1416d153c01b16b70474bff087825b62f1bad18` |
| `modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/PredictiveOriginsAcceptanceSuite.scala` | `d749e32873e024c37ff8542b9d1ae56cf21170bc9e5da1243ceef6ff417d256e` |
| `modules/mvpa-spatial/shared/src/test/scala/scalafim/fmri/mvpa/spatial/SpatialPredictiveAcceptanceSuite.scala` | `52afe509684753b58dab8b2631d000739a4f6bd7d3b895820099120d4b056e18` |
| `modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/QueryReuse.scala` | `d24727dee20b4106eb8a9e7d4b5941098611d412e10c17a8fda848d7b2203c6a` |
| `modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/QueryReuseSuite.scala` | `317a06c166de69c8e764256e9c531611ce820d76bbb4a70eaacd540faecc4205` |
