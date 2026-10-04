# UMVPA M3.05 structured optimizer evidence

Local isolated candidate over `de06159303e0c10db9cb3889e2af616af07d0e58`; branch `work/umvpa-finish-20261001`. Provider pins match the predictive/query report. No landing or publication is implied.

## Implemented scientific scope

Fixed-Psi structured reduced-rank regression of centered neural X on centered target Y, with `||A_v|| <= g_v`, `C^T C = I`, envelope sparsity/weighted TV, independent signed-loading graph smoothing and ridge. Caller-declared repeatable evidence is required; single-pass sources, foreign axes, unsupported centering and non-unit continuous metric/block weights are refused. The empirical target Gram is distinct from declared target priors.

Gale provides proximal/primal-dual A/g solves, projected-gradient C solves, polar SVD and supervised target-Gram eigendecomposition. No private numerical solver family, dense neural effect F, dense X or p-by-p Psi is introduced. Bounded target coordinates retain Y and q-by-q Grams; initialization streams one X-transpose Y column at a time. It squares spectral conditioning and refuses insufficient numerical rank at relative Gram tolerance1e-12.

Completed artifacts require same-final-point A/g, TV-dual and C stationarity, cone/orthogonality feasibility and objective stability. Iteration exhaustion returns factors/trace with `artifact=None`. Shifted objectives omit a fixed data-only constant and can be negative. Warm starts retain existing coordinates and append an orthonormal complement with zero A columns. Nonconvex stationary fits make no global-optimum claim.

## Independent evidence and review

The independent read-only expert reviewed equations and allocation sites, found warm-expansion mean changes, rank-deficient polar refusal, q-squared/augmented precision overflow omissions, a mislabeled conditioning diagnostic and unbudgeted history. All were repaired. The final bounded source approval found no remaining blocker. Root additionally asserted completed artifacts for each multistart outcome, as requested by the final review. The expert ran no builds; runtime results below belong to the parent.

The14 shared regression cases include:

- Coupled Psi=[[3,1],[1,2]] with envelope/TV optimum A*C-transpose=(3,-3.5), g=(3,3.5), full objective1.425, data-only constant9 and shifted objective-7.575.
- Anisotropic target C optimum(.6,.8), objective-2.46, versus the incorrect normalized-cross-product shortcut.
- Independent four-index loading objective/gradient finite differences, including ridge and signed smoothing; envelope/TV values checked separately.
- Zero polar trial projection; warm-prefix mean preservation despite duplicated cold columns; rank expansion; independently known cold/warm penalty path maps.
- Multiple converged starts retain different stationary objectives(-4.205,-1.805), with artifact presence and no global claim.
- Operator/dense parity with counted widths; zero-read workspace/replay/foreign-axis/q-squared refusals; explicit exhaustion and config errors.

## Runtime receipts

Full owning module gates in log17 passed299 JVM+299 JS tests. Its later compile step failed on an unfinished M2.07 draft; that draft was moved outside source and is not accepted. After the final multistart assertion change, log18 passed14 focused optimizer JVM+14 JS tests and warning-clean `scalafimCompileAll`. Complete successful log18 contains no warning/error lines.

| Raw evidence | Exit | Result | SHA-256 |
|---|---:|---|---|
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/optimizer-oracles-jvm-16.log` | 0 | 14 JVM independent fixtures | `d12d7505ab760fbcdee51a1796505fd600edb675dd763acafbe1606a5c37899e` |
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/optimizer-final-jvm-js-17.log` | 1 | 299 JVM+299 JS pass; subsequent unrelated draft compile failed | `102ac55cd12b36add73df775d9c50d848c3dbc01841830b953e9a61b321edddf` |
| `/private/tmp/scalafim-umvpa-finish-evidence-20261001/optimizer-final-compile-oracles-18.log` | 0 | 14 JVM+14 JS final fixtures; compileall passes | `2dd5d87c80c9d63e7720312edd061f8dd8253664729c9a7f3907a732d71eb51b` |

## Source identity and limits

- `modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/StructuredPatternOptimizer.scala` SHA-256 `0d3000bc31e64ead791fcfa5b9b4906f26bcc78380a3611368cd06eee85e9ec4`.
- `modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/pattern/StructuredPatternOptimizerSuite.scala` SHA-256 `8c1341059faed9d0b60fbf9ad9391d5b3da233d394dc1a9d40f13f3462496690`.

Workspace is a conservative plan for adapter matrices, covariance products and retained numeric history. Provider-internal scratch, object overhead and cumulative allocation are excluded; no measured process-memory or severe-conditioning reliability claim is made. Operator-column bounds include conservative cold-start work for warm starts. Capacitance condition lower bound and diagonal variance ratio are separately named, neither asserted as the full Psi condition number. Fixed covariance, caller provenance and preparation receipts remain explicit declarations, not joint noise estimation or population-inference proof.
