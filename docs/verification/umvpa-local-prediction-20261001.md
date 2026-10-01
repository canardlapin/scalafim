# UMVPA M3.08 local prediction verification

Source base: `ca8e0676889c43e1f488f183827a051555b27cb4`; isolated branch `work/umvpa-finish-20261001`. Mote packet `bd-01M2BNGJNQSVJQM863HGBW14W3`.

Checked hard-ROI injections gather A and every low-rank noise column by parent ordinal. The local head factors marginal Psi_RR, rebuilding its precision filters and component Gram; it never crops global precision or decoding weights and never performs a high-dimensional refit. The inherited C, target prior and task rank are preserved, including m<r and m<h. A selected intercept is retained. Local input methods accept only the exact local axis and retain no observation reader or preprocessing callback. Outside-measurement preparation is refused; local-coordinate preparation is an explicit caller provenance declaration.

Nine shared fixtures include independent marginal predictions 1/4 versus cropped-precision 2/7 and cropped-filter zero, complementary noise-suppressor perturbation, singular local calibration with valid Gaussian posterior, all noise factors when m<h, reordered ROI/intercept/forward means, ordered categorical priors, and scope/resource refusals. The four-row synthetic loss is 5/32. The production assessment API checks common-parent ordinal exclusion from declared training, ROI selection and tuning, checks the training axis against the artifact binding, and binds identified observation/target/unit columns. Target weights average coordinates within each row, rows within units, and units equally. Unequal unit sizes have independent expected loss 3/16. Receipts frame actual values, column identities and scopes. Tests cover provenance changes, training/selection/tuning overlap, a foreign restored witness, zero reduction/budget, and positive weight normalization underflow refusal.

Independent read-only mathematical review approved the marginal-head derivation and found three assessment defects: omitted column provenance, disappearing positive reduction weights and unbudgeted whole-scope ordinal copies. All were repaired with regression coverage. Overlap checking and receipt writing now stream training/selection/tuning ordinals. Preparation and scope remain caller declarations; the API cannot authenticate unrecorded analyst access or establish that external Psi is the historical fitting covariance.

Final ROI source was unchanged through gates 35 and 36. Gate 35 passed 22 JVM and 22 JS focused tests (9 ROI plus 13 then-current relational tests). Gate 36 passed mvpa 341 JVM/341 JS, mvpa-fit 46/46, mvpa-dataset 80/80, mvpa-artifacts 12 JVM/4 JS: 950 tests. `scalafimCompileAll` passed on both platforms, with no Scala compiler warning/error diagnostics. Later rectangular-consumer review repairs are independently gated; no relational acceptance follows from this report. The existing Java 25 sun.misc.Unsafe runtime warning is separate from Scala compiler diagnostics.

This qualifies hard-ROI prediction and identified synthetic held-out loss under a shared learned global subspace. It does not claim maximal local information, recovery of dimensions omitted by global rank, arbitrary general-measurement support, real-data predictive superiority, model-selection qualification or inference validity. General measurements require a separately admitted covariance capability; they are not approximated by a hard-ROI injection. Adapter workspace admission uses checked BigInt arithmetic before gathering/solving and excludes resident parent values, provider-private scratch and object overhead.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.

Source SHA-256:

```
ad216ff6fa0b808322f87290b606e5c0ce42d6e10687c02439e147582e90ccd7  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/LocalPatternPrediction.scala
fa8c5339bb829483ee3d6190a9df0ab3bd6d8e1a16480820929d37fbbd3be18c  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/PatternPrediction.scala
32a244449566a11766af0a1a3601ca79538d86ec4e5ce1fcac1f6c01020e03c3  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/ResidualCovariance.scala
401aaa322b550a3f8327dd663ae7482edba879ee86b2b033951c734b524d735b  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/pattern/LocalPatternPredictionSuite.scala
```

Raw log SHA-256:

```
1be8da6a68d63076957cd7cc48340647bd87a030ad82a7940280a5800a8127ae  roi-relation-owning-36.log
1af1894f88dd34443e0cb3182bdef8c13ca4bd81513d72532b7fe91f9afc6965  local-relation-repaired-35.log
```
