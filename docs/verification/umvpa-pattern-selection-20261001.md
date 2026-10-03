# UMVPA M3.07 training-scoped model selection

Source base: `b1c5b0ae205c13e97a1218ab6852a29783872f6a`; isolated branch `work/umvpa-finish-20261001`. Pinned providers: Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`, Multivar `f74d631720d65147c51496dcbdd37c01912de1cb`, Alder `e555bad92307af1c2cbc104aef398cb9d9de88f0`, resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

Mote packet `bd-01M2BNGGJM58RH244YCGMGZDVT`. The bounded materialized adapter executes actual Alder learner/search lifecycles over identified training populations. TwoStagePatternFit obtains a structured pilot under identity Psi, fits diagonal-plus-low-rank covariance to its centered training residuals, and reruns the same structured optimizer under that fixed covariance. It is a two-stage procedure, not joint optimization of mean and covariance.

Every learner centers neural/target values, estimates categorical priors or empirical continuous target Sigma, and fits residual covariance using only its supplied native training rows. Classification retains an unweighted zero-sum codebook and restores neural intercept meanX - A C^T meanH. Continuous prediction restores the training target mean. Singular target priors, missing training classes, unknown target codes and failed numerical candidates refuse explicitly. Inner designs are bound to each outer training axis; native exact partitions require the actual declared analysis complement and refuse unsupported purged/ordered plans rather than replacing them. Outer assessment values never enter the inner learner/search.

Native metadata checks refuse same-sized foreign neural and regression target axes before fitting. Alder's declared-source/selected-ID fingerprint is preserved separately from a canonical digest of actual training inputs/targets. Serving identity also frames target means, task/codebook semantics and numerical identity. Ordered candidates, all solver/covariance policies, spatial graph and inner-plan receipts bind the plan. Changing only outer assessment targets preserves the corresponding fit's training content, serving identity and factors; shifting training target means under the same declared provider fingerprint changes serving identity and predictions.

Native Alder search minimizes equal-fold mean row task loss: misclassification fraction or uniform-coordinate mean squared regression error. Failed folds invalidate candidates. Fixed costs are O learner calls; nested costs sum_o(C J_o + 1). Each successful learner uses two structured fits and one covariance fit call; covariance sensitivity models are counted separately. Real fixtures check fixed 2 versus nested 10 (or 14 for three candidates) calls and rich fitted artifacts/audits.

Independent Gaussian fixtures yield held-out MSE 1/7 for Psi=I3 and A=(2,1,-1); two-response A^T A=[[6,1],[1,2]] gives posterior covariance [[3,-1],[-1,7]]/20 and average MSE 1/4. Correlated Psi=I3+4*1*1^T yields A^T Psi^-1 A=62/13 and MSE 13/75. Actual rank, covariance-floor, support/signed-penalty and sparsity candidates converge numerically yet differ in held-out loss; native search selects the better decoder. Unbalanced classification checks priors (3/4,1/4), the restored intercept and accuracy 1. These are controlled fixtures, not general predictive superiority or statistical calibration claims.

Allocation admission uses BigInt before dimensions/products and refuses excessive learner-call, training-materialization, residual/covariance and retained-artifact budgets. Residual construction streams X one neural column and projects Y directly through C, avoiding p-by-p identity or p-by-q mean matrices. Centered X and residuals remain materialized. Bounds cover adapter-owned numeric cells; resident admitted rows, provider scratch, object/search-audit overhead, cumulative allocation and separately budgeted structured solver work are excluded. This does not qualify total process peak memory or streaming covariance fitting.

Independent read-only review accepted the scoped calculations, cost law and latest semantic repairs. Focused gate53 passed the initial 5 selection cases and 21 core cases per platform; later native-axis/content/rank/noise additions passed gate56 before final spatial-policy/refusal additions. Gate60 passed all 11 selection cases on JVM and JS; its subsequent core test exposed an invalid identity-baseline test call, repaired separately. Final owning gate61 is the authoritative combined latest-source gate (results below).

Source SHA-256 at gate61:

```
e721dc3ba81ad6c20f9aad5919527362931cd07f98c8b80a5e4fc2540feccbc6  build.sbt
f4edf0223a47eaf1f2ee5e376f98455e1c47593a1a9dd2bc16e1cba70fa18ed2  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/TwoStagePatternFit.scala
f0057d519eacf5a057aadcbc65378bc0d3b13c290d4a654de9924a8789c99199  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/pattern/TwoStagePatternFitSuite.scala
4993e44fd7f1ef98b150f9a8f86afd0e5d85dcd7344b53252ad2b880756ec81e  modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderPatternSelection.scala
d7a7986da7200612d2bfe48099d94f7127c22b9145af0553850d20f2f52c63b2  modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/AlderPatternSelectionSuite.scala
```

Final combined owning gate61 exited 0: mvpa 354 JVM/354 JS, mvpa-fit 49 JVM/49 JS, and mvpa-dataset 91 JVM/91 JS: **988 tests**. `scalafimCompileAll` passed both platforms in 260.6 seconds. No warning/error diagnostics occurred in the raw final gate. The frozen source manifest was rechecked unchanged.

Raw log SHA-256:

```
769b0e637ea40f32109a41fcc4383eabd0d618299f379ee97c151a9d60016666  gate61-owning-pattern-relational.log
74764ea09a6b80d58f861ca4091fff80086ab5b69b19f3bf4d4597bfbe77f659  gate-m307-two-stage-rerun.log
ec644123c50bd853644b1b92d2934849bf71e26f1c981ba7cb3ae8b3ec01c7f5  gate53-pattern-and-reuse.log
099577875b97b6ea2bac6159cf4d6273c405bfe144e8dcbfb9a8296573920c9a  gate60-pattern-and-metric.log
```
