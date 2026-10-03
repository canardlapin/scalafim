# UMVPA M3.06 prediction heads verification

Source base: `677c3eeeb7c4de2f76faef424007799a68e8b240`; isolated worktree branch `work/umvpa-finish-20261001`. Mote packet `bd-01M2BNGEK6DP1GZ3MFQSQARXVK`.

The same experimental fitted factors now expose raw precision filters/scores, unshrunk calibrated component scores, Gaussian posterior component scores and target means, categorical likelihoods with ordered class priors, and forward encoding. Explicit target covariance is supplied in declared target coordinates; target scale metadata is not silently interpreted as variance. All prediction solves have component dimension r. Alder attaches actual input-dependent predictive pipes and checks the fitted training fingerprint, sample-axis/native-row mapping and supported head before completing training.

The Gaussian decoder computes `W B^-1 L^T u`, with `Phi=L L^T`, `B=I+L^T G L`, and `L W^T=C^T Sigma`, using Gale factorizations. It avoids routing decoding through posterior component values that can underflow under factor rescaling. Independent fixtures compute the joint neural Gaussian inverse separately, including a rank-two noncommuting Phi/G case and a representable 5e-251 mean whose intermediate component posterior underflows. Direct categorical scores, intercepts, deficient filter calibration, singular/asymmetric priors, derived overflow, foreign axes and prior identity changes are tested. Independent read-only scientific review approved the bounded equations and repaired admission, subject to parent gates.

Final owning gates in `prediction-final-27.log`: mvpa JVM 308, JS 308; mvpa-dataset JVM 80, JS 80; mvpa-artifacts JVM 12, JS 4. All 792 tests passed. Durable archive round trips are JVM-specific; the shared metadata envelope is tested on JS. `scalafimCompileAll` passed (both platforms); overall exit 0 in 246.4 seconds. No Scala compiler warning/error diagnostics were present. The JVM archive tests emit the existing Java 25 `sun.misc.Unsafe`/Scala runtime deprecation warning, which is distinct from compiler warnings. The 25 log separately records the 9 core and 3 native-pipe fixtures on both platforms; that overall run failed on a subsequently corrected archive-test syntax error. The unrelated unfinished relational draft was moved outside the source roots for this qualification, after gate 26 exposed its rank-loop hang. No relational acceptance is inferred from these prediction gates.

Limits: these are experimental fixed-covariance heads, not held-out performance, optimizer-global-optimum or inference qualification. Singular target covariance or singular induced component covariance refuses Gaussian construction; singular G refuses only unshrunk calibration. Stabilization is explicitly recorded as no added jitter/ridge. Artifact admission checks covariance axis/rank capability, not authentication that externally supplied numerical Psi equals the historical fitting covariance. The current archive profile preserves factors, target geometry, centering and capability metadata; numerical Psi and explicit Sigma must be supplied again by the caller. Workspace counts cover adapter allocations and exclude provider scratch/object overhead. No neural-by-neural inverse or compulsory neural-by-target effect is created.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`. Source SHA-256:

```
66334e12fb00faaa0241b3899dce2aaf4de2a6f1be76f422673ee4cf5baa8ae0  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Observations.scala
0ebad27a78e7fef2303b1fe0801d008503e452e949abdfbfc6022db900c6c29f  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/PatternPrediction.scala
80df4ef96bf947db4254a98fda1971bc720cbf693240e1c1457ffc7ace0e8633  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/pattern/PatternPredictionSuite.scala
6dac0df03aefc1a4230c548052556d067316ab8e01d7810677530f5a6a284f48  modules/mvpa-dataset/shared/src/main/scala/scalafim/fmri/mvpa/dataset/predictive/AlderPatternPrediction.scala
0d58372881ce01ba84f71ec0e75837e531b691d04575eb956db4c8ed018f1d80  modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/AlderPatternPredictionSuite.scala
1b434a2b7a043e80a05070ec6f6335a9d53aaaeedf72dd56e8c1a1da57ab2d03  modules/mvpa-artifacts/jvm/src/test/scala/scalafim/fmri/mvpa/artifacts/PatternPredictionArchiveSuite.scala
```

Final raw log SHA-256: `59fe950dc8e09f92ec5d8d83c7ad96dc86ca1d67c38d442f231505402c51947b`.
