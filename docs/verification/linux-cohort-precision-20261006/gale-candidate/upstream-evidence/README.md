# Paired stored-predictor residual comparison candidate

Base: published Gale54e73f8e8f1218c4cb115a850aa80e1a36c6978f. This local candidate adds a generic, shared numeric capability; it contains no ScalaFIM optimizer, callback, quota, prior, or admission-policy change.

The public workspace accepts one unchanged response, two aligned row-major binary64 designs, and the actual previous/candidate coefficient arrays. It encloses deltaPredictor=(D1-D0)beta1+D0(beta1-beta0), then sum(deltaPredictor*(deltaPredictor-2r0)). It does not subtract independently rounded RSS values or accept a common additive energy constant. Arithmetic expands finite primitive endpoints by one representable value and rejects NaN, overflow and infinite endpoints. Hot loops use reusable primitive arrays and scalar mutable endpoints, not interval objects per operation.

The optional profile certificate bounds old coefficient suboptimality by upper(||D0' r0||²)/lower(lambdaMin(D0'D0)), using outward interval Gershgorin rows. Candidate minimum minus old minimum is bounded above by returned-model deltaUpper plus the old gapUpper. Strict negativity is required. A nonpositive sufficient Gram bound is unresolved; no candidate refit, rank assertion, inverse or eigensolver is introduced. Priors remain outside the generic RSS certificate.

## Independent controls

Thirteen bit-exact witnesses retain actual stored D/beta/y, including source-log-bound synthetic cohorts from Darwin JVM, Darwin Node24.21 and hosted Linux Node24.21, plus six finite synthetic controls. The shared Scala suite recomputes two complete RSS sums as exact BigInt dyadic rationals, independently of the paired formula, and checks enclosure and direction. The source MP reports remain separate cross-checks.

- Darwin NodeV15 and Linux NodeV2 are genuinely uphill and classify Increase; neither certifies profile decrease.
- Linux NodeV15/V17 and the mathematical Darwin descent witnesses certify strict negative profile upper bounds.
- Poor previous beta gives a returned-model decrease but a true profile increase; the old-gap guard prevents false certification.
- Positive definite but non-diagonally-dominant Gram [[1,2],[2,5]] remains unresolved.
- Equality/subnormals, common-energy cancellation, dyadic column scaling, shapes, nonfinite input, finite overflow, immutable output/workspace reuse and128 deterministic paired-matrix cases are included.

The DarwinJVMV15 pair was constructed by an additional diagnostic after RemainingEvaluationQuota. It is a valid numeric witness, but it proves no quota-free domain rescue. Generic numeric truth, domain response/point binding and exact-evaluation eligibility remain separate. No admission-count increase is claimed.

## Gates

Selected new suite:23/23 JVM. Full coreJVM:786/786. Full coreJS:774/774. Both full gates ran scalafmtCheckAll and emitted no compiler warnings. The23 new tests are included in full totals, not added again. JDK21.0.12.1 and sbt1.11.7 were used with requested3g/4CPUs and private sbt global/boot/ivy state. The historical Node version was not captured; the earlier Node24.1 wording was stale. The additive [runtime correction](runtime-correction.md) records23passing tests under explicitly measured Node24.21.0, preserving the original archive/receipt. Optional native lanes were not run.

Portable patch/bundle and the source-bound receipt archive are prepared by the root coordinator/worker after gates. Publication is withheld pending concrete root review and separate scientific domain integration.
