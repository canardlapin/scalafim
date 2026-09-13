# Unified MVPA migration parity baseline

Recorded 2026-09-13 for M0.04
(`bd-01M2BNEE0WP2AMSQYF5D14G45Y`). This freezes migration
behavior; it does not admit a replacement implementation or claim statistical
performance.

## Source and oracle receipt

- ScalaFIM source receipt:
  `e071e83b3a23bc6f304b1e72625d6281d9cfc9ba`. The MVPA production and test
  roots were clean at the M0.01 audit; unrelated repository work remains
  outside this packet.
- Independent generator:
  `tools/mvpa/generate_migration_parity.R`, SHA-256
  `1cc21629f2020bd869a6518c945a3e7b73a28265f8954dbefe50ebd2091e7fc0`.
- Generated receipt:
  `docs/scenarios/fixtures/mvpa.migration-parity.v1.r.json`, SHA-256
  `d33dcb04f08acdbe6bf88a2be4b444611cd35eb47d562ebd0672b8f180aca79d`.
- Portable Scala constants:
  `MvpaMigrationParityFixtures.scala`, SHA-256
  `07b43b26101e02c1e4cbf3c14588515fa647a86a00841bf1bdbb47ebadff29cb`.
  Its embedded JSON digest binds the checked values to the independent
  receipt. The consuming `MvpaMigrationParitySuite.scala` has SHA-256
  `7f64430df113eb73f1802266c8a4e84e2114ca6592fe3b260e72b44687c76b28`.

The generator uses base-R arithmetic and does not load ScalaFIM or translate
its helpers. It computes sample standard deviations, class means, linear
centroid scores, stable softmax values and ordered partition-pair dot products
directly. Regeneration is deterministic and checked without rewriting the
canonical fixture:

```sh
LC_ALL=C LANG=C Rscript tools/mvpa/generate_migration_parity.R --check
```

## Frozen classifier contract

The full-fit fixture has seven training rows, three features and three classes
whose first-occurrence order is `zeta`, `alpha`, `beta`; counts are 3, 2 and 2,
so priors are 3/7, 2/7 and 2/7. It freezes all of these values plus:

1. per-feature training means and sample standard deviations with denominator
   `n - 1`;
2. class centroids after exactly one application of that fitted training
   transformation;
3. test rows transformed with the same fitted state, never test-fitted state;
4. scores `x dot centroid - 0.5 * squaredNorm(centroid) + log(prior)`;
5. stable softmax probabilities, probability-column class identity and
   first-column tie behavior; and
6. predicted class labels.

The test also evaluates a deliberate double-scaling counterfactual and requires
its probability matrix to differ by more than 0.1. That canary makes a second
Z-score application visible rather than merely restating the expected output.

The cross-validation fixture uses disjoint test folds of sizes 2, 3 and 4.
Fold accuracies are 1, 2/3 and 1/2. The frozen public result is six correct over
nine tested samples, or 2/3; the unweighted fold mean is 13/18 and is explicitly
not the estimand. Probability columns are realigned to the global first-seen
class order `b`, `a`, sample rows are returned in source-index order, and
`TestedSamples` is 9. Existing repeated-test-row averaging remains a separate
contract in `ClassificationSuite`.

## Frozen identity-metric relation contract

The relational fixture contains three conditions, three features and three
partitions. For every condition pair it computes all six ordered distinct
partition pairs `(0,1)`, `(0,2)`, `(1,0)`, `(1,2)`, `(2,0)`, `(2,1)` and
averages the signed cross-products. RDM pair order is `(b,a)`, `(c,a)`,
`(c,b)`, matching the lower-triangle order `(1,0)`, `(2,0)`, `(2,1)`.

Raw distances are `[-2/3, 10/3, 10/3]`; optional feature-count normalization
divides by three to give `[-2/9, 10/9, 10/9]`. The negative first distance is
intentional. This is the existing identity-metric signed squared-Euclidean
baseline. It must not be relabeled estimated-noise or noise-normalized
crossnobis; those claims require their own estimator and qualification.

## Tolerance and failure policy

Every compared value is finite and small, no iterative solver is involved, and
feature scales are moderate. Numeric comparisons use
`abs(actual - expected) <= 1e-12 + 1e-12 * abs(expected)`. Non-finite RDM
inputs and training folds missing a response class remain typed failures with
stable messages. Ill-conditioned solver behavior is outside this fixture and
must use its method-specific conditioning court rather than a looser global
tolerance.

## Verification

Focused fixture court:

| Platform and scope | Command | Result |
| --- | --- | ---: |
| JVM fixture | `sbt -Dsbt.supershell=false 'mvpaJVM/testOnly scalafim.fmri.mvpa.MvpaMigrationParitySuite'` | 4 passed, 0 failed |
| Scala.js fixture | `sbt -Dsbt.supershell=false 'mvpaJS/testOnly scalafim.fmri.mvpa.MvpaMigrationParitySuite'` | 4 passed, 0 failed |
| JVM owning module | `sbt -Dsbt.supershell=false 'mvpaJVM/test' 'mvpaFitJVM/test'` | MVPA 118 passed; MVPA-fit 38 passed; 0 failed |
| Scala.js owning module | `sbt -Dsbt.supershell=false 'mvpaJS/test' 'mvpaFitJS/test'` | MVPA 118 passed; MVPA-fit 38 passed; 0 failed |

The full gates protect integration but do not replace the independent
calculations above. No test passed with a caveat or was ignored, the build
emitted no compiler warnings, and no production source or public signature
changed in M0.04. The fixture lives under `docs/scenarios/fixtures` as an
auxiliary migration receipt; it is not registered as an active workflow
scenario and therefore does not claim a `ScenarioResult`-level end-to-end
qualification.
