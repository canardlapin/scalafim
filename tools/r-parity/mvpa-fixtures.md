# MVPA Parity Fixtures

The shared MVPA parity fixtures live in:

```text
modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/MvpaParityFixtures.scala
modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/MvpaRReferenceFixtures.scala
modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/MvpaParitySuite.scala
modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/MvpaPropertySuite.scala
modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/MvpaAdversarialSuite.scala
```

`MvpaParityFixtures.scala` is intentionally tiny and hand-auditable.
`MvpaRReferenceFixtures.scala` contains generated external-reference values;
regenerate its fixture blocks with:

```sh
Rscript tools/r-parity/generate_mvpa_r_parity_fixtures.R
```

Keep both fixture families deterministic across JVM and Scala.js, and prefer
adding a new explicit fixture over mutating an existing one.

Current fixture contracts:

- RDM: squared Euclidean, squared Euclidean normalized by feature count,
  Euclidean, and correlation distances are separate estimands.
- Crossnobis: raw and feature-normalized distances are separate estimands.
- RSA scoring: partial Pearson uses labeled control `RdmModel`s and aligns them
  before residualization.
- Classifiers: correlation-centroid, SWIFT-style centroid, and ridge LDA have
  simple probability oracles.
- Feature-RSA core: fold-local standardized ridge maps are checked in both
  feature-to-pattern and pattern-to-feature directions, with regional metrics
  and a searchlight execution path anchored to R-generated values.
- Naive cross-decoding: source prototypes, target row correlations, softmax
  probabilities, predicted classes, and accuracy are checked for regional
  engine execution and the specialized searchlight scanner.
- Property and guardrail tests: deterministic generated cross-decoding cases
  compare the specialized scanner to the reference engine, standardized feature
  models are checked for affine source-transform invariance, and the scanner has
  a deterministic benchmark checksum workload.
- Adversarial tests: ridge feature-model fits are checked on near-collinear
  high-dynamic-range predictors, non-finite selected features fail locally
  without poisoning clean ROIs, scanner source-data failures are matched against
  the reference engine, and degenerate zero-variance cross-decoding prototypes
  still produce finite normalized probabilities.
- Benchmarks: `MvpaBenchmarkHarness` records deterministic checksums and accepts
  an injected clock so tests do not depend on wall-clock timing.

When comparing to R or Python reference code, normalize the estimand before
checking values or timing kernels. In particular, do not compare unsquared
Euclidean distances to squared Euclidean distances, and do not compare raw
crossnobis distances to feature-normalized crossnobis distances.

Fixture generation should produce a short table with:

```text
name, item_order, estimator, normalize_by_features, expected_values
```

Timing comparisons should report the same estimator table plus the checksum
used by `MvpaBenchmarkHarness`. The checksum is not a scientific output; it is a
guard that the timed workload actually ran the intended kernel.
