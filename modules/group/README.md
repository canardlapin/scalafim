# scalafim-fmri-group

Cross-compiled JVM/Scala.js group-inference core for ScalaFIM. The module owns
typed group designs, responses, weighting, contrasts, statistics, multiple
comparison control, and result values. Plotting and concrete renderers remain
application-boundary concerns.

The numerical engine checks design rank with scaled, pivoted Gale QR and
preserves coefficient units and named contrast meanings. Weighted fits process
one sample at a time, using reusable buffers and restoring physical covariance
units. Inputs and outputs are still dense; this is not a file-streaming executor.

`FirstLevel.groupData` aligns voxel results to `GroupSpace.VoxelAxis` by voxel
index and checks the requested contrast identity and strictly positive, finite
standard errors. Geometry-free axes require canonical zero-based sample indices.
Voxel-to-parcel reduction must be supplied explicitly before group assembly.
The bridge does not establish registration or recover affine provenance absent
from `TContrastResult`.

`GroupEngine.fit` and `GroupGlm.wls` return typed global errors. Weighted sample
failures are identified in `GroupFit.failures` and carried into term and contrast
results. Their map positions are unavailable (NaN); valid positions remain usable.
An entirely failed weighted map returns `Left(AllSamplesFailed(...))`. Consumers
must check availability before plotting or exporting statistical maps.

Mixed-effects estimation and inference are separate, explicit choices:

```scala
import scalafim.fmri.group.*

// data must carry valid sampling variances; rows must be independent subjects.
val model = GroupModel.mixedEffects(
  data, design,
  tau = TauEstimator.PauleMandel,
  inference = MetaInference.ModifiedKnappHartung
)
val result = model.flatMap(GroupEngine.fit(_))
```

The new constructor has no estimator or inference defaults. Modified
Knapp–Hartung multiplies covariance by `max(1, Q_RE / (n-p))` and uses `t(n-p)`.
This improves several small-sample cases but does **not** establish calibrated
inference for arbitrary precision imbalance or noisy first-level SEs. The older
`randomEffects` constructor retains DL/z compatibility. Fixed effects treat
sampling variances as known and assume no between-subject heterogeneity.
See the [repair qualification](../../docs/verification/group-repair-2026-09-08.md)
for numerical fixtures, calibration boundaries, and performance evidence.

A group result can be projected into the shared renderer-neutral plotting DSL:

```scala
import intaglio.*

final case class GroupEstimate(index: Double, contrast: String, estimate: Double, lower: Double, upper: Double)

val effects = plot(estimates)
  .aes(_.index, _.estimate)
  .geomPoint()
  .geomErrorBar(_.lower, _.upper)
  .axisTitles("Contrast", "Effect")
  .build
```

The application chooses SVG, Canvas, Java2D, or JavaFX only after the program
has compiled to a shared `Scene`.

Run the portable suite directly with:

```sh
sbt groupJVM/test
sbt groupJS/test
```


## One-sample symmetry inference

`GroupSignFlip` supplies a separate two-sided test of a common subject-effect
center. It does not modify PM estimates or relabel a sign score as a t statistic.
One valid first-level contrast per independent subject is required. Subject
errors must be symmetric conditional on selection and, when used, precisions.
Symmetric heterogeneity and unequal/noisy variances are allowed under that
contract; an arbitrary population with only a zero average is not enough.

```scala
import scalafim.fmri.group.*

val inference = for
  plan <- GroupSignFlipPlan.compile(data.subjects,
    GroupSignSampling.MonteCarlo(draws = 9999, seed = 1729L))
  result <- GroupSignFlip.test(data, GroupDesign.intercept(data.nSubjects),
    contrast = "memory-minus-baseline", plan = plan,
    assumption = GroupSymmetryAssumption.IndependentSymmetricErrorsConditionalOnSelectionAndPrecisions,
    weighting = GroupSymmetryWeighting.EqualSubjects,
    nullCenter = 0.0)
yield result
```

Use `GroupSignSampling.Exact` for complete enumeration with 2–16 subjects.
Monte Carlo signs are independent draws with replacement; p-values include the
observed identity, `(1 + exceedances)/(1 + draws)`. Exact two-sided p-values have
a theoretical floor of `2/2^n`; at n=8 this is .0078125. Ties can raise the floor.
`FixedInverseVariance` uses supplied variances but requires their sign-invariant
provenance. Equal weighting requires no first-level SE or df.

Plans sort subject IDs canonically and bind draws to those IDs. Reordered input
rows and spatial blocks reuse the same experiment. Cache admission checks byte
limits before allocation. Results retain algorithm/version, null center,
assumptions, method, seed/draws, p-value floor, score, descriptive weighted mean,
counts and identified sample failures. Do not interpret failed samples as valid
zero exceedances. No full draws-by-voxels distribution is retained.

Covariate designs, repeated subject rows and general T/F contrasts are refused.
Raw residual flipping does not inherit the one-sample guarantee. These are
pointwise p-values; a shared sign plan alone does not establish spatial FWER.
Testing different null centers permits confidence-set inversion, but this API
supplies no interval endpoints or connectedness guarantee. Keep PM/mKH as an
explicit approximate option; neither method is an automatic general GLM default.
See the [qualification report](../../docs/verification/group-symmetry-2026-09-08.md)
and [reproduction tools](../../tools/group-symmetry/README.md).


## Contrast-specific design review

`GroupContrastDiagnostics.review(subjects, design, contrast)` supplies a reusable,
response-independent review for a named scalar equal-subject OLS contrast. It
binds every output row to the supplied subject ID and reuses the same scaled,
pivoted Gale QR and contrast alignment as the fitter.

```scala
val review = GroupContrastDiagnostics.review(data.subjects, design,
  GroupContrast.term("patients"))
```

The review retains design leverage, signed standardized contrast influence,
identity-working-model variance shares, residual degrees of freedom and an
identity-working-model CR2/Satterthwaite information diagnostic. Equivalent
covariate recodings and contrast units preserve the review. The variance shares
sum to one; their inverse squared concentration is labeled
`workingEffectiveContributors`, never the independent participant count.

These values are suitable for linked design/participant/contrast plots. They
are **not** observed precision weights, residual outlier measures or an inference
admission gate. A numerically saturated subject remains visible, with explicitly
unavailable CR2 working information. The review supplies no p-values.

The [nuisance qualification](../../docs/verification/group-nuisance-2026-09-08.md)
found excess false positives for both identity-target CR2/Satterthwaite and the
specified restricted HC2 wild bootstrap, including fresh confirmation. Neither
has been added as a production inference default. Other covariance targets,
HC3 variants, weighting strategies or bootstrap schemes are different methods
and require their own qualification; this result does not reject all robust or
bootstrap inference.
