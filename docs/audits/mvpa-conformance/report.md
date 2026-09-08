# ScalaFIM MVPA external-conformance study

Date: 2026-08-26

## Verdict

ScalaFIM now reproduces the matched RSA geometry in this court to floating-point
roundoff. Observation distances, fixed-precision crossnobis, signed distances,
Pearson and average-rank Spearman comparison, and intercepted OLS agree with the
original MATLAB RSA Toolbox, Python rsatoolbox, or PyMVPA wherever those tools
state the same estimand and convention.

Predictive MVPA is not yet entitled to the same verdict. Its exact
leave-one-run-out schedule, fold-local standardized-centroid predictions,
accuracy, and fixed-support results match PyMVPA, but the public ScalaFIM source
cannot bind `run` as an identified generalization axis. The executable path
currently names `samples`. This is **numeric parity, receipt failure**, not full
scientific-plan parity.

The architectural waist is largely vindicated: identified axes, typed evidence,
distinct validation and relational pairing designs, measurement frames, typed
estimands, local outcomes, and separate scientific/execution receipts all paid
for themselves in auditability and adversarial error localization. The public
construction experience is not yet elegant enough, and two public paths
allocate heavily. Those are reasons to compress and profile the one ontology,
not to add adapters, legacy surfaces, or a second “easy” architecture.

This report records **local evidence, not hosted proof**. External references
were executed in pinned local environments; the checked Scala fixture ran on
the JVM and Scala.js. No hosted workflow result or MATLAB runtime performance
claim is inferred.

## What was compared

The references are deliberately kept distinct:

- the [original MATLAB RSA Toolbox](https://github.com/rsagroup/rsatoolbox_matlab)
  at revision `91ad43359c8a6355de1dd726e69038b7e798720a`, with its numerical
  subset executed under GNU Octave 11.1.0;
- [Python rsatoolbox](https://github.com/rsagroup/rsatoolbox) 0.3.2 on Python
  3.12.10, NumPy 2.5.2, and SciPy 1.18.1;
- [PyMVPA](https://github.com/PyMVPA/PyMVPA) source revision
  `f699189b5b7e7a1bcaaf6f0a19aa077d8879b422`, reported as 2.6.5.dev1,
  translated by its upstream Python-3 build and run with Python 3.11.13,
  NumPy 1.23.5, and SciPy 1.11.4.

The canonical generated fixture has SHA-256
`807fef98da8c60719c9f86122838266a7cd5811eb4fdd906e5c4706479822e7a`.
External programs generate fixture evidence only. Production ScalaFIM does not
load their types, emulate their dispatch, or carry a compatibility layer.

## Claim vocabulary

The coverage ledger contains 29 coverage rows. Every row has exactly one of
these dispositions:

| Disposition | Rows | Meaning |
| --- | ---: | --- |
| `exact_parity` | 12 | Same estimand and conventions were directly compared. |
| `convention_equivalent` | 1 | Agreement requires a declared reversible convention map. |
| `conditional_prediction_parity` | 1 | Labels and metrics agree; raw score definitions do not. |
| `numeric_parity_receipt_gap` | 2 | Numerics agree, but ScalaFIM cannot truthfully state a required scientific boundary. |
| `reproducible_downstream` | 2 | The value is derivable from a typed result but is not a first-class estimand. |
| `intentional_divergence` | 2 | ScalaFIM deliberately owns a different policy or objective. |
| `absent_no_claim` | 7 | The capability is unsupported and no parity claim is made. |
| `pending` | 2 | Intended overlap remains untested. |

The authoritative row-level account is `coverage.csv`. “Absent” and “pending”
are not silently counted as failures or successes.

## Numerical results

All cross-reference reductions used an absolute tolerance of `1e-10`.

### Observation geometry

| Scenario | Result | Boundary |
| --- | --- | --- |
| PyMVPA squared Euclidean | exact | Raw condensed squared distance. |
| PyMVPA Euclidean | exact | Unsquared Euclidean norm. |
| PyMVPA correlation distance | max gap `2.22e-16` against Python rsatoolbox; exact Scala fixture | Per-observation Pearson correlation distance. |
| Python rsatoolbox `euclidean` | convention-equivalent, max mapped gap `8.88e-16` | The name means squared Euclidean divided by channel count. |
| PyMVPA target Pearson/Spearman | exact downstream values | ScalaFIM has no first-class observation-RDM model-query estimand. |

The explicit Euclidean convention seam matters. Comparing method labels alone
would incorrectly report disagreement or, worse, silently equate raw squared
distance with a per-feature-normalized value.

### Crossvalidated geometry and RSA

| Scenario | Maximum observed gap | Boundary |
| --- | ---: | --- |
| Python rsatoolbox versus original MATLAB crossnobis | `2.22e-16` | Fixed identity or declared precision, local `1/P` normalization. |
| rsatoolbox leave-one-out versus uniform ordered all-pairs | `2.22e-16` | Equivalent only for the balanced one-effect-estimate-per-run fixture. |
| ScalaFIM fixed-precision crossnobis | within `1e-10` for every checked pair and measurement | Precision is certified and signed null estimates remain negative. |
| Original MATLAB versus Python intercepted OLS | `4.23e-16` | Unstandardized model with an intercept. |
| Original MATLAB versus Python Pearson | at most `6.77e-17` | Canonical signed RDM vector. |
| Original MATLAB versus Python average-rank Spearman | at most `1.11e-16` | Ties receive average ranks. |

This does not claim parity for rsatoolbox's weighted-model cosine objective,
Kendall variants, whitened RDM comparison, residual covariance estimation,
bootstrap crossvalidation, or noise ceilings. ScalaFIM intentionally consumes a
certified precision rather than smuggling one estimator into the MVPA core.

### Predictive validation

The formula-matched PyMVPA classifier and ScalaFIM
`StandardizedNearestCentroid` both fit population-variance standardization and
class centroids on each analysis fold. On the declared four-run fixture:

- all analysis and assessment sample schedules agree;
- all out-of-fold predicted labels agree;
- whole-support accuracy agrees exactly;
- four identical declared supports produce identical predictions and accuracy;
- built-in PyMVPA LDA and ScalaFIM `RidgeLda` agree on labels and accuracy for
  this fixture, but raw decision values are not comparable.

The truthful Scala call with `GeneralizationAxis("run", ...)` fails closed with
`UnknownDesignAxis(run)`. That guard is correct; the source model is incomplete.
The numerical path using `samples` is therefore recorded as
`numeric_parity_receipt_gap`, not promoted to `exact_parity`.

Native sphere-searchlight neighborhood construction remains `pending`. The
fixed-support court proves local analysis execution only; it does not establish
center order, radius/boundary policy, or support identity parity between
PyMVPA's query engine and ScalaFIM's voxel/surface frame builders. A direct
cross-tool leakage-lifecycle perturbation also remains `pending`, although the
Scala predictive lifecycle passed its own held-out-data and own-target attacks.

## Adversarial and law evidence

The study records 28 cross-platform assurance rows. Every one passed on the JVM
and Scala.js. The focused runs covered:

- positive affine distance metamorphisms and explicit failure for a constant
  correlation row;
- held-out feature and own-target perturbation against an independent
  train-only oracle;
- sample permutation, class-order reversal, seed identity, missing training
  classes, and fail-closed strategy admission;
- pair reversal, direct versus materialized closure, independent axis reorders,
  measurement composition, and independence falsification;
- hand-computed bilinear crossnobis, dense/operator parity, complete axis
  identity, reindexing occurrence identity, and materialization laws;
- hostile residual capabilities, measurement-frame identity, registered
  identity-loss/reorder mutants, design coverage/capability forgery, and
  precision certification/substitution attacks.

The external scenario batch comprised nine tests in three suites on each
platform. Two additional focused law/adversarial batches comprised 46 test
invocations on each platform; four observation-conformance tests overlap those
descriptions, so these counts must not be added as unique tests. The final
complete module run passed all 310 `mvpaJVM` tests and all 310 `mvpaJS` tests.

## Performance

Performance used fixed shapes, prepared all data and plans outside timing, and
validated the same checksums before admitting samples. ScalaFIM used JMH 1.37
on JDK 25.0.1 with three 500 ms warmups, seven 500 ms measurements, one fork,
one thread, and the GC profiler. Python used nine repeats of `perf_counter_ns`.

| Matched public call | Fastest observed median | ScalaFIM median | ScalaFIM JVM B/op | Interpretation |
| --- | ---: | ---: | ---: | --- |
| correlation observation RDM, 96 x 64 | Python rsatoolbox `0.191 ms` | `3.655 ms` | `5,155,431` | Public observation construction is about 19.2x slower on this host and allocates heavily. |
| identity crossvalidated RDM, 8 x 8 x 64 | ScalaFIM `0.354 ms` | `0.354 ms` | `713,005` | Relational public path is strong; Python rsatoolbox median was `2.120 ms`. |
| Pearson RDM query, 28 pairs | ScalaFIM `0.008 ms` | `0.008 ms` | `23,328` | Query kernel is strong. |
| intercepted OLS, 28 pairs | NumPy `0.011 ms` | `0.021 ms` | `59,048` | Same small problem; no broad solver claim. |
| centroid LORO, 32 x 32 | PyMVPA `2.203 ms` | `2.431 ms` | `4,992,056` | Similar timing, but Scala retains the run-axis receipt defect. |
| 16 fixed supports | ScalaFIM `31.916 ms` | `31.916 ms` | `62,708,150` | PyMVPA was `33.833 ms`; allocation is the material concern. |

These are one-host diagnostics, not thresholds. Python `tracemalloc` excludes
most NumPy/SciPy native allocation and is not comparable to JVM bytes/op.
Original-toolbox times were obtained under Octave and are not MATLAB
performance. Scala.js has work-law evidence but no timing claim. JMH's JDK-25
`Unsafe` warning came from JMH 1.37 itself.

## Ergonomics and construction cost

Physical executable LOC were measured from marked scenario and reusable
support regions. They are descriptive, not a synthetic quality score.

| Scenario | Implementation | Direct executable LOC | Reusable support LOC |
| --- | --- | ---: | ---: |
| observation RDM | Python rsatoolbox | 18 | 2 |
| observation RDM | PyMVPA | 24 | 2 |
| observation RDM | ScalaFIM | 18 | 78 |
| crossvalidated RDM | Python rsatoolbox | 54 | 2 |
| crossvalidated RDM | original MATLAB toolbox | 19 | 11 |
| crossvalidated RDM | ScalaFIM | 20 | 182 |
| RDM-model comparison | Python rsatoolbox | 28 | 62 |
| RDM-model comparison | original MATLAB toolbox | 10 | 30 |
| RDM-model comparison | ScalaFIM | 61 | 182 |
| predictive validation/supports | PyMVPA | 21 | 70 |
| predictive validation/supports | ScalaFIM | 48 | 142 |

ScalaFIM's scientific call sites become coherent once evidence exists, and its
outputs have the best identity visibility, typed error locality, leakage
assurance, and receipt detail in this comparison. Its routine construction is
far too ceremonial. The references are terse partly because coordinate basis,
units, provenance, independence claims, and execution receipts are optional or
implicit; deleting those concepts would improve LOC while damaging the design.

The appropriate ergonomic move is a small set of task-oriented builders that
construct the same identified evidence, design, frame, estimand, and strategy.
They must return the same typed `AnalysisResult`, expose inferred/defaulted
choices before execution, and add no second plan/result hierarchy.

Reference ergonomics have their own costs:

- PyMVPA required Python-2-to-3 source translation, NumPy 1.23.5 for
  `np.float`, a visible SciPy `extradoc` shim, and assessment-order
  reconstruction because `CrossValidation` did not retain source sample keys;
- Python rsatoolbox is concise for canonical methods, but imports plotting/cache
  dependencies and carries key distance conventions partly in method strings;
- the original MATLAB calls are short and readable, but scientific identity is
  positional and Octave needs a no-op `import` plus explicit utility paths.

## Detailed findings and Mote ledger

| Finding | Severity | Consequence | Mote |
| --- | --- | --- | --- |
| `MVPA-CONF-F001` | high | PyMVPA requires its upstream Python-3 translation build. | `bd-01M0Z4BSGQRHSDGXKHR50D62KC` |
| `MVPA-CONF-F002` | high | PyMVPA requires a NumPy line retaining `np.float`. | `bd-01M0Z4BSGQRHSDGXKHR50D62KC` |
| `MVPA-CONF-F003` | medium | A visible SciPy documentation-keyword shim is required. | `bd-01M0Z4BSGQRHSDGXKHR50D62KC` |
| `MVPA-CONF-F004` | medium | PyMVPA assessment sample identity must be reconstructed. | `bd-01M0Z4BXVMSCT0MDXJ6M6AW0E8` |
| `MVPA-CONF-F005` | medium | Octave proves only the pinned MATLAB numerical subset. | `bd-01M0Z4BSGQRHSDGXKHR50D62KC` |
| `MVPA-CONF-F006` | high | Euclidean names hide incompatible square/normalization conventions. | `bd-01M0Z4BVMF6N0Z112WTYH2APMJ` |
| `MVPA-CONF-F007` | high | LOO/all-pairs crossnobis equivalence is conditional on balance. | `bd-01M0Z4BW3HZ0273VD7DJRJSF8T` |
| `MVPA-CONF-F008` | medium | Observation-RDM model comparison lacks an owned estimand. | `bd-01M0Z4BXVMSCT0MDXJ6M6AW0E8` |
| `MVPA-CONF-F009` | critical | Predictive run generalization cannot be truthfully bound. | `bd-01M0Z5MJ46C70JQ9NNN4GPMJQQ` |
| `MVPA-CONF-F010` | high | Routine public construction is too ceremonial. | `bd-01M0Z68QE7FFW1RDT8FPX2S4NT` |
| `MVPA-CONF-F011` | medium | Fixed supports do not prove native searchlight semantics. | `bd-01M0Z69N65WFHE7DCPVZXG4J9P` |
| `MVPA-CONF-F012` | medium | Observation target similarity needs an ownership decision. | `bd-01M0Z696YYJC1QRNQRF8SF2CVV` |
| `MVPA-CONF-F013` | high | Observation/predictive public calls allocate heavily. | `bd-01M0Z7JBET61VHWHKSZRJ8HZ7A` |
| `MVPA-CONF-F014` | low | The passing local gate still exposes four dataset deprecations per platform. | `bd-01M0Z8QAXS7H3MP6PES44FCTW8` |

The append-only `findings.jsonl` contains evidence and implications in full.

## Architectural consequences

1. Keep the identified-evidence waist. It caught a real run-axis modeling defect
   rather than allowing a plausible number to launder an incorrect plan.
2. Add identified grouping evidence to predictive observation sources. Do not
   weaken `GeneralizationAxis` validation or relabel runs as samples.
3. Build a concise façade over the same objects. No retired analysis, response, or fold-plan types,
   universal payload, or compatibility adapter should
   return.
4. Profile identity/result construction before changing numerical kernels. The
   relational timings localize the likely problem away from a universal
   linear-algebra rewrite.
5. Decide observation-RDM query ownership explicitly. Either add a typed
   observation query, promote into relational evidence, or document a
   downstream operation; do not add a generic stringly scorer.
6. Certify native spatial support construction separately. Fixed-support parity
   is already useful and should not be overextended.
7. Preserve deliberate non-ownership. Noise estimation, inference,
   hyperalignment, sensitivities, and permutation APIs should enter only with a
   concrete estimand and owning module—not because another toolkit has a menu
   entry.

## Reproduction and assurance boundary

`tools/reference/mvpa-conformance/README.md` gives complete environment,
regeneration, fixture, ergonomics, and performance commands. The ordinary
Scala gate must remain external-runtime-free:

1. validate the committed lock, ledgers, findings, harness hashes, and report;
2. check generated Scala fixture freshness;
3. run the portable MVPA suites on the JVM and Scala.js;
4. compile the JMH project and optimized Scala.js link;
5. run external Python/Octave regeneration separately when changing the
   canonical fixture or reference lock.

Local evidence obtained for this study includes the pinned external reference
reduction, generated-fixture freshness, focused JVM/Scala.js conformance and
law courts, complete 310-test JVM and 310-test Scala.js module runs, and the
bounded JMH/Python/Octave host receipt. The isolated unified local gate also
passed formatting, compilation, JMH compilation, optimized Scala.js linking,
and three workflow-consumer tests. It emitted four pre-existing deprecation
warnings from `modules/dataset` on each platform, so this evidence is not called
warning-clean. Hosted CI has not been observed in this checkout, and the
external Python/Octave regeneration is not an ordinary sbt-test dependency.
