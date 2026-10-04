# UMVPA M4.06: frozen component arithmetic

Issue: `bd-01M2BNHDZT6TN086GDNEA6VK4K`.
Base: `41a84b88fd837ec3c348d3f245e73943da3603b3` (landed UMVPA wave 2).
Candidate branch: `work/umvpa-component-20261003`.
Worktree: `/private/tmp/scalafim-umvpa-component-20261003`.

## Implemented scope

`FrozenComponentConfirmation.freeze` binds the ordered association and
incremental families to the existing C1 contract, discovery exposure,
component projections, nuisance column declarations, and continuous target
metric. It refuses unknown/foreign exposure and incomplete or duplicate
families. The caller remains responsible for authentic physical provenance
and for identical meanings of named nuisance columns across datasets.

`ComponentConfirmation.association` computes paired, nuisance-adjusted
correlations between discovery-frozen brain and target scores. The working
nuisance design includes an intercept, even if one was not supplied. Aliased
residual scores refuse at relative tolerance `1e-12`. This procedure currently
requires independent confirmation rows. An admitted voxel covariance does
not establish a joint score distribution or permit dependent association.

`fitHeads` fits a full unpenalized OLS head and one separately refitted reduced
head per component using discovery training rows, named nuisance columns,
and the frozen brain projection. Gale pivoted QR supplies every solve, with
rank tolerance `1e-12`; there is no private solver, rank repair, or ridge.
Training rows receive equal OLS weight. The original continuous target
coordinates and their frozen diagonal metric define the prediction task.

`incremental` applies these fixed heads to confirmation data. It retains the
training column layout and intercept choice without any confirmation refit.
Loss is the **sum of metric-weighted squared target errors**, not a normalized
R-squared or a target-prior-scaled error. Improvement is reduced minus full
loss; negative improvements are retained. Rows receive equal weight within
their declared unit, and units receive equal weight in the summary. Every
declared unit must have data; any nonfinite row refuses the entire result.
There is no CV-fold pseudo-replication or silent complete-case deletion.

Results retain component and row coordinates, independent-unit mappings,
member IDs, source identities, frozen plan/head identities, and planned owned
storage. They expose `PendingFrozenProtocol`, with no significance or
population/generalization admission. A set of numeric results is not a new
uncorrected family admission.

## Resource behavior

Each operation applies each brain/target source once as a matrix projection.
The fixture operator receives one vector application per projection column
(two in this fixture). All reduced-head fits reuse training scores; all
confirmation comparisons reuse confirmation scores. No dense rows-by-voxels
brain matrix or voxelwise fit is constructed.

Each entry point checks a conservative numeric-cell budget before any source
read. Budget tests use a poison source to establish zero reads on refusal.
The bound excludes borrowed evidence, projections and heads, provider/Gale
scratch, object overhead, and process RSS. This is an allocation/read contract,
not an end-to-end benchmark or measured speedup. Runtime still includes the
cost of applying the caller's source operators.

## Independent evidence

`tools/mvpa-inference/generate_component_fixtures.R` uses base-R `lm`,
`predict`, `cor`, and `tapply`; it does not call the Scala implementation.
The deterministic Walsh fixture has two correlated components (correlation
0.5), two response targets, intercept/drift nuisance, and target metric
`diag(1,2)`. Analytic conditional variances are 0.75.

- Equal-row held-out improvements: `4.5`, `12.75`.
- Coefficient deletion instead gives `6`, `17`, demonstrating the different
  estimand when the reduced head is not refitted.
- Equal-unit improvements for unit sizes 6 and 2:
  `3.3066243270259346`, `12.1726497308103738`.
- Full mean held-out loss: `0.375`.
- Paired nuisance-adjusted correlations:
  `0.7977240352174656`, `0.8571428571428571`.

The shared suite additionally checks omitted intercepts, negative predictive
usefulness under held-out distribution change, preserved training reads,
wrong rows/nuisance bindings, unknown exposure, missing/nonfinite outcomes,
incomplete units, rank deficiency, perfect correlation, and alias refusal.
Tolerances are `1e-12` for simple correlations/coefficients and `1e-11` for
compound loss calculations on this unit-scale fixture.

Initial JVM run: 9/11 passed; two fixture constructions incorrectly declared
repeated rows as independent. The fixtures now use the existing known block
covariance contract; no production admission was weakened. Focused rerun:
11/11 passed. Base-R 4.5.1 oracle passed.

Full owning-module gates passed on both platforms, without provider overrides:

- `python3 tools/build/sbt-warm mvpaJVM/test`: 391 passed, zero failures.
- `python3 tools/build/sbt-warm mvpaJS/test`: 391 passed, zero failures.
- `LC_ALL=C Rscript tools/mvpa-inference/generate_component_fixtures.R`: passed.
- `git diff --cached --check`: passed.

All 11 new shared tests ran on each platform. The full JVM and JS logs contain
no test failures, skipped tests, or compiler warnings. The initial cold build
reported an existing upstream `linops4s/build.sbt` task-linter warning; the new
Scala sources compiled without warnings. These are local engineering checks
and author validation, not an independent peer review or hosted CI run.

Raw logs and exit-status sidecars live under `/private/tmp/`:
`umvpa-component-jvm-full.log`, `umvpa-component-js-full.log`, and
`umvpa-component-r-oracle.log` (the R command has no wrapper sidecar).

Verified source/log SHA-256 manifest:

```text
3a3e40a6751cfb84a4646b1728bbcc62eb316f4fb3c1260781aeb419127c781d  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/pattern/ComponentConfirmation.scala
97fda79bc9bde794e86a735a7d12b7450255bfe8b41445618dd96e6d9b2b3494  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/pattern/ComponentConfirmationSuite.scala
d9fdddadb2af3f37b28037704d9cf99343c07a5e9bc658dc377f6f155663332f  tools/mvpa-inference/generate_component_fixtures.R
79b09b0b02de2aa81c5eca8d951934a9e1fe0b0a15e0742070c796b29bc4b93f  /private/tmp/umvpa-component-jvm-full.log
32461aab09cd94a3f150635690b3790e1ae8a0a69721a9ab2822de690f56798f  /private/tmp/umvpa-component-js-full.log
c908367162557ad9924cc9ddf516711f9639b3bf5786effdf452a3eeba8152a7  /private/tmp/umvpa-component-r-oracle.log
```

## Remaining acceptance boundary

This is the arithmetic and leakage/resource portion of M4.06, not completion
of the entire ticket or epic. Component p-values, uncertainty procedures,
family-complete randomization/multiplicity integration (M4.08), and the frozen
Monte Carlo calibration (M4.09) remain outstanding. Categorical loss/head
fitting and dependent paired association return typed unavailable outcomes.
Neither this author validation nor the deterministic R fixture substitutes
for independent scientific calibration or the final release audit.

No provider pin changes, push, merge, or publication are part of this work.
Next: connect these frozen per-unit loss and paired-score procedures to an
admitted reference/randomization contract, then qualify them under the frozen
inference protocol before exposing inferential claims.
