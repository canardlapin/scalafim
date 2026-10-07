# Cascade34 realization and region overlap qualification

Scope: PHRF-30 (`bd-01M26A2AWJJ0RW7D2MHBA7RPEY`) and STP-P7.05
(`bd-01M39Q59GYTZGCFVZE5PZ7CZS8`). Work starts from ScalaFIM
`7989608d32ed8283ed11f5ff3df54c7c7761a23e` in an isolated worktree; unrelated
changes in the shared checkout are excluded. `source-manifest.json` identifies
the exact implementation, tests, generator, and dependency pin qualified here.

## Cascade34

`Cascade34Realization` binds one validated family shape to the exact continuous
seven-state realization. Its two lower-triangular Erlang blocks have rates
`p = exp(a)` and `u = p * logistic(b)`. The input vector is
`(p,0,0,u,0,0,0)` and the observation row is `(0,0,1,0,0,0,-rho)`.
The same trial amplitude enters both branches. There is no division by
`1-rho` and no horizon truncation.

Transition, injection, and observation jets use the existing chart and
`JetLayout` ordering: value, `a,b,rho`, then `aa,ab,a-rho,bb,b-rho,rho-rho`.
Caller-owned buffers are fully overwritten within the required prefix, with
capacity and finite nonnegative duration checks before writes. Construction
validates the point against the supplied family chart and checks decoded rates.

The transition uses a finite Erlang polynomial, with first/second derivatives
expressed through neighboring Poisson weights. This avoids `0 * Infinity` and
preserves derivatives when the value itself underflows. Large arguments use
independently evaluated log-domain weights. The slower-rate chain uses
`logistic(-b)` to retain accuracy near equal rates.

### Independent evidence

`tools/validation/cascade34_realization_reference.py` generates the portable
Scala fixture by differentiating a **dense matrix exponential** at 80 decimal
digits with mpmath 1.3.0. This is independent of the production polynomial.
SciPy's dense exponential separately checks transition values; maximum absolute
disagreement with mpmath is `5.412337245047638e-16`. Versions and hashes are in
`oracle.json`.

The 12 fixture points cover zero and tiny intervals, ordinary and long delays,
`rho=0`, `rho=1`, almost equal rates, and underflow in each branch. All ten jet
components are checked for all matrix entries, including structural zeros.
Observable product-rule jets are checked against independent scalar Erlang
derivatives and the existing family. Differentiated semigroup checks exercise
the mixed second-derivative cross terms. Strict subnormal-scale checks supplement
the general `3e-14 + 3e-11 * abs(expected)` tolerance so a zeroed derivative
cannot pass merely because the general absolute tolerance dominates it.

The off-grid covariance test propagates state covariance over observation
intervals and compares cross-observation entries to an independent scalar
design matrix oracle `I + X X^T / 2.1`. Zero state is defined immediately before
run-start events. Start events enter once; subsequent intervals are
`(previous,current]`. Coincident independent trials with gains `+1,-1` add
covariance, and cross-branch covariance is retained. Future events are excluded.

Independent scientific review found no blocking formula or contract issue.
Its suggested strict undershoot-tail test was added before final qualification.

To regenerate the fixtures, from the repository root in a Python environment
with the versions recorded in `oracle.json`:

```sh
python tools/validation/cascade34_realization_reference.py \
  --out-dir docs/verification/cascade-overlap-20260930
```

### Limits

This qualifies a portable point-observation, impulse realization, not the
PHRF-10 finite-state likelihood backend. Box forcing, observation averaging,
Kalman/innovation inference, runtime speedups, and scientific calibration of the
shape chart remain outside this change. Covariance scheduling is test-only;
the public API provides operators. Callers must define run resets and event
admission as part of their model. Broad-chart probes are numerical checks, not
a fitted or calibrated physiological prior.

## Overlap

Generic Dice/Jaccard methods live in locus4s `Region`, not in a duplicated
ScalaFIM set algebra. The archived candidate was reviewed and applied to current
upstream main `abafedb`, with executable documentation added. The published
commit is `a67bc87c33b5da8a5dc2cad49919c015b59f3050` on
`feature/region-overlap`; local, tracking, and remote branch hashes agreed.
[Draft upstream PR #6](https://github.com/canardlapin/locus4s/pull/6) preserves
the review boundary. No main merge or artifact release was performed.

Same-owner typed methods return Dice `2|A intersect B|/(|A|+|B|)` and Jaccard
`|A intersect B|/|A union B|`. Two empty regions return 1. Checked methods enforce
the same live owner even for empty inputs; matching persisted identity requires
explicit alignment/rebinding. Counts widen before arithmetic. Sparse overlap
uses the existing intersection allocation; compact empty/whole inputs stay
constant-size.

Owner tests exhaust all 16-by-16 pairs of subsets of a four-point domain against
independent Scala sets, check `Int.MaxValue` compact domains, and reject unsafe
cross-owner comparisons. `checkAll`, `testFullOptJS`, and `docsCheck` passed:
87 JVM, 87 JS, and 87 optimized-JS tests, plus the executable guide. Scaladoc
reported repeated-classpath flag warnings; strict compilation passed.

ScalaFIM pins the published immutable revision. Its public region aliases are
exercised by `RegionOverlapConsumerSuite`; atlas overlap keeps its existing
efficient one-pass neuroimaging reporting implementation.

The first consumer run passed but loaded both the new locus4s build and
image4s's older locus4s source pin. To remove this ambiguity, image4s changes
only its locus4s pin, in published commit
`03288b6d5c4e8c24d3fcfa22df1e977839f007d0`
([draft image4s PR #14](https://github.com/canardlapin/image4s/pull/14)). ScalaFIM pins that image4s commit
as well. Its affected locus boundary passed 38 JVM and 36 JS tests, and
`scalafmtSbtCheck` passed. Repository-wide image4s `fmtCheck` failed on three
unchanged geometry files; `image4s-format-baseline.json` proves their bytes
match the base commit. This pre-existing failure was preserved, not repaired
by unrelated formatting changes.

Reframe4s also referenced the old image4s build. A second classpath inspection
exposed duplicate image4s core/geometry entries even after the direct pin bump.
The one-line companion change is published as
`221f46163f5039e62c74e042e1c50f0762757fc0`
([draft reframe4s PR #2](https://github.com/canardlapin/reframe4s/pull/2)), based
on ScalaFIM's existing reframe4s revision, without importing unrelated main
changes. Its resample capability suite passed on JVM and JS. `upstream.json`
records local/tracking/remote SHA equality for all three clean owner clones.

`dependency-graph.json` records the final effective atlas consumer classpaths
on JVM and JS, requiring clean source checkouts, one source build and one
classpath entry per module for each of locus4s, image4s, and reframe4s.
Earlier full atlas runs passed (115 JVM and
79 JS tests); after the last one-line pin change, final consumer checks cover
the full locus-data suite and four atlas overlap/coverage/parcellation suites.
No source override substitutes for the immutable pins in these checks.

## Executed checks

Final command results and full-log hashes are recorded in `checks.json` and
compressed logs under `checks/`. Builds use bounded heaps and a task-private
sbt source staging directory. The dependency source pin is exercised without a
local locus4s override. No hosted CI result is inferred from local tests.

| Check | Result |
|---|---|
| locus4s core/data/laws JVM, JS, optimized JS | 87 / 87 / 87 passed |
| locus4s executable guide and Scaladoc | passed; repeated-classpath Scaladoc warnings |
| image4s affected locus JVM / JS | 38 / 36 passed |
| reframe4s resample capability JVM / JS | 1 / 1 passed; core and resample compiled |
| Final aligned locus-data JVM / JS | 27 / 27 passed |
| Final aligned selected atlas suites JVM / JS | 33 / 33 passed |
| Final classpath identity | one clean locus4s, image4s and reframe4s source implementation per module on each platform |
| HRF JVM / JS | 259 / 259 passed |
| HRF laws JVM / JS | 82 / 82 passed |
| Final JVM realization suite after tail-review addition | 9 passed |
| `scalafimCompileAll` on final aligned pins | passed on JVM and JS; no compiler warnings or errors |

PHRF-30 is complete as a locally qualified realization. STP-P7.05 is ready for
upstream review, with the three draft PRs still unmerged. The ScalaFIM commits
remain local in the isolated worktree; this report does not claim a main-branch
landing, hosted CI success, or release.
