# PHRF reference neighborhoods and original-observation certificates

Checkpoint begun 2026-10-07, measurements completed 2026-10-08 UTC. ScalaFIM
base: `101c5ab6790554fe4f2bf781be1a748cef84ac8e`. This packet completes the
fixed-shape neighborhood, bounded-bank coverage and certificate feasibility
experiments. **The epic and production fast path remain unqualified.**

## Decision

The evidence does not refute the statistical PHRF model. It rejects broad-domain
coverage for the two tested eight-reference banks at the frozen amplitude
tolerance. Local corrected readouts work, and useful certificates against the
original observation equations now exist. Their useful neighborhoods are small;
the dense diagnostic certification work is outside the production budget.

This is not an impossibility proof for every possible placement of eight
references. It is a reason to stop treating the current broad-domain configuration
as nearly finished. Another expanded optimizer sweep cannot repair a readout
whose local approximation or finite HRF representation already fails at a
known shape.

The next bounded experiment should specify a scientifically justified HRF domain
and support together, then freeze reference placement before testing fresh shapes.
A shared pilot could place references if its cost and independence are explicit.
If the broad domain is essential, changing the reference/correction budget or
developing a different representation is a new contract, with its own resource
qualification. None of these changes has been silently adopted here.

## Protocol and scope

All experiments use 600 observations at the actual sampling-frame times
(`0.5, 1.5, ...` seconds), impulse trial events, three conditions, six nuisance
columns, AR(1) coefficient 0.3 with exact initial-row scaling, and the
condition-centered trial penalty with lambda 1. The full original design
evaluates the analytic Cascade34 impulse response at each sample/event lag over
the entire acquisition. There is no impulse-convolution quadrature or 48/96-second
tail truncation in that reference design.

Responses have signed condition means `(1.2, -0.8, 0.4)`, deterministic
trial heterogeneity `0.25 sin(0.71 j + 0.31)`, intercept/trend and AR noise with
noise/signal RMS ratio 0.1. Fresh shape seed is `2026100801`; response noise seed
is `2026100802`. The same standardized noise is reused as shapes change. This is
exploratory conditional accuracy, not an independent scientific confirmation
cohort or a coverage-probability estimate.

Both layouts are declared in unit coordinates of the full physical shape chart:
the corners `{0,1}^3` and inward quarters `{0.25,0.75}^3`. Both contain eight
references. Each fresh coordinate is uniform on `[0.02,0.98]`. Every fresh shape
is evaluated against all eight references offline; the best result is an
optimistic reference-availability check, **not a legal runtime router**. The
nearest-two subset is recorded independently. The 32 N=300 shapes are the first
32 of the N=30 cohort. Comparisons across horizons and layouts are paired;
they must not be pooled as independent samples. No decoder runs in this audit.

The amplitude target is the existing relative L2 error of `1e-3`. The alternating
signed-query absolute target `1e-6` is a separate exploratory target, not a new
scientific admission rule and not a reinterpretation of the query's Float32
conversion tolerance.

The 48-second basis uses maximum rank 32 and selects 10; the 96-second basis uses
maximum rank 24 and selects 12. Both use the same `1e-3` kernel tolerance,
`BlockedPartial(96)` compilation and blocked trial preparation with block size 32.
Both layouts in an experiment share the same physical family, events and basis.

## Coverage result

Each entry is the count meeting the amplitude target among fresh shapes. The
corrected columns allow an offline search across all eight references. The exact
column factors the prepared basis equations at the requested shape and compares
them with augmented QR of the full original observation design.

| Trials | Support | Bank | Corrected vs prepared | Corrected vs original | Exact prepared vs original |
|---:|---:|---|---:|---:|---:|
| 30 | 48 s | corners | 0/64 | 0/64 | 30/64 |
| 30 | 48 s | quarters | 12/64 | 4/64 | 30/64 |
| 30 | 96 s | corners | 0/64 | 0/64 | 58/64 |
| 30 | 96 s | quarters | 11/64 | 11/64 | 58/64 |
| 300 | 48 s | corners | 0/32 | 0/32 | 16/32 |
| 300 | 48 s | quarters | 1/32 | 0/32 | 16/32 |
| 300 | 96 s | corners | 0/32 | 0/32 | 28/32 |
| 300 | 96 s | quarters | 2/32 | 2/32 | 28/32 |

Nearest-two prepared-amplitude pass counts equal best-of-eight counts in these
samples. Neither bank has a fresh signed query meeting the exploratory `1e-6`
target. All eight coverage ScenarioResults are `Fail`, with explicit blocking
scientific and runtime-routing caveats.

For N=300, even the inward bank's 95th percentile of best prepared-relative
error is about 5.4%, versus the 0.1% target. This isolates a readout approximation
problem independently of the HRF representation error.

The records separately contain:

- `preparedRelativeError`: corrected versus exact prepared-basis readout;
- `exactOriginalRelativeError`: complete representation discrepancy, including
  basis/grid effects and finite support;
- `relativeTailDesignError`: analytic truncated versus analytic full design;
- `relativeBasisDesignError`: prepared basis versus analytic truncated design.

The complete representation discrepancy must not be described as tail error
alone. The default chart nevertheless has a clear support mismatch: its minimum
positive rate is 0.25 and minimum undershoot/positive rate ratio is 0.1, so the
Erlang-4 undershoot can peak at `3 / (0.25 * 0.1) = 120` seconds. Neither a
48-second nor a 96-second horizon uniformly captures that part of the chart.

## Local neighborhoods

N=30 tests each reference along three inward axes and an inward diagonal at
unit-chart radii `0, .001, .003, .01, .03, .1, .25`. All references in both
layouts meet the prepared amplitude target in all four sampled directions
through radius `.03`; at `.1`, every reference has a failing direction.
N=300 samples `0, .01, .1`: all references pass all directions through `.01`
and have a failure by `.1`. These are empirical ray prefixes, not certified
balls or a volume-covering argument; intermediate or other directions are untested.

Against the original design, several slow shapes already fail at their own
reference because of representation error. At 96 seconds, node 4 still fails
at the reference in both layouts. `ray-prefixes.json` preserves the complete
per-node result, including empty original-accuracy prefixes.

## A useful original-observation certificate

Gale now supplies an outward exponential and a generic validated linear-system
bound. ScalaFIM supplies the scientific observation adapter. For an enclosed
normal system `G x = b`, a finite reference inverse candidate `M` and a readout
candidate `a`, define

```
r = b - G a
z = M r
D = I - M G
eta = ||D||inf
E = ||z||inf / (1 - eta)
```

If outward arithmetic proves `eta < 1`, every enclosed system is nonsingular
and `||x-a||inf <= E`. Individual components use
`|z_i| + ||D_i||1 E`. A signed query uses `|q'z| + ||q'D||1 E`, which preserves
cancellation; it does not sum absolute coefficient errors unnecessarily.
The readout's floating query contraction is separately enclosed. The amplitude
relative bound uses a lower bound on the unknown true coefficient norm.

The exponential uses positive Taylor terms, an outward geometric remainder,
exact range reduction by halving and outward squaring. Platform `math.exp`
accuracy is not assumed. Tests also use independent 80-digit decimal brackets.

The domain adapter encloses chart conversion, continuous Erlang-3/4 evaluation,
all observed lags, AR(1) whitening including the first row, the condition-centered
penalty, normal equations and right-hand side. Its input scope is the exact
declared binary chart coordinates, event/sample times, data and nuisance design
values. It does not certify the construction of the supplied nuisance basis,
physical acquisition uncertainty, non-impulse drives, other HRF families,
arbitrary whitening or other penalties.

The candidate inverse is computed once at the same prepared reference matrix,
using Gale's solver. Its accuracy is not trusted; the defect test validates it
against the enclosed original matrix. There is no requested-shape factorization
inside the certificate.

At N=30, two corner references (0 and 7), two horizons and four inward diagonal
radii produce 16 point attempts. Twelve at radii `0, .001, .01` certify and have
relative amplitude upper bounds below `1e-3`. Four attempts at `.1` refuse
because the contraction cannot be proved below one. Observed augmented-QR
coefficient and query errors fall within every reported certificate.

For example, at 96 seconds, node 0 plus radius `.001` has an observed signed-query
error `3.30865e-7` and a certified bound `3.98341e-7`, below the exploratory target.
Its amplitude bound is `3.99096e-5`. This establishes useful local certification,
not global or production admission.

Sixteen shape-box attempts at inward side lengths `1e-6, 1e-5, 1e-4, 1e-3`
certify conditioning for the first twelve and refuse the four largest boxes.
These certificates are uniform **nonsingularity/conditioning** results, not
uniform readout accuracy for all responses or decoder trajectories. Refusal
does not prove actual singularity or mathematical impossibility.

## Work and resource accounting

Every corrected readout records one residual correction, three reference inverse
applications and zero requested-shape factorizations. Direct signed queries
retain zero trial amplitude values. The dense QR and exact prepared solves are
explicit diagnostic comparators, outside that readout count.

The original-observation certificate materializes dense interval normal equations
and performs cubic defect validation. At N=30 it retains 45,864 scalar endpoints
and constructs 421,200 normal/right-hand-side product terms per point, in addition
to HRF evaluation, construction temporaries, the reference inverse and validation
work. Its recorded seconds cover enclosure construction only, not an end-to-end
certificate. It is not the allowed single-extra-inverse production certificate.

The actual production caps remain eight references, at most two evaluated per
voxel, one correction, at most one extra inverse for certification, 256 MiB and
the 120-second B0 gate. This experiment changes none of those caps and performs
no complete B0 timing qualification. Prior incomplete B0/stress timing evidence
still applies; the expanded 901-request decoder remains a diagnostic.

At 1,200 trials, 48-second preparation succeeds with 204,786,040 reported shared
reference bytes and 9,116,727 retained preparation doubles. These are scoped
estimates, not total engine peak memory. Extending to 96 seconds requests
24,022,669 retained values and refuses the unchanged 16,000,000-value preparation
limit. Thus extending support alone does not provide a scalable remedy.

Host: macOS 15.1.1, M2 Pro (10 cores, 32 GiB), JDK 21.0.12.1, Node 24.21.0;
resident sbt uses a 5 GiB heap and four visible cores. Other host work was present.
These exploratory timings are not portable throughput claims.

## Implementation, reproduction and provenance

The audit exposed an independent grid bug: floating interpolation could put the
last grid node one ULP outside its declared chart. `NodeGrid.coordinateOf` now
returns the declared endpoint for the first/last index; shared regression tests
cover Cascade34 grids of sizes 2, 3 and 7.

The generic primitives landed in [Gale PR #24](https://github.com/canardlapin/gale/pull/24).
ScalaFIM pins `62c0aeb3374557874a17f870c9ae05e3290c21cf`, the tested topic commit
preserved by merge `8b47aa8a432304a0fe33cc7baf879daf6f24d0a4`, avoiding unrelated
optimizer additions on upstream main. Gale's full core tests passed (810 JVM,
798 Scala.js), formatting passed, and hosted checks and approval passed. Bugbot
was skipped because of its usage limit and was not counted as review evidence.
`upstream.json` records the complete source provenance.
`validation.json` and compressed logs record final platform gates. `manifest.json`
binds this source overlay and every evidence artifact; run `python3 -S
docs/verification/phrf-neighborhoods-20261007/verify.py` from the worktree.
Earlier verification packets remain frozen at their own source revisions.

| ScalaFIM regression gate | JVM | Scala.js |
|---|---:|---:|
| Full `fit` suite | 707 passed | 649 passed |
| New neighborhood/observation suites, recovery and stress integration | 11 passed | 11 passed |
| Targeted `PhrfTrialSuite` comparison | 17 passed | 17 passed |

The recovery suite checks all 80 stored reference cases on each platform with
unchanged energy, shape and admission tolerances. This is behavioral regression
evidence, not a claim of bitwise identical optimizer trajectories. The build's
existing multiple-main discovery message is retained separately from compiler
warnings in the validation receipt. `scalafimCompileAll` passed on both platforms
with no compiler warnings. ScalaFIM validation is local; this checkpoint does not
claim hosted ScalaFIM CI or a ScalaFIM merge.

Generate the measurements with the pinned Gale build:

```sh
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialNeighborhoodAuditMain /tmp/neighborhoods-n30.json neighborhoods' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialNeighborhoodAuditMain /tmp/neighborhoods-n300.json neighborhoods 300 32' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialNeighborhoodAuditMain /tmp/certificates.json certificate' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialNeighborhoodAuditMain /tmp/stress-preparation.json stress-preparation'
```

Source development used an explicit sibling Gale override. The final gates use
the published revision in `build.sbt`. PHRF-11 remains in review, PHRF-14 and
PHRF-33 remain open, and the epic remains in progress. `CertifiedOriginalEquations`
continues to refuse until a supported original-model certificate is integrated
and charged within the production contract.
