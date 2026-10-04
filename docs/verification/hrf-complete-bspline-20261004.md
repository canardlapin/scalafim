# HRF complete B-spline basis: readiness receipt (cluster C5)

Status: ready for independent review. Not landed, not pushed.

## Provenance

- Bead `bd-01M28WR74HX73YMKTVJ1DJCH4M` (P1, closed): "B-spline basis drops its
  intercept spline, so it cannot represent a flat response". Found by the PLS
  Neuro basis-set review (2026-09-11). Core triage on 2026-09-12 asked for an
  intercept-inclusive option or an explicit policy. Agent `codex-low-fruit`
  implemented the option on 2026-09-12 and closed the bead. The bead says the
  work was "implemented locally, not published". It was never committed.
- The uncommitted canonical-checkout files were preserved as
  `wip/hrf-complete-bspline-20261004` at `c5d6b055`, on top of main `53097f3f`.
  The five files are `HrfDescriptor`, `HrfFunctions`, `HrfIdentity`,
  `HrfIdentitySuite` and `CompleteBsplineSuite`, all in `modules/hrf`. Triage
  classed cluster `C5-hrf-complete-bspline` as LANDABLE-AFTER-REVIEW, noting the
  identity-string shift for every B-spline.
- Review branch `review/c5-hrf-complete-bspline-20261004`:
  `c5d6b055` merged with main `e275bffc` as `4da8b38a`. The merge had no
  conflicts. Main's seam fixes to `Basis.scala` do not touch B-spline code.
- The NEWS entry for this change was preserved separately, in cluster C9
  (`wip/core-triage-docs-20261004`). Triage says each entry lands with its
  cluster, so this branch carries it. One sentence was added about identity
  stability.

## Semantics

`Hrfs.bspline(nBasis, span, degree, includeIntercept = false)` is the default.
It is unchanged: the legacy fmrihrf 0.4.0 convention, which is the full clamped
basis minus its first spline. Interior knots sit at `floor(span) * i / (m + 1)`.
The basis is zero at onset and cannot represent a constant once interior knots
exist.

`includeIntercept = true` selects the *complete* basis:

- the full clamped B-spline basis of order `degree + 1` on `[0, span]` with
  `W = max(nBasis, degree + 1)` columns;
- `W - degree - 1` interior knots, uniform over the actual span (`span * i / (W - degree)`),
  so fractional spans get even intervals;
- partition of unity on the closed window (constants are representable;
  generally nonzero at onset);
- exactly zero outside `[0, span]`. The legacy mode instead clamps outside
  times to `t = 0`, which gives zero for that basis anyway;
- the integration policy is `PiecewisePolynomial` with the uniform breaks, so
  `ResponseFunctional.WindowMean` stays exact;
- no column normalization in either mode.

In R, this is `splines::bs(t, knots = <uniform interior>, degree, intercept = TRUE,
Boundary.knots = c(0, span))`.

The snapshot also added `require` guards to `Hrfs.bspline` in both modes:
`degree >= 0` and a positive, finite `span`. These guards apply to legacy
callers too. A non-positive or non-finite span previously produced a
meaningless basis and now fails at construction. No in-repo caller or test
relied on the old behavior; all gates below pass.

## R comparison

The generator
`docs/verification/hrf-complete-bspline-20261004/generate_complete_bspline_fixture.R`
writes `modules/hrf/shared/src/test/scala/scalafim/fmri/hrf/fixtures/CompleteBsplineRFixtures.scala`.
It ran under R 4.5.1 with splines 4.5.1 and serializes through
`tools/r-parity/receipt_serialization.R` (13 significant digits; values below
1e-14 become zero). The generator canonicalizes times before evaluation, so the
serialized times are exactly the points R evaluated. The fixture SHA-256 is
`873ab96a14d7caf6199ef2562ca6dd3446f955485f42dda6c5664f901cdbceb2`.
The run's output is in `generator-output.txt` in this receipt's directory.

The fixture covers six configurations:

| Name | nBasis | Degree | Span | Width |
|---|---|---|---|---|
| `cubic5_span24_5` | 5 | 3 | 24.5 | 5 |
| `cubic8_span24` | 8 | 3 | 24 | 8 |
| `quadratic6_span17_3` | 6 | 2 | 17.3 | 6 |
| `linear4_span10` | 4 | 1 | 10 | 4 |
| `quartic12_span32_75` | 12 | 4 | 32.75 | 12 |
| `cubic2_minimum_span7_75` | 2 | 3 | 7.75 | 4 (no interior knots) |

Each configuration is evaluated at every knot, every knot-interval midpoint,
both endpoints and four irrational offsets.

The new test `complete basis matches the generated splines::bs(intercept = TRUE) fixtures` in
`CompleteBsplineSuite` asserts every value to `1e-12`. That bound is set by the
13-digit serialization. The test passes on JVM and Scala.js. The snapshot's own
tests remain: a hand-transcribed `bs` fixture, the analytic Bernstein basis,
partition of unity for degrees 0 to 4 and fractional spans, zero outside
support, exact window-mean weights and flat-response reconstruction.

The relation to fmrihrf is as follows:

- **Installed fmrihrf 0.4.0** (`hrf_bspline`) is the legacy convention:
  `splines::bs(df = N, intercept = FALSE)` with quantile knots on
  `[0, floor(span)]`. Its row sums range from 0 to 1. scalafim's default matches
  it, as pinned by the existing `BsplineParitySuite` and `HrfRParitySuite`. The
  complete mode has no fmrihrf 0.4.0 counterpart.
- **fmrihrf checkout `40c2125`** (unreleased; changed in `18d418f`, 2026-09-29,
  "fix B-spline ... bases") redefines `hrf_bspline(N)` as the clamped basis with
  `N + 2` functions, uniform knots over the actual span, and the first *and* last
  splines dropped, so every column vanishes at both ends. The generator sources
  that function and confirms `hrf_bspline(N) == complete(N + 2)[, -c(1, last)]`
  with a maximum absolute difference of 0 across four configurations, including
  fractional spans. The two codebases therefore now share the knot placement:
  uniform over the actual span. They differ in intercept handling. fmrihrf HEAD
  drops both boundary splines; scalafim's complete mode keeps all of them.
- **Follow-up (not in scope).** When fmrihrf releases `18d418f`, scalafim's
  *legacy* default will no longer match R. It uses `floor(span)` quantile knots
  and drops only the first spline. That gap concerns the default and parity
  fixtures, not this change, so it needs its own bead.

## Identity-change analysis

The snapshot changed the HRF parameter record for **every** B-spline from
`bspline(1:2,1:3)` to `bspline(1:2,1:3,5:false)`. That shift has the following
consequences:

- The record feeds `HrfDescriptor.canonicalId` (`hrf-descriptor/v2|...`). That
  ID feeds basis-element identity, design column IDs and `design-schema/v2`
  fingerprints. It also feeds `PreparedGlsArtifact`, `CoefficientAxis` and
  `ResultManifestWriter` sidecars.
- It is not versioned. The descriptor prefix stays `v2`, while the v2 receipt
  (`docs/verification/hrf-design-identity-20261001/README.md`) treats any
  structural identity change as breaking. That receipt says existing artifacts
  keep their strings and must be rebuilt from the model specification, never
  rewritten. A silent in-version shift would make every previously written
  B-spline design or fit artifact fail fingerprint and axis compatibility
  against a rebuilt v2 design. There would be no prefix to tell the two apart.
- The shift is not needed. The record is length-framed, so a two-field and a
  three-field `bspline(...)` record cannot collide.

Resolution on this branch, made in `HrfIdentity.scala`:

```scala
case HrfParams.Bspline(count, degree, false) =>
  record("bspline", count.value.toString, degree.toString)
case HrfParams.Bspline(count, degree, true) =>
  record("bspline", count.value.toString, degree.toString, "complete")
```

The effect is as follows:

- Legacy B-spline identities are byte-identical to main. This covers every
  existing descriptor, element ID, column ID and fingerprint, so no persisted
  artifact changes.
- The complete basis gets a distinct identity. It is a different response
  space, so it must have one, and the test
  `complete and legacy descriptors distinguish different response spaces`
  covers this. No descriptor version bump is needed, because no existing
  encoding changes meaning.
- The tag is `complete`, a semantic token, rather than the boolean rendering
  `true`. A later third mode, such as fmrihrf HEAD's interior-only basis, can
  then take another tag without a further breaking change.

The reviewer should rule on whether this conditional encoding is acceptable or
whether the owner prefers the snapshot's always-three-field form plus a v3
bump. That form changes every identity in the system and requires rebuilding
artifacts.

## Golden and fixture audit

The following command searched the repository for B-spline identity
dependence:

```
git grep -n -E 'bspline\(1:|bspline\([0-9]+:|HrfParams\.Bspline|"bspline' -- ':!*.gz'
git grep -l -E 'hrf-descriptor/v2|design-schema/v2'
```

- **`HrfIdentitySuite`** holds the only literal B-spline identity golden.
- **`DesignIdentitySuite`, the `fingerprint-oracle.json` / `numeric-oracle.json`
  identity oracles and `ResultManifestWriterSuite`** pin SPMG and FIR designs
  only. None of them pin a B-spline design.
- **The model, fit and `firstLevelLaws` suites** have no B-spline identity
  goldens. `FirstLevelGenerators` builds B-spline kernels generatively, so no
  literal strings are involved.
- **The R-parity fixtures** (`HrfRParityFixtures`, `BsplineParitySuite`) contain
  numerical values for the legacy basis only. They are unchanged.

Golden updates relative to main:

| Golden | Main | This branch |
|---|---|---|
| `HrfIdentitySuite` legacy `Bspline(2, 3)` | `bspline(1:2,1:3)` | `bspline(1:2,1:3)` (unchanged; the snapshot's `5:false` value was reverted) |
| `HrfIdentitySuite` complete `Bspline(2, 3, true)` | (none) | `bspline(1:2,1:3,8:complete)` (new) |

No other golden changed.

## Gates

Every gate was run with `TERM=dumb python3 tools/build/sbt-warm <targets> < /dev/null`
from the review worktree, one batch at a time. `memory_pressure` reported at
least 47% free before each batch. JDK `-release:17` is in `build.sbt`.

| Gate | Result |
|---|---|
| `hrfJVM/test` | 283 passed, 0 failed |
| `hrfJS/test` | 283 passed, 0 failed |
| `hrfLawsJVM/test` | 82 passed, 0 failed |
| `hrfLawsJS/test` | 82 passed, 0 failed |
| `designJVM/test` | 445 passed, 0 failed |
| `designJS/test` | 444 passed, 0 failed |
| `modelJVM/test` | 53 passed, 0 failed |
| `modelJS/test` | 53 passed, 0 failed |
| `fitJVM/test` | 565 passed, 0 failed |
| `fitJS/test` | 512 passed, 0 failed |
| `firstLevelLawsJVM/test` | 83 passed, 0 failed |
| `scalafimCompileAll` (`-release:17`) | exit 0 in 1582 s; no source-level `[warn]` or `[error]` lines (only sbt GC-pressure notices and a dependency build.sbt lint) |

The JVM and JS totals differ for design and fit because of platform-specific
suites under `jvm/src/test`.

## Items for the reviewer

- **Conditional identity encoding.** Decide whether to accept it (see above).
- **`includeIntercept: Boolean`.** This is a boolean on a public constructor and
  on `HrfParams.Bspline`. `AGENTS.md` prefers a closed `enum`, for example
  `BsplineSupport.Legacy | Complete`. The boolean was kept to stay
  minimal-diff with the snapshot.
- **New guards in `Hrfs.bspline`.** They also apply to the legacy default (see
  Semantics).
- **fmrihrf `18d418f` drift.** The legacy default will drift from R when that
  commit is released. This needs a separate bead.
