# General model-authoring provider work — 2026-10-02

ScalaFIM remains a general library. Consumer audit data are frozen test fixtures;
no application-specific defaults, runtime dependencies, or external fixture paths
are introduced into the library. Public contracts are described in
[Model authoring APIs](../design/model-authoring-apis.md).

## Ticket artifacts

| Ticket | Artifact |
| --- | --- |
| SF1 `bd-01M3YZSYTNY6GP4KHJA8QAZ87M` | Design and response review graphics, landed on local main at `fa6bd6a9f7d773144885c57849cd298521c1772c` |
| SF2 `bd-01M3YZSZXFKKGD2XNXZ9RA2XYV` | Schema-bound design diagnostics with explicit rank and degenerate-column policies |
| SF3 `bd-01M3YZT0YXG97E11D99M5W34AN` | Estimability, row-invariant F geometry, sparse aliases, run combination and high-pass variance ratios |
| SF4 `bd-01M3YZT25ZC3R3V0CESB7RQ9DJ` | Formula printer, source spans and strict versioned portable JSON codecs |
| SF5 `bd-01M3YZT389J5F3XMQKMR5TZEX7` | Explicit HRF support/normalization/conventions, paired effects, centered products, shared slopes, serial and basis orthogonalization |
| SF6 `bd-01M3YZT4B04CMTWJJB6W4HKXHV` | Missing-value semantics, typed derived declarations, explicit bins, observed-only transforms and event response normalization |
| SF7 `bd-01M3YZT5D24TV7GG9Q4P7QC114` | Trial identities and target LSS designs, typed confounds/censoring, GLS/LSS serialization and run-specific sigma combination |
| SF8 `bd-01M3YZT6GEJJJK9KM1K1DG963G` | Unpaged matrix raster, raw CSV sidecar and term-level incremental compilation |
| SF9 `bd-01M3YZT7KF2JBJKH0QS3N7YV0H` | Event and trial-phase timing summaries with caller-supplied parent timing |

The grammar tickets cover the ledger items assigned in their ticket bodies.
Other ledger proposals, such as an entire HRF gallery or nominal-AR design
visualization, are not implicitly added to these tickets.

## Contracts and qualification boundaries

- JSON supports the portable subset authorized for SF4. Runtime callbacks,
  custom default kernels and other unsupported executable configurations fail
  explicitly; no extension registry or silent fallback is claimed.
- Derived declarations and onset selection are composable preprocessing APIs.
  Callers choose each term's population and retain the supplied row mappings.
  The formula builder does not silently discard pre-run events.
- Existing single-character logical operators remain supported with Kleene
  semantics. A consumer can impose a narrower editor syntax independently.
- Basis orthogonalization uses one transform per cell/modulator over all supplied
  scans, with effective HRFs and original scaling provenance. It rejects
  event-specific kernels and per-event normalization. Numerical window readouts
  require an explicit discretization rule, retained in hypothesis receipts.
- Design/contrast diagnostics are unwhitened OLS geometry. Weighted run results
  are conditional on caller-supplied sigmas. Serializing a GLS strategy does not
  qualify nominal-noise design diagnostics.
- Corrected orthonormal-row F expectations replace the scale-dependent historical
  values. Source hashes are embedded in the self-contained audit fixtures.
- The eight-spike df result is verified for the frozen 240-scan matrix and first
  eight source scans; it is not a promise that every eight spikes add rank eight.
- No release, hosted CI, browser latency, or remote publication is claimed.

## Review fixes — 2026-10-03

An independent review of `8ac43538` found correctness, portability and evidence
defects. They were fixed on this branch in `a03e4f57`, `1e4b2618`, `cbab2ee2`
(diagnostics), `dcf66cea` (formula, expressions, JSON), `6c16a104` (response,
SPM basis, modulators), `ad870cc4` (trials, LSS, confounds, summaries, raster),
`bbaa3f3c`, `72ae0b14` (guide), `6b3ebffd` (guarded expression evaluation) and
`37a0ca07` (spm-1s span, per-run readout transport, observed-only
orthogonalization). A second independent review of the merged result found the
issues addressed by the last two commits. Contract changes are recorded in the
[Model authoring APIs](../design/model-authoring-apis.md) guide, including its
known-limitations list.

Decisions recorded with the owner: pooled within-cell modulator SD uses
`SS_within / (n − k)`; the frozen stop-modulator grid expectation is therefore
`sqrt((n − k) / (n − 1))` rather than the dump's `n − 1` value.

Superseded statements above: the corrected F expectations are now emitted by an
independent NumPy oracle (`tools/fixtures/generate_contrast_diagnostics.py`)
rather than inline literals; `spm-1s` is SPM12's informed basis (`spm_get_bf`
with `spm_orth`) rather than the raw one-second difference.

## Verification

All commands used `python3 tools/build/sbt-warm` in the isolated task worktree,
at the merge of `37a0ca07`.

| Target | JVM | Scala.js |
| --- | ---: | ---: |
| `hrf/test` | 273 passed | 273 passed |
| `design/test` | 436 passed | 435 passed |
| `model/test` | 48 passed | 48 passed |
| `fit/test` | 361 passed | 350 passed |

Total: 2,224 passing test executions. JVM/JS count differences are JVM-only
suites (IO, performance guardrails, raster PNG export). `scalafimCompileAll`
completed with exit 0 and no compiler warnings. `git diff --check` passed.
The original receipts below predate the review fixes.

### Original receipts (`8ac43538`)

| Target | JVM | Scala.js |
| --- | ---: | ---: |
| `hrf/test` | 265 passed | 265 passed |
| `design/test` | 358 passed | 357 passed |
| `model/test` | 39 passed | 39 passed |
| `fit/test` | 353 passed | 342 passed |

Total: 2,018 passing test executions. `scalafimCompileAll` also completed with
exit 0 and no warnings. `git diff --check` passed.

[Command receipts](model-authoring-20261002/checks.json) identify exact qualified
commands, source-log hashes and retained compressed raw logs. Earlier combined
logs include later, separately identified failures; those failures are not
presented as passes. In particular, the first full compile exhausted local disk
space after both fit suites passed. The retry passed after reclaiming only this
session's generated build output.

The [independent review record](model-authoring-20261002/review.json) binds the
basis-transform and response-discretization review to source hashes. Its verdict
is a bounded source review, separate from execution evidence. Its source hashes
identify the `8ac43538` files; the review fixes changed several of them, so it
does not cover the current sources.

SF2–SF9 and the review fixes are committed on `work/model-studio-20261002`; SF1
alone is landed on local main. The shared main checkout's unrelated changes were
preserved. No push was performed.
