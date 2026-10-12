# Rank residual-basis portability repair — 2026-10-12

The current score-orthogonal permutation method now has **exact JVM/Scala.js
exceedance-count agreement on all eight retained resource fixtures**, with
all 199 accepted permutations identical. No new dataset or pilot stream was
generated. Multivar [PR #4](https://github.com/canardlapin/multivar/pull/4)
contains the upstream repair; ScalaFIM pins its immutable tested commit
`2bc14d14ba42a678fbbe169293153eddaba6175c`.

## Demonstrated cause

The working nuisance design prepends an intercept to inputs that already
contain one. Its four-column QR has measured rank three. Before this repair,
Huh–Jhun basis construction applied every QR reflector, including the fourth
reflector formed from roundoff left in a redundant column. That arbitrary
rotation changes the residual coordinates being permuted.

On the three-column n=80, p=6, q=4 fixture, the redundant reflector differs
by 0.3512 between platforms, the residual basis by 0.6581 and projected X
by 1.6689. The first three reflectors agree to rounding error, as do the
observed statistics and every actual permutation. Exceedance counts were
`[0,2,155,187]` on JVM versus `[0,2,154,181]` on JS. Thus identical seeds
and invariant observed roots did not imply identical permutation experiments.

Multivar now factors only the measured independent nuisance columns, in the
original QR pivot order, when the design is rank deficient. It uses Gale's
existing QR capability. Full-rank coordinates and the Theil path retain
their existing behavior. This changes the realized finite permutation sample
for redundant designs; the source pin is part of the experiment identity.

After repair, the largest cross-platform basis difference is 1.11e-16 and
all eight count vectors agree exactly. `audit.json` records before/after
counts, reflector-column gaps, projections, observed/null statistics and
trace hashes. The compressed traces retain every accepted candidate and
permutation, rather than relying on seed equality alone.

## Verification

- Upstream `compileAll testAll smokeCheck`: 1,573 tests pass across JVM and
  Scala.js; ordinary and inference API snapshots are unchanged. Full GitHub
  CI passed, including compatibility, docs and published-artifact smoke.
- ScalaFIM full MVPA suites: 473 pass and four unrelated opt-in skips on
  each platform. The retained trace test passed on both platforms.
- The dedicated replay and the full MVPA replay produced byte-identical
  traces on each platform. The archived `fixed-first` traces are the first
  eight records; the next eight were checked byte-for-byte identical.
- The upstream smoke documentation task emitted its existing repeated
  `-classpath` warning. ScalaFIM's build reports existing unused intaglio
  settings. The changed Scala sources compile warning-clean.

Run the independent trace audit from the repository root:

```sh
python3 tools/mvpa-inference/paired/audit_portability.py \
  docs/verification/umvpa-rank-portability-20261012/current-JVM.jsonl.gz \
  docs/verification/umvpa-rank-portability-20261012/current-JS.jsonl.gz \
  docs/verification/umvpa-rank-portability-20261012/fixed-first-JVM.jsonl.gz \
  docs/verification/umvpa-rank-portability-20261012/fixed-first-JS.jsonl.gz \
  /tmp/rank-portability-audit.json
```

To replay, extract the eight TSVs from the same retained input archive used
by `tools/mvpa-inference/paired/probe.py`; verify `inputs.json`, and place their
absolute paths in a list file. In a fresh sbt environment:

```sh
SCALAFIM_RANK_TRACE_LIST=/tmp/cases.txt SCALAFIM_RANK_TRACE_OUTPUT=/tmp/new-rank-trace \
python3 tools/build/sbt-warm 'mvpaJVM/testOnly *RankPortabilityTraceSuite' \
  'mvpaJS/testOnly *RankPortabilityTraceSuite'
```

Outputs are suffixed `-JVM.jsonl` and `-JS.jsonl`. Always use a new output
prefix. The test accepts only the eight exposed fixture inputs, never pilot
phase data. `source-lock.json` binds the after-repair source and both provider
revisions. Initial diagnostic compile failure (an inaccessible QR field) and
subsequent successful traces are preserved in the logs.

## Historical comparator and scientific boundary

The [earlier failed preflight](../umvpa-rank-paired-20261011/README.md) remains
unchanged. Its original historical coefficient-permutation implementation
still has four cross-platform count mismatches. This patch does not relabel
that implementation or alter its archived source, classpath or results.

The **paired three-method preflight therefore remains uncleared**. Before a
pilot can be frozen, its protocol must explicitly choose an identifiable
historical comparator treatment and pass a complete retained-input preflight.
Agreement of binary decisions is not a replacement for exact counts. All
12,800 proposed pilot streams remain unopened; no launch authority or rank
admission is produced here.

Bounded repair mote `bd-01M4Q4HSD33AF2CRRCY9B2WP0V` is complete. Scientific
parent `bd-01M2BNHMAR9S58QCMT2DEM16VN` remains open with that preflight boundary.
