# UMVPA M5.03 salvage: two parallel group implementations

Date: 2026-10-10. Branch `work/umvpa-group-salvage-20261010`, based on
`origin/main` at `8e85ef61`.

Two sessions implemented UMVPA-M5.03 ("group summaries, heterogeneity, and
held-out-subject prediction") from the same parent `69c03404` on 2026-10-07:

- **A**, `0b6eaa77`, landed on main. Its `mvpa-group` sources are unchanged on
  main since then (`git log 0b6eaa77..origin/main -- modules/mvpa-group` is empty).
  The rank calibration test helpers it touched have changed on main since then.
- **B**, `47e14319` on `work/umvpa-next-20261007` (plus `1abc6ad4`, which holds
  only mote ops). B was never merged.

This packet compares the two, keeps the stronger part of each, and records
what was dropped. It is engineering and numerical evidence only. It makes no
Monte Carlo type-I, coverage or scientific-release claim, and M5.04 stays open.

## Decisions

| Component | Decision | Decisive reason |
| --- | --- | --- |
| `SubjectGroupSummary` | Keep A | Same native numerics. A derives scope from the calculation, so a mismatch cannot be built; B checks the pairing at runtime. A also refuses SE ≤ 0. |
| `HeldOutSubjectPrediction` design | Keep A | Only A has subject adaptation, which acceptance item 2 requires. A also lets one subject span several independent units, and it reports between-subject variability. |
| Held-out exposure accounting | Port from B into A | A defect in A: held-out outcomes were read without being recorded. |
| I² documentation | New | A cross-check against metafor found that I² has a different definition there (see below). |
| `calibration_readiness.py` and its test | Salvage, adapted | Main still accepted bare `"passed"` strings as confirmation prerequisites. |
| `RankCalibrationRecord` and its suite | Drop | Replaced on main by `CalibrationProtocolSupport.RankMetrics` (record schema 2), which uses a different, incompatible schema. |
| B's equal-variance R oracle | Keep as an additional fixture | It is an independent closed-form check. It now runs against main's code. |
| B's logs, `verification.json`, `implementation.md`, rank-observability README, `readiness.json` | Drop | They describe B's unmerged sources. |

### Group summaries

Both versions use the same pipeline: `input.marginalModel(GroupDesign.intercept(n))`
→ `GroupEngine.fit`. Mean, SE, Q, I², tau² and the reference distribution all
come from the shared `group` module, so neither version has its own group
estimator. The only arithmetic each version adds itself is the empirical subject
mean and variance (n − 1 denominator). A uses a two-pass algorithm and B uses
Welford's algorithm. Both are numerically stable and give identical results on
every fixture. A also returns pointwise p-values from the native
`GroupStatistic`: Normal for the fixed-effect and z policies, t(n − 1) for
modified Knapp–Hartung (mKH). It labels them as unadjusted. Both versions refuse
a single subject, nonfinite output and partially failed native maps.

On API design A is better. `SubjectMeanScope` is derived from the
`SubjectGroupCalculation`. B's `SubjectGroupSummaryTarget` has to be matched
against the calculation at runtime. A's `SubjectPopulationContract` records the
same declarations as B, plus conditioning, first-level nuisance and the
multiplicity family. A's tests are also stronger: an unequal-variance PM oracle
on all four cells, a check of the defining Q equation, refusal of a partial map,
and the zero-heterogeneity boundary. B's tests covered only an equal-variance
case.

### Held-out-subject prediction

The two versions build different products. A wraps the admitted M4.06
`ComponentConfirmation.incremental` OLS heads. It reports per-subject full and
reduced losses and signed improvements, re-weighted to equal rows within a
subject and then equal subjects. Either shared training or single-subject head
adaptation is allowed. B scores a frozen `PatternPrediction` head, giving
calibrated components or a Gaussian target mean, as mean squared loss. It
forbids adaptation and requires every independent unit to be a whole subject.

A covers more of M5.03 and its reducer passes an invariance test (unequal blocks
inside a subject give the same subject results). It is kept.

**Defect found in A.** `evaluate` checked that the supplied assessment exposure
account was untouched, then read the held-out outcomes without recording the
read. Concrete failure on main:

1. `plan.evaluate(brain, targets, account)` succeeds.
2. `account` still passes `ExposureControl.untouchedConfirmation(account, Holdout)`.
3. So both the same evaluation and `HeldOutSubjectPrediction.freeze(...,
   assessmentExposure = account)` for replacement heads are still admitted.

Nothing records that the held-out subjects' outcomes have been seen. B's
reviewer had found and fixed the same defect in B. Commit `4b8b9546` ports that
fix:

- `evaluate` now returns `HeldOutSubjectEvaluation(result, exposure, readAttempted)`.
- Binding, exposure and both budgets are refused before any read, and the
  supplied ledger comes back unchanged. The second budget belongs to the M4.06
  procedure; it is checked through `ComponentConfirmation.incrementalOwnedCells`,
  which is now public.
- The payload read and the derived-loss view are recorded as instrumented
  holdout events. A failed read still records the attempt.
- Typed M4.06 errors are preserved as `SubjectPredictionError.Component`.

New tests confirm that the returned ledger refuses both re-evaluation and a
replacement plan. As before, a fresh snapshot of the same evidence cannot be
detected, because the exposure system has no registry. The module guide states
the caller's duty to carry the returned ledger forward. `evaluate` is called
only from `mvpa-group` tests and the module README; no other module, example
or document calls it.

No numerical bug was found in either side's subject reducers or group summaries.

## Oracle cross-checks

All runs used R 4.3.3; session files are kept next to each script.

1. **Main's oracle reproduces.** `umvpa-group-summaries-20261007/oracle.R`
   regenerates its `expected.tsv` byte for byte.
2. **B's oracle against main's code.** [`equal-variance-oracle.R`](equal-variance-oracle.R)
   is B's script, unchanged, and regenerates B's TSV byte for byte
   ([`equal-variance/`](equal-variance/)). Its input is y = (−1, 1, 3) with v = 1.
   The new test `SubjectGroupSummarySuite` "equal known variances…" applies it
   to two measurements and two task coordinates. Main's summary matches:

   | Quantity | Value | Tolerance |
   | --- | --- | --- |
   | FE mean / SE | 1 / 0.57735 | 1e-12 |
   | Q / I² | 8 / 0.75 | 1e-12 |
   | Empirical variance | 4 | 1e-12 |
   | PM tau² | 3 (391 at the second measurement) | 1e-9 relative |
   | mKH SE | 1.1547 | 1e-9 |
   | p-values | metafor values | 1e-12 relative (FE), 1e-9 (mKH) |

   There is no disagreement.
3. **Main's oracle against B's formulas.** B's summary calls the same native
   engine and uses the same unbiased variance formula, so on main's inputs it
   gives the same numbers. B produces no p-values. Running B at its own base
   would test `69c03404`'s group engine, which `8b5f9dde` has since repaired
   (normal tails and PM stopping), so this was not repeated.
4. **Third-party reference.** [`metafor-crosscheck.R`](metafor-crosscheck.R)
   runs metafor 4.8-0 (`rma` with FE, PM and `test = "adhoc"`, which is
   metafor's modified Knapp–Hartung) on both oracle inputs. Results are in
   [`metafor-crosscheck.tsv`](metafor-crosscheck.tsv). Main's expected FE and
   PM-mKH mean, SE, p, Q and tau² agree with metafor to a worst relative
   difference of 2.5e-13.
   - **Definitional finding.** metafor reports Paule–Mandel I² as
     tau²/(tau² + s²). Main, B and the native engine all report
     Higgins–Thompson (Q_FE − df)/Q_FE. On main's unequal-variance cell (1, 4, 1)
     these give 0.667 and 0.75. The two coincide under equal variances or
     DerSimonian–Laird, so B's equal-variance oracle cannot see the difference.
   - This is a labelling issue, not an arithmetic bug. The definition is now
     stated in the `SubjectHeterogeneitySummary` scaladoc and the module guide,
     and pinned by a test. M5.04 should decide which I² it qualifies.

To reproduce the metafor check, install metafor into a private library and
point `R_LIBS` at it:

```sh
R_LIBS=/path/to/lib LC_ALL=C Rscript \
  docs/verification/umvpa-group-salvage-20261010/metafor-crosscheck.R \
  docs/verification/umvpa-group-salvage-20261010
LC_ALL=C Rscript docs/verification/umvpa-group-salvage-20261010/equal-variance-oracle.R \
  docs/verification/umvpa-group-salvage-20261010/equal-variance
```

## Calibration readiness (salvaged from B)

On main, `validate_manifest(..., "confirmation")` accepted:

- `prerequisites[gate] == "passed"` strings;
- any truthy `resource_approval`.

Commit `3453d886` adds `calibration_readiness.admission_errors`, which
confirmation validation now requires to be empty. It requires:

- hash-bound prerequisite receipts with the exact phase, cell ids and source
  locks;
- a runtime lock with verified loaded provider sources and JVM flags that match
  the approved budget;
- a metric authorization bound to the protocol, the cell digest and the locked
  `unified-mvpa-rank-metric-bindings-v1.md`;
- frozen per-cell rank metric bindings;
- a complete inventory review;
- measured, process-backed cost probes.

Unknown phases or operations are refused. Adaptations to current main:

- The rank source closure uses `CalibrationProtocolSupport` (schema 2), not
  `RankCalibrationRecord`.
- Three-column nuisance and metric binding are recorded as resolved by their v1
  specifications.
- Rank scope is computed rather than hardcoded: 64 defined entries, 480,000
  confirmation datasets, 959,520,000 non-identity draws. B said 32 entries and
  240,000 datasets; that was true before main froze the three-column nuisance
  population.
- Historical pilot facts are read from `pilot-summary.json` rather than copied.
- B's tests of its own v2 record schema were dropped. Main's
  `test_calibration_protocol.py` already covers raw versus closed metrics.

```sh
PYTHONPATH=tools/mvpa-inference python3 -m unittest discover -s tools/mvpa-inference -p 'test_calibration*.py'
python3 tools/mvpa-inference/calibration_readiness.py --output /tmp/readiness.json
```

The first command ran 24 tests: 16 existing and 8 readiness tests. The report
runs on current main and lists every refusal. No readiness JSON is committed,
because it hashes the working tree and would go stale on the next commit.

## Dropped, with reasons

- **B's `SubjectGroupSummary`, `HeldOutSubjectPrediction` and their suites.**
  They are superseded by A. B's typed target pairing and fixed-head
  `PatternPrediction` scoring could become a separate future adapter if a
  consumer needs absolute held-out loss for decoding heads. It is not merged as
  a parallel API.
- **`RankCalibrationRecord.scala` and its suite.** They do not compile against
  main: `Case.inputSha256` is absent and the schema string conflicts with main's
  `record_schema: 2`. Its extra provenance fields have no equivalent on main:
  - `source_commit`, `source_lock_sha256` and `case_input_sha256`;
  - checks on the receipt's seed, stop reason and alternative;
  - observed statistics.

  They are listed below as a follow-up and are not grafted in.
- **B's `calibration_protocol.py` v2 validator** (`p_raw`/`p_closed`). Main's
  `validate_rank_record` checks the same closure and also the exceedance counts.
- **B's logs, `verification.json`, integration and rank-observability READMEs,
  `implementation.md` and `readiness.json`.** They describe B's unmerged bytes
  and test counts (mvpaGroup 23), which do not exist on main.
- **B's independent review.** Its findings about B's code do not carry over.
  Its exposure finding (1) applies to A and was the basis for the port above.
  Its other findings were about B's own output-coordinate fixtures and
  replay/batch budget, which A does not have.

[`M5.04-queue.md`](M5.04-queue.md) updates B's M5.04 queue for current main.

## Gates

| Command | JVM | JS |
| --- | --- | --- |
| `python3 tools/build/sbt-warm mvpaGroupJVM/test` / `mvpaGroupJS/test` | 31 passed | 31 passed |
| `python3 tools/build/sbt-warm mvpaJVM/test` / `mvpaJS/test` | 468 passed, 3 skipped | 468 passed, 3 skipped |
| Python calibration tests | 24 passed | n/a |

`mvpaGroup` went from 27 tests to 31: two exposure tests and two summary
cross-checks were added. No module depends on `mvpaGroup`. The `mvpa` change
only widens the visibility of one function. `python3 tools/build/sbt-warm scalafimCompileAll`
(both platforms) exited 0 with no compiler warnings or errors.

`mvpa-group` was not scalafmt-clean on main: `SubjectGroupBridge.scala` and its
suite fail `scalafmtCheck` without edits. CI gates only `firstLevelLaws`.
Following the project rule to keep diffs local, the touched files were not
reformatted as a whole.
