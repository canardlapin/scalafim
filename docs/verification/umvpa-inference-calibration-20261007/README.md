# M4.09 calibration infrastructure, simulator QA, and pilots

This packet qualifies engineering controls and the declared simulator subset. It
does not release statistical claims, complete the frozen confirmation campaign,
or close M4.09. No final audit archive has been created.

## Executed evidence

- The external R seed fixture checks raw UTF-8 SHA-256, full 64-bit decimal seeds,
  independently expanded L'Ecuyer-CMRG noise states, and an analytic affine known-
  covariance oracle. The portable Scala fixture runs on JVM and JavaScript.
- Python has nine protocol guards, including plan versus execution, forbidden
  confirmation from a proposal, incomplete denominators, retry identity, and
  population-null versus conditional-null contradictions.
- `qa-R0-supervised`, `qa-R1-supervised`, and `qa-R2-supervised` each generated the
  prescribed 10,000 independent n=80, p=6, q=4 intercept Gaussian datasets. They
  passed the frozen mean, covariance, and noiseless controls. Observed worker RSS
  was 108–113 MB and elapsed process time 1.3–1.5 seconds. The initial R0 run's
  unavailable resource monitor remains recorded as inconclusive in `qa-R0`.
- `integration-gates.json` records parent-executed module gates: JVM 662 passes
  and one explicit opt-in skip; JS 661 passes and one explicit opt-in skip. These
  are engineering tests. The subsequent opt-in timeout override is separately
  identified in `pilot-inputs-ready.json`. A subsequent full MVPA run passed
  454 tests with one explicit campaign skip on each platform; the additive
  source-bound proof is in `integration-gates.json`.
- The corrected adapter was qualified on JVM and JS and committed as
  `3b8d35e6aa989e2c6a5dadebabb2db608fc478cd` before execution. The B=1999 cost
  fixture then succeeded, followed by three pilots of exactly 200 datasets each
  at B=199. All 600 pilot records are evaluated; none failed or disappeared.
  Raw records, process receipts, worker logs, and independent descriptive
  analysis are retained. See [pilot-summary.md](pilot-summary.md).
- Closed pilot errors are R0 8/200, R1/H2 6/200, and R2/H3 2/200. R2's
  descriptive CP90 upper bound .03114 is below the frozen near-nominal lower
  bound .035. This warning remains visible. Raw stage p-values were not
  recorded; [preconfirmation-metrics.md](preconfirmation-metrics.md) documents
  the required metric binding before confirmation.

Simulator QA does not test type I error, confidence coverage, power, FWER, or FDP.
The R0/R1/R2 subset does not replace the full frozen scenario inventory.

## Prepared execution inputs

`pilot-inputs-ready.json` locks all 601 prepared TSV case files and the current
source/manifest hashes. Data files are temporary under `target/umvpa-calibration`.
The three pilot lists each contain exactly 200 datasets with B=199. The separate
`fixture-B1999-locked` list contains one fixture dataset with B=1999 for cost
measurement. No production statistic results were generated during preparation.

The first committed B=1999 cost invocation failed before statistic computation:
the test adapter passed a dotted scenario name to `EstimandId`, which admits only
letters, digits, hyphens, and underscores. Its worker log and resource receipt
are retained; it produced zero numeric records. The corrected test-only adapter
uses a domain-separated SHA-256 of the complete original scenario, ordinal, and
actual protocol root seed for
internal identifiers, while output retains the unchanged scientific scenario.
Nonfatal evaluation exceptions become failed dataset records, with null
inferential outputs, and do not erase later assigned cases. Small portable
regressions cover the real dotted identifier and that failure boundary. These
corrections passed parent JVM/JS qualification and were committed before retrying
with a fresh output path. Simulator QA and all prepared input values remain
unchanged. The successful retry is in `run-fixture-B1999-locked-identifier-v2`;
the original failed invocation remains in `run-fixture-B1999-locked`.

Protocol section 3 requires the portable fixture to be committed before a pilot.
Root owns that commit and each actual run. The actual four successful runs used
the retained `resource-supervisor-identifier-v2.py`; their process receipts bind
its hash and limits. The underlying fixture selector is shown below for
reference; it does not independently enforce the process resource limits:

```sh
SCALAFIM_CALIBRATION_CASE_LIST="$PWD/target/umvpa-calibration/fixture-B1999-locked/case-files.txt" \
SCALAFIM_CALIBRATION_OUTPUT="$PWD/target/umvpa-calibration/fixture-B1999-records.jsonl" \
SBT_WARM_HEAP=2g \
python3 tools/build/sbt-warm 'mvpaJVM/testOnly scalafim.fmri.mvpa.inference.RankConfirmationSuite'
```

Use a fresh output file and replace the fixture list with `pilot-R0`, `pilot-R1`,
or `pilot-R2` for the approved pilot cells. The JS selector is the same class in
`mvpaJS/testOnly`. Shut down and restart the warm server between each input list:
the JVM test reads the server's launch environment, not a later client's changed
environment. Set `SBT_WARM_CPUS=4` explicitly. One process, at most four CPUs, a 2 GiB JVM heap, a monitored
3 GiB process RSS cap, and a 900-second cell wall cap are required; the test timeout
alone does not enforce RSS or CPU. Preserve all partial records if interrupted.
Source-lock validation is separate from a manual environment variable. The
initial adapter explicitly refuses confirmation and simulator case phases.

Every result stamps the actual stage-103 resampling child seed, actual draw count,
completed compact fits, member identities, and the declared null vector. R1 and
R2 contain nonnull leading canonical roots; they are not globally null families.

## Remaining release boundaries

The confirmation counts remain 10,000 null/refusal datasets and 5,000 alternative
datasets, with B=1999 and dataset bootstrap B=9999. Frozen tolerances and failures
are retained. No pilot result can substitute for these counts.

The catalog retains unresolved nuisance/population definitions, temporal HRF/TR
and fourth task-column inputs, selection-aware C2 inference, mixed association/
prediction families, per-component voxel partial-null invariance, and FDR
references. Population zero incremental effect does not necessarily imply the
fixed-trained-head conditional null: both truth labels and any contradiction
must be retained. Estimated-to-known covariance promotion is inadmissible.

The available voxel factory is an omnibus-all-target Gaussian candidate with a
known common row shape, Huh–Jhun residualization, common actions, and owned source
capture. It has no released strong-FWER claim before independent qualification.
M4.07 and the larger M4.08/M4.09 scientific release prerequisites remain explicit.
Resource refusal is a result; dimensions, dataset counts, or draw counts must not
be reduced to obtain a passing receipt.
