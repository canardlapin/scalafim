# PHRF frozen canonical-like domain gate

Source base: `61029ed3847b93827405482f11be704d3408f379`. The protocol was frozen
before running the candidate cohort; `protocol.sha256` binds it. This is an
experimental fixed-shape readout proposal, not a production-default change or a
calibrated physiological prior.

## Rationale and frozen choices

SPM's canonical HRF has gamma-component modes near 5 and 15 seconds and an
undershoot/positive area ratio of 1/6. Those three features anchor this proposal.
Cascade34 uses Erlang orders 3 and 4, so matching those features does **not**
reproduce SPM's component widths or its complete HRF. The equivalent Cascade34
anchor is `kappaP=0.4`, `kappaU=0.2`, `rho=1/6`.
[Source: official SPM implementation](https://raw.githubusercontent.com/spm/spm12/main/spm_hrf.m).

The declared sensitivity envelope has positive-component modes 4–6 seconds,
undershoot/positive rate ratio 0.4–0.6, and area ratio 0.1–0.3. These choices
imply undershoot-component modes 10–22.5 seconds. Component modes are not the
attained peak and trough of the signed mixture; the receipt reports those
separately at all 27 boundary-grid points. The selected widths are our proposed
sensitivity envelope, not population quantiles from the cited source.

Empirical HRFs vary across subjects and regions, so a canonical anchor alone
cannot establish population coverage. Any proposed production restriction still
needs empirical scientific justification for its intended population and task.
[Handwerker, Ollinger and D’Esposito (2004)](https://www.sciencedirect.com/science/article/abs/pii/S1053811903007584)
measured such variability and its consequences for statistical analysis.

Support is 96 seconds, with the unchanged kernel tolerance `1e-3`, maximum rank
24, `BlockedPartial(96)` compilation and `Blocked(32)` trial lowering. The
smaller domain may reduce attained rank; no compiler cap or approximation
accuracy is relaxed. An outward analytic bound uses Erlang survival functions:

```
integral_H^infinity |h(t)| dt <= S3(kappaP_min * H)
                                + rho_max * S4(kappaU_min * H)
Sn(x) = exp(-x) * sum_{j=0}^{n-1} x^j/j!
```

This uniform absolute tail-mass bound does not certify derivative tails,
coefficient accuracy or a decoded fit. Those must not be inferred from it.

## Routing and attempted cohort

Eight references occupy the centers of the eight equal cells in normalized
chart coordinates: `{.25,.75}^3`. The router selects the nearest reference in
normalized squared Euclidean distance, breaking ties by the smallest index.
It sees only requested coordinates. It does not evaluate objectives or use the
response, reference residuals or oracle results. Exactly one reference and one
residual correction execute per candidate readout. The true requested shape is
known in this conditional experiment; no decoder has been qualified.

Primary fresh shapes cover the entire unit chart, without the previous 2%
interior trim: 64 shapes at N=30 (seed 2026100803), and 256 at N=300 (seed
2026100804). These two shape cohorts use separate seeds. All 27 points
of `{0,.5,1}^3` are recorded separately, including corners and faces. Primary
noise/signal RMS ratio is 0.1. Ratios 0 and 1 reuse the first 32/64 primary shapes
as paired sensitivity checks. Response noise uses distinct per-shape seeds
`2026100805 + 10000*N + pointIndex`, with the same standardized noise across
paired noise ratios. There are 155 N=30 and 411 N=300 attempted requests.

All observations, events, signed heterogeneous amplitudes, nuisance regressors,
AR(1) whitening and lambda=1 condition-centered penalty follow the previous
neighborhood audit. Original-design augmented QR and exact prepared readouts
are separately charged diagnostic comparators; neither can replace the routed
candidate. Readout approximation, complete representation error, analytic tail
truncation and basis/grid discrepancy remain separate reported quantities.
The signed contrast is computed from retained amplitude outputs; this run does
not claim a query-only storage or execution measurement.

The conditional primary gate requires >=95% of fresh requests to meet the
existing original-amplitude relative L2 target of `1e-3`, together with the
frozen readout-work contract. Boundary and sensitivity counts are retained in
full. They are not pooled as independent evidence or silently dropped. A
conditional ScenarioResult reports that exact gate. A distinct production
ScenarioResult keeps scientific, decoding, certification and resource caveats
blocking; a conditional pass cannot close the epic.

## Resource scope and stop rule

The N=1200 preparation attempt retains the current 16-million-scalar cap.
Receipts list preparation, retained source data, full-reference and value-reference
bank estimates, and eight-worker objective arrays. These are scoped numerical
array estimates, with exclusions, not engine peak live-memory measurements.
Diagnostic construction can temporarily retain both the fixture's original bank
and the candidate bank. No complete B0 throughput measurement is claimed.

Run the frozen proposal once and keep failures. The domain, references,
correction count, tolerance and sample counts must not be retuned in response
to this cohort. Defaults and `CertifiedOriginalEquations` remain unchanged.

## Result: useful representation, insufficient routed coverage

The conditional N=30 gate passes. The N=300 gate fails: 232/256 fresh cases
(90.625%) meet the amplitude target; at least 244/256 are needed. Both production
ScenarioResults remain `Fail`. No parameters were changed after these results.

| Trials | Cohort | Noise ratio | Corrected/original passes | Exact prepared/original passes | Corrected p95 relative error |
|---:|---|---:|---:|---:|---:|
| 30 | Fresh | 0.1 | 64/64 | 64/64 | 0.07218% |
| 30 | Boundary | 0.1 | 16/27 | 27/27 | 0.12262% |
| 30 | Paired sensitivity | 0 | 32/32 | 32/32 | 0.08119% |
| 30 | Paired sensitivity | 1 | 31/32 | 32/32 | 0.08418% |
| 300 | Fresh | 0.1 | 232/256 | 256/256 | 0.11599% |
| 300 | Boundary | 0.1 | 2/27 | 27/27 | 0.23265% |
| 300 | Paired sensitivity | 0 | 58/64 | 64/64 | 0.10569% |
| 300 | Paired sensitivity | 1 | 52/64 | 64/64 | 0.21087% |

All 566 attempted requests are retained. Every routed candidate uses one
reference, three inverse applications, one correction and zero requested-shape
factorizations. Exact comparators never feed the routed result. The exact
prepared readout meets the original-observation amplitude target in every case;
its fresh N=300 p95 error is 0.01488%. This separates the representation remedy
from the remaining reference-readout approximation problem. These are sampled
agreements, not uniform coefficient certificates.

The basis attains rank 8. The outward uniform tail-mass upper bound is
`0.00036876633813037025` in positive-component-area units. The 27 attained-shape
probes span peak times 3.804–5.963 s, FWHM 6.065–9.985 s, and trough times
16.803–32.756 s. These widths and troughs reinforce that this is not an SPM HRF
replica; suitability for a scientific population remains a separate gate.

At N=1200, preparation succeeds with 11,390,325 retained scalars, below the
unchanged 16,000,000 cap. The bandwidth is 240. The listed shared arrays with
eight full references total 327,114,344 bytes (311.96 MiB); adding eight listed
objective workers gives 314.30 MiB. This exceeds the 256 MiB target even before
the excluded storage is accounted for. The value-reference-only scenario is
141.18 MiB, but it does not implement the full corrected readout and terminal
curvature contract. It cannot be substituted as a passing memory receipt.

N=300's corresponding full-bank/eight-worker estimate is 28.38 MiB, so the
stress-memory failure should not be attributed to the ordinary N=300 geometry.
Construction times are diagnostic on the shared M2 Pro host, not throughput
qualification. No complete production execution or live-memory admission ran.

## Next implementation gate

Post-hoc diagnosis finds that the 24 N=300 fresh failures are much farther from
their reference along `logKappaP`: mean normalized axis distance 0.229, versus
0.125 in the whole fresh cohort. The other two axis distances barely differ.
This suggests investigating anisotropic placement, such as more resolution in
the positive-rate direction, rather than choosing a smaller domain after seeing
these failures. It does not establish that any proposed placement will pass.

The next bounded work package is:

1. Support an explicit bank of at most eight reference points independently of
   the regular decoder grid. Choose placement on a separate training cohort,
   then freeze it before a new confirmation cohort. Keep one routed readout and
   one correction. The current cohort becomes development evidence.
2. Audit which derivative/reference arrays the corrected readout and terminal
   admission actually retain. Test a compact or on-demand cache policy with
   complete construction/scratch/work accounting. Do not claim the 141.18 MiB
   value-only estimate as the answer.
3. Recheck fresh accuracy and stress storage before the next full timing sweep.
   Original-equation certificates within the work allowance, decoder admission,
   scientific domain validation and calibration remain open.

## Validation and reproduction

`TrialDomainGateSuite`, `TrialNeighborhoodAuditSuite` and
`DecodedTrialCheckpointSuite` pass on both JVM and Scala.js: nine tests per
platform. Compiler output is warning-clean; the existing multiple-main discovery
message is retained in `validation.json`. Changes are confined to shared/JVM
test support and evidence; production code and the Gale pin are unchanged.

```sh
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly *TrialDomainGateSuite *TrialNeighborhoodAuditSuite *DecodedTrialCheckpointSuite' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialDomainGateMain docs/verification/phrf-domain-gate-20261008/protocol.json /tmp/phrf-domain-gate.json gate' \
  'firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialDomainGateMain docs/verification/phrf-domain-gate-20261008/protocol.json /tmp/phrf-domain-stress.json stress' \
  'firstLevelLawsJS/testOnly *TrialDomainGateSuite *TrialNeighborhoodAuditSuite *DecodedTrialCheckpointSuite'
python3 -S docs/verification/phrf-domain-gate-20261008/verify.py
```

`manifest.json` binds source and artifact hashes, while `protocol.sha256` binds
the pre-measurement specification independently. Prior verification packets stay
frozen at their own revisions. PHRF-11 remains in review; PHRF-14/33 and the epic
remain open. This is a documented failed gate with a narrower, evidence-backed
next engineering step, not a release qualification.
