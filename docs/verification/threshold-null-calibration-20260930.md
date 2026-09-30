# Threshold null calibration — packet 3 results (2026-09-30)

Mote: `bd-01M2GTMY1KYB94RGJARBB1GBK8`, packet 3. This packet implements the
predeclared [protocol](threshold-null-calibration-protocol-20260930.md) as
`threshold.null-calibration.*.v1` scenarios in
`modules/threshold/shared/src/test/scala/scalafim/fmri/threshold/scenarios/NullCalibrationScenarioSuite.scala`.
It adds one `build.sbt` edit: `threshold` now depends on `scenarioTestkit` in the
test scope only.

## Held-out calibration result

The profile was `SCALAFIM_THRESHOLD_CALIBRATION=calibration` (R = 2000 per
condition, held-out base seed 7310930), run once, on the JVM. All 16
conditions are **Pass**, and all 32,000 replicates completed with no `Left`.
The per-condition observations are in
[calibration-results.txt](threshold-null-calibration-20260930/calibration-results.txt).

FWER, as errors out of 2000, is identical for maxT and WY unless both are
shown:

| Condition | Complete null | Partial null (strong control) |
| --- | --- | --- |
| exact n=6, white, greater | 99 (0.0495; size 3/64 = 0.0469) | 56 (0.028) |
| exact n=6, white, two-sided | 70 (0.035; size 2/64 = 0.0313) | 68 (0.034) |
| exact n=6, smooth, greater | 91 (0.0455) | 89 (0.0445) |
| exact n=6, smooth, two-sided | 50 (0.025) | 53 (0.0265) |
| MC n=10, white, greater | 107 (0.0535) | 82 (0.041) |
| MC n=10, white, two-sided | 115 (0.0575, P(X≥k \| α) = 0.071) | 92 (0.046) |
| MC n=10, smooth, greater | 108 (0.054) | maxT 110 / WY 113 (0.055 / 0.0565) |
| MC n=10, smooth, two-sided | 102 (0.051) | maxT 107 / WY 113 (0.0535 / 0.0565) |

- **Liberal gate.** No condition reaches the P < 0.001 liberal gate. The
  smallest tail is 0.071.
- **Exact-size checks.** Where the size is exactly known, the observed
  rates match it: all lower tails are ≥ 0.058.
- **Agreement.** maxT and WY agreed in every complete-null replicate.
- **Strong control.** Under partial nulls WY rejects a superset of maxT, as it
  should, and both stay within the liberal gate.

## Pull-request profile and its power

The profile uses R = 200 and the development seed, on both platforms.
`thresholdJVM/test` and `thresholdJS/test` each pass 60/60, with no warnings
(log `c81c55e623321739…`).

A planted bug in which null draws are not oriented (streamed reducer and WY)
fails all four exact two-sided conditions, through the identity-row check
(log `f6843e8efa64869d…`). In the Monte Carlo two-sided conditions it doubles
the FWER to 0.095 (19/200, P = 0.006), which R = 200 cannot flag at the 0.001
gate. The pull-request profile is therefore a regression smoke test. It
detects contract breaks and gross miscalibration. The R = 2000 held-out run is
the calibration evidence, and there the same doubling would give about 190
errors against 100 expected.

## Finding: HierScan reporting and alpha allocation diverge from the reference

Every HierScan complete-null condition recorded **0/2000** errors under the
predeclared event, "any significant region". That passes the protocol, but it
is not evidence of calibration.

An exploratory probe, on the development seed only, with the held-out seed
untouched, used log `ecc510136e854d0b…`. It showed a depth-1 node rejection
rate of 4–12 in 200 (about α), and zero terminal hits in every case:

- **Reporting (verified).** `HierScan.applyTests` records a hit only when the
  rejected child is terminal. A rejected coarse region whose children are all
  accepted is dropped from `significantRegions` and `reject`. The procedure's
  own family-wise rejection therefore never reaches the result. The R
  reference (`neurothresh::hier_descend`) records every rejected child as a
  significant region.
- **Alpha allocation (derived, not yet measured).**
  - R spends `γ · budget` on each node's test and passes
    `(1 − γ) · budget`, weighted by child prior mass, to the rejected
    children, so a whole tree spends at most α.
  - The Scala port tests each node at its full budget and gives each rejected
    child `budget / nChildren`. Under a partial null, a correctly rejected
    signal node is followed by further tests whose budgets add up. The bound
    is roughly α(1 + 1/8 + 1/64 + …), so strong FWER control is not
    guaranteed.
  - The protocol does not score HierScan under partial nulls, so this
    calibration run neither confirms nor refutes it.

**Consequence.** HierScan is not admitted as a calibrated method by this
packet. Its complete-null FWER claim holds for the events it reports, but the
result omits valid rejections and the allocation lacks a strong-control
argument. Packet 4 will align HierScan with the reference:

- Report every rejected node.
- Spend alpha with γ, weighted by prior mass.
- Score region-level error under partial nulls in a new, dated protocol with a
  fresh held-out seed.

## Method admission after packet 3

| Method | Status |
| --- | --- |
| `MaxT` (`runMap`, `singleStep`) | Admitted for FWER under sign-flip-exchangeable nulls: complete and partial null, white and smooth fields, heterogeneous subjects, n = 6 exact and n = 10 Monte Carlo. |
| `WestfallYoung.stepDown` | Admitted on the same terms, including strong control in the tested partial nulls. |
| `HierScan` | Not admitted; see the finding above. |
| TFCE, cluster-FDR, RFT, voxelwise FDR | Unavailable: not implemented, and removed from `ThresholdMethod` in packet 1. |

## Not covered

These are unchanged from the protocol:

- first-level uncertainty propagation;
- non-symmetric errors;
- adaptive HierScan priors;
- the unused `QValue`, `EvidenceScore`, `PSide` and `StatKind.T.df`.
