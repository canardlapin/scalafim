# Joint effect-and-variance Gaussian bootstrap: confirmation result (2026-09-30)

Mote: `bd-01M21BNZR9ZBRAYY9JD5WCQ8KX`. Declaration: `proposal-bootstrap-v2` (owner-signed). Decision rule:
`decision-rule/v2` (owner decisions of 2026-09-30).

## Outcome

**All three candidates are Declined** under the pre-registered rule:

| Candidate | Outcome | Null cells (Pass / Unresolved / Fail) | Reason |
|---|---|---|---|
| B-plug | Decline | 8 / 4 / 0 | Definite power loss against native PM/mKH in `C-n20-DI-Vspread-T0-N8` |
| B-fixV | Decline | 3 / 4 / 5 | Null Fails (anti-conservative), as the conditioning analysis predicted |
| B-EB | Decline | 10 / 2 / 0 | Definite power loss against native PM/mKH in `C-n20-DI-Vspread-T0-N8` |

Notes on the outcome:

- A research-ticket Decline with evidence is a valid outcome. No production method is admitted, and the native
  PM/mKH and equal-subject HC3 baselines remain what they were.
- Gain holds for every candidate in only one power cell, `C-n8-DG-Vrev-T2-N8`. Neither Bound sub-family qualifies:
  - n ≥ 20 contains Unresolved cells for B-plug and B-EB;
  - ν ≥ 40 contains an Unresolved cell for every candidate and has no power cell with a gain.

## Provenance

| Item | Value |
|---|---|
| Frozen commit | `d4e2280a1dc6dcb018c5161a9c3129d30358ed8e` |
| manifest-v2 | sha256 `b37f83fa…` ("frozen for confirmation"); parent `cells.json` `76e6785b…` |
| Selection | pilot `selection.json` `8c3f57c1…` (six data-selected cells plus six fixed) |
| Pilot tree | digest `67bd26ba…` |
| Design | 12 cells, R = 20000, B = 999, roots 2026100201–05, 4 threads |
| Compute | 9567.5 CPU s = 2.66 core-hours (ceiling 15), 2896 s wall |
| Output | `/private/tmp/scalafim-execution-20260929/bootstrap-confirmation-20260930/`: 16 cell streams, all `.sha256` verified, no partial files |
| Output digests | `outcomes.json` `12fb773a…`; output tree `35271a92…` (sha256 of the sorted `shasum -a 256` listing) |
| Log | `bootstrap-confirmation-20260930-run` |

- The verdicts were read from `outcomes.json` after the run completed.
- No rate was read or printed before the confirmation ended. The pilot's descriptive summaries and the earlier heavy
  log were never read. The heavy log stays sealed.

## Descriptive null rejection rates (read after completion; not decision inputs)

Failures count as rejections. The frozen thresholds are Pass at k ≤ 1149 (rate ≤ .0575) and Fail at k ≥ 1456
(rate ≥ .0728).

| Cell | B-EB | B-fixV | B-plug | known-v/frozen τ² | ν swapped | u* omitted | recentred | uncentred |
|---|---|---|---|---|---|---|---|---|
| n20-DG-flat-T0-N8 | .0464 | .0669 | .0469 | .0553 | .0559 | .0524 | .0486 | .0000 |
| n20-DG-rev-T0-N8 | .0551 | .0825 | .0532 | .0683 | .0670 | .0559 | .0543 | .0000 |
| n20-DG-spread-T2-N40 | .0532 | .0547 | .0526 | .0493 | .0510 | .0809 | .0556 | .0000 |
| n20-DI-flat-T0-N8 | .0530 | .0795 | .0534 | .0681 | .0669 | .0573 | .0546 | .0000 |
| n20-DI-spread-T0-N8 | .0428 | .0609 | .0418 | .0452 | .0510 | .0510 | .0447 | .0000 |
| n8-DG-rev-T2-N8 | .0481 | .0628 | .0474 | .0290 | .0408 | .0722 | .0587 | .0000 |
| n8-DG-spread-T2-N40 | .0666 | .0690 | .0654 | .0283 | .0462 | .1207 | .0892 | .0000 |
| n8-DI-flat-T0-Ninf | .0367 | .0367 | .0367 | .0208 | .0140 | .0480 | .0441 | .0000 |
| n80-DG-flat-T0-N8 | .0565 | .0759 | .0593 | .0727 | .0730 | .0558 | .0588 | .0000 |
| **n80-DG-rev-T0-N8** | **.0694** | **.0973** | **.0647** | .0933 | .0936 | .0568 | .0632 | .0000 |
| n80-DI-flat-T0-N8 | .0550 | .0765 | .0585 | .0747 | .0747 | .0541 | .0580 | .0000 |
| n80-DI-spread-T2-N8 | .0495 | .0513 | .0503 | .0522 | .0517 | .0475 | .0500 | .0000 |

Power (candidate rejection rate at the frozen alternative):

| Cell | B-EB | B-fixV | B-plug |
|---|---|---|---|
| n20-DG-spread-T2-N40 | .433 | .436 | .434 |
| n20-DI-spread-T0-N8 | .351 | .387 | .347 |
| n8-DG-rev-T2-N8 | .361 | .404 | .366 |
| n80-DI-spread-T2-N8 | .470 | .477 | .471 |

The native comparator's rates are in the per-study records but are not tabulated here.

### Interpretation (descriptive)

- **The original failure cell, n80 reversed-variance τ²=0 ν=8 (native PM/mKH historically 8.85%).**
  - B-plug (6.47%) and B-EB (6.94%) reduce the inflation, but neither is within the .065 margin with confidence;
    both are Unresolved.
  - B-fixV (9.73%) reproduces the inflation. That is predicted: conditioning on the observed v does not correct the
    incidental-variance problem.
- **Discriminating controls behaved as predicted:**
  - the uncentred bootstrap never rejects;
  - omitting u* inflates at τ²=.2 (up to 12.1%);
  - swapping ν and freezing τ² at a known v inflate in the ν=8, τ²=0 cells.
- **B-EB is the best calibrated** (10/12 Pass, none Fail). It still loses power against the native comparator in the
  intercept-only spread-variance cell, which triggers the pre-registered Definite-loss Decline. The bootstrap adds
  cost without a demonstrated benefit over native PM/mKH in the confirmed domain.

## Limits

- Evidence covers these 12 confirmation cells only. All other cells remain exploratory.
- Only Gaussian Model J is covered. Non-Gaussian and y–v-dependent stress was not part of the confirmation.
- A Decline does not show that no uncertainty-aware bootstrap could work. It shows that these three declared
  candidates did not meet the pre-registered bar.
