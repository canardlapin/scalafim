# Rank metric clarification v1

Accepted by the user's 2026-10-07 instruction to proceed with the recommended
raw-stage/closed-sequential split, before the expanded pilot or confirmation.
This addendum resolves metric ambiguity in the frozen inference calibration
protocol; it does not change alpha, populations, seeds, budgets, or rate bands.

| Population and cell | Primary metric | Retained secondary evidence |
| --- | --- | --- |
| R0 null | Any closed sequential rejection across H1–H4 (FWER) | Every raw and closed stage p-value |
| R1 null | Raw H2 rejection (pointwise type I) | Closed H2 and closed true-null-family FWER |
| R2 null | Raw H3 rejection (pointwise type I) | Closed H3 and closed true-null-family FWER |
| R3 null | Raw H4 rejection (pointwise type I) | Closed H4 and closed true-null-family FWER |
| R3 alternative | Closed H3 rejection, new root .20 (standard power) | Every raw and closed stage p-value |
| R1, R2, R4 alternative | Descriptive closed rejection of the last nonzero root (.50, .30, .12) | No standard-power threshold on these cells |

The near-nominal CP90 band applies to the primary null calibration metrics.
Closed partial-null rates can be conservative and remain visible; they are not
relabeled as raw rates or required to reach the lower calibration bound.
Detectable rank and all power summaries use closed decisions. Schema 2 retains
actual raw exceedances, plus-one probabilities, cumulative-maximum closed
probabilities, and both decision vectors at alpha .05. Historical aliases
`p_values` and `reject` continue to mean closed sequential values.

The previous R0/R1/R2, n80, p6/q4, intercept null pilots each consumed their
fixed 200 datasets with B199. Their closed counts (8, 6, 2) are immutable
historical evidence, including R2's conservative result. Raw values were not
recorded and cannot be reconstructed from closure. Those three cells will not
be rerun or assigned invented raw counts. The expansion runs the remaining
61 previously unused scenario streams: 12,200 datasets and 2,427,800 distinct
nonidentity draws, with no confirmation streams consumed.

Pilot rates are descriptive feasibility evidence, not scientific admission.
Confirmation requires the original independent-oracle, complete-inventory,
source/runtime-lock, and resource gates. The strong-minus-weak power contrast
still requires its own explicitly paired population/estimand definition; the
R1 and R4 results alone do not define a same-rank strength contrast.
