# Metric binding required before confirmation

The preserved pilots record the four closed adjusted p-values
`pAdjusted(k) = max(p(1), …, p(k))` and corresponding sequential decisions.
Upstream also returns each raw step p-value in
`candidateArithmetic.receipts(k).pValue`. Raw values were not recorded for these
600 datasets. Taking differences or otherwise inverting the cumulative maximum
cannot recover them. These pilots will not be retrospectively rerun or relabelled.

The frozen protocol specifies closed operational rank rejection in section 5,
while section 8.2 separately names partial-null pointwise calibration and R0
closed-sequence FWER. A confirmation receipt must explicitly bind which decision
is used for each primary criterion before any confirmation stream is observed.
Retain both raw and adjusted p-values, their decisions, and the original null
vector in future records. Adding observability does not authorize changing the
statistic, family, alpha, effect, dataset/draw count, or calibration band.

The two decision meanings must remain distinct:

- A raw stage decision describes calibration of that individual remaining-root
  null statistic. It cannot replace the closed sequence for a detectable-rank
  claim.
- Closed decisions determine operational sequential rejection and family error.
  Under partial nulls, failure to detect an earlier nonzero root can suppress a
  later true-null rejection. Conservative behavior still fails a prescribed
  near-nominal lower bound if that lower bound is assigned to the closed metric.

Current closed R2/H3 pilot error is 2/200; its descriptive CP90 interval
[.001780, .03114] lies below .035. Preserve this result. It neither proves a raw
stage calibration defect nor licenses a different primary metric selected after
seeing which one passes. If the intended metric requires a protocol clarification
or versioned amendment, authorize and freeze that decision explicitly before
confirmation. Keep the original criterion and disagreement visible.

The full confirmation inventory, source lock, scientific prerequisite admission,
and resource approval are also incomplete. No environment flag or pilot summary
establishes confirmation readiness.
