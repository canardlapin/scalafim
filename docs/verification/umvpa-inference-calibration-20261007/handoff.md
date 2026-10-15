# Local handoff — 2026-10-07

Worktree: `/private/tmp/scalafim-umvpa-family-group-20261007`.
Branch: `work/umvpa-family-group-20261007`, based on `d6e922a0`.
No push, pull request, or merge was performed for this slice.

Implementation commit: `06948508ed4b191cc702c51000b2db6b0ca3f505`.
Actual pilot source commit: `3b8d35e6aa989e2c6a5dadebabb2db608fc478cd`.
Subsequent evidence commits add retained run artifacts and handoff records;
they do not change the methods or relabel the pilot source.

## Native status

- **M5.02** `bd-01M2BNHVYX5971WGE1C78K5041` is closed. The subject/group
  bridge preserves full covariance and uncertainty/df provenance, uses adopted
  group models, and rejects unsupported joint claims and invalid sources.
  Evidence: `../umvpa-subject-group-20261007/`.
- **M4.08** `bd-01M2BNHJ25MD4S4SKXZS6WDP9K` remains doing. The scoped voxel
  omnibus candidate is implemented and engineering-qualified. M4.07 is still
  doing; mixed component/prediction families, component partial-null references,
  FDR, and independent strong-FWER qualification remain unavailable.
  Evidence: `../umvpa-family-randomization-20261007/`.
- **M4.09** `bd-01M2BNHMAR9S58QCMT2DEM16VN` remains doing. Simulator QA covered
  30,000 datasets; three actual pilots covered 600 datasets at B=199. A separate
  B=1999 fixture succeeded. The full frozen confirmation campaign was not run.

Full affected gates passed 662 JVM and 661 JS tests, with one declared opt-in
campaign skip on each platform. Additive test-only corrections have their own
source-bound receipts in `integration-gates.json`. The warm server was shut
down after the last pilot, preventing calibration launch variables from leaking
into future ordinary test gates.

## Next actions

1. Resolve and freeze raw pointwise versus closed operational metric roles.
   The user clarification is pending. Retain the original closed pilot warning:
   R2/H3 2/200, CP90 [.001780, .03114]. No raw stage reconstruction is possible.
2. Complete the scientific scenario inventory and population definitions,
   including temporal HRF/TR/fourth-column inputs, nuisance covariance, and
   population versus fixed-trained-head conditional prediction nulls.
3. Qualify native M4.07 and the required M4.08 family references. Engineering
   receipts do not admit these scientific prerequisites.
4. Freeze executable source/runtime locks and actual confirmation resource
   admission. Thirty-two defined rank cells alone require 240,000 datasets and
   479,760,000 dataset-level draws; the ~8.38-hour pilot kernel projection is
   neither a measured campaign cost nor resource admission.
5. Execute the unchanged 10,000-null/5,000-alternative, B=1999 confirmation
   streams only when those prerequisites are met. Preserve failures and refusals.

The original checkout has concurrent ProfileHrf work. Keep this branch isolated
until integration is explicitly requested. Claim/reservation releases hand off
ownership only; neither inference mote is closed by them.
