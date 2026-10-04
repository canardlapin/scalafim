# Native TrialBanded ML backend (2026-09-30)

Mote: `bd-01M3RG7S2JG1576PM9MWT5HM7Q`. Base: `105656fc` (accepted public/executor
`43db2e83`, criterion `0017`, helper `4ec40483`). Gale comes from the accepted
local override `-Dscalafim.gale.build=ROOT/gale-logdet` (base `18d24dbb` plus the
accepted `BandedLogDetJet` source). This is local-provider evidence only: the pin
is unchanged and there is no hosted qualification.

## Scope

`TrialBandedMlBackend` implements the fixed `CoherentTrialMlBackend` over one
TrialBanded worker. It does not enable executor ML: `ProfileHrfFit`'s early
refusal, the public criterion wiring, scientific calibration, performance and
PHRF-11/29/14/15/16 closure all stay open.

- **One factor.** `TrialBandedObjective.reference` now makes its only N-sized
  factorisation through the helper's stamped `TrialAcceptedTrialBand`, built from
  a frozen canonical copy of the value band. The reference retains the bundle,
  so raw E (`scoreReference`/`fullJet`) and the determinant use its exact factor
  instance. Numerics are unchanged: the existing fit suites and the TrialBanded
  laws pass.
- **One token per evaluation.** Each continuous evaluation mints one
  `CriterionReference` (Value or Full order) that both raw and determinant carry
  by identity. `pointAt` mints the response `CriterionEpoch`. Node references
  are minted once at setup.
- **Nodes.** Determinant jets are response independent and are cached at setup
  from the node references; that helper work is setup work. `valueAtNode` and
  `jetAtNode` reuse the cached full node reference. Workers never refactor.
- **Continuous.** `valueAt` builds one value-only reference and returns raw E
  plus the scalar constrained determinant of the same factor; it forms no
  determinant derivatives. `jetAt` builds one full reference, then computes the
  raw full jet and the helper determinant jet from it, and releases it. Public
  `jetAt` and `logDetAt` are never paired.
- **Refusals.** Response length/finiteness, coordinate dimension/finiteness,
  node range and missing epoch are typed and refused before numerical work. A
  rank-deficient `[F, XM]` is refused by the existing release factor while the
  node bank is built, so no backend exists for it.

## Work

The 22-field attempted snapshot is unchanged. `TrialBandedWork` gains counters
outside that snapshot:

- accepted-band N factorisations and failures, taken from the factory's actual
  receipt. A pre-factor construction refusal (for example, nonfinite active band
  entries) charges no Gale factorisation to either `factorAttempts` or the
  N-factor counters, but still charges a reference failure;
- legacy scalar-determinant membership RHS columns;
- helper solves, RHS, membership columns, small B factors and log-determinant
  jets, charged inside `determinantJet`, where the helper runs.

Every backend attempt's `TrialMlWork` is a before/after delta of those counters,
so no caller can hide helper work. The fields are:

- `referenceAttempts`;
- `nFactorAttempts`/`nFactorFailures`, one per reference;
- `solveAttempts` and `rightHandSideAttempts` (legacy plus helper);
- `membershipRightHandSides`, legacy plus helper. The helper duplicates the
  scalar determinant's membership solve and small factorisation, and both are
  counted until explicitly removed;
- `derivativeRightHandSides`;
- `smallFactorAttempts` (legacy release and scalar-determinant factors, plus the
  helper's B);
- `logDetRecursionAttempts`;
- `failures`: refused attempts, counted once each.

Setup work combines the node-bank build with the node determinant jets. A
refusal during setup reports partial node work.

## Evidence

The shared `TrialBandedMlBackendSuite` (8 tests) runs on JVM and JS under the
override. The oracle is independent:

- X(theta) is assembled from the design source and compiled basis at the actual
  shape; X, F and y are whitened by an explicit two-run AR(1) recurrence with
  reset.
- `K = I + (XP)(XP)ᵀ/lambda` and `D = log|K|` come before nuisance.
- `E = r'K⁻¹r` is the GLS profile over `Z = [F, XM]`, with Gale dense solves.

It shares only the compiled basis and Gale's dense kernels with production, and
never the accepted band, the release or the factor.

- Off-node Gaussian 2D and Cascade 3D (sigma2 = 2.5, nuisance, unequal counts,
  a singleton): E, D, J, score and condition means match the dense values
  (J to about 1e-15 relative). Centered finite differences at h = 2e-3 and 1e-3
  check every gradient and Hessian entry of E, D and J; errors are about 1e-6
  and shrink on halving. In 3D, |H02 - H11| of D is more than 1000x the oracle
  error.
- Value/full coherence at the same coordinates: E, D and amplitudes agree. Each
  evaluation has one reference: raw and determinant are `eq`; value and full
  references are distinct.
- Work: value calls charge 1 reference, 1 N factor, 2 small factors, C
  membership columns and zero log-determinant recursions or derivative columns.
  Full calls charge 1 reference, 1 N factor, 3 small factors, 2C membership
  columns (the duplication), (d + d(d+1)/2)·C derivative columns and 2 recursions.
- Nodes: setup charges 2 log-determinant recursions, 1 N factor, 1 reference
  and 2C membership columns per node. Worker node calls charge none of them, and
  node values reuse the cached full reference. Node results agree with the
  continuous path at node coordinates. `newWorker` shares owner, cache and setup
  receipt but not the epoch.
- Pre-factor accounting control, for outside-chart accounting only and not
  for admissible decoder candidates. At finite Gaussian coordinates (5, -1000)
  the kernel overflows and the factory refuses before Gale. Both value and full
  calls report 1 reference attempt, 0 N factors, 0 small factors, solves, RHS
  or log-determinant recursions, and 1 refusal. The legacy deltas are
  1 reference attempt, 1 reference failure, and 0 factor attempts, failures and
  solves. A following in-chart evaluation succeeds with one N factor.
  - On the original `6a5d0838` this control fails: it reports a phantom N
    factor attempt and failure (`logs/trial-ml-native-repair-prefix.log`).
  - The partial-release failure control with positive factor and solve work
    remains in `TrialBandedWorkReceiptSuite`.
- Refusal before pointAt, bad response length or NaN, and bad coordinates are
  all typed with zero factor work, and epochs increase. Aliased nuisance is
  refused as `ReleaseRank`. lambda = 1e-6, 1 and 1e4 with coincident trials are
  all admitted and match the dense model.
- Unchanged decoder, through `TrialMlDecoder` with sigma2 = 0.05 (comparable to
  the fixture's residual variance):
  - Scored node energies are J, and differ from raw E.
  - With the default budget, the retained `DecoderCounters` show a scalar
    candidate (`exactEvaluations` >= 1) and a reserved terminal verification.
    Terminal evidence is available and bound to the returned coordinates and
    `dataHessian`. The status is `BudgetExceeded`; no stationarity or
    qualification is claimed.
  - The returned energy is J at the returned point.
  - With a fixture-specific budget, the decode is Accepted, with terminal
    evidence bound to the returned coordinates and `dataHessian`.
  - The same decoder on raw E alone lands elsewhere (axis 1: 0.477 vs 0.451).
  - Missing capability and invalid sigma2 are refused.
  - With sigma2 = 2.5 or the default budget the decoder reports
    `BudgetExceeded`, an existing decoder behaviour outside this scope.
- Planted bugs, each caught (backend mutants run on 6a5d0838; the backend is
  unchanged by the counter repair):
  - a separately minted determinant reference fails 5/7;
  - a hidden full determinant jet in a value call fails 1/7 (this first
    survived and exposed the accounting gap fixed above);
  - dropping D from values fails 2/7;
  - a node using a neighbour's cached determinant fails 2/7.

Focused suites on JVM and JS: the new suite, CriterionJet, TrialMlObjective,
TrialConditionalSolve, ProfileTrialReadout, TrialBandedWorkReceipt and
TrialConstrainedLogDetJet (55 tests), plus the TrialBanded laws (9 tests) and a
warning-clean `scalafimCompileAll`. Logs, metadata and source hashes are in the
Mote handoff.
