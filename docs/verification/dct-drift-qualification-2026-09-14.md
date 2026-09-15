# Cutoff-period DCT drift qualification

Date: 2026-09-14
Tracker: `bd-01M21273A1SGJHQZP1W1X24SFZ`
Recovered source candidate: `81f6c16e6ab4f2626e74eb2e4f7444a2009714bb`
Adopted Gale provider: `18d24dbb5056122032b0278f8bad557a9bb1cf23`

## Admitted scope

ScalaFIM now has an explicitly parameterized `BaselineBasis.Dct` policy. For a
run with `N` original scans, repetition time `TR`, and cutoff period `P`, it
includes DCT-II components

```text
k = 1, ..., floor(2 N TR / P)
```

The endpoint is inclusive and component zero is excluded. Intercept policy
remains separate. The basis is constructed on each run's complete original
scan grid and censoring selects rows from that basis; censoring never compresses
or rebuilds the grid. Mixed-length and mixed-TR runs may therefore contribute
different numbers of drift columns.

The cutoff is positive, finite, explicit, and part of structural identity using
its exact `Double` bits. A cutoff at or below twice a run's TR would request a
component outside the finite DCT-II grid and is rejected rather than truncated.
Run-specific audit receipts retain original scan count, TR, acquisition start,
cutoff value and bits, component count, normalization, grid policy, endpoint
policy, and constant exclusion.

The recovered candidate's unrelated event-column-role changes and historical
binary receipts were not adopted. The generic DCT kernel is owned by Gale;
ScalaFIM retains only the fMRI cutoff policy, schema identity, and adapter.

## Independent evidence

- A four-scan radical-value oracle checks the first two nonconstant DCT-II
  columns independently of Gale and checks the intercept separately.
- Boundary probes check exact-cutoff inclusion, either side of the boundary,
  zero-column short runs, invalid cutoffs, finite-grid rejection, and large
  physical units.
- Mixed-run tests check variable column spans, structural identities, receipts,
  original-grid censoring, acquisition-time provenance, and explicit alias
  handling for supplied cosine confounds.
- An independent modified Gram-Schmidt Frisch-Waugh-Lovell oracle checks native
  selected coefficients and a contrast for complete data and for reordered,
  censored original rows. Production uses Gale QR and the selected-estimate
  adjoint, so the oracle does not reuse the production solve.
- The pre-existing latent DCT API is delegated to the same Gale capability and
  its fixture, orthonormality, projection, and reconstruction tests remain
  unchanged.

## Provider qualification

Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23` is published at
`canardlapin/gale` `refs/heads/main`; live remote SHA equality was checked. On
that commit:

- `coreJVM/test`: 658 passed
- `coreJS/test`: 648 passed
- `lawsJVM/test`: 54 passed
- `lawsJS/test`: 54 passed
- `scalafmtCheckAll`, `testAll`, and `compileAll`: passed
- `docs/mdoc`: completed with five pre-existing unrelated broken-link warnings

## ScalaFIM qualification

The ordinary pinned dependency path, without a sibling-build override, passed:

- `designJVM/test`: 249 passed
- `designJS/test`: 249 passed
- `latentJVM/test`: 44 passed
- `latentJS/test`: 44 passed
- `modelJVM/test`: 28 passed
- `modelJS/test`: 28 passed
- `fitJVM/test`: 336 passed
- `fitJS/test`: 324 passed
- `scalafimCompileAll`: passed across JVM and Scala.js without compiler warnings

These counts include the eight design-policy tests and the independent fitting
equivalence test on both platforms. Final clean-commit SHA evidence is recorded
below after the implementation commit is isolated from the concurrent checkout.

## Admission boundary

This qualifies basis construction, cutoff selection, schema/provenance behavior,
original-grid censoring, nuisance alias policy, and native selected OLS fitting
equivalence. It does not claim whole-pipeline SPM equivalence, does not introduce
a default cutoff, and does not establish that a separate filtering operation
commutes with AR/GLS whitening.

## Clean candidate

Pending implementation commit and isolated-worktree rerun.
