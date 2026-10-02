# Frozen-candidate rank arithmetic, 2026-10-02

Mote `bd-01M2BNHG03Y1EV95H0CMQ86NRN`, over native baseline
`424b8541c9ed83e97c4407b14be9bf3b372a4f2c`. Generic statistical kernels are
committed locally in Multivar
`ab811e257dd67f77e8c3b70cb1ea600f274429a3`, whose parent is the rotation
candidate `e1146d1835d19cbdedaf65df02af6bad6e691e64`. Nothing has been pushed.
ScalaFIM's default Multivar pin remains `f74d631720d65147c51496dcbdd37c01912de1cb`;
this draft requires the explicit local source override. Neither M4.07 nor
the UMVPA epic is closed by this packet.

## Method and admission boundary

The implementation follows the complete-coefficient, stepwise canonical
permutation construction described by
[Winkler et al., primary manuscript](https://arxiv.org/pdf/2002.10046).
Gale owns QR, converged SVD and Cholesky. Multivar owns canonical residual
coordinates, statistical rank arithmetic and resample4s actions. No dense
inverse or eigensolver family was added to a native domain module.

The initial observed fit retains all `min(P,Q)` singular roots, including exact
zeros. The existing latent CCA fit intentionally omits numerically unsupported
roots; using it here would silently omit higher-rank hypotheses. Complete
coefficient complements retain the extra dimensions when `P != Q`. Each step
removes only the preceding canonical variables. The nuisance design includes
the actual intercept and every declared nuisance column. Huh–Jhun coordinates
use the actual pivoted-QR residual basis without re-centering those coordinates.
Generic Theil coordinates require the actual selected source rows and a full
rank positive residual selection Gram matrix. Neither basis alone establishes
an exchangeability law.

For an orthogonal row permutation, each tail's within-block Gram matrix is
unchanged. Therefore cached tail QR bases followed by a newly computed compact
cross-basis SVD give the same statistic as a fresh canonical refit. Every
accepted draw and every step computes that new SVD. Numerical convergence,
rank tolerance, observed diagnostics, maximum null residual/orthogonality
errors and actual completed compact-fit counts are retained. Numerically
perfect roots or failed decompositions refuse the complete result.

Native `RankFrozenSubspaces` binds the separately dimensioned brain/target
projections to actual discovery endpoints and its training-only exposure
snapshot. Native confirmation checks actual row axes, independent-unit mapping,
source endpoints and the confirmation design identity. One projection is
applied to each provider; no dense original neural matrix is materialized.
The result retains actual residual basis, method, nuisance working design,
tolerances, versioned basis digest, caller joint-law receipt and assumption,
source identities and the frozen candidate descriptors.

Only declared spherical joint Gaussian errors over independent confirmation
rows are currently supported. Known voxel covariance does not establish a
joint row law. Dependent-time, repeated-subject and otherwise unsupported block
actions remain typed unavailable before source reads. Receipts bind caller
declarations; they do not authenticate physical evidence origin or its actual
distribution.

## Fixed sampling and independent fixtures

Native execution accepts exactly `B` distinct non-identity unrestricted
transformations. Identity and duplicate candidates are rejected before any
statistic is evaluated. Candidate stream IDs, identity/duplicate counts,
candidate budget, actual action and algorithm/seed lineage are retained.
Finite-group size and owned transformation storage are preflighted. Candidate
exhaustion returns a typed error with actual accepted/requested counts rather
than a completed inferential result. Generic with-replacement execution remains
an explicit separate policy, including identity ties.

Each rank uses the same accepted transformations, inclusive exceedances and
the plus-one denominator `B+1`. Closed testing uses prefix maximum p-values;
there is no favorable rank selection, early stopping or dropped family member.
Owned numeric cell ceilings include the adapter arrays and retained
transformation indices. Borrowed evidence, private provider/Gale workspace and
receipt collection overhead remain excluded; this is not a peak-RSS bound.

`tools/mvpa-inference/generate_rank_fixtures.R` is an independent base-R
QR/SVD/triangular-solve arithmetic oracle with explicit transformations and no
mutable RNG state. It preserves all complements. The `P=3,Q=2` counterexample
gives second-step null statistic `0.054936300920663678`; dropping the extra
left dimension gives `0.027090948245031738`. Reversing the imbalance with the
inverse transformation agrees. Roots `0.8,0.3`, observed Wilks statistics and
nuisance-adjusted roots agree within explicit tolerances. For the tiny complete
six-transformation group, scalar cross-products `[13,11,15,12,17,16]` establish
the exact inclusive p-value `2/3` independently of CCA/SVD. Sampling all five
non-identity transformations with plus-one gives that same value.

Upstream tests also compare cached-tail randomized statistics with fresh
classical CCA fits for both dimension orders and rank-zero/rank-one inputs.
Analytic fixtures anchor identity zero roots separately: the generic latent
CCA reference legitimately refuses a completely unsupported zero spectrum.
Native tests exercise actual rank-zero/one/two frozen workflows, intercept plus
redundant nuisance columns, foreign discovery/rows, holdout exposure, unsupported
law and resource refusal before provider access. These are unit arithmetic and
workflow fixtures, not scientific calibration.

## Verification and exact provenance

`gate165-rank-peer-repairs-integration.log` exits 0 for owning Multivar inference,
`mvpa`, `mvpaFit`, `mvpaDataset`, `mvpaSpatial` and `mvpaArtifacts` tests on both
platforms: 645 JVM and 637 JS tests. `scalafimCompileAll` passes in 134.6 seconds.
No Scala compilation warning appears in that complete log. Existing Java 25
archive-child Unsafe warnings remain visible.

`upstream-rank-clean-final-gates.log` exits 0 for
`inferenceJVM/clean inferenceJS/clean compileAll testAll smokeCheck mimaCheck`:
723 JVM and 721 JS tests, ordinary default Scaldoc, local artifact publication
and public-artifact consumer compile all pass. The three Scaldoc duplicate
classpath flag warnings remain; there are no Scala source warnings. MiMa has
empty previous-artifact sets, so this is not a released compatibility proof.
Cleaning removes the obsolete companion binary from the earlier case-class
draft; the final Scala result class has no synthesized `copy/apply/unapply`.
Both generated public-surface guards pass. The ordinary analysis surface is
unchanged. Earlier documentation failures remain in their original logs; this
later ordinary default gate passes without package filters or disabled docs.

Standalone Multivar resolves Gale `099832ff15c8a4a8fcf3398c7b779fb4bbc12434`;
native source integration resolves Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`.
Both are checked; they are not asserted to be the same provider revision.
Native source integration shares the exact resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`
source graph through the optional upstream build URI. Standalone/public builds
retain its revision-tagged artifact. No eviction check was suppressed.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
`m407-checked-sources.json` records 432 checked build/source/test/API/oracle
hashes. SHA-256 receipts:

- Source manifest: `03228fa9fa3540482d0773a143095c69f9327aa8e9c0d3c82b98546698fe34f3`.
- Native integration log: `95e15c4d0d90b9b589450ff178f2e032ec4638457715fe9aa2e6f4af9662644a`.
- Clean upstream gate: `ab25b2167078051efd37d35050e37e5d960e3f0b62687a477553912d2ee7370a`.
- R generator: `c18101e6a9a944458397613f190fe63839f5d4bb0a22436f8e526cfb26f843f3`.
- R output: `2c43b9247a4e6d098ca04d07bc57228a56272af0a53333d5496980884ab7a24e`.

## Outstanding acceptance

`RankConfirmationResult.admittedDetectableRank` returns typed unavailable.
Calibration status is `PendingFrozenProtocol`. Arithmetic rank concerns these
fixed candidate spaces and supplies neither a population rank claim nor an
upper bound on the original feature-space rank.

No seed-vector qualification, 200-by-199 pilot, 10,000-by-1,999 null calibration,
5,000-by-1,999 alternative calibration, complete M4.08 multiplicity integration,
hosted gate or default published-pin test has been completed for this draft.
M4.07 acceptance criterion 2 therefore remains open, along with unsupported
dependent/block actions. Final SHA-bound peer review and upstream publication
are separate gates. Unit fixtures must not be promoted to frozen-protocol
calibration evidence.
