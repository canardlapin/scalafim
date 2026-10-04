# Frozen discovery and confirmation design, 2026-10-02

Mote packet `bd-01M2BNHA2YRMAX9N2RBAKBZQ0F` implements admission of a
fixed-discovery C1 design. It produces no statistic, p-value, selection-aware
calibration, or population result. Source baseline is
`32fad7101208bd0a922165e05123ffc5f6f8a657` in the isolated UMVPA worktree.

## Retained contract

Discovery retains the actual pattern artifact, numeric brain and target
projections with their endpoint axes, independent-unit mapping, and declared
support, rotation and preprocessing receipts. Its content identity includes
the factors, target policy, centering/intercept, rank and coordinate gauge,
training binding and lineage, and fit diagnostics. Confirmation retains its
nominal rows, actual row-to-unit mapping, and preprocessing receipt. Its
snapshot is population metadata; later inferential methods must separately
bind their actual observation and target values.

The C1 contract names conditioning, null, estimand/generalization,
multiplicity family, approximation and discovery/confirmation stages.
Admission checks disjoint declared independent units, exact nominal row reuse,
the snapshot-bound exposure reference and all recorded attempts, and a
finite full-column-rank nuisance matrix with residual degrees of freedom.
Failed payload reads and derived-score attempts also block untouched C1.

Independent Gaussian confirmation requires one row per independent unit.
Dependent-time designs retain an actual row-bound diagonal-plus-low-rank
covariance. Repeated-subject designs additionally check its actual
cross-subject blocks. Normalizing loadings by the square roots of diagonal
variances before multiplication prevents finite large scales from hiding
dependence through overflow. This check is conservative relative to total
variance normalization. Estimated covariance remains explicitly uncalibrated
and refuses the known-Gaussian capability. C2 and G1/G2/G3 return actionable
unavailability rather than acquiring unsupported claims.

## Evidence and limits

Eleven shared regressions cover actual exposure attempts, same-row relabeling,
independent foreign nominal datasets with identical local ordinal keys,
foreign projection/nuisance/training axes, changed factor/lineage/diagnostic
identities, numeric repeated blocks, deterministic covariance identities,
the `D=1e200, U=1e100` overflow counterexample, covariance tolerance/work
refusals, and dependent-time nuisance degrees of freedom.

Independent read-only review approved the admission logic at source hash
`97599c5fcc320b112de86cc0c2e6439d5c119ce2770730e34e452346fea64b16`
and suite hash
`0a653a0927417d75d6681c7b3dd6d96c1ce81f06a359e40f42995a9110e8465a`.
The subsequent source edit documents the global unit-key convention and
removes an unused row-key cache; it changes no admission branch.

Unit keys must globally identify the same independent subject or unit across
populations. Nominal descriptors and caller-declared units do not authenticate
the physical origin of renamed foreign axes or subsets. Gaussian assumptions
are declarations, and immutable exposure snapshots cannot establish freshness
of external history. Support, rotation and preprocessing receipts do not
prove those external operations. These limits remain part of the public
admission boundary.

Nuisance and repeated-block budgets use checked arithmetic before owned
workspace allocation. They exclude borrowed inputs, covariance construction,
object overhead and private Gale workspace; they do not bound process peak
memory. No calibrated inference, complete selection uncertainty, real-world
independence, or subject generalization is qualified by this packet.

Published-pin gate133 passed the full owning core suites: 351 JVM and 351
Scala.js tests, including all eleven confirmation-design regressions on each
platform. It also passed affected fit/dataset/spatial/archive/estimates-IO
suites and the whole-call allocation probe: 546 JVM and 532 JS tests overall.
`scalafimCompileAll` passed warning-clean on both platforms. The uncommitted
rotation draft was held outside this gate and restored afterward; no build
pin changed. Final source hashes are attached to the Mote completion record.

Evidence root: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
Raw log `gate133-confirmation-execution-pinned.log` SHA-256:
`3da60ea39def5defafc23443973a94d9f2fd6898b9e8915628e4d5ae3cea46ac`.
The 186-entry source manifest `gate133-pinned-sources.json` SHA-256 is
`3b4b1937f1bf6701c60627b75cdb4add1bbcf9a5743a0d7988ae648d5a287c69`.
