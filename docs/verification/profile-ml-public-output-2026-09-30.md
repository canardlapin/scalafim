# Same-worker native ML conditional public outputs

This local PHRF-11 implementation admits ExactShape with PreparedBasisResidual
for the existing trial/condition amplitude and signed-query requests. Other ML
modes and original-family certification refuse before reader acquisition.
Outputs are conditional at the actual decoded shape. Amplitude covariance is
unavailable; the decoder's HJ shape SD is not amplitude uncertainty.

The pointed ML worker performs its existing exact readout once, copying the
real nuisance block only on success. Its measurement helper applies the
prepared normal operator once to that same N+F solution. It performs no second
factor, band solve, pointAt, response encoding or whitening. Raw E and raw
condition coefficients are compared with coherent terminal raw evidence;
arithmetic means of conditional trial amplitudes are rendered separately.
Success and failure are memoized, with owner/epoch/coordinate guards checked
before numerical work and before reuse after a later pointAt.

The unchanged 22-field objective ledger includes actual exact-reference setup
factor/solve/RHS work and the final amplitude band solve. A single exact-readout
factor request does not mean a single total factor or band solve: constructing
that reference retains its existing release/determinant factors and setup RHS
solves. The public numerical subtotal overlaps the global trial ledger and is
not added twice. ML criterion and immutable setup ledgers remain separate.
Measurement attempts, failures and normal actions are separate local counters.

Storage is scoped, not peak memory: the backend owns N+F coefficient scratch;
the measurement solver owns ten N+F arrays, d coordinates, (d+3)*m basis
coefficient values, jetComponents*fineCount kernel values, and two C condition
arrays (one integer). Persistent ML scratch is recorded at worker creation,
including decode refusals. The objective/encoded response and immutable source/bank
retain their existing storage. A successful epoch memoizes the existing raw N
trial vector even for query-only requests; query results retain zero public
trial-amplitude values. Nuisance and condition result vectors and the normalized
public vector (when requested) are additional retention. No total allocation or
engine-peak claim follows from the wrapper high-water receipts.

Validation evidence and immutable source/log hashes are recorded in the final
external manifest under /private/tmp/scalafim-execution-20260929. Independent
prepared-basis dense controls cover Gaussian and Cascade, F=0/F>0, off-node
coordinates, signed outputs and explicit two-run AR reset; these are routing
controls, not a native scientific campaign, calibration or throughput admission.
Full PHRF-11/29, scientific qualification, performance and release remain open.

A retained JVM attempt used an overbroad profile-law wildcard: all 198 fit
profile tests passed, but the unrelated frozen ConditionMilestone Gaussian C0
cohort gate failed (admission 0.945 < 0.95). Its full log is
`profile-ml-output-jvm-03.log`; this is not a passed scientific gate. The queued
Scala.js wildcard attempt was stopped by its verified owned sbt PID before
law execution (exit 143). Final required law checks use the specific existing
TrialBandedSuite. No cohort, seed, decoder budget, tolerance or gate was changed.
