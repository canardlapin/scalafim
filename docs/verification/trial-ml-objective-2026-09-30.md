# Trial ML criterion adapter: bounded verification, 2026-09-30

This candidate implements the six-path consumer seam over base
`9235c3c79ed91d5abe7965129844325edc299292`. Mote issue:
`bd-01M3RDEHQ5PSX1CQCQ19RF3GPR`. The unchanged ShapeDecoder consumes
`J = E + sigma2 D` through all four ShapeObjective entry points. The public
ProfileHrfFit ML refusal remains in place. There is no native coherent backend
implementation or native ML admission in this candidate.

## Contract and interpretation

`CriterionJet.Input.Penalized` accepts only a stamped raw energy jet.
`Input.TrialMl` requires a checked coherent raw/determinant pair. Dedicated
scalar determinant jets carry no amplitudes. Constructors check dimension
1..3, full derivative lengths, finite values, exact Hessian symmetry, amplitude
count, owner, reference identity and coordinates before assembly. The adapter
also checks the exact response-epoch object and requested coordinates. Epochs
advance monotonically; even a refused response clears prior evidence.

J, gJ and HJ are formed directly in energy units. Score is `-J/(2 sigma2)`;
its evaluation avoids overflowing an intermediate inverse variance, including
a subnormal regression. Assembled value, gradient, Hessian and score overflow
are typed errors. A Gale Cholesky check of HJ supplies the curvature flag;
finite indefinite HE or HJ is valid evidence. A profiling/Gram refusal is
separate. The adapter performs no response-size factorization or solve.
Its small HJ curvature check does perform a chart-size factorization.

One frozen sigma2 configures both adapter and decoder. The prior is passed
unchanged in energy units and applied exactly once by ShapeDecoder. The checked
facade validates finite chart-dimensional prior means and precision, exact
symmetry, and nonnegative eigenvalues with the existing Gale spectrum policy;
it introduces no negative-eigenvalue tolerance.

The wrapped legacy result uses these meanings:

| Field | Meaning |
| --- | --- |
| `decoded.energy` | Prior-free J, not residual E |
| `decoded.dataHessian` | Prior-free HJ, not HE |
| `decoded.augmentedHessian` | HJ + 2 times prior precision |
| `decoded.conditionalSd` | Square roots of the diagonal of 2 sigma2 HJ inverse |
| `TerminalEvidence.Available(criterion).raw.jet` | Raw penalized E, gE, HE, amplitudes and raw curvature |
| `criterion.determinant` | D, gD and HD, independent of response |
| `criterion.minimizationJet` | Prior-free J, gJ, HJ and raw amplitudes |
| `criterion.score` | Maximization score, excluding prior |

The ledger retains at most maxJets successful full evaluations. Entries retain
only small immutable scalar jets, condition amplitudes and identity tokens;
tokens contain no factors, response arrays or N-sized derivative storage.
Returned coordinates and HJ resolve terminal evidence. A rejected last jet
cannot replace the accepted raw evidence. Repeated coordinates must reproduce
all raw/determinant/criterion quantities exactly within one response epoch;
a discrepancy is a typed coherence failure. Decoder energy is not compared
bitwise with reconstructed J because prior addition/subtraction can round.
Failed terminal verification and NoAdmissibleNode produce
`TerminalEvidence.Unavailable`; no additional terminal evaluation is made.
All decoder statuses and budget reasons are preserved.

## Native owner interface

These declarations are package-scoped in TrialMlObjective.scala:

```scala
trait CoherentTrialMlBackend:
  def grid: NodeGrid
  def amplitudeCount: Int
  def owner: CriterionOwner
  final def intrinsicLambda: Double = owner.intrinsicLambda
  def pointAt(response: Array[Double]): TrialMlAttempt[CriterionEpoch]
  def valueAtNode(node: Int): TrialMlAttempt[CoherentCriterionValue]
  def jetAtNode(node: Int): TrialMlAttempt[CoherentCriterionJet]
  def valueAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionValue]
  def jetAt(coordinates: Vector[Double]): TrialMlAttempt[CoherentCriterionJet]
  def setupReceipt: TrialMlWork
  def workerWorkSnapshot: TrialMlWork
```

`TrialMlAttempt[+A]` contains `Either[TrialMlFailure,A]` and immutable
`TrialMlWork`, including refused attempts. Work counts reference attempts,
N-factor attempts/failures, solve/RHS attempts, membership and derivative RHS,
small-factor attempts, logdet-recursion attempts and failures. Setup is separate
from worker work. Adapter attempt work sums actual backend receipts; validation
refusals before backend invocation report zero backend work. Full and value
success types are distinct. Missing full capability refuses at facade creation,
before any response is pointed or encoded.

The native owner must mint CriterionReference from ONE accepted factor/value
bundle, owning canonical A and the derivatives at the frozen coordinates.
Both raw and determinant components must carry that exact reference object;
raw additionally carries the pointAt epoch object. Node determinant jets belong
to setup; continuous values must not secretly compute full determinant jets.
Token equality is a consumer guard, not evidence of factor identity. This
adapter never independently calls existing raw jet and logdet APIs.

## Executed analytic evidence

The backend fixture independently defines E and J as scalar analytic functions,
and obtains D = (J-E)/2 and its derivatives. No native factors or native work
counts are measured by this fixture. Its reference-attempt counts are a receipt
translation harness, not a performance claim.

The quadratic fixture has E=(x+.4)^2, D=(x-.6)^2, sigma2=2. J's off-node
optimum is 4/15; raw E's optimum is -.4. The second coordinate has
E=2(y+.2)^2 and D=.5(y-.8)^2, giving independent optimum 2/15. The grid
ranks x=.5 for J and x=-.5 for E. Every scored node and the nonlocal ambiguity
gap are checked against independent scalar values.

| Witness | Actual work checked |
| --- | --- |
| 2D full-jet refinement | 25 node scores, 2 full jets, 0 energy-only evaluations; 28 backend attempts including pointAt; ledger 2 |
| Energy-only refinement | 2 full jets, 1 energy-only candidate, 1 terminal verification; ledger 2 |
| Analytic indefinite-node fallback | Positive raw HE, negative HJ; 1 fallback, 1 energy-only candidate, 1 terminal verification |
| Rejected exponential Newton candidate | Returned node raw E=.16 and amplitude=3 survive a later rejected full jet; ledger 2 |
| Refused energy-only terminal verification | BudgetExceeded with existing verification-failed reason; ledger 1; terminal evidence unavailable; exactly 2 full-jet attempts |
| Next response | Fresh epoch/evidence; prior ledger cleared even after node refusal |
| Strong 2D prior | HE=(2,4), HJ=(6,6), augmented H=(30,4;4,22); SD checked with an independent Gale solve of HJ |
| Prior rescue control | Augmented curvature 22.56 cannot rescue prior-free criterion curvature -1.44; SD remains unavailable |

The suite executes four actual E-only method mutants without changing production
source: scoreNode loses J ranking; jetAtNode loses the indefinite-HJ fallback;
jetAt loses terminal HJ/status; energyAt accepts an exponential overshoot that
J rejects. Each mutant fails its corresponding separate unchanged-decoder
witness. The mutant implementations remain in the test source. There was no
production source swapping or restoration hidden from the test run.

Constructor tests cover d=1/2/3, malformed dimensions and lengths, every
nonfinite determinant derivative slot, exact mixed Hessian symmetry,
owner/reference/coordinate mismatch, epoch mismatch at the adapter, invalid
sigma2, assembled overflow, sigma2=2.5 scaling, raw amplitudes/HE preservation,
and finite indefinite curvature. Other controls cover refusal buffer clearing,
coherence failure on repeated coordinates, retained-count limits, no admissible
node, and malformed destinations refusing before backend work.

## Logs and source closure

All commands used `/private/tmp/scalafim-execution-20260929/run-sbt.py` and its
shared sbt.lock. Sources were held fixed while each wrapper was queued/running.
Raw logs and JSON metadata reside under
`/private/tmp/scalafim-execution-20260929/logs/`.

| Log | Actual outcome |
| --- | --- |
| criterion-writer-check01.log | Exit 1: sandbox denied sbt.boot.lock before build; retained |
| criterion-writer-check02.log | Exit 1: newly unused ProfileCriterion test import under fatal warnings; retained and fixed |
| criterion-writer-check03.log | Exit 0: CriterionJetSuite 8, TrialMlObjectiveSuite 13, ProfileReductionSuite 5; 26 JVM + 26 JS tests; no warnings/errors |
| criterion-writer-compile01.log | Exit 0: scalafimCompileAll completed all 89 alias targets on JVM+JS; zero warning/error lines |

The final external source manifest is
`/private/tmp/scalafim-execution-20260929/criterion-writer-source6.json`.
It records SHA-256 for the six paths, including this document, and the immutable
base. The final handoff records the local commit and verifies its six blobs
against that manifest. No commit identity is claimed until that closure.

## Remaining gates

Native dense time-domain Gaussian2D/Cascade3D score, finite-difference gradient
and Hessian witnesses, two-run whitening/nuisance controls, one accepted-factor
closure, determinant value/full-jet coherence, helper and native work receipts,
and native unchanged-decoder integration remain pending the coherent core seam.
These analytic tests cannot satisfy those gates. No hosted Gale pin,
publication, executor ML admission, original-family certificate, calibration,
performance qualification or full PHRF-11/29/14/15/16 closure is asserted.
ShapeDecoder, ProfileReduction implementation, TrialBanded, helper, executor,
model, output and readout production sources were not edited.
