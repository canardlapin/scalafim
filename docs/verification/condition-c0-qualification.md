# C0 bounded qualification protocol

This is a test-only experiment with fixed Gaussian D2 alternatives:
`gaussian-d2-6n-8j-2e-4c` (baseline) and `gaussian-d2-6n-12j-2e-8c`
(candidate). Both use six Newton steps, two exact evaluations, grid 15x15,
noise variance 1, strict projected Newton correction 1e-9, unchanged weak SD
limits and normalization. Total jets include initial and terminal jets. Four
and eight candidate attempts are caps **per iteration**. Source inspection of
`ShapeDecoder.decode` establishes total attempt bounds 9 and 13: every
attempt consumes a post-initial jet or an exact evaluation, and terminal jets
consume quota without adding attempts. No total attempt is compared with the
per-iteration cap.

Only the candidate may qualify. Each JVM/JS, frozen/fresh, SNR 1/.5 cell needs
200 indexed attempts, at least 190 admitted, finite admitted p95 latency
<=.02 s, width <=.05 s and relative signed amplitude L2 <=1e-3, bounded work,
coherent admitted terminal evidence, and resolved reference adequacy. Baseline
results remain descriptive. Small DEV cells cannot qualify. SNR .25 is
reported only; neither Gaussian results nor a budget change qualify LWU.
LWU's historical 83% weak-domain admission restriction remains unresolved.
PHRF-21 remains open; statistical interval qualification belongs to PHRF-14.

The standalone study defaults to **development**, exactly 200 responses for
SNR 1 seed 101 and SNR .5 seed 102. Default MUnit runs one response per SNR
through the paired study, complete audit and JSON report, plus zero-budget,
unavailable-audit, bounded-reference-exhaustion and sink-failure controls.
The six-response oracle adequacy campaign runs outside MUnit: chart fractions
(.15,.25), (.5,.5), (.85,.75) at both SNR 1 and .5, with DEV seeds 101/102.
It compares 51x21/four-start/14-level and 61x25/six-start/16-level direct references within the
predeclared .002 s latency, .005 s width and 1e-4 amplitude margins. This is
search stability on a modest panel, not a proof of a global optimum.

The reference uses original-family exact-time convolution, the production
shared AR whitening and nuisance projection, and Gale Cholesky solves.
Grid geometry is response-independent and cached; continuous geometry is
never cached. Each start has 14 main / 16 dense refinement levels and at most 256 sweeps
(four continuous probes per sweep). Any unfinished start makes that response
unresolved, including when another start gives a finite best value. Reports
include cache numeric payload, retained continuous geometry high-water (one),
continuous evaluations, refinement sweeps and failures. These are numeric
payload/reference-retention measures, not whole-process peak heap. A direct
energy at an admitted decoder point materially below the searched reference
(by more than 1e-9*max(1,abs(reference energy))) marks oracle adequacy unresolved;
it never makes the decoder the truth or changes decoder selection.

Each terminal audit uses a fresh jet buffer at the returned coordinates and
reports recomputed energy, gradient, Hessian, signed amplitudes, curvature,
conditional SD and bound-active free-coordinate Newton correction separately
from the fit's reported values. Conditional SD is independently derived by
Gale Cholesky/inverse solves, requiring finite positive inverse diagonal and
SD; no clipping and no substitution from fit SD. Unavailable/nonfinite jets
have no recomputed evidence and carry an explicit failure. All-active bounds
still require positive curvature. Audits do not consume decoder quota and are
counted separately. Every indexed output contains all seven work counts:
nodes, jets, exact, terminal, candidates, steps and fallbacks. JSON lines retain
per-voxel status/refusal causes, all attempted coordinates, paired status
transitions, Accepted-to-refused IDs, newly admitted IDs and their individual
errors/p95 summaries; unavailable numeric evidence is JSON null with a reason.

The performance fixture is generated from the compiled basis design; it is
not the scientific direct-time oracle. It follows the public
`CompactConditionPreparation.whiten` and `CompactConditionRuntime.fit` paths
with at most 256 response columns. It audits **all three** signed amplitudes
and both coordinates as Float32, finite conversion, bounded roundtrip error,
source ID, sample ID, status, refusal cause and all seven work counts. A sink
exception stops the run immediately. Optional retained DEV records are
limited to 256; production measurement keeps no per-voxel output collection.
Receipts state maximum raw/whitened block and gather array bytes, emitted and
retained Float32 numeric payload bytes. These byte counts omit object/header,
metadata, provider transient and collector overhead; they are not a complete
engine live-memory or peak measurement. The default sink consumes a record
for conversion/checksum and writes no external file. External output IO and
supported retention-route qualification remain open.

Cold preparation reports basis compilation, expansion, certification and
preparation separately; warm reuse is explicit. Runtime/node-bank setup,
raw block allocation, input generation, whitening (including its internal
allocations), gather, public projection+decode+readout and sink are separately
timed. End-to-end elapsed time includes uninstrumented loop/setup costs.
Static fixture initialization is reported separately, outside method elapsed
time. Projection/decode/readout remain one public-call timing; this harness
does not invent internal subphase timings. JVM endHeapBytes is after-run
process heap, never peak or engine live state; maxHeapBytes records the
standalone limit. JS runs its own workload and reports heap unavailable.
The measurement default is 256 DEV responses, not 100000. An actual 100k
launch needs the matching reviewed source freeze and successful candidate
science-gate receipt, a frozen classpath/launcher and -Xmx256m JVM constraint.
An sbt -Xmx3g run is not 256 MiB qualification. No extrapolated or quiet-machine
speed claim is supported.

Seed 7000930101 was consumed by the earlier one-voxel smoke; its **entire**
stream is retired to DEV, with the original receipt preserved. Fresh SNR 1
uses 7000930201; .5 uses 7000930102; reported-only .25 uses 7000930103. They
remain unobserved. Before fresh work, complete the prior-use audit and freeze
all four sources, protocol, production prerequisites, provider/build/toolchain
identities, classpath content and launch scripts, and obtain independent
parent review of that exact manifest. Fresh entry requires an explicit
reviewed-freeze argument. Do not tune policy, gates, panel or margins on fresh
results. CLI flags document a required receipt; they do not themselves
constitute scientific acceptance or authorization.

Preserve the invalid-filter r1 receipt (zero JVM tests / unsupported JS) and
the r2 MUnit timeout (260.51 s control, 297.69 s command, exit 1, JS unrun).
Neither is an accuracy failure or qualification evidence. The older
stationarity r1 receipt ran four tests per platform; new affected checks must
be tied to the new source hashes. Full frozen cohorts run in bounded
standalone JVM/JS processes after source checks and freezing, never by raising
the default MUnit timeout. No fresh or 100k result is part of pre-fresh closure.

Standalone entry `scalafim.fmri.laws.profile.ConditionC0QualificationMain`
dispatches `oracle`, `study development`, `development <DEV count>` and
`measurement baseline|candidate <count>`. The Node entry reads its own argv.
The launcher freezes and verifies JVM classpath and linked JS files, runs
JVM with `-Xmx256m`, records actual raw/meta exit and keeps the shared sbt
resource lock throughout each standalone process. Indexed reference records
include reference coordinates, all signed amplitudes, exact residual energy,
decoder direct energy when admitted, and each oracle failure reason.

After preserving179af271, the original bbf30ff4 freeze, executable classpath
bytes and negative receipts, one DEV-only stop-resolution repair freezes
MAIN14 and DENSE16 levels from the independent source diagnosis. The same
bounded compass loop has an analytic coupled SPD quadratic control for its
coarse7 resolution, fixed14 improvement, boundary clamping, actual tested
mesh, poll/depth accounting, nonfinite refusal and zero-sweep exhaustion.
All grids/starts/sweep caps, seeds, decoder/scientific gates and adequacy
margins remain fixed. Indexed per-start coordinates, signed amplitudes,
energies, requested/completed levels, sweeps, evaluations, final TESTED mesh
and termination are printed for all six shape-panel comparisons and random
DEV references. This is one predefined resolution repair, not iterative
depth selection; if it fails, adequacy remains unresolved. Full200/fresh/100k
remain outside this repair. A new forward-only freeze and parent review are
required before any promotion.
