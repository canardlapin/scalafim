# Explicit reference decoder integration — 2026-10-08

This continues `e5aeae2c` with an opt-in, single-bank decoder/readout path. It
is an integration checkpoint; bounded production decoding is still unqualified.
The previous frozen bank selection and confirmation measurements are preserved.

`ProfileDecodePolicy.trialReferences` accepts a `TrialReferenceDecodePolicy`:
explicit chart-bound points, one or two frozen candidate indices, and full-jet
storage policy (compact reconstructed second bands by default). Construction
checks the eight-reference bank cap, distinct in-range candidate indices, and
one/two-reference routing cap. Public preparation checks the complete chart and
requires penalized condition-centered trial events; ML and condition routes
refuse this policy before preparing any bank.

```scala
val policy = existingPolicy.copy(
  nodesPerAxis = Vector.empty,
  trialReferences = Some(TrialReferenceDecodePolicy(frozenPoints, Vector(1, 6)))
)
val prepared = ProfileHrfFit.prepare(plan, selection, whitening, policy)
```

The caller freezes the route before decoding responses. `ShapeDecoder` captures
it when constructed. Every routed value score is paid and enters the data/prior
comparison. Refused routes never trigger a scan of other points. Explicit points
have no inferred grid neighbours or parabolic fallback. Exact score ties choose
the smallest reference index. Ambiguity describes only scored alternatives, not
search coverage of the whole bank or a global-optimality certificate.

The prepared owner constructs one explicit reference bank, shared by decoding
and public corrected readout. Decoder workers and readout workers share its
immutable factors and full reference jets. Final readout chooses a nearest
reference **within the frozen candidate set**; it cannot spend a third reference
outside the already scored route. The unused grid configuration is neither
constructed nor part of the explicit-path identity. Provenance binds ordered
coordinates, routing indices, storage, and exact continuous-factor scope.

Continuous candidate, energy, and terminal jet evaluations still construct
exact actual-shape factors, with existing attempted/completed work counters.
Thus `maxJets`, candidate attempts, and exact-evaluation limits remain truthful,
but this integration does not satisfy the zero-actual-shape-factor production
allowance. Expanded search budgets in the Gaussian integration tests are
explicit diagnostic controls. No original-equation certificate is enabled.

The old 255/256 N=300 fixed-shape confirmation result does not establish decoder
accuracy or accuracy after restricting final readout to a frozen pair. The old
231.89 MiB graph measured a bank and workers, not this complete prepared owner,
its provenance, response reader, candidate temporaries, or peak live memory.
Neither figure is transferred to the new path as a qualification claim.

## Evidence and reproduction

Shared regressions cover analytic continuous terminal state, paid routing,
prior/ambiguity, refusal and worker recovery, missing grid fallback, invalid
routes, physical axes, exact operator agreement for public corrected readout,
identity changes, and unsupported ML/chart admission. The public integration
fixture includes a closer excluded reference to prove final readout stays
within its frozen pair. A three-dimensional scientific law exercises the
selected 4x1x2 compact bank directly through public preparation and execution.

The small JVM diagnostic has a protocol frozen before measurement. It uses the
existing B0 boundary-shape fixture, eight responses per N=30/N=300 geometry,
noise ratio 0.1, the default decoder budget, and a declared geometric pair
[1,6]. It is not a fresh random-shape accuracy experiment. All attempted voxel
statuses, exits, factors and readout outcomes are retained in `diagnostic.json`;
no confirmation samples are used to select the route or retune the bank.
The driver validates the protocol and prior selection hashes and refuses to
replace measurements.

Both N=30 and N=300 retain all eight attempts. **All 16 are refused with
`CurvatureNotPositive` at starting reference 6**, with zero emitted amplitudes.
Each geometry pays 16 node scores, eight full node jets and 48 reconstructed
second bands. Neither proceeds to a continuous factor or conditional readout.
This is a failed default-budget boundary-shape diagnostic, not an accuracy pass
or a fast successful output. The existing fixed-shape confirmation remains
unchanged, and no bank/route retuning follows this diagnostic.

Validation passes 720 fit tests on JVM and 662 on Scala.js, 13 affected laws
on each platform, and full JVM/JS compilation. After the routing allocation
cleanup, the three affected fit suites pass another 53 tests on each platform.
Compiler warnings are absent; the existing multiple-main discovery message is
recorded separately.


Reproduce in this worktree with `SBT_WARM_HEAP=5g python3 tools/build/sbt-warm`
and the commands in `validation.json`. For a diagnostic rerun, copy the frozen
protocol into a new sibling packet directory and invoke
`firstLevelLawsJVM/Test/runMain scalafim.fmri.laws.profile.TrialReferenceDecodeMain <packet-directory>`.
`python3 -S docs/verification/phrf-reference-decode-20261008/verify.py` checks
the evidence hashes, attempted-cohort completeness, work arithmetic and gate logs.

Next work includes an admitted strategy for indefinite reference curvature and
a continuous-shape scoring/terminal strategy that respects the
original solve and factor allowance, followed by a preregistered decoder accuracy
cohort with independent sufficiently searched shape references. Complete
original-family approximation/certification, domain calibration, peak-live
memory, and B0 throughput remain open; boundary failures stay in the record.
