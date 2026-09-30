# Response-action evidence v1 (refusal-only): verification record

Date: 2026-09-30. Mote: `bd-01M31KPY7EK45HKWB9TJS1VFWJ`. Base: integration commit `b11d1909`
(contains the Core-3 `InferenceEvidence` types). Design: `proposal-first-level-contract-v2.md`
(sha256 `db8935aa…`), Part A only, owner sign-off OD-1..OD-8 as drafted.

## What v1 guarantees

1. **No positive outcome exists.** `ResponseActionEvidence.evaluate` is pure and total with codomain
   `ResponseActionRefusal` = `UnsupportedVersion | SourceMismatch | ModelMismatch | ActionMismatch |
   Unavailable`. A compile-time Mirror test pins exactly these five cases, so adding a positive case
   breaks the build's tests. The most positive-looking request (declared white Gaussian, Kronecker
   spatial claim, declared origin, encoded linear null, non-identity cyclic action, all features
   `Estimable`, `Known` conditioning) returns `Unavailable(NoPositiveContractInVersion)`.
2. **First failure wins, in the specified order (OD-7):** version; source fields in binding order
   (`Unit, Observation, Design, Preparation, Noise, RunCombination, Readout, ReadoutAxis, Columns,
   Selected, Features, RealizedNoise, RealizedCombination`), then the status scope (`StatusUnit,
   StatusFeatures, StatusPlane, StatusOutsideSupport`); model; action; unavailable reasons in declared
   order ending with `NoPositiveContractInVersion`.
3. **Status evidence is looked up exactly and fails closed.** `ResponseStatusScanner.scan` requires the
   source unit revision to equal the binding (`Left(Conflict)` otherwise), then looks up exactly
   `coefficients.find(_.observation == b.observation)` and the `Fit(b.observation)` plane, requires
   `evidence.columns == b.columns` (ordered) and `b.selected ⊆ inferableColumns`. Each absence is a
   typed `StatusEvidence.Absent(reason)` and reads nothing; there is no all-Estimable fallback.
4. **Reads are restricted to the bound in-support features.** The scanner reads only the unit's ordered
   domain support, only after its feature digest equals the binding's, in chunks of at most
   `min(chunk, limits.maximumCells)` samples, and verifies each receipt's selection and cell count.
   `OutsideSupport` among bound features becomes `SourceMismatch(StatusOutsideSupport)`; `Unrecorded`
   and every other non-`Estimable` code is counted (`NonEstimableFeatures`). IO failure, cancellation,
   an unknown code or a mismatched receipt returns `Left`; no summary exists.
5. **Nothing is inferred from fitted quantities.** The binder reads the coefficient axis, preparation
   provenance, engine/summary flags, AR whitening provenance and fixed-effects policy. It never reads
   coefficients, fitted values, per-voxel covariance, residual variance, `VoxelFitStatus` or
   `EstimabilityEvidence`. `Estimable`/`Known(text)` are treated as declarations only.
6. **Construction trust (OD-8).** `ResponseSourceBinding`, `StatusSummary` and `ReadoutAxis` are final
   classes (not case classes) with `private[scalafim]` (or private) constructors, `apply` and `copy`.
   Tests compiled in an external package prove `apply`, `new`, `copy` and `fromProduct` do not compile;
   the codec decodes only to the untrusted `DecodedBindingClaim`, which has no conversion to a binding.
7. **Platform-stable identity.** All digests are portable SHA-256 over length-prefixed canonical text;
   numbers are encoded by IEEE-754 bits. Golden digests recorded on the JVM are reproduced exactly on
   Scala.js (codec bytes, feature identity, and the six binder digests of a real OLS fit).

## API (new files only; no existing file edited)

- `modules/estimates/shared/.../ResponseActionEvidence.scala`: `ProviderDigest`, `ConditionLevelId`,
  `ReadoutRowId`, `ReadoutDefect`, `ReadoutAxis.parse`, `ConditionAction` (+ `parseRows`),
  `NoiseScope`, `RealizedNoise`, `RealizedCombination`, `ResponseSourceBinding`, `DeclaredTemporal`,
  `DeclaredCombination`, `SpatialClaim`, `DistributionClaim`, `ModelOrigin`, `DeclaredResponseModel`,
  `NullConstraint`, `ResponseActionRequest`, `StatusSummary`, `StatusAbsence`, `StatusEvidence`,
  `SourceField`, `ModelDefect`, `ActionDefect`, `UnavailableReason`, `ResponseActionRefusal`,
  `ResponseActionEvidence.evaluate`.
- `modules/estimates/shared/.../ResponseBindingCodec.scala`: `private[scalafim] ResponseDigests`
  (portable SHA-256, feature identity), `DecodedBindingClaim`, `BindingCodecError`,
  `ResponseBindingCodec.{encode, decode}` (deterministic, canonical-only decoding; not persisted, OD-3).
- `modules/fit-estimates/shared/.../ResponseSourceBinder.scala`: `ReadoutRow`,
  `MaterializedUnitBinding`, `BindError`, `ResponseSourceBinder.bind`, `CanonicalProvenance`.
- `modules/fit-estimates/shared/.../ResponseStatusScanner.scala`: `ResponseStatusScanner.scan`.

Model-mismatch rule (implemented and documented in code): a declared white law admits only realized
`White`; a declared `FixedSigmaT(spec)` admits realized `FixedAr(…, Some(spec))`, mismatches realized
`White` or `FixedAr(…, Some(other))`, and treats estimated/robust (`EstimatedWhitening`) or learned
(`LearnedResponseSubspace`) processing as unavailable rather than contradictory (OD-6). `Unrecorded`
never mismatches; it yields `NoiseIncomplete`/`CombinationIncomplete`. The combination rule is analogous.

## Evidence

All builds used `run-sbt.py` on worktree `/private/tmp/scalafim-response-evidence-20260930`
(logs under `/private/tmp/scalafim-execution-20260929/logs/`), after mutants were restored.

| Step | Tasks | Exit | Result | Log SHA-256 |
|---|---|---|---|---|
| `rae-final-jvm.log` | `estimatesJVM/test fitEstimatesJVM/test` | 0 | 43/43, 50/50 | `5437fdff6a9c33ab5a310b420da7be7bcbc6fc493d6ba6fcd8f845d46400b13d` |
| `rae-final-js.log` | `estimatesJS/test fitEstimatesJS/test` | 0 | 43/43, 42/42 | `c92a2e200566ce34a8f28eac4aef6b1db8e985210d85edc2fa9da25d4f472e78` |
| `rae-final-compileall.log` | `scalafimCompileAll` | 0 | 0 warnings | `73deedb61e3657d57b16bd93ef45222b20e8b230fdadf1e24f4ffa73e648c794` |

New tests (identical on both platforms): `ResponseActionEvidenceSuite` 15, `ResponseBindingCodecSuite` 5,
`responseaction.external.ResponseActionAccessSuite` 6, `ResponseSourceBinderSuite` 10,
`ResponseStatusScannerSuite` 8 (44 per platform). All pre-existing estimates and fit-estimates suites pass
(JVM has 8 extra JVM-only readback tests).

Coverage of the specified v1 acceptance list: (1) mismatch table with one row per `SourceField`,
including a same-set reordered feature identity, same-design reordered columns, and summaries from
another unit/observation; (2) every `StatusAbsence`, each reading nothing, including
`Some(InferenceEvidence)` without `Fit(obs)`; (3) scans with chunk 1, 2 and 40 over one `Constant` and
one `Unrecorded` bound sample with exact counts, an instrumented source proving only the ordered
support is read and out-of-support samples never are, `OutsideSupport` refused, wrong receipt
selection/cells refused; (4) IO failure and cancellation give `Left` with no summary; (5) external
`compileErrors` for `apply`, `new`, `copy`, `fromProduct` of the three trust-bearing types, decoder
yields only `DecodedBindingClaim`; (6) white vs `EstimatedAr`, `FixedSigmaT` digest mismatch,
combination mismatches, `FixedAr(…, None)` gives `NoiseIncomplete`; (7) raw-action parser (bins split,
bins permuted, unequal bins, non-condition-major, not a permutation), `ActionDefect`s, identity policy;
(8) precedence across all stages; (9) PLS fixtures (balanced 5/5/5, unequal 9/4/2, identity-marginal
joint counterexample, AR truth fitted by OLS on a real fit) all `Unavailable`; (10) codec golden bytes
built independently in Python, byte-exact round trip for every realized variant, canonical-only
decoding; binder refuses an axis-less dense result and records `EstimatedAr` (real GLS fit) and
`EstimatedWeights` (real fixed-effects fit). A seeded 30,000-case generated property checks the
first failure against an independent collect-all oracle and that every stage, every `SourceField`
and every unavailable reason is reached; no case is positive (the codomain has none).

Mutation evidence (each applied alone, JVM tests run, original restored and SHA-256 verified;
originals `ResponseActionEvidence.scala` `a11547ac…466a4c`, `ResponseStatusScanner.scala`
`bd4e7ab7…a461ad1`):

| Mutant | Killed by |
|---|---|
| M1 model check before source check (precedence swap) | 4 tests (mismatch table, field order, precedence, oracle property) |
| M2 scanner accepts a missing `Fit(obs)` plane | exact-lookup test (`FitPlaneAbsent`) |
| M3 `Unrecorded` counted as `Estimable` | unavailable-order chain and oracle property |
| M4 scanner reads the whole grid instead of the bound support | 3 scanner tests (instrumented reads, limits, OutsideSupport) |
| M5 `NullNotEncoded`/`IdentityActionPolicy` order swap | unavailable-order chain and oracle property |

Upstream finding: the native `DesignFingerprint` text is not JVM/Scala.js stable, because
`DesignAudit.canonical` renders rank-preview doubles (`tolerance`, `diagonalR`, `conditionEstimate`)
with `toString`. The golden test caught it on Scala.js. The binder therefore derives its
`scalafim.response-design/1` identity from the fingerprint's matrix shape and IEEE-754 value bits plus
the structural rows, columns and audit rendered bit-exactly. The design module is not changed here.

## Not claimed

- Any positive contract, admission, calibration or randomization validity. v1 refuses every input.
- Part B (B1 Gaussian fixed-linear theorem, B2 Fisher randomization, B3 studentized permutation);
  no covariance, `M = AGΣ_TG'A'` or exact certificate is computed.
- Persistence: no metadata, wire, codec-in-store, `EstimateUnit`, status-schema or estimates-io change
  (OD-3). The codec's bytes are not written anywhere.
- Model truth: a binding records processing, not the generating law. AR truth fitted by OLS is recorded
  as `White` (test), and cannot be detected from the declaration, the design or fitted covariance.
- Provider authentication: digests are identity evidence, not scientific truth or authenticated origin;
  `private[scalafim]` trust is procedural inside `scalafim.*` (OD-8) and must be revisited before any
  positive case.
