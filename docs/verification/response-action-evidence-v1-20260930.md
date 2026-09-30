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
   StatusFeatures, StatusPlane, StatusSelected, StatusOutsideSupport`); model; action; unavailable reasons in declared
   order ending with `NoPositiveContractInVersion`.
3. **Status evidence is looked up exactly and fails closed.** `ResponseStatusScanner.scan` requires the
   source unit revision to equal the binding (`Left(Conflict)` otherwise), then looks up exactly
   `coefficients.find(_.observation == b.observation)` and the `Fit(b.observation)` plane, requires
   `evidence.columns == b.columns` (ordered) and `b.selected ⊆ inferableColumns`. Each absence is a
   typed `StatusEvidence.Absent(reason)` and reads nothing; there is no all-Estimable fallback.
4. **Reads are restricted to the bound in-support features.** The scanner reads only the unit's ordered
   domain support, only after its feature digest equals the binding's (a different unit revision or
   feature identity is `Left(Conflict)` before any read), in chunks of at most
   `min(chunk, limits.maximumCells)` samples, and verifies each receipt's selection and cell count.
   `OutsideSupport` among bound features becomes `SourceMismatch(StatusOutsideSupport)`; `Unrecorded`
   and every other non-`Estimable` code is counted (`NonEstimableFeatures`). IO failure, cancellation
   (including cancellation observed only after the last chunk), an unknown code or a mismatched
   receipt returns `Left`; no summary exists.
   A `StatusSummary` records the unit revision, plane, feature identity, selected columns and the
   number of features scanned; its counts must be nonnegative and sum to that positive number. A
   summary scanned for one selection is refused for another (`SourceMismatch(StatusSelected)`).
5. **Nothing is inferred from fitted quantities.** The binder reads the coefficient axis, preparation
   provenance, engine/summary flags, AR whitening provenance and fixed-effects policy. It never reads
   coefficients, fitted values, per-voxel covariance, residual variance, `VoxelFitStatus` or
   `EstimabilityEvidence`. `Estimable`/`Known(text)` are treated as declarations only.
6. **Construction trust (OD-8).** `ResponseSourceBinding`, `StatusSummary` and `ReadoutAxis` are final
   classes (not case classes) with `private[scalafim]` (or private) constructors, `apply` and `copy`.
   Tests compiled in an external package prove `apply`, `new`, `copy` and `fromProduct` do not compile;
   the codec decodes only to the untrusted `DecodedBindingClaim`, which has no conversion to a binding.
   **Scope of this claim:** it holds for Scala sources outside `scalafim.*` only. `private[scalafim]`
   is public in JVM bytecode, so Java (or reflection) on the JVM can call these constructors. v1 is
   refusal-only, so a forged value can at worst mislabel a refusal. **v2 requirement:** before any
   positive case, issuance must not rely on Scala qualified-private access (for example an unforgeable
   issuer capability or a verified provider signature).
7. **Platform-stable identity and lossless encoding.** All digests are portable SHA-256 over canonical
   text whose tokens are prefixed by their UTF-8 byte length; numbers are encoded by IEEE-754 bits.
   Text with unpaired UTF-16 surrogates is refused at construction (`ConditionLevelId`,
   `ProviderDigest` schema, binding observation and columns, decoded claims), so encoding is lossless for
   every admissible binding. **What is bounded in decoding:** each token's declared byte length is
   checked against the bytes remaining before it is read; the row count must satisfy
   `rows × 4 ≤ remaining bytes` and each column-list count `n × 2 ≤ remaining bytes` before any
   iteration; a length prefix longer than 9 digits is refused. Validation of the decoded claim is
   linear in the input: `ReadoutAxis` is checked in one hash-map pass, and the selected-column subset
   check uses sets. Total decoding work is therefore O(input bytes) (expected, with hashing); a
   decoded claim is accepted only if it re-encodes to exactly the input bytes. There is no absolute
   size cap: an input of n bytes may still produce O(n) objects.
   Golden digests recorded on the JVM are reproduced exactly on Scala.js: codec bytes (including a
   non-ASCII golden), feature identity, all six binder digests of a real one-run OLS fit, and four of
   the six (preparation, noise, run combination, features) for a compiled two-run design. Design and
   readout identity of that design differ between platforms upstream (see the finding below).

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

**Spec drift (tightening):** the approved design lists `StatusSummary(unit, plane, features, counts,
conditioning)` (5 fields). v1 has 7: `selected` and `samples` were added after review, with the invariant
that counts are nonnegative and sum to a positive `samples`. This only removes representable states and
adds the `StatusSelected` refusal; it admits nothing new.

**Owner question (not changed here): the rule is asymmetric.** `FixedSigmaT` declared against realized
`EstimatedAr` gives `Unavailable(EstimatedWhitening)`, while `White` declared against realized
`EstimatedAr` gives `ModelMismatch`. The implementation reads "white" as "no temporal processing
permitted" and a fixed Σ_T as a law that estimated processing merely approximates (OD-6). The owner
should confirm or choose a symmetric rule before any positive version.

## Evidence

All builds used `run-sbt.py` on worktree `/private/tmp/scalafim-response-evidence-20260930`
(logs under `/private/tmp/scalafim-execution-20260929/logs/`), after mutants were restored.
Round 2 (review repairs) gates:

| Step | Tasks | Exit | Result | Log SHA-256 |
|---|---|---|---|---|
| `rae-r2-final-jvm.log` | `estimatesJVM/test fitEstimatesJVM/test` | 0 | 48/48, 52/52 | `77c7ae52a3aeef5bd48fbdddf7c5fc89ac7ec5ad05156e6fe79ecbbefbbaedc1` |
| `rae-r2-final-js.log` | `estimatesJS/test fitEstimatesJS/test` | 0 | 48/48, 44/44 | `f3e536e80ad8ff4c4d5e273a32fa2793e78664286b38cf6422d5339c39c67703` |
| `rae-r2-final-compileall.log` | `scalafimCompileAll` | 0 | 0 warnings | `2430d0042e9d42ed78e3925359689c7cb3a433a9d87f05457cc1ff7a3fdc9772` |

Round 3 (linear decode validation) gates. The CompileAll step first ran `estimatesJVM/clean
fitEstimatesJVM/clean estimatesJS/clean fitEstimatesJS/clean`, so all four targets were recompiled
from scratch (12, 12, 5 and 5 sources) with 0 warnings:

| Step | Tasks | Exit | Result | Log SHA-256 |
|---|---|---|---|---|
| `rae-r3-final-jvm.log` | `estimatesJVM/test fitEstimatesJVM/test` | 0 | 50/50, 52/52 | `13e80cc4ef6ed39f4192bb33ff3d1d82cc8d45bc419236811c910503aab9d9a4` |
| `rae-r3-final-js.log` | `estimatesJS/test fitEstimatesJS/test` | 0 | 50/50, 44/44 | `f781bce99e905744d19ed9b52aacc49f92e59688bfad6fda9e013ddfa910ceb7` |
| `rae-r3-final-compileall.log` | clean (4 targets) + `scalafimCompileAll` | 0 | 0 warnings | `940fee5e5ae16cecbb3f3fae2e8dcfd7d5baaf26dfa8004373b302900c02e4b8` |

Decode timing (test-built inputs, asserted under 1 s each): 8000 conditions × 1 bin (79,761 bytes)
29.9 ms JVM / 44.8 ms JS; 20000 columns (578,450 bytes) 48.0 ms JVM / 84.9 ms JS. The reviewer measured
12.1 s and 5.5 s on JS before the fix. The linear `ReadoutAxis.defect` is checked against the original
algorithm (kept test-local) on 13 adversarial and 20,000 seeded random axes, which reach every defect kind.

Round 2 table above is superseded by round 3. Round 1 gates (commit `4907ba41`): `rae-final-jvm.log` 43/43, 50/50; `rae-final-js.log` 43/43, 42/42;
`rae-final-compileall.log` exit 0.

New tests (identical on both platforms, round 3): `ResponseActionEvidenceSuite` 18, `ResponseBindingCodecSuite` 9,
`responseaction.external.ResponseActionAccessSuite` 6, `ResponseSourceBinderSuite` 11,
`ResponseStatusScannerSuite` 9 (53 per platform). All pre-existing estimates and fit-estimates suites pass
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
**Limit of the oracle:** it restates the same decision tables from the specification and shares
`ConditionAction.isIdentity`, `ReadoutAxis.conditions` and `StatusSummary.count` with the
implementation. It therefore guards ordering and precedence (collect-all head versus first failure),
but it cannot detect a misreading of the specification that both copies share.

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
| MS (round 2) scanner's final `cancelled()` check dropped | late-cancellation test (cancel observed after the last chunk) |
| MW (round 2) `SharedAcrossRuns` requirement for `SingleRun` dropped | coefficient-scope test (`RunSpecific`, one acquisition) |

MS and MW survived the round-1 suite (reviewer finding); both are killed after round 2
(`rae-r2-mut-MS-drop-final-cancel-check.log`, `rae-r2-mut-MW-drop-shared-scope-requirement.log`), with
sources restored and SHA-256 verified.

Upstream finding: the native `DesignFingerprint` canonical text is not JVM/Scala.js stable, and the
divergence is broader than the rank preview. `DesignAudit.canonical` renders rank-preview doubles
(`tolerance`, `diagonalR`, `conditionEstimate`) with `toString`. The reviewer also found that, for the
compiled two-run design, the row and column canonical text before `|audit=` differs between platforms
(hash 359819000 on one platform vs -2115733256 on the other). Only the fingerprint's `|matrix=RxC` shape
and `|values=` IEEE-754 bits are used here. The binder derives its `scalafim.response-design/1`
identity from those two segments plus the structural `RowLayout`, `StructuralColumn`s and
`DesignAudit` objects rendered bit-exactly by `CanonicalProvenance` (not their `canonical` strings).
The one-run binder golden agrees across platforms in all six digests. For the compiled two-run
design, preparation, noise, run-combination and feature digests agree exactly, but design and
readout digests differ. The readout digest covers only column IDs, row labels, estimand IDs and
one-hot weight bits, so by elimination the native structural **column IDs themselves** differ between
JVM and Scala.js for designs compiled by `FmriModelBuilder`. This cannot be repaired at the binder,
because the column IDs are the identity carried into `EstimandBinding`. The test pins design and
readout per platform, and a tripwire assertion (`JVM != JS`) fails once upstream converges. The design
module is not changed here; the native fingerprint and column-ID text need their own repair ticket.

Root cause (traced by the delta reviewer): `modules/hrf/shared/src/main/scala/scalafim/fmri/hrf/HrfDescriptor.scala:235`
`canonicalId` interpolates `Double` values and case-class `toString`. It flows through
`BasisElement.infer` (`Basis.scala:145-146`) and `DesignSchema.scala:158` / `:295` into the structural
`ColumnId`. Observed: JVM `span=24.0 … SpmgParams(5.0,15.0,…)` versus Scala.js `span=24 …
SpmgParams(5,15,…)`. Latent upstream issues in the same area: `Basis.scala:82` (`FirBin.stableLabel`)
interpolates Doubles, and `Basis.scala:82-89` uses a literal `%02d` without the `f` interpolator, which
yields IDs such as `basis-1%2502d`.

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

## Peer review repair (review of `134dea0d` by backlog-worker-20260930: OBJECT)

The independent peer review found two defects in `134dea0d`, each reproduced on the JVM and Scala.js. Both are repaired here.
Review report: `/private/tmp/scalafim-response-review-20260930/review.md`.

- **R1 (medium): feature identity was lossy.**
  - Standard UTF-8 encoding replaces an unpaired surrogate with `?`. So the admissible domain frames `"frame\uD800"` and
    `"frame?"` shared one feature digest, and the scanner read a source whose frame differed from the binding.
  - Canonical digest bytes now use generalized UTF-8 (WTF-8, `ResponseDigests.lossless`). It is byte-identical to UTF-8 on
    well-formed text, so every earlier golden is unchanged. It is injective over all Java strings: an unpaired surrogate
    becomes its own three-byte sequence. Token length prefixes count those bytes.
  - The new goldens for `"scanner\uD800"` and `"scanner?"` were computed independently with Python's
    `encode("utf-8", "surrogatepass")`.
  - The reviewer's scanner probe is adopted as a regression test: the source is refused with `Conflict` before any read.
  - The persisted codec is unaffected. It encodes only bindings whose text is already well-formed, and it still refuses
    invalid UTF-8.
- **R2 (low): valid AR orders did not round-trip.**
  - The number parser read at most nine digits, so a valid AR order such as 1000000000 encoded but did not decode.
  - It now reads up to ten digits and range-checks them against `Int.MaxValue`. AR orders 1, 999999999, 1000000000 and
    `Int.MaxValue` round-trip for both AR variants. `2147483648` and `9999999999` are refused. Count bounds against the
    remaining input are unchanged.
- **Mutation evidence.**
  - Reverting both repairs (lossy digest bytes and the nine-digit parser) fails both new codec tests (`rae-r4-mut-lossy`).
  - Reverting only the digest bytes (`rae-r4-mut-lossy-scanner`) did not fail the scanner regression. The WTF-8 token
    length prefix alone already separates the two frames.
  - Reverting the full lossy encoding, both the digest bytes and the token length, fails the scanner regression
    (`rae-r4-mut-lossy-scanner2`: 1 of 10 tests fail).
  - Sources were restored after each run and checked by hash.
