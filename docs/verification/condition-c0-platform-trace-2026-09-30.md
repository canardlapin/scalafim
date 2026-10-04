# C0 literal platform trace — 2026-09-30

The identical literal response reproduces the platform status difference:
JVM returns **BudgetExceeded / CandidateAttemptCap**, Scala.js returns
**Accepted**. Arithmetic differences are directly observable before decoding
in the compact projection and in callback ordinal 1 (`scoreNode`, node 2).
The private decoder comparison/acceptance branch is not directly observed.
This is a one-column diagnostic, not C0 scientific or performance qualification.

## Scope and identity

Mote child `bd-01M3S6A7FGP86G9RFYQ4MCB269`, parent
`bd-01M25Q0JY8GA7CJRKZF2934PJ0`, actor `exec-diagnostics`. Base
`16f7e35b5fe4c8ff9ed00f35c860f364138be33b`. Exactly four owned test/document
paths; no production, decoder, policy, tolerance, budget, seed or build edits.

Specimen: development SNR `.5`, seed `102`, voxel `120`; named frozen manifest
`451be51b9abca9c127c9d8d2f012d7ef871f262922ec5c68ae3f7f06513e023d`
is the SHA-256 of `ROOT/c0-repair-freeze/manifest.json`. Its harness bytes
match the base harness exactly. All three frozen prerequisite hashes match.
The current base's `build.sbt` differs from the historical freeze; this report
records the current build/provider/classpath identity, rather than asserting
historical executable parity. `ROOT` below is `/private/tmp/scalafim-execution-20260929`.

Candidate `gaussian-d2-6n-12j-2e-8c` is unchanged: 6 Newton steps, 12 jets,
2 exact evaluations, 8 candidate attempts per iteration, total upper bound 13,
stationarity tolerance `1e-9`. Native Unnormalised coordinates/amplitudes,
no prior, fixed noise variance `1.0`.

## Response-only capture and literal fixture

One temporary JVM-only test called the existing `directCohort(200, .5, 102L)`
response generator, then the existing `preparation.whiten(200, data)`, and
selected exactly `whitened(row * 200 + 120)` for rows 0–599. This is the
same row-major indexing and whitening as `runStudy`. No decoder, reference
oracle, qualification suite or cohort study ran. The temporary test/helper
were removed from final sources; their exact source bytes are archived below.
Capture raw/meta exits were both 0, one test passed, duration 63.498366 seconds.
Capture sources were hashed and rechecked unchanged before literal generation.

The final fixture contains exactly 600 signed decimal IEEE-754 Long literals
and returns a fresh array via `Double.longBitsToDouble`. All values are finite.
It contains no generation, RNG, convolution, whitening or decoder calls.
Raw-word SHA-256 (600 big-endian signed 64-bit words):
`242e59305e674af3bcc69dead2d157036735043e02047afbab1df0920365c20a`.
Portable checksum `062a58836c4a30fc` uses initial word
`0xcbf29ce484222325`, then `(hash xor rawWord) * 0x100000001b3` modulo 2^64
per word; this is a raw-word checksum, not a cryptographic hash. Both platforms
validate count, finiteness, checksum and fresh array ownership. The independent
parser also reconstructs the literals and validates their SHA-256.

The package-visible factory projects only the literal column with the existing
preparation, points a fresh `CompactConditionObjective`, and returns projected
response/energy for direct observation. Each decode uses a fresh objective,
fresh counters and a directly constructed public `ShapeDecoder`.

## Transparent callback evidence

Each wrapper callback makes exactly one delegate call. `grid` and
`amplitudeCount` delegate directly and generate no events. The wrapper copies
coordinates, returned scalar/Boolean, jet energy/gradient/Hessian/amplitudes/
curvature, ordinal and all eight counters before/after. It does not call an
objective for logging, mutate a buffer or write runtime files. Numeric strings
are classified raw IEEE-754 words (`finite:`, `nan:`, `infinity:`), preserving
signed zero and explicit nonfinite bits.

On **each** platform, the unwrapped and wrapped terminal serialization agrees
bit-for-bit for every result field, including both Hessians, conditional SD,
ambiguity gap, step count and budget exit. All eight terminal counters agree.
All callback before/after snapshots agree (the objective itself does not change
decoder counters). Decoder increments occur outside callbacks: node score and
initial node jet increments follow the callback; continuous jet/exact increments
precede it. Matching callback counts equal terminal nodeScores, jets and
exactEvaluations; total callbacks equal their sum. No additional callback work
is introduced. This does not claim no allocation or timing overhead.

## Actual platform comparison

The first direct callback value divergence is ordinal **1**, `scoreNode(2)`:
JVM `finite:4098a94f6b81912e` (1578.3275585408078),
JS `finite:4098a94f6b81912c` (1578.3275585408073). Ordinal 0 agrees.
Before decoder entry, projected response coordinate 0 already differs:
JVM `finite:bfda7673ee36818d`, JS `finite:bfda7673ee368146`;
projected energy agrees exactly. This narrows the observed arithmetic difference
to preparation/projection or earlier shared-input execution, but does not isolate
which preparation, transcendental or accumulation operation caused it.

Both platforms score the same 72 nodes and choose node 177. Ordinal 72 node-jet
energy is JVM `4095b6e89c10cb8a`, JS `4095b6e89c10cb78`. The first continuous
callback is ordinal 73, with different coordinate words. Callback operation
names and node IDs agree through ordinal 75; JS then ends, while JVM continues
with ordinals 76–84 (seven full jets and two energy evaluations).

At ordinal 74 JVM jet energy is `4095b67d48af63ff`, and ordinal 75 is
`4095b67d48af6402`; JS has `4095b67d48af63f3` then `4095b67d48af63ed`.
The inference that the increasing JVM candidate is rejected while the decreasing
JS candidate is accepted is **inferred** from energy/counter sequences and the
unchanged source. The wrapper does not observe private `current`, `value`,
`accepted`, `scale` or the comparison predicate. It does not prove a single
branch cause, original-family accuracy or appropriateness of a tolerance repair.

| Terminal field (raw IEEE-754 words for numbers) | JVM | JS |
| --- | --- | --- |
| status | `BudgetExceeded` | `Accepted` |
| budgetExit | `CandidateAttemptCap` | `None` |
| node | `177` | `177` |
| newtonSteps | `3` | `3` |
| energy | `finite:4095b67d48af63fe` | `finite:4095b67d48af63ed` |
| coordinates | `['finite:401d5ca1d38d6441', 'finite:3fe981944c925daf']` | `['finite:401d5ca1d70351b8', 'finite:3fe981944b42e11f']` |
| amplitudes | `['finite:bfe59eda5f5262d7', 'finite:3ff3a36b62af5abb', 'finite:3fec3e1ed6e9fc97']` | `['finite:bfe59eda5bea12e2', 'finite:3ff3a36b64fdac6e', 'finite:3fec3e1ed1191ce2']` |
| dataHessian | `['finite:404cd5264f87081b', 'finite:4041aa56b23a1010', 'finite:4041aa56b23a1010', 'finite:40741a283295c50e']` | `['finite:404cd526511cdb5d', 'finite:4041aa56a0c76281', 'finite:4041aa56a0c76281', 'finite:40741a282d7d4701']` |
| augmentedHessian | `['finite:404cd5264f87081b', 'finite:4041aa56b23a1010', 'finite:4041aa56b23a1010', 'finite:40741a283295c50e']` | `['finite:404cd526511cdb5d', 'finite:4041aa56a0c76281', 'finite:4041aa56a0c76281', 'finite:40741a282d7d4701']` |
| conditionalSd | `['finite:3fc8aed74fa29320', 'finite:3fb4e71924a8ae0e']` | `['finite:3fc8aed74d5fc63d', 'finite:3fb4e71925f7d9b8']` |
| ambiguityGap | `finite:4012ac9e1c0f4200` | `finite:4012ac9e1c0f4400` |

| Work field | JVM | JS |
| --- | ---: | ---: |
| voxels | 1 | 1 |
| nodeScores | 72 | 72 |
| jets | 11 | 4 |
| exactEvaluations | 2 | 0 |
| candidateAttempts | 12 | 3 |
| terminalVerifications | 0 | 0 |
| newtonSteps | 3 | 3 |
| fallbacks | 0 | 0 |

There are 85 JVM callbacks and 76 JS callbacks. Full parsed records preserve
all values and snapshots. `c0-platform-trace-complete-comparison.json` enumerates
all common callback, projection, terminal and counter differences, plus the nine
JVM-only callback records. No between-platform equality assertion is imposed.

## Execution and independent parsing

The shared serialized builder ran:

```sh
python3 ROOT/run-sbt.py ROOT/c0-platform-trace c0-platform-trace-final.log \
  'show firstLevelLawsJVM/Test/fullClasspath' \
  'show firstLevelLawsJS/Test/fullClasspath' \
  'firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.ConditionC0PlatformTraceSuite' \
  'firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.ConditionC0PlatformTraceSuite'
```

The exact command is preserved in metadata. Child and wrapper actual exits 0;
JVM **2 tests passed**, JS **2 tests passed**, no failures/errors; duration
98.699655 seconds after acquiring the lock. Only the new trace suite executed;
the existing qualification suite compiled but did not run. No compile-all.
All 1,352 recorded source/build hashes were rechecked unchanged after execution.
No test reruns after this pass. Full-log scan found no errors or Scala compiler
warnings. It did find one Scala.js linking warning about multiple existing main
classes; this is disclosed, and the run is not described as warning-clean.

Independent parser command:

```sh
python3 ROOT/parse-c0-platform-trace.py > ROOT/c0-platform-trace-parser-output.json
```

Its recorded actual exit is **0**. It parsed exactly two trace lines (JVM and
JS), validated 532 classified numeric words, literal SHA/checksum, exact
within-platform results/counters and callback count/counter formulas. It found
the first direct divergence at ordinal 1. It performs no objective, fitting,
whitening or decoding. Parser command/exit, output, empty stderr, parsed records
and complete comparison are retained separately.

Runtime: sbt **1.11.7**, Scala **3.7.4**, sbt JVM **Homebrew Java 25.0.1**,
Node **v26.7.0**. Runtime executable paths/hashes and version evidence are in
`c0-platform-trace-runtime-providers.json`. The classpath closure contains
109 listed entries (both targets), hashed jars and directory contents, plus the
two actual linked JS artifacts and 12 clean provider checkout identities.
Directory hash protocol is sorted relative path, NUL, raw file bytes, NUL.

| Current source/build path | SHA-256 |
| --- | --- |
| `.jvmopts` | `bf1a1d03f789112c9c74214eb9d1bf918ef5021cd1101d09b44a1426911a6bf0` |
| `build.sbt` | `83a0f8a63710f7082910e8e593b0b8ee447ca7b584fcb55c49e11a14908e8f36` |
| `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/ConditionC0LiteralFixture.scala` | `410f97cf8cd64553914cdead0b941fd36ad7e37f4023b220ae2db97f74a77e42` |
| `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/ConditionC0PlatformTraceSuite.scala` | `dc098d5fb1eb0e995df4dcf7be63e5fcb5110b97c951457a8df9b9a6507fcd71` |
| `modules/first-level-laws/shared/src/test/scala/scalafim/fmri/laws/profile/ConditionC0QualificationSuite.scala` | `86d77854a382cffc75db861f9e5cba9036f0b4fe8da78853349f791150ea35de` |
| `modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/CompactCondition.scala` | `a079bd7733d258b25d1967da9a83c788a85eda5815e7ef23eeb7c0f64c5483e6` |
| `modules/fit/shared/src/main/scala/scalafim/fmri/fit/profile/ShapeDecoder.scala` | `397f6826ee94bf761984cc592e0e1a397e31be68120546499852896218df8dcb` |
| `project/build.properties` | `bc2e83307ba4a40cb286711cee6f644d9bca62ab2d791053000c7c9ce7a82bb2` |
| `project/plugins.sbt` | `4a7fc382ac03c04e9a8dd19b9e0c217bc12c6491e7aaa5250b5f718263f65a10` |

| Actual provider checkout | Git HEAD | Source status |
| --- | --- | --- |
| `/Users/bbuchsbaum/.sbt/1.0/staging/05a8f1a231c171f1bca0/locus4s` | `58c9739be51345ad9adc4bc9c9e7335023254ec9` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/34c0071f90095ef07887/zarr4s` | `2a5ba963b151b62c739d1bf5a19d49202bb6ff29` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/34fcacf7027b8ae3b2f8/intaglio` | `596b398af380079e4b251535230d0bc03cd88c51` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/41e96dde036f148f012a/image4s` | `26a74ad99b9ee49a9555344e19b82d69a2ba50e4` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/55929e145617b909ca3a/multivar` | `c4329fc95688929236c942cca889aa67ad17cbe0` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/57a292f3a6ded9e259cf/ravel` | `9c5669399ab8e2a11402e71973dd5f1e2f2c13f4` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/5a3fe250684692aa42c1/gale` | `da38f8c429294657d30ec29f06eae3fab636d428` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/5a90f48e170e9e640ee1/gale` | `099832ff15c8a4a8fcf3398c7b779fb4bbc12434` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/77e7bdc71bddb8077fcd/bids4s` | `a33678390614a91fadbdef13f22970e78c26e091` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/aa1fd63832323d604703/reframe4s` | `9a4508351d74567147b8ea3221d82db89e5892b0` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/b4b08747efd9a8151229/graph4s` | `b585e594eec4567bad78ae23206c1a8f535bcd5e` | clean |
| `/Users/bbuchsbaum/.sbt/1.0/staging/cd3a5082f5d2990355dd/image4s` | `2695f891cbec31a7f565a9b39e2554fe3b6d4b40` | clean |

## Preserved artifact receipts

All paths in this table are relative to `ROOT`; raw logs retain the complete
capture and both trace records. Metadata retains real child return codes.

| Artifact | SHA-256 |
| --- | --- |
| `logs/c0-platform-trace-capture.log` | `b26c43c731f0270cfc8d04a9dcf7bb76d5d4fbf1b71470af025d593a337228c7` |
| `logs/c0-platform-trace-capture.log.meta.json` | `40c48cd255e9a1c8bc0a08a507b44885da8af809740e5510b41659a4492caba2` |
| `logs/c0-platform-trace-final.log` | `06f110819b63c1bc0ff116ff23d461c80c0748cd6cc798adb11d952a42fcd4f8` |
| `logs/c0-platform-trace-final.log.meta.json` | `af6f9f072663f8c2d81f40d010a67e128f33aeaa358330d69c06b2d8a85de202` |
| `c0-platform-trace-capture-sources.json` | `9463ed67de0481d39aa74564784cf342cae5e0805f20d745de99e4b4730e8fb6` |
| `c0-platform-trace-final-sources.json` | `aa177b7d1dc5eac783092f4c1f72f6b516fe1b69e580a67755d46725b25c4854` |
| `c0-platform-trace-fixture-identity.json` | `34050d27cb92e6b763bee8937dae05bffb99d44e6b6fcde4e18c622a0f2a54b1` |
| `c0-platform-trace-frozen-origin-check.json` | `5ff285560574457c6ecf00be82be52d143ee4197d9b83e02ffc7d8e38c87b373` |
| `c0-platform-trace-runtime-providers.json` | `085bbbf2540ee8169ef5210920e51f56acb17b74865668ea1c120ba84bdf06f2` |
| `c0-platform-trace-classpath-closure.json` | `4cf7f35ccda74dafc5504dfb726ef8b5c1c80bf0e9e2243b18699451f7dbade2` |
| `parse-c0-platform-trace.py` | `702bd1630e324ebc44ce09dc11c0c6c07f5c621f7246d8441ebfe1125f8bbe84` |
| `c0-platform-trace-parser.meta.json` | `7d847e5faa5982b6c52d06ecc35eba4376cb1dd7c99420adae3397fc16a4a12d` |
| `c0-platform-trace-parser-output.json` | `27e784bfb0a09f7c1baf6b0097bb3bb8d57dcf617da505835e3971d826ec5165` |
| `c0-platform-trace-parser-stderr.log` | `e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855` |
| `c0-platform-trace-parsed-records.json` | `f3226b42c4bd5f20c53be5f19186dfdad037a765f21343c0b5f1ed9c2c18bceb` |
| `c0-platform-trace-complete-comparison.json` | `8facad3ccb0b6f79a70280a043f1af53bca5d89032595cafea7a60dfc424c73a` |
| `c0-platform-trace-capture-source/ConditionC0QualificationSuite.scala` | `88fe28bfafe36357d2fe20197f5f4bb10600ec34910f761a8d451c30dd47cc8c` |
| `c0-platform-trace-capture-source/ConditionC0PlatformTraceSuite.scala` | `1471dc317f56e66dedba5a64d809f5606111f63b021efac73715cf2b32f19f9b` |

The document's final hash and exact four-path local commit are recorded in
`ROOT/c0-platform-trace-handoff-receipt.json` after commit, avoiding a circular
self-hash. The Mote child is handed back for independent review with reservations
released, without closing the child or parent. No push, publication, production
repair, altered science gate, native/ML certificate or performance qualification.
