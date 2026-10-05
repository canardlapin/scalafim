# Design identity v2 CI blocker resolution

Mote: `bd-01M3ZPMAYZX8YCM7KWVDWSQDYV`.

The CI reproducibility blocker is resolved by the maintainer's previously
chosen option (b): exact-content goldens for platform-exact FIR data,
structural SPMG goldens, and an SPMG wire-format golden built from fixed
matrix and rank-evidence bits. Computed SPMG values have a fixture-specific
4-ULP comparison bound. That bound does not establish universal numerical
bit parity or guarantee detection of every numerical algorithm change.
`DesignFingerprint` continues to distinguish exact matrix contents.

The implemented fix is `7ff8e6096c7fba7467c4740ad8ced3f948550842`. It is an
ancestor of the isolated verification tree,
`3f261adde7fb716cb997e290516fa49379121c81`.

[PR #15](https://github.com/canardlapin/scalafim/pull/15) merged as
`7c23941f1dfcf0f3f29a2250e9772f796798c7dd`. Its exact-head
[CI run](https://github.com/canardlapin/scalafim/actions/runs/37126277734)
passed all three Ubuntu x86-64 / Java 17 jobs: full repository JVM/Scala.js
compile and test, focused first-level gates, and scientific coverage.
The isolated `canardlapin` GitHub profile was verified before reading these
results on 2026-10-04.

Local command on the verification tree:

```sh
python3 tools/build/sbt-warm designJVM/test designJS/test
```

JVM: 445 passed; Scala.js: 444 passed. Both commands exited 0, with zero
failed tests or errors.
The local sbt server uses Java 25 on macOS arm64; the Java 17 / Linux
evidence comes from the hosted run above. Raw local output and exit-status
metadata are retained at
`/private/tmp/scalafim-mote-queue-evidence/design-identity-gate.log`.

No runtime numerical algorithm or tolerance was changed in this resolution.
Portable bit-deterministic HRF math remains an optional design policy,
separate from the resolved CI blocker.
