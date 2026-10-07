# Explicit sbt compiler stack, 2026-10-06

Protected Linux JDK17 run [37535757067](https://github.com/canardlapin/scalafim/actions/runs/37535757067)
failed in batch 11/16 while compiling `phrfComparisonJVM` test sources:
Scala 3.7.4 `StackOverflowError` with repeated `typedInfixOp` frames. The first ten
batches passed 8,285 tests (19 declared skips); comparison tests and later batches
were not reached. Focused and coverage checks passed on that head.

The unchanged sealed `AdapterGoldenBytes.scala` fixture contains a finite chain
of 295 string concatenations. Standalone compilation of that exact fixture
reproduces `StackOverflowError` at `-Xss1m` and succeeds at `-Xss4m`. The local
Temurin 21/macOS default stack is 2 MiB; the failed Linux run did not record its
effective stack. Explicit heap settings suppress sbt launcher's automatic 4 MiB
stack setting, exposing platform defaults.

`.jvmopts` now explicitly sets `-Xss4m` for sbt compiler threads. Its existing
heap setting stays 6 GiB. Restart an existing warm server to apply the changed
JVM option.

## Local qualification

- Standalone same-source Scala 3.7.4 compilation: 1 MiB exit 1, 4 MiB exit 0.
- Full comparison JVM test compilation at controlled 1 MiB: exit 1, recursion
  guard identifies the same fixture while pickling.
- After restart with explicit 4 MiB: all 52 JVM and 31 JS test sources compile,
  both commands exit 0. Effective VM flags confirm 4096 KiB stack and 4 GiB heap
  for this bounded local gate; Java is Temurin 21.0.12.1 on Darwin arm64.
- These are compilation controls; test execution remains a protected native
  qualification requirement.

`standalone-receipt.json` retains source/compiler-jar hashes and commands.
`module-compilation-receipt.json` binds both module gates and compressed logs.
All 46 sealed S1/S2/S11 paths must pass the original preservation verifier before
publication/merge. The three protected checks must pass at the new exact head.
