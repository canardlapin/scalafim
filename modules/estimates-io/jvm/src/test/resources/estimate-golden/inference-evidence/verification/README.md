# Source-bound verification closure

All final jobs completed with actual exit code 0, through the shared run-sbt lock.
`logs/` preserves full raw outputs and wrapper metadata, including failed startup,
fixture/decoder corrections and the existing compatibility regression before repair.
Only `jvm-final-r2`, both `js-*-final`, `compile-all-final` and `fresh-readback-final`
are final passing receipts. Earlier runs are development history, not extra qualification.

`source-freeze.json` hashes all 2,214 project source/input files before the final
gates; `provider-source-closure.json` hashes the 12 actually loaded clean source
builds and records their commits/trees. `runtime-closure.json` hashes every actual
JVM classpath jar and directory file used for fitter-free fresh-process readback.
The final doc and generated receipts are documentary additions after the freeze;
`source-stability.json` records unchanged tested inputs and all 51 old golden hashes.
`test-summary.json` records nonzero completed test counts; no warnings or skips
occurred in the full test/CompileAll logs. Standalone JDK25 prints a Scala LazyVals
Unsafe deprecation warning, preserved in its raw successful receipt.

`readback-argv.json` is the exact fresh JVM argv. The captured runtime is Homebrew
Java25.0.1, sbt1.11.7, Scala3.7.4 and Node26.7.0. The shell's default java was22;
the sbt eval receipt identifies its actual launcher runtime rather than guessing.
The supplied scripts preserve the original execution paths for reproduction and
provenance. No tests import fit for status readback, and the reader classpath has
no fit or fit-estimates module.

Literal `SHA256SUMS` covers the independently authored fixture inputs; generated
receipts have their separate exact inventory in `manifest.json`. Regenerate literal
inputs by copying generate.py/README.md into an empty directory before invoking
Python, so receipt attachments remain separate from fixture authoring.

These checks establish the bounded typed contract and tested physical/lifetime
paths. They do not establish native producer parity, performance, peak memory,
power-loss recovery, HDF conformance, statistical calibration or whole-Core completion.
