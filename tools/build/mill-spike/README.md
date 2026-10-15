# HRF build-tool experiment

This is an exploratory comparison, not a replacement ScalaFIM build. It copies
the current HRF sources into disposable standalone sbt 1.11.7, sbt 1.13.0 and
Mill 1.1.10 builds. Production module sources and the repository's build outputs
are never edited or cleaned by this runner.

Run with JDK 21, sbt, Python 3, curl and Node on PATH:

```sh
python3 tools/build/mill-spike/run.py \
  --directory /private/tmp/scalafim-build-spike --repetitions 3
```

Choose a fresh directory for each run. The runner downloads a hash-checked Mill
bootstrap, uses normal user dependency caches, and records command logs,
source/template hashes, test counts and individual timings in `timings.json`.
Mill itself is downloaded by its official launcher. Both build templates use
Scala 3.7.4, Scala.js 1.22.0, the production HRF dependencies and compiler flags,
and CommonJS/Node tests. Both JVM and JS tests must pass with matching counts.

Each measurement covers JVM and Scala.js together. Tools run sequentially;
Mill uses one task worker to match the sequential sbt command list. Build JVMs
use a 3 GiB heap limit and four active processors. Initial downloads, build
definition compilation and test warmup are excluded from measurements.

- `warm-noop`: compile without changing inputs, with the daemon running.
- `incremental-edit`: change one error-message implementation in a copied
  production source, then compile. Restore the original before running tests.
- `warm-tests`: run all HRF tests with compiled test classes and JS output.
- `clean-compile`: delete module compile outputs, then compile; dependency
  downloads and build definitions remain cached. Cleaning is outside the timer.
- `cold-noop`: start a fresh process with existing compiled outputs. sbt uses
  batch mode; Mill uses `--no-daemon`.

The templates preserve each tool's ordinary test execution model: sbt's JVM
tests run in its build JVM (`fork := false`); Mill's JVM tests fork. This
difference is part of the measured default workflow, not a controlled
comparison of MUnit execution speed. Neither memory nor energy consumption is
measured. Three repetitions on a developer machine are exploratory, and do
not establish full-repository or CI speedups. The templates omit inactive
coverage instrumentation and unrelated plugins; those still need qualification
in a production migration.

The production build also loads many external source builds that are absent
from these matched HRF builds. Its startup and warm-command overhead must be
reported separately; comparing its cold startup directly with this small Mill
prototype would not isolate a build-tool improvement.
