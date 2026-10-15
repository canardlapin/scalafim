# Local sbt and Mill comparison — 2026-10-05

Retain the sbt 1.11.7 project pin and the existing warm-server workflow. Mill
1.1.10 successfully builds the HRF module on JVM and Scala.js, but its faster
unchanged-work checks did not translate into a faster edit/compile/test cycle
in this experiment. sbt 1.13.0 also did not demonstrate a performance gain.
These measurements do not justify migrating the production build.

## Installed toolchain

- ARM64 Eclipse Temurin JDK **21.0.12.1+1**, installed under
  `~/Library/Java/JavaVirtualMachines/temurin-21.0.12.1+1.jdk`.
- sbt **1.13.0 launcher**, installed under `~/.local/share/sbt/1.13.0` and linked
  from `~/.local/bin/sbt`. The launcher honors the repository's **1.11.7 runtime
  pin** in `project/build.properties`; both tested runtimes are cached.
- `~/.zprofile` sets `JAVA_HOME` and adds Java and `~/.local/bin` to `PATH`.
  Open a new terminal or run `source ~/.zprofile` in an existing one.
- Existing Node.js **24.1.0** used for Scala.js. Mill is confined to the
  disposable experiment; it is not the repository's production build.
- Both installation archives passed their upstream SHA-256 checks. Exact
  downloads and digests: [toolchain-downloads.json](build-tools-20261005/toolchain-downloads.json).

The [sbt 1.13.0 release](https://github.com/sbt/sbt/releases/tag/v1.13.0)
contains maintenance fixes and parallel archive IO APIs, but those additions
do not establish an application-specific speedup. An experimental version
bump, associated CI edits, and receipt-generator edits were reverted after
the comparison. Future runtime upgrades should also update receipt toolchain
reporting, which currently contains the 1.11.7 baseline.

## Matched HRF measurements

Median wall-clock seconds from three repetitions; lower is better. Every row
covers **both JVM and Scala.js**, using the same copied production sources,
Scala 3.7.4, Scala.js 1.22.0, strict compiler flags, dependencies and fixtures.
All three builds passed **301 tests per platform**, with zero failures.

| Workload | sbt 1.11.7 | sbt 1.13.0 | Mill 1.1.10 |
| --- | ---: | ---: | ---: |
| Warm unchanged compile | 0.436 | 0.580 | 0.163 |
| Single-file implementation edit | 0.942 | 1.013 | 4.384 |
| Clean module compilation | 3.908 | 4.911 | 15.932 |
| All HRF tests, compiled outputs warm | 18.797 | 18.894 | 21.622 |
| Fresh process, unchanged compile | 2.704 | 3.066 | 1.363 |

Hardware: Apple M2 Pro, 10 logical CPUs, 32 GiB RAM, macOS 15.1.1. All tools
used Temurin 21.0.12.1. Build JVMs were limited to 3 GiB heap and four active
processors. Each tool ran sequentially; Mill used one task worker to match
the serial JVM/JS sbt command list. Downloads and initial build/test warmup
were excluded. The sbt runtime comparison used the same 1.13.0 launcher.

The incremental edit changes one error-message body in
`TemporalDerivativeConvention.scala`; each tool recompiles one production
source per platform. The clean-compile logs confirm 41 production sources
compiled per platform, so those rows are not cached no-ops. Original source
hashes were verified after restoration. Test reports match on both platforms.

These are exploratory desktop measurements, with fixed tool ordering and only
three repetitions, not a statistically controlled general build-tool ranking.
JIT warmup remains visible: clean-compile times fell across repetitions in all
three tools. Inspect the individual observations before drawing conclusions
about small differences. Test execution follows each tool's normal model:
sbt runs JVM tests in its build process, while Mill forks them. Memory, energy,
full-build Mill scaling and CI throughput were not measured. Coverage, JMH,
formatting and unrelated plugins are outside the small matched builds.

Individual observations, source/template hashes, commands and test counts:
[timings.json](build-tools-20261005/timings.json). All 45 measured commands
succeeded. [Compressed logs](build-tools-20261005/logs.tar.gz) include the
matched runs and the full-checkout sbt compatibility gates.

## Full-checkout check

The production composite build resolved **352,111 settings**. With sbt 1.13.0
as a compatibility trial, a fresh-server HRF JVM+JS unchanged compile took
**30.676 s** with dependencies and outputs cached. Subsequent invocations of
`python3 tools/build/sbt-warm ';hrfJVM/compile;hrfJS/compile'` took
**0.577, 0.506 and 0.555 s**. The small Mill prototype omits that composite
build graph; its startup timing must not be presented as a measured speedup
for the full repository. See [full-sbt-timings.json](build-tools-20261005/full-sbt-timings.json).

The sbt 1.13.0 trial passed `scalafimCompileAll` on both platforms without
compiler warnings, plus `hrfJVM/test` and `hrfJS/test` (301 tests each). Exact
pinned Gale and Resample4s artifacts were prepared through
`tools/prepare-pinned-dependencies.sh`. This is build compatibility evidence;
it does not replace the full repository test campaign or scientific/native
qualification. The final runtime pin remains 1.11.7.

After restoring that pin, the full checkout loaded under sbt 1.11.7 and
Temurin 21.0.12.1 and again passed `hrfJVM/test` and `hrfJS/test`, 301 tests
each. This verifies the installed toolchain with the final retained build.

## Migration complexity

The manually written HRF Mill build needs no changes to library code and
provides a working pattern for shared/JVM/JS source roots and MUnit. Extending
this to all of ScalaFIM is a separate integration project:

| Area | Required work |
| --- | --- |
| Local module graph | Translate 50 `crossProject` declarations, including single-platform adapters, production/test-only edges, resources and extra test source inclusions. |
| External builds | Replace 59 `ProjectRef` declarations. The root has 12 provider revision pins; the observed transitive build loads 13 distinct provider repositories, including multiple revisions of some. |
| Provider strategy | Publish exact provider artifacts for Mill to consume, or maintain Mill builds for providers. A transitional sbt bootstrap is possible but retains sbt and its setup costs. Preserve revision identity, Scala version differences, dependency diamonds and local development overrides. |
| Build policy | Reimplement boundary-check tasks and their compile dependencies, strict warning scopes, test-only modules and current coverage thresholds. |
| Tooling and CI | Qualify JMH, scoverage, formatting, native adapters, source-bound receipts, staged test batches, IDE setup and build scripts. Mill has relevant plugin capabilities, but this experiment does not establish configuration parity. |
| Acceptance | Compare the exact module/source/dependency inventory, run all portable tests on both platforms, and measure a provider-dependent workflow before replacing sbt. |

My planning estimate is **several focused days for a functional root port,
with roughly one to two engineer-weeks for provider and CI qualification**,
assuming reusable provider artifacts. This is an estimate, not an observed
migration duration; migrating provider builds as well expands the scope.
Mill's [migration guide](https://mill-build.org/mill/migrating/migrating.html)
and [automatic importer documentation](https://mill-build.org/mill/migrating/auto-migrating.html)
describe migration as scaffolding plus manual integration and qualification.

For now, keep the working sbt setup. If build productivity becomes a bottleneck,
first measure the actual slow workflow. A further Mill experiment should cover
`design` and its provider chain, with full source/dependency parity, before
committing to a whole-repository conversion. sbt 2 is also a migration, with
changes to build language, settings, plugin APIs and task caching; see the
[official migration guide](https://www.scala-sbt.org/2.x/docs/en/changes/migrating-from-sbt-1.x.html).

## Reproduce

The [benchmark runner and methodology](../../tools/build/mill-spike/README.md)
create disposable builds outside the checkout:

```sh
python3 tools/build/mill-spike/run.py \
  --directory /private/tmp/scalafim-build-spike --repetitions 3
```

Use a fresh destination and avoid concurrent compilation. The checked-in
sbt/Mill templates contain the exact matched build configurations. The report
is bound to HRF source hashes from handoff `ea6fe3ad43ece091ebcccd85fbc6cf80801d705f`;
rerunning the script copies the current checkout's HRF sources and records
new hashes.
