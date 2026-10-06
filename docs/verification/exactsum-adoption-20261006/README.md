# Published Gale ExactSum adoption — 2026-10-06

The root AR source now uses the published default Gale source URI:
`https://github.com/canardlapin/gale.git#54e73f8e8f1218c4cb115a850aa80e1a36c6978f`.
The staged checkout HEAD and ExactSum source SHA256 match the qualified upstream
revision. No sibling checkout or process-local source override was used.

The five AR source paths are byte-identical to the earlier qualified rehearsal:
private generic ExactSum implementation and suite removed; imported
`gale.numeric.ExactSum`; typed capacity refusal mapped at every add/merge site;
existing pair-count overflow precedence and nonfinite checks retained. The
summary self-merge regression reaches the declared capacity in sixty merges and
verifies typed refusal plus preservation of source statistics and live exact state.

| Default published-pin gate | Passing tests |
| --- | ---: |
| Full AR JVM | 155 |
| Full AR Scala.js | 153 |
| Affected fit JVM | 51 |
| Affected fit Scala.js | 51 |
| Total | 410 |

The selected fit suites are `PooledGlsPreparationSuite`,
`BoundedRobustPreparationSuite`, `GlsSuite` and `TrialReadoutSuite`; they retain
native numerical fixtures, bit-exact chunked pooling, robust AR and readout checks.
All gates returned exit 0 with zero compiler warnings.

AR core classpaths contain no Ravel entry. Fit classpaths contain exactly one
Ravel core, at the existing `9c566939` source revision; the optional Gale interop
project's separately loaded Ravel graph is absent from these consumer classpaths.
The existing transitive Gale `099832ff` remains after the direct Gale `54e73f8e`
entry in fit classpaths. These dependency exports and source HEAD checks are
retained in the archive.

Runtime: Temurin JDK 21.0.12.1+1, Scala 3.7.4, sbt 1.11.7, Node 24.21.0,
requested 4 GB heap and two active processors. Root worktree server shutdown
returned exit 0 after the completed gates. Root coordinates the separate Linux
JDK 17 baseline check; these local results do not substitute for it.

`receipt.json` records exact source, pin, classpath and archive hashes.
`logs-and-sources.tar.gz` contains raw logs, command metadata, dependency exports,
staged-source identity, all final source files, source patch and tested build.sbt.
Verify retained evidence without sbt or network calls:

```sh
python3 -S verify.py
```

This agent changed only the five authorized AR source paths. Root owns the Gale
pin/comment edit, Mote history, commits and publication. No private numerical
helpers or decoder policy changes were added.
