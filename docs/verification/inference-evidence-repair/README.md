# Cancelled status-read repair receipts

This append-only receipt directory covers the bounded repair after independent
review of `edd1feafdde5ad783a4d2771c2d3fb841cdbcc85`. Only the status reader,
its JVM suite/probe, and the existing verification report are changed. The original
literal fixture directory and all its original verification receipts are retained
byte-for-byte.

`pre-fix-source.json` identifies the unchanged reader and new sentinel tests.
`source-freeze-before-repair.json` is a historical snapshot before the reader fix;
it is not the final tested source. `source-freeze.json` identifies the final
2,246 computational inputs. Its exclusions are the final report and this receipt
directory, which are documentary additions. The regression suite is identical
between the failing reproduction and repaired gates.

The pre-fix raw log exits 1 with two sentinel failures: late cancellation exposes
`2,99`, and a late invalid-code read exposes `2,98,97`. The repaired suite checks
late cancellation, callback exception, invalid code, support mismatch and
truncation after a successful internal read. All failures preserve the caller's
whole array. A later valid read still uses the same source; successful reads
preserve selected plane/sample order and leave excess array capacity unchanged.

The first affected JVM batch also exits 1 on an unchanged shared-covariance
suite's global descriptor equality assertion: the count decreases from 452 to
450. Its complete raw log is retained. The final JVM batch uses the command-line
setting `set estimatesIoJVM / Test / parallelExecution := false`, so IO suites do
not overlap that process-global measurement. No assertion or tracked build
definition is altered. Both JS batches and CompileAll use the ordinary targets.

`logs/` contains complete raw output and actual wrapper metadata, including both
failed runs. `test-summary.json` distinguishes those failures from final exits.
`provider-source-closure.json` captures all 12 actually loaded clean provider
builds; `source-stability.json` checks their source and the frozen project inputs,
including all 51 old golden hashes. `runtime-closure.json` hashes the actual fresh
reader classpath; `readback-argv.json` records the exact separate Java process.
The capture and fresh readback are performed while holding the shared sbt lock.

`capture_evidence.py` captures source/provider identity, parses complete gate
results, and captures/runs the fitter-free reader. `manifest.json` inventories
these new artifacts separately from the unchanged original fixture manifest.
Original schemas, shared API, fit producers, pooled/HDF paths and dependency pins
are outside this repair. Independent SHA-bound review and adoption remain open;
these IO checks do not imply whole-Core or scientific qualification.
