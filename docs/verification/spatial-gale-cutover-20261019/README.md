# Spatial Gale cutover, 2026-10-19

`d3b65f123` pins the existing boundary contracts before implementation:
empty/out-of-range ROI rows retain their throwing admission messages, and
cache-v1 output matches an independent 278-byte fixture including signed zero.
JVM 10 and JS 7 baseline checks passed. [cache_golden.py](cache_golden.py)
encodes that ASCII fixture using Python's standard big-endian `struct`, with
no ScalaFIM or Gale code. It reproduces the digest in the JVM cache test.

`849694260` moves spatial callers to the existing pinned Gale APIs:
`DMat.gatherRows`, `CSR.toCOO`, `DoubleLinearOperator.applyTo`, and stored-entry
traversal. The private row-copy/conversion loops, forwarding alias and COO
array-extraction extensions are deleted. ROI QC retains its original non-empty
and bound checks before delegation. Cache writing streams the same entries in
the same order. Support discovery still includes every stored source column,
including explicit zeros, sorted and deduplicated. QC inspects provider entries
without repeatedly materializing row/column/value array triples.

Existing assertions, R fixtures, tolerances and public domain APIs are retained.
Additional QC checks reject wrong coordinates, values, dimensions and entry
counts. All surviving comparisons in the migrated tests retain their original
expected values and tolerances. The final source reduces production code by
48 lines; no new provider API, dependency, pin or compatibility facade is added.

The final gates passed on the frozen implementation source:

| Target | Passed | Skipped |
| --- | ---: | ---: |
| spatial JVM | 231 | 0 |
| spatial JS | 206 | 0 |
| atlas JVM | 120 | 0 |
| atlas JS | 74 | 0 |

`scalafimCompileAll` also passed for both platforms without warnings. This
covers portable spatial laws/R parity, stored cache bytes and raw numeric
roundtrips, atlas consumers and the repository Compile dependency closure.
The baseline startup separately emitted existing upstream build-definition and
unused-key warnings. No test tolerance or admission policy was relaxed.

[receipt.json](receipt.json) records exact SHAs, commands, counts and log hashes.
Raw logs and the verified input manifest are in the common Git directory under
`verification/spatial-gale-cutover-20261019/`. The broader image provider task
still needs components, volume neighborhoods and remaining duplicate cleanup.
U3 still needs a weighted geodesic provider contract and sphere/lookup extraction:
the pinned graph4s shortest-path API is unweighted and is not an equivalent
replacement. ScalaFIM remains local/unpublished. Hosted CI, new native scientific
qualification and performance measurements were not run.
