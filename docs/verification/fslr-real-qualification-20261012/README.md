# Real fsLR qualification — 2026-10-12

The current public route passes the frozen P1–P3, P5 and P6 gates on newly
acquired, digest-locked TemplateFlow assets. The original P4 replay of 15
consumer maps remains pending: the user confirmed those inputs are on another
machine. The locked TemplateFlow GM volume supplies an additional public
consumer check, not a substitute for that replay.

| Frozen gate | Current result |
| --- | --- |
| P1 convergence, residual ≤ 1e-8 mm | All 32,492 vertices per hemisphere; zero unavailable; at most 30 iterations |
| P2 oracle placement error ≤ 1e-6 mm | 1.5021e-8 mm left; 1.5311e-8 mm right |
| P3 nearest voxel, 1 mm and 2 mm grids | Zero disagreements on both hemispheres |
| P4 original 15 consumer maps | Pending external inputs |
| Additional locked GM consumer | Zero coverage, value, voxel or sign disagreements |
| P5 medial wall and display identity | Exact 2,796/2,776 wall vertices; display identity preserved |
| P6 JVM admission, placement and GM mapping ≤ 30 s | 14.0832 s |
| P6 Scala.js placement ≤ 120 s | 62.4228 s; separate load/hash/decode 55.1086 s |

The route identity is
`scalafim-route:sha256:68fc557306abb1b15dc02007d80f2a067d2190a3c1c421f3e339d8877350be6a`.
No policy, threshold, display default or production numerical code changed.
The publisher-methods frame declaration and its limits remain those in the
[original contract](../fslr-pointwise-inverse-20261002.md).

The JVM batch passed 30 tests (21 fixture tests, nine public-route/file tests).
The Scala.js batch passed 13 tests (one real placement, seven portable fixture,
five public-route tests). Required real-asset flags were enabled, and the JVM
timing assertion was enabled. Neither batch skipped a test.

TemplateFlow4s `67097a9ecab7ea4ed8a81eb3532861096a3b2282` fetched ten source
assets and converted the locked HDF5 map. The two converted files reproduce
the frozen hashes exactly. [Asset verification](asset-verification.json)
checks all 12 files, sizes and hashes. The assets remain external; the
[lock and commands](../../../tools/surface/fslr-qualification/README.md)
make acquisition and qualification repeatable.

`gate-receipts.json` contains extracted numerical receipts;
`gm-disagreement.json` retains the complete consumer comparison;
`source-lock.json` binds the tested sources at base
`139bbcae5cfec6733af93443c88722a2046839a9`. Compressed logs retain acquisition,
conversion and both test batches. The first acquisition command supplied an
extra `--` and printed usage while returning zero; it downloaded nothing.
The corrected command and independent hash verification are both retained.
`gm.spec.json` records the actual local input path used; relocate that path to
the verified cache when replaying it on another machine.

Mote `bd-01M4Q4KTV7H5XD0B5XWCEH1WAH` and its parent
`bd-01M44W2HXQ5PY3YE33D5W37A04` retain the external P4 dependency. This packet
does not claim that all six original gates have been rerun.
