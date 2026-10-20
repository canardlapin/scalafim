# Volume atlas extraction: local qualification

The candidate is qualified locally on JVM and Scala.js. It is not published;
`eco-neuroatlas-volume` remains in review until publication and fresh default-pin
acceptance. The previous routing and topology branches are preserved.

| Repository | Exact candidate | State |
| --- | --- | --- |
| neuroatlas4s | `7cfe1369c0c098ab7539d6a4d60646b80eb08d00` | Local; main remains `375ceb229f707a6e9fb447a3e24564e8c5cd3036` |
| ScalaFIM | `e86bbcaf5fbfa9c0387cf4d9ea76e0eb11085218` | Local consumer cutover, unpushed |

## Result

neuroatlas4s core owns the runtime-neutral publication records, parcel identity,
codecs and validation. Optional volume and JVM IO modules own exact volume
realizations, coordinate queries, parcel/series reductions, overlap, adjacency,
volume template policy, inspected MNI bridge admission and catalog/asset loading.
The migrated ScalaFIM implementations are deleted. No compatibility aliases or
second membership owner are introduced. The remaining surface/mixed code has
explicit [destinations](ownership.md).

Publication bytes, digests, persisted identifiers, asset sources/licenses,
provider frame/grid owners, background/unknown label admission, sparse parcel
ids, display order, reduction/NaN/empty-parcel behavior and route order/inverse
refusal are retained. The original query refusal regression test caught a
changed diagnostic and now passes unchanged. The native TemplateFlow oracle
continues to refuse the misleading reverse file and qualify numerical inverses
with its original residual gates.

## Verification

**743 tests passed; none skipped** at the frozen candidates:

| Gate | Passed |
| --- | ---: |
| Standalone neuroatlas4s core JVM / JS | 13 / 13 |
| Standalone volume JVM / JS | 21 / 21 |
| Standalone JVM catalog/asset/NIfTI IO | 26 |
| ScalaFIM spatial / atlas / MVPA-spatial / examples JVM | 345 |
| ScalaFIM spatial / atlas / MVPA-spatial / workflow examples JS | 294 |
| Offline dependency coherence guard | 10 |

All ScalaFIM modules and examples compile on JVM and JS. The native MNI/ITK
oracle and atlas-to-MVPA workflow pass. Eight added portable contracts cover
noncommuting affine order, grid direction, inverse/parallel ordering, missing
maps with empty demand, wrong frames and surface admission, foreign live grids,
and subset/background/empty-parcel behavior. The actual provider route remains
inspectable, including its inverse map and quality.

The loaded-build audit finds one revision per repository across 19 source
builds. Only neuroatlas4s uses a checkout override. Gale stays at `779ed3d`.
Actual core/volume compile classpaths contain no ScalaFIM, surface or display
runtime. Core has only locus4s dependencies; enforced source checks retain the
runtime and platform boundaries.

[qualification.json](qualification.json) records exact commands, revisions,
exit statuses, test summaries, classpath/source audit and SHA-256 hashes.
Raw logs, retained failed attempts and a verified thin provider recovery bundle
live in the shared Git directory under `verification/neuroatlas-volume-20261020/`.
No all-repository test run, hosted CI, independent review, timing budgets or new
native visual qualification is claimed.

## Reproduce

Use the exact local neuroatlas4s candidate with
`-Dscalafim.neuroatlas4s.build=/path/to/neuroatlas4s-at-7cfe136` and bounded sbt
batches with a 4 GiB heap:

```text
neuroatlasVolumeTestAll
spatialJVM/test atlasJVM/test mvpaSpatialJVM/test atlasExamplesJVM/test workflowExamplesJVM/test
spatialJS/test atlasJS/test mvpaSpatialJS/test workflowExamplesJS/test
scalafimCompileAll examplesCompile
python3 -m unittest discover -s tools/ci -p test_pinned_dependencies.py
```

The receipt contains exact source-qualified classpath commands and the existing
owned sbt global base used to avoid the known fresh-template Zinc path issue.
After provider publication, remove the checkout override and repeat the gates
before accepting the cutover.

Production Scala delta: ScalaFIM **-4,313**, neuroatlas4s **+4,491**, ecosystem
**+178**. ScalaFIM adds no module; neuroatlas4s adds two optional artifacts.
This intermediate extraction improves ownership and removes the local volume
implementation; it does not establish the final ecosystem-wide LOC or module
reduction required at U6.
