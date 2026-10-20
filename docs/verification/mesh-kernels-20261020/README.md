# Mesh lookup and sphere qualification — 2026-10-20

Published mesh4s candidate and public mesh-pin qualified bounded slice of
`eco-u3-kernels`. ScalaFIM remains local; the atlas override is still required.

- ScalaFIM source: `4c357723979a63e151ca2e9b02f493c140b8c119`
- mesh4s candidate: `b9e0553b85113d52fc758747d62ddc0fa343e033`
- Required local atlas candidate: `7cfe1369c0c098ab7539d6a4d60646b80eb08d00`

ScalaFIM's surface resampling now delegates nearest lookup, positive radial face
lookup, and sphere geometry to domain/frame-owned mesh4s APIs. Its registration
choices, conventional radius, sparse operator, normalization and no-hit fallback
remain downstream. See [ownership and remaining work](ownership.md).

## Correctness witnesses

The corrected synthetic fixtures reproduce two defects in the old code. On an
anisotropic point cloud, local buckets choose a vertex about 300 units away
instead of one 1.1 units away. An unrelated bucket near a distant query causes
radial lookup to miss the valid triangle along the ray and choose a single
fallback vertex instead of barycentric weights `(0.25, 0.25, 0.5)`.

The new upstream tests compare nearest lookup with an exhaustive Euclidean scan,
and radial lookup with an independent plane intersection plus Gram-system solve
on oblique faces. The ray court spans query scales `1e-200`, `1`, `1e4`, and
`1e200`. Tests also cover ties, empty inputs, antipodes, boundary tolerance,
disconnected/open owners, Float32 points, large finite distances, typed frame
refusal, degenerate-face admission, immutable inputs, and sphere owner/frame
retention. Existing R/neurotransform and Workbench parity tests remain intact.

## Gates

| Gate | Passed | Skipped |
| --- | ---: | ---: |
| mesh4s geometry JVM | 35 | 0 |
| mesh4s geometry JS | 31 | 0 |
| ScalaFIM surface JVM | 325 | 0 |
| ScalaFIM surface JS | 282 | 2 |
| ScalaFIM surface-view JVM | 256 | 0 |
| ScalaFIM surface-view JS | 256 | 0 |
| Total | 1,185 | 2 |

`scalafimCompileAll` passes for all modules/examples on JVM and JS, with no
compiler warnings. Provider formatting and dependency-pin checks pass. The
loaded build has 19 clean source checkouts (including ScalaFIM), one revision
per provider; the original qualification used mesh4s and neuroatlas4s as explicit local overrides.
The subsequent public mesh-pin gate uses only the atlas override.
Gale remains `779ed3d8169cdd9f4eb9ab10b1ca9defadc27715`.

The skipped cases are the existing synthetic same-size placement/performance
court and P6-JS real-field pointwise placement court. These remain unqualified.
No whole-repository test run, hosted CI, new native visual acceptance, or lookup
speed/allocation result is claimed.

## Reproduction and retained evidence

Raw logs, exit metadata, source-build inventory, and a verified provider bundle
are retained under the shared Git directory's
`verification/mesh-kernels-20261020/`. [qualification.json](qualification.json)
records exact SHAs, hashes, commands, counts, and paths.

Use the normal standalone mesh4s candidate clone and the isolated ScalaFIM
`eco/mesh-kernels-20261020` worktree. The first linked provider checkout hit the
known sbt-git/JGit `NoWorkTreeException`; its source backup remains at
`../mesh4s-eco-kernels-linked-20261020` and is not the qualified candidate.

```sh
sbt -batch -J-Xmx4g \
  -Dscalafim.neuroatlas4s.build=../neuroatlas4s-eco-volume-20261020 \
  surfaceJVM/test surfaceJS/test surfaceViewJVM/test surfaceViewJS/test \
  scalafimCompileAll
```

The actual log command additionally selects the established per-checkout sbt
global base. It is recorded in the receipt, not required as a portable default.

Retained adverse evidence:

- `legacy-lookup-witness-01.log`: initial nearest fixture refused unused
  vertices; its distant-ray example passed. This is not bug evidence.
- `legacy-lookup-witness-02.log`: corrected admitted fixtures fail exactly the
  two lookup regressions; six previous tests pass.
- `provider-geometry-01.log`: linked-worktree build environment refusal.
- `provider-geometry-02.log`: initial geometry gates pass (34 JVM, 30 JS).
- `provider-geometry-03.log`: strengthened final provider gates pass (35/31).
- `consumer-surface-01.log`: initial consumer gates pass (325/282, two skips).
- `qualified-consumer-02.log`: final source gates, view tests, and compile pass.

mesh4s publication was approved in chat and completed non-force. Candidate HEAD,
local main, tracking main, and live GitHub main all match
`b9e0553b85113d52fc758747d62ddc0fa343e033`, with 0/0 divergence.
`published-mesh-consumer-03.log` then qualifies the public mesh4s pin at
ScalaFIM `95fdcbe8e`: 325 JVM and 282 JS surface tests pass, with the same two
existing skips and no compiler warnings. The build loads mesh4s from immutable
staging, not a sibling checkout. The JSON receipt retains the publication
receipt and all loaded source revisions.

The previously qualified atlas candidate still awaits its existing publication
answer; no all-default-provider qualification or hosted CI is claimed.
