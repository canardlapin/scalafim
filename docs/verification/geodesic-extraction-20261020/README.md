# Generic geodesic extraction — 2026-10-20

Locally qualified at ScalaFIM `380476951300985979aefe67a903885b4f5d62cf`.
Independent review is blocked; all new provider candidates remain unpublished.
This does not close U3 or the eco epic.

ScalaFIM no longer contains its private Dijkstra or weighted-adjacency builder.
graph4s owns one shared primitive engine for prepared single-source distances
and existing directed routes. mesh4s supplies the owner-preserving adapter.
ScalaFIM retains validation, cache, ordered matrix requests, legacy direct/
spherical policy and its infinity convention. See [ownership](ownership.md).

| Candidate | Exact SHA |
| --- | --- |
| graph4s | `613371f3bd0b12ce99352db0c637645eb62c6283` |
| mesh4s | `5ed45de9ab39deb22865438d65abcd50f9f8eb4a` |
| neuroim4s | `6d20f52e6701f10126f36a4fc97bf97eeb99fa24` |
| neuroatlas4s | `7a662192a1a5769e92c9809feec0286f47728419` |

The latter two are pin-only descendants of their prior qualified implementation.
Relative to public neuroatlas4s main, its candidate also includes the previously
qualified volume extraction. Its old `7cfe136` checkout remains untouched.

## Correctness evidence

The new graph court compares source distances with independent Floyd–Warshall
on 48 cyclic/disconnected graphs, including zero-cost edges. Existing directed
route tests retain parallel arc identities, edge-order ties and custom Hash
semantics; they also compare with a directed all-pairs oracle. Mesh tests compare
24 graph-cost sets on a two-component open mesh with a second Floyd–Warshall
court, retain exact owners/face order and prove wrong-owner calls do not compile.

Tests cover cost snapshot mutation, result lifetime across sources, empty graphs,
invalid finite/nonnegative costs, all-zero costs upstream, reachable overflow
versus unreachable results, and finite alternatives to overflowing routes.
ScalaFIM's independent cost oracle, full-value cache collision witnesses and
ordered repeated-column regressions remain intact. A new Root case preserves
its historical infinity convention for overflowing repeated columns.

| Gate | Passed |
| --- | ---: |
| graph algorithms JVM / JS / Native | 23 / 23 / 23 |
| mesh adapter JVM / JS | 3 / 3 |
| mesh laws JVM / JS | 19 / 19 |
| neuroim routing JVM / JS | 14 / 14 |
| neuroatlas volume JVM / JS, volume IO JVM | 21 / 21 / 26 |
| ScalaFIM surface JVM / JS | 331 / 288 |
| ScalaFIM surface-view JVM / JS | 256 / 256 |
| ScalaFIM spatial JVM / JS | 231 / 206 |
| ScalaFIM atlas JVM / JS | 81 / 61 |
| ScalaFIM MVPA-spatial JVM / JS | 25 / 25 |
| Total | 1,969 |

There are two existing surface JS placement/performance skips. Every invoked
check exits zero. All modules/examples compile on JVM and JS, without Scala
compiler warnings. Scala Native tests passed with the installed clang15
deprecation advisory retained in the raw log. No whole-repository test run,
hosted CI, new viewer acceptance or performance result is claimed.

The loaded build has 19 source builds, with one graph4s revision throughout.
Four explicit provider checkout overrides select the candidates above. Gale
remains `779ed3d8169cdd9f4eb9ab10b1ca9defadc27715`.

## Review blocker

The requested restricted read-only Opus CLI run returned
`usage_limit_reached` (429), zero model tokens and no review verdict. Fray's
daemon was unavailable. The user has been asked whether an independent Astra
review can replace Opus (dashboard Q10); silence does not waive the requirement.
No permission or billing settings were changed.

## Reproduction and evidence custody

[qualification.json](qualification.json) records exact commands, logs/hashes,
provider commit counts, source-build SHAs and verified bundle SHA-256 values.
Raw evidence is retained in the shared Git directory under
`verification/geodesic-extraction-20261020/`. Four verified Git bundles include
all candidate commits over their public base revisions.

Use the Root worktree `eco/mesh-kernels-20261020` and four normal sibling clones
named `<provider>-eco-distances-20261020`. The recorded sbt commands set:

```text
-Dscalafim.graph4s.build=../graph4s-eco-distances-20261020
-Dscalafim.mesh4s.build=../mesh4s-eco-distances-20261020
-Dscalafim.neuroim4s.build=../neuroim4s-eco-distances-20261020
-Dscalafim.neuroatlas4s.build=../neuroatlas4s-eco-distances-20261020
```

Root gates are bounded into surface/view and integration/compile invocations.
Standalone provider commands are in the receipt. Source overrides qualify the
local candidate set; they are not proof those commits are on GitHub.

`graph-provider-01.log` retains the initial passing checks with an unused import
warning. The import was removed; final graph log03 passes warning-clean at its
final SHA. No regression failures or weakened numerical tolerances were hidden.
The failed Opus attempt and its exit metadata are also retained.

Next: obtain a real SHA-bound independent review; then, with explicit publication
authorization, publish the coherent provider set in dependency order and rerun
without source overrides. Complete the remaining U3 acceptance/review gates
before closing the task. ScalaFIM publication remains separate.
