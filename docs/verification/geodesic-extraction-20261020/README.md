# Generic geodesic extraction — 2026-10-20

Locally qualified at ScalaFIM `380476951300985979aefe67a903885b4f5d62cf`.
Independent Astra source review approves; all new provider candidates remain unpublished.
This does not close U3 or the eco epic.

ScalaFIM no longer contains its private Dijkstra or weighted-adjacency builder.
graph4s owns one shared primitive engine for prepared single-source distances
and existing directed routes. mesh4s supplies the owner-preserving adapter.
ScalaFIM retains validation, cache, ordered matrix requests, legacy direct/
spherical policy and its infinity convention. See [ownership](ownership.md).

| Original qualification candidate | Exact SHA |
| --- | --- |
| graph4s | `613371f3bd0b12ce99352db0c637645eb62c6283` |
| mesh4s | `5ed45de9ab39deb22865438d65abcd50f9f8eb4a` |
| neuroim4s | `6d20f52e6701f10126f36a4fc97bf97eeb99fa24` |
| neuroatlas4s | `7a662192a1a5769e92c9809feec0286f47728419` |

The latter two are pin-only descendants of their prior qualified implementation.
Relative to public neuroatlas4s main, its candidate also includes the previously
qualified volume extraction. Its old `7cfe136` checkout remains untouched.
Atlas source review subsequently required a refusal-contract repair;
`f03287dd870bb65394914e69be0ecfcc4b9334d9` now supersedes the atlas candidate
in this table. Its additional qualification is recorded below.

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

## Independent review

Both requested restricted read-only Opus CLI attempts returned
`usage_limit_reached` (429), zero model tokens and no review verdict. The retry
reported the account's monthly spend limit; billing and permission settings
were unchanged. Fray's daemon remains unavailable. The user explicitly approved
independent Astra substitution in chat; dashboard Q12 and the earlier Q10 are
answered. The SHA-bound review approves Root `3f0b901d2` and the exact four
provider candidates above, with no blocking findings. The
[review receipt](independent-review.json) records file-level evidence and limits.
The reviewer independently verified six main log hashes and exit metadata;
no sbt tests were run by the reviewer. Concurrent README/qualification edits
are outside that source verdict. The addendum approves replacing the atlas
candidate with `f03287d`, after verifying unchanged dependency/solver artifacts.
Atlas-volume implementation review is recorded separately. Publication, immutable-pin
acceptance and broader U3 acceptance remain required.

The resumed admission preflight confirms clean provider candidates, correct
repo-local GitHub identity, and unchanged public main revisions at the recorded
bases. Each public base is an ancestor of its candidate. This is preparation
for publication, not publication or immutable-pin acceptance.

## Consumer edge-order follow-through

Local source `e3076c750` addresses the earlier topology review's migration-guidance
follow-up under `eco-u3-consumers`. The surface README now shows endpoint-keyed
cost reconstruction and requires the same vertex ordering. It explicitly refuses
to treat an unlabelled saved positional vector as safely migratable by length.

The added asymmetric-cost tetrahedron witness pins a hand-calculated full matrix
and repeated source/target requests under reversed face order. It retains a
negative witness: blindly reusing the original positional vector changes a
distance while passing length/numerical validation. All 17 geodesic cases pass
on each of JVM and JS. This is guidance and regression evidence; the public
positional API still depends on the caller's edge correspondence. No production
solver or persisted identity changed. Broader U3 consumer acceptance stays open.

## Atlas review repair

The separate atlas reviewer objected to `7a66219`: an unsupported custom
coordinate endpoint leaked a provider `MissingDomain` failure instead of the
former `NoTransformRoute`. Two new regression cases reproduced that failure.
Repair `f03287d` restores the refusal after successful graph admission, preserving
genuine malformed-provider failures, identity routes, and custom endpoints
explicitly declared by the transform registry.

Independent Astra review approves the cumulative extraction and repair. All
24 volume JVM, 24 volume JS and 26 JVM IO tests pass. The
[atlas review receipt](atlas-volume-review.json) retains the initial objection,
before/after logs and verified thin repair bundle. ScalaFIM atlas callers also
pass 81 JVM and 61 JS tests with the four explicit provider checkout overrides.
These are local development
gates; default immutable-pin consumer acceptance is still required.

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

Next: with publication authorization, publish the reviewed provider set (including
atlas `f03287d`) in dependency order and rerun
without source overrides. Complete the remaining U3 acceptance/review gates
before closing the task. ScalaFIM publication remains separate.
