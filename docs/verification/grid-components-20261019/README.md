# Grid component extraction candidate

The portable D3 component traversal belongs in image4s. ScalaFIM retains radial atom
sampling policy. The prepared consumer change removes its private six-face flood
fill and candidate lattice-coordinate copies (45 net production lines).

These candidates are **local and unpublished**:

| Repository | Candidate | Parent / observed GitHub main |
| --- | --- | --- |
| image4s | `ba7ca3dae7181b4b3267b2d3220f0c50f06b93df` | `0816f10eab8034fe09ba46a8ffc9df852e779633` |
| reframe4s | `a9134f2cd92e884fa3c9ceafcbcfe52cf60f8295` | `750e01506b74e73d0929c28518d18989d25e3a45` |

The reframe4s change only advances its image4s pin. Both repositories must move
together to preserve one image4s revision in ScalaFIM's graph. The existing user
publication approval covered mesh4s, so these pushes still require approval.

## Behavior and evidence

`GridComponents.connected` accepts `GridDomain[F, D3, S]` and `Region[S]`, returning
regions on that same generative domain owner. Foreign owners and D2 grids fail to
compile. It reuses GridDomain's ordinal conversions, supports face (6), face/edge
(18), and face/edge/corner (26) connectivity, and allocates traversal storage in
proportion to active support. Components are ordered by smallest ordinal; members
are in domain order. Connectivity is defined in lattice coordinates.

The consumer explicitly selects face connectivity. The seeded key, distance
policy, tiny-mask identity path, and input selection order are preserved.
`cc2cfb383` adds a golden test run against the original implementation on JVM and
JS before migration: a shuffled mask on a 3×4×5 grid with four components and three
sampling levels. The migrated implementation reproduces the exact atom ordering.

| Gate | JVM | JS |
| --- | ---: | ---: |
| Full image4s-locus tests | 44 | 42 |
| Full ScalaFIM latent tests | 45 | 45 |
| Existing RadialBasisSuite, including R parity fixtures | 20 | 20 |
| reframe4s lie tests | 22 | 22 |
| reframe4s resample tests | 13 | 11 |
| reframe4s resampling/map laws | 50 | 48 |
| Full ScalaFIM image tests | 352 | 322 |
| Full ScalaFIM spatial tests | 231 | 206 |

The component suite includes an independent all-pairs equivalence-class oracle
over every 2×2×2 mask for each connectivity (768 comparisons per platform), plus
asymmetric and singleton axes, boundary wrapping, empty/full support, contact
types, stable ordering, and domain ownership. No tolerances or fixtures were
weakened. The initial prototype's FiniteDomain/FiniteSpace compile failure is
retained in the raw evidence; the final API instead uses the static owner proof.

All-module compilation passed on JVM and JS without Scala source warnings. The
build inventory contains exactly one image4s build and one reframe4s build, each
pointing to its candidate checkout. Existing Intaglio unused-setting lint remains.
The table totals 1,493 passing test executions with zero skips; the pre-migration
golden was also checked separately on both platforms.

## Publication and integration boundary

The consumer migration is saved as [consumer-cutover.patch](consumer-cutover.patch),
including the coherent pair of ScalaFIM pins. It applies to the baseline containing
`cc2cfb383`. Qualification uses explicit local image4s/reframe4s checkout overrides.
It does **not** establish default remote-pin resolution or hosted CI.

After approval, publish image4s first, then the matching reframe4s pin, using
non-force pushes to their respective `github` remotes. Refresh main heads first;
if either parent changed, stop and integrate rather than force-pushing. Verify
remote SHA parity, apply the consumer patch, and run focused JVM/JS gates plus
`scalafimCompileAll` without local overrides before committing that migration.
ScalaFIM publication is separate and remains unauthorized.

The provider checkouts and durable recovery bundles, exact source hashes, raw-log
hashes, commands, and actual exit statuses are recorded in [receipt.json](receipt.json).
Raw logs and bundles are under the shared Git directory's
`verification/grid-components-20261019/`. No full ScalaFIM test-all run or new
scientific/performance qualification is claimed. The wider provider task remains
open for neighbourhood extraction and remaining duplicate removal.
