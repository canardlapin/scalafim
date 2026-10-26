# Grid component extraction

The portable D3 component traversal belongs in image4s. ScalaFIM retains radial atom
sampling policy. The consumer cutover removes its private six-face flood
fill and candidate lattice-coordinate copies (45 net production lines).

The approved provider commits are **published on main**:

| Repository | Published commit | Parent / previous GitHub main |
| --- | --- | --- |
| image4s | `ba7ca3dae7181b4b3267b2d3220f0c50f06b93df` | `0816f10eab8034fe09ba46a8ffc9df852e779633` |
| reframe4s | `a9134f2cd92e884fa3c9ceafcbcfe52cf60f8295` | `750e01506b74e73d0929c28518d18989d25e3a45` |

The reframe4s change only advances its image4s pin. Both repositories must move
together to preserve one image4s revision in ScalaFIM's graph. The user approved
these publications with “proceed”. Each non-force push was followed by exact
local HEAD, tracking-ref and remote-main SHA parity checks; both owned provider
checkouts remained clean.

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

| Prepublication local-provider gate | JVM | JS |
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

Prepublication all-module compilation passed on JVM and JS without Scala source
warnings. Its build inventory contains exactly one local image4s build and one
local reframe4s build. The table totals 1,493 passing test executions with zero
skips; the pre-migration golden was also checked separately on both platforms.

## Published-pin consumer integration

The exact [consumer-cutover.patch](consumer-cutover.patch) is applied to ScalaFIM,
including the coherent pair of immutable provider pins. Its radial source blob is
`c6df1fa6836ea857cd62e5efebfe541a9d32b61a`, identical to the source qualified before
publication. The private flood fill and candidate lattice-coordinate copies are
deleted. No public radial API, persisted schema, tolerance, or fixture changes.

Default published-pin qualification **passed**: 1,241 tests (zero failures or skips)
across latent, existing radial parity, image and spatial suites on JVM and JS.
Every ScalaFIM module compiles on both platforms without Scala source warnings.
A fresh server used no provider overrides; its complete build inventory contains
exactly one remote source URI per provider, including the two published revisions
above. Fetched provider checkouts match those SHAs and have no tracked changes.
Source input blobs remained unchanged throughout qualification. ScalaFIM
publication remains a separate action.

[receipt.json](receipt.json) preserves the prepublication qualification snapshot,
including local-only candidate state at that time. The provider publication and
default-pin evidence is recorded in [publication.json](publication.json). Raw logs,
exact source input hashes, SHA-parity receipts and durable provider recovery bundles are under
the shared Git directory's `verification/grid-components-20261019/`.

No hosted CI, full ScalaFIM test-all run, or new scientific/performance qualification
is claimed. Existing Intaglio unused-setting lint and reframe4s build-task lint are
retained separately from Scala source warning checks. The wider provider task
remains open for neighbourhood extraction and remaining duplicate removal.
