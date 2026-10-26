# Geodesic extraction ownership and acceptance boundary

| Owner | Responsibility |
| --- | --- |
| graph4s algorithms | One primitive CSR Dijkstra engine, shared by existing directed routes and prepared indexed-undirected single-source distances. Finite nonnegative cost admission, cost snapshot, owned result arrays, reachability and overflow. |
| mesh4s graph adapter | Exact triangle-topology vertex/edge owners and existing primal correspondence. Transport mesh edge costs into graph order; return vertex-domain queries. No numerical solver, geometry repair or renumbering. |
| ScalaFIM surface | Neuro-facing validation, legacy direct/spherical metric policy, ordered sparse/subset matrix requests, repeated columns, full-value cache identity, and its established infinity convention. |
| neuroim4s / neuroatlas4s | Pin-only descendants to keep the graph provider coherent. Their implementation is unchanged from the previously qualified versions. |

`PreparedShortestPaths` snapshots costs once and reuses the immutable graph/cost
snapshot across sources. Each source query has independent primitive scratch
arrays; a later query or a mutation of the caller's cost callback does not change
an earlier result. Sources and target queries require the exact indexed graph's
opaque vertex owner. mesh4s also checks the edge field's live owner before
constructing its adapter. No cast declares correspondence or registration.

A graph result of `None` is unreachable. `Some(+Infinity)` means reachable with
no representable finite Double path cost. Zero edges and all-zero graphs are
valid upstream. ScalaFIM keeps its pre-existing requirement for at least one
positive geodesic cost and explicitly uses `distanceOrInfinity` to preserve its
matrix convention for both unreachable and overflowing paths. Primitive reads
avoid constructing `Some` for each matrix entry; this is not an allocation or
speed benchmark claim.

The existing `DirectedRoutes` API retains finite-route selection, parallel arc
identity, caller edge-choice order and custom `Hash` behavior. Only its private
search engine is shared. Its existing differential and route tests remain in
place; their source formatting changed to satisfy the repository formatter.

The mesh graph-cost adapter does not require `PiecewiseEuclideanMetric`:
strict positive intrinsic lengths and triangle inequalities would reject valid
legacy nonnegative graph costs. A prepared adapter is local to the matrix
computation, not retained as a second permanent owner by `MeshTopology`.

## Candidate chain

- graph4s `613371f3bd0b12ce99352db0c637645eb62c6283`
- mesh4s `5ed45de9ab39deb22865438d65abcd50f9f8eb4a`
- neuroim4s `6d20f52e6701f10126f36a4fc97bf97eeb99fa24`
- neuroatlas4s `7a662192a1a5769e92c9809feec0286f47728419`
- ScalaFIM source `380476951300985979aefe67a903885b4f5d62cf`

The new neuroatlas4s candidate includes the previously qualified volume
extraction plus a pin-only update. The prior atlas checkout and exact
`7cfe136` receipt remain preserved; publication must not misdescribe the new
atlas candidate as only a pin relative to public main.

Every provider candidate is currently local. Coherent development gates select
four explicit checkout overrides and keep Gale at `779ed3d`. Default immutable
pin acceptance requires coordinated publication, a fresh exact-SHA remote audit,
and gates without those source overrides. Publish in dependency order:
graph4s, neuroim4s and mesh4s, then neuroatlas4s. ScalaFIM publication is separate.

## Review and remaining work

The requested Opus review did not execute: the CLI returned
`usage_limit_reached` (429), with zero model tokens and no verdict. Fray's daemon
was unavailable; a restricted read-only Opus CLI attempt was retained instead.
The user has been asked whether an independent Astra review may replace Opus.
Until that explicit decision and an actual SHA-bound verdict, the review
requirement remains unmet. No provider publication is implied by local gates.

After required review and authorized publication, qualify default pins. Retain
the cache collision, repeated-column, Floyd-Warshall, owner-refusal and
scientific fixture courts through any subsequent changes. Complete the remaining
U3 acceptance audit and full acceptance/review gates before closing U3 or the eco
epic. No hosted CI, new native viewer acceptance or measured speedup is claimed.
