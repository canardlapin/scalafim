# U3 mesh4s topology cutover, 2026-10-19

Implementation: `3488cfbc2121137e4d0f17b1cf47a44571b652d5`, on
`eco/u3-topology-20261019`. Baseline: `eeea0600d244634bacfb54f5557069fc2d9cde98`.
Fixture follow-through is recorded in `9c180664e` and `1cb288d5f`.
Additional local follow-through is isolated in `529526e3d` (JavaFX test probes)
and `8c1de2757` (raw storage chart metadata and visibility).
The bounded full-suite qualification is complete. Consolidated source is
`d9216dc8831143c1b6e7f4760f987947c03c4ff6`; Opus review is pending.
Nothing has been published.

## Ownership and admission

`TriangleMesh` composes a strict mesh4s `TriangleTopology` and a `PointField`
on its vertex space. Read-only packed coordinate and face views provide
primitive indexed reads; exporting mutable arrays explicitly copies. Coordinate
replacement retains the topology owner. `SurfaceSet` and `SurfaceGeometryPair`
share that owner after exact ordered-connectivity comparison. Framed transport,
spherical scaling, and display morphs retain connectivity.

The internal point field is `private[surface]`. Each field has a fresh ephemeral
storage chart, an uninterpreted quantity/unit with no canonical conversion scale,
and no anatomical convention or persistent frame key. Sharing a topology does
not establish coordinate-frame alignment. Physical coordinate meaning enters
through `FramedSurface`; an explicit metre-frame placement and transport
regression guards against an implicit millimetre interpretation.

Semantic and structural `SurfaceLocusDomain` constructors restore their original
persistent key, build the topology on that restored finite space, and expose a
refined `Vertex = S` topology. Geometry, point fields, and attached fields use
that same runtime owner. A count-only owner with a semantic annotation would
weaken alignment guarantees and is not used. Legacy scene hex keys, MVPA decimal
keys, and atlas SHA-256 surface identities retain their established encodings;
full ordered connectivity remains the compatibility proof.

Admission rejects unused vertices, duplicate or malformed faces, inconsistent
orientation, nonmanifold edges, disconnected vertex fans, and nonfinite
coordinates. The checked constructor retains typed provider audits. Synthetic
fixtures now satisfy those invariants; the provider checks were not weakened.
MeshTopology adapters derive provider edge order and map graph weights by
endpoints when graph4s chooses a different canonical order.

## Frozen scientific fixture

All 37 files in the committed `real_chain` acquisition remain byte-identical
to `50e5a5724`. The retained subject contributor subset has 167 vertices and
176 faces, but 61 source vertices have disconnected fans. The fixture-only
`FanSplitFixture` makes 69 extra local copies: 236 vertices with the same 176
oriented faces, face rows, and corner coordinates. Each copy has explicit
lineage; original acquisition vertex IDs are not unique local identities.

Input rows are duplicated through that lineage. Forward oracle columns are
remapped through their unique original winning face without changing any
coefficient. The scenario asserts `P_split E = P_original` for every original
field and retains the original native/analytic bounds and mutation controls.
The fsaverage and target meshes, scalar outputs, and native evidence files are
unchanged. This establishes pointwise sampling and forward interpolation for
fields constant across copies. It does not establish geodesic, smoothing, area,
mass-normalization, inverse, or adjoint equivalence. Original native tooling and
acquisition paths are unavailable; no new native observations or native
acceptance of the split derivative are claimed.

## Verification

All 97 unique targets passed in 40 bounded batches: 10,145 passing tests,
8 skips, zero failures or errors in the retained successful batch records.
The eight skips are four PHRF external/heavy cases, two surface JS opt-ins,
and the external CMRG packet on JVM and JS. Raw ignored markers are also
retained in the logs. These skipped cases have not been newly qualified.
The final matrix source is `5b984197a`; per-batch source SHAs and explicit
reuse proofs are retained in [gates.json](gates.json).

The later raw-chart follow-up at `8c1de2757` passed 320 surface JVM tests and
277 surface JS tests, with the same two JS skips. This includes identity
goldens, restored-domain ownership, placement/transport, resampling, ribbon
fill, and the frozen real-chain scenario. `scalafimCompileAll` passed on that
source for JVM and JS with no compiler warnings. The consolidated source's
3,054 source/input files are byte-identical to the separately tested
`77257a7e0`; only the separately tested IO example resource differs from the
compile/surface source. See [final-source-equivalence.json](final-source-equivalence.json)
and the named delta receipts. The manifest and raw logs remain in the common
Git evidence directory; exact Git source commits preserve the portable inputs.

The JVM allocation court passed its bound of less than 64 KiB for 1.8 million
packed coordinate/face reads. This is an allocation check, not a wall-time
benchmark. Inventory validation, the scenario manifest, and six pin-policy
tests passed. Both JavaFX Test/compile targets passed on `529526e3d`;
display/runtime probes were excluded.

The matrix freezes source, build, examples and resources with SHA-256 manifests.
Batches restart this worktree's sbt server, with at most two Scala.js modules
per batch. Full logs/statuses are under the common Git directory's
`verification/topology-cutover-20261019/`. The stage chain is
`full-suite-3488cfbc2`, `full-suite-resume-9c180664e`,
`full-suite-resume-1cb288d5f`, `full-suite-repair-products-1cb288d5f`, and
`full-suite-resume-io-fixture-5b984197a`. Failed stages are retained; passing
records are not relabelled as one later-source run. Each controller stopped
at its first failure or deadline, and the last completed successfully.

The exact mesh4s prerequisite is
`54b7278880e6db38c543a4a565355afd864e576a`, recovered into a complete Git bundle
and a normal source clone. Its ordinary batch `mesh4sCheckAll` gate passed on
that unchanged commit. The first warm-runner attempt reached passing tests but
failed in the runner's subsequent `eval` completion receipt, with a generated
build auto-import error. Both attempts and their exit statuses are retained.
The provider commit is still unpublished; current local ScalaFIM gates use
`-Dscalafim.mesh4s.build=/private/tmp/scalafim-mesh4s-u3-source-20261019`.

## Fixture follow-through and retained failures

The initial full run stopped at batch 16: an isolated Three.js backend test
still used an inconsistently oriented tetrahedron. Only two face orientations
changed in `9c180664e`; the complete Three.js target passed all 55 tests. All
3,052 other source/input hashes were identical, so the first 15 passing batches
were carried forward. The resumed raster/Three.js batch then passed.

Batch 17 exposed three additional example-fixture conflicts. `1cb288d5f`
corrects the GIFTI tetrahedron to match the portable fixture, reorders complete
face rows to test topology drift with a valid endpoint, and uses a closed
bipyramid for the synthetic corpus-size fixture instead of repeated strip
faces. Its comment explicitly excludes cortical-anatomy qualification. All
existing assertions and the visual hash remain unchanged. The example targets
passed 30 JVM and 29 Scala.js tests. Only those three example inputs changed;
the first 16 passing batches do not depend on that example project and are
carried forward with their original source references. Production Scala source
is unchanged throughout these two fixture corrections. Each continuation
freezes its own complete source/input manifest; the original failures remain
retained rather than being rewritten into a successful single-run claim.

The two JavaFX Test-only probe corrections in `529526e3d` also preserve strict
admission: coordinate replacement retains topology, and the viewport isolation
probe changes a valid diagonal rather than reversing one face. Both JavaFX
`Test/compile` targets passed; their display/runtime probes were not run.

Independent pre-review found that the raw point field previously exposed a
singleton frame with the provider's default millimetre unit. `8c1de2757` removes
those unsupported physical/alignment claims. It changes one production file,
two regression suites, one external-package visibility suite, and surface
documentation. This follow-up was qualified separately on JVM and JS while
the library matrix remained frozen at `1cb288d5f`. Matrix receipts retain their
actual source SHA; they are not relabelled as a single run of the later source.

Batch 31 stopped during `mvpaFitJS` compilation: provider class directories
were absent while incremental records still described their products as current.
The classpath contained the correct pinned locus4s provenance project; its
source was clean and unchanged. Inspection found 33 such orphaned incremental
records in this worktree's private sbt base. With its server stopped and the
base exclusively locked, the records were renamed in place and copied into
evidence so sbt would rebuild the missing products. No source or output
directory was deleted. The cause of the absent products is unknown. All source
and input hashes remained identical; the continuation reuses the first 30
passing batches and reruns the failed batch before proceeding. This is build
recovery, not a source fix or evidence that the earlier compile failure passed.
The template's provenance analysis contains 109 absolute references to the
source base; its seeded counterpart contains the new base's product references
and one historical source-base reference. Template reuse is a likely mechanism,
without a destructive causal reproducer. Follow-up
`bd-01M5AK2RR8G62KMX3NDPWCRM3D` tracks that tooling isolation defect separately.

Batch 39 confirmed the same inconsistent winding in the separate JVM IO
example's bundled GIFTI file. `77257a7e0` corrects only its second face
`0,1,3` to `0,3,1`; all coordinates, counts and existing IO/parcel assertions
remain unchanged, and all three example tests passed. The equivalent isolated
primary-branch commit is `5b984197a`. No production Scala changes. The first 38
passing library batches do not depend on this IO example project; their exact
receipts are reused while the four additional example targets are rerun.

## Adverse attempts

Iterative logs preserve the initial JGit `NoWorkTreeException` from using a
worktree attached to a bare recovery repository; a normal clone resolved that.
They also preserve constructor-access errors, remaining mutable-array test
assumptions, strict synthetic-fixture failures, disconnected fans in the
original retained subject subset, provider edge-order expectation changes,
and the allocation court's Java 17 API-target correction. No compiler policy,
numerical tolerance, native bound, or admission invariant was relaxed.

All iterative raw logs and metadata, resource hashes, and provider attempts are
retained under the common Git directory's
`verification/topology-cutover-20261019/`. Required independent Opus review is
still pending. This receipt does not close `eco-u3-topology`, either
simplification phase, or a later eco milestone. Hosted CI and publication have
not been performed.
