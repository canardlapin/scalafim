# Local anatomical routing candidate - 2026-10-20

This candidate moves volume-only admission, registration routing, sampling and
scoped volume reads into optional neuroim4s modules. It also moves generic
directed weighted route search into graph4s. ScalaFIM retains mixed-domain and
functional integration, with the replaced volume and NIfTI implementations
removed. See [ownership.md](ownership.md) for the remaining destinations.

## Immutable source set

| Repository | Baseline | Local candidate |
| --- | --- | --- |
| graph4s | `6a72b58b564933234768f025e67228736ef2f7c5` | `856e6fb770cf5f4c5512b0607d8c3733e75e7460` |
| neuroim4s | `018a9f2e80db4f2d2945c7eba867835ab9144135` | `7ed49f12b1a90e18ea5ed67d48343e98a7b9547b` |
| ScalaFIM | `e98cc987041c3ea013c3637ba3f48cb7e24877a3` | `36f5ad0e20d9ba5c22f90a1ffa73ba22fe33a6eb` |

The shipping cutover is `405c024988d6542181e567fe1d80e05bb6a677ba`;
`36f5ad0e2` adds only the verified dead meta-build-helper deletion and its
reservation operation. Later handoff commits contain documentation/tracker
operations only. Exact receipts are in [qualification.json](qualification.json).
Verified thin Git bundles for the two provider candidates are retained beside
the raw logs, with their checksums in that receipt.

Gale remains `779ed3d8169cdd9f4eb9ab10b1ca9defadc27715`. Other provider pins
are unchanged. Both new provider commits are local and unpublished. ScalaFIM's
candidate pins require explicit matching source overrides until publication;
they are not remote-resolvable release pins yet. The accepted
`eco/u3-topology-20261019` branch remains at `e98cc987` with published pins.

## Preserved contracts

- Actual image4s/spatial4s grids and frame owners remain authoritative. A
  checked `VolumeGrid` owns D3 and mask admission; external construction cannot
  bypass it. A same-world frame with a different runtime owner is refused for
  a registration map.
- Routing remains forward-first, with declared geometric inverse availability,
  the same quality deficit penalty, minimum route quality and ordered parallel
  alternatives. Graph lookup honors Cats `Hash`; shortest costs are checked
  independently against Floyd-Warshall on 48 small cyclic graphs.
- Pullbacks retain coordinate order; values are sampled once. Analytic
  interpolation/coverage and a chained quarter-voxel witness distinguish this
  from repeated interpolation. Empty demand cannot make a missing map executable.
- Sparse support, target/observation order, lazy preparation, file freshness
  before cache hits, endian/scaling and closed read resources are retained.
  Hand-written NIfTI fixtures independently check three-axis storage order and
  decoding; truncated reads return a typed error and close the channel.
- Existing recipe/fingerprint prefixes and geometry text are retained. Shared
  provider code and tests have no ScalaFIM, surface, atlas, response or dataset
  dependency. Optional NIfTI IO stays JVM-only. The field adapter now refuses a
  non-volume domain at preparation, before constructing the volume-only reader.

## Reproduce locally

Check out the exact commits above. From the ScalaFIM candidate, add these source
overrides to the sbt commands below:

```sh
-Dscalafim.neuroim4s.build=../neuroim4s-eco-routing-20261020
-Dscalafim.graph4s.build=../graph4s-eco-routing-20261020
```

Use absolute paths if running elsewhere. Provider gates use
`-Dneuroim4s.graph4s.build=../graph4s-eco-routing-20261020` from neuroim4s.
Use separate sbt JVMs for the bounded batches; do not run `scalafimTestAll` in
one process. Local commands used a 4 GiB heap and the existing owned sbt global
base at `~/.cache/scalafim-sbt/bases/dc58d8731036fb76` to avoid the known
fresh-template Zinc absolute product-path issue.

```text
graph4s: algorithmsJVM/test algorithmsJS/test
neuroim4s: routingJVM/test routingJS/test routingIoJVM/test
ScalaFIM JVM: spatialJVM/test atlasJVM/test mvpaSpatialJVM/test
             atlasExamplesJVM/test workflowExamplesJVM/test
             workflowExamplesJVM/runMain scalafim.examples.workflows.runAtlasMvpaWorkflow
ScalaFIM JS:  spatialJS/test atlasJS/test mvpaSpatialJS/test workflowExamplesJS/test
compile:     scalafimCompileAll examplesCompile
```

The committed receipt records exact commands, exit statuses, summary counts and
SHA-256 hashes of retained raw logs. Raw logs, including earlier compile
refusals, live under the shared Git directory's
`verification/neuroim-routing-20261020/`. The existing isolated native atlas
oracle is included in `atlasJVM/test`; optional timing budgets were not enabled.
No new native visual qualification or hosted CI is claimed.

The final affected gates passed **759 tests**: graph4s 18 per platform,
neuroim4s routing 14 per platform plus 4 JVM reader tests, ScalaFIM JVM batches
384 and JS batches 307. All modules and examples compiled on both platforms.
The subsequent deletion of the unused meta-build helper is checked by a fresh
build load and spatial JVM/JS compilation; shipping sources are unchanged.

## Simplification limits

Production Scala source delta across these commits (excluding tests, docs and
build files): ScalaFIM **-662** lines, neuroim4s **+1,194**, graph4s **+94**;
ecosystem total **+626**. Removing the unused ScalaFIM meta-build copy of
`FrameErasureGate` deletes another **274** build Scala lines; the active
neuroim4s boundary checks remain. No ScalaFIM module is added; neuroim4s gains two
optional artifacts. This slice reduces ScalaFIM's implementation and isolates a
closed anatomical contract, but it does not establish an ecosystem-wide LOC or
module-count reduction. The new provider has one implementation of volume
sampling/reads; remaining mixed/functional adapters and their later U5/functional
destinations are explicit in the ownership map. U6 must assess final counts
across the complete extraction.

Publication requires separate user approval under AGENTS.md. Mote review status,
if used for this candidate, is not independent reviewer evidence. The eco epic
remains incomplete.
