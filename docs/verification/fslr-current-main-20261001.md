# Current-main fsLR route handoff

Work tag: `fslr-reconcile-20261001`. Native Mote tickets, in dependency order:
`bd-01M3W0PRGEYAEK7QRCKJ8HZ5ED` (Java 17 CI), then
`bd-01M35BHNDCHM6YXCKYX0544TP3` (fsLR reconciliation).

The reconciliation initially started at ScalaFIM
`966502b7f984bf7da44e655f47b76ef3c035b5ad` and was moved onto current main
`c05a0811e96295c105633bbad0325790e22377c6` after the HRF/design identity work
landed during verification. It uses selected reference API and fixture files from PR #8 head
`206243b37adff1ab3dc67f7ee4dda44a9d42e547`. It retains current-main image4s
world identity, surface sampling tallies, transform adapters and repository
gates. It does not import the older branch's provider revisions or build.

## What is admitted

The numerical contract covers digest-bound declarations, exact template
variant/release/cohort equality, ordered hemisphere mesh identity, medial-wall
exclusion, explicit bridges, nearest-voxel sampling and per-vertex receipts.
The current-main reframe4s provider owns affine composition and displacement
interpolation. One replicated voxel layer encodes ITK's constant edge
displacement through that provider; the adapter checks support against the
original field. Outside support is still unavailable to a typed route.

The frozen SimpleITK fixture is an independent numerical oracle, including
oblique fields, stage order and half-voxel borders. The real forward fixture
checks 200 canonical transform points. Neither oracle proves that a Conte69
surface is anatomically registered to a particular volumetric template.

**The MNI152NLin2009cAsym-to-fsLR32k route is not anatomically qualified.**
The exact Conte69 volumetric registration chain, anatomical template release
and cohort remain missing. A catalog revision identifies acquisition metadata;
it does not establish those anatomical facts. GIFTI Talairach codes, file names,
and grey-matter overlap are insufficient replacements.

The locked HDF5 conversion is a forward point map from 2009c to 6Asym. The
requested surface lookup needs the opposite direction. Pinned reframe4s
`9a4508351d74567147b8ea3221d82db89e5892b0` does not expose an exact pointwise
inverse with per-query residual/iteration evidence. `FrameBridge.displacement`
therefore returns `ReferenceError.PointwiseInverseUnavailable` for an inverse
request. A materialized lattice inverse is a different numerical route and
requires its own declaration and qualification.

## Dependency and resource closure

[The resource lock](fslr-current-main-20261001-resources.json) records all 12
direct source dependency revisions and the paths, sizes and SHA-256 values of
12 locally observed TemplateFlow resources. All 14 resolved source checkouts
were checked clean, including the transitive linops4s revision and additional
Gale revision recorded separately in the lock. No dependency revision is changed.
The portable SHA provider is the already-pinned zarr4s core at
`2a5ba963b151b62c739d1bf5a19d49202bb6ff29`; uPickle is `4.1.0`.

The acquisition/point-map format contract is TemplateFlow4s
`67097a9ecab7ea4ed8a81eb3532861096a3b2282`, catalog
`templateflow@d79aacb1ad7d1c52e5d10ad88f48fd8af6e5ae56`. TemplateFlow4s
is not a ScalaFIM build dependency. This lock was checked against the local
cache; it is an observed-byte record, not the missing original acquisition
receipt or proof of anatomical registration.

The canonical manifest is
`34bdcea2dab7c0fccde6e6607cd080fbcea087abaf19721925b5b8fb61d85318`,
derived from source HDF5
`2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe`.
The reader binds the manifest bytes, source identity, stage bytes, frame names,
application order, RAS-mm vector semantics and canonical NIfTI header before
constructing a map. Matching digests do not override contradictory semantics.

The locked cortex ROI files contain one float32 `INTENT_NONE` vector with
strict 0/1 flags and explicit CortexLeft/CortexRight metadata. The dedicated
reader checks that representation and binds the exact mask asset to route
disclosure. The left/right cortex counts are 29,696 and 29,716 out of 32,492.

## Ordinary consumer example

`scalafim.examples.reference.FslrRouteExample` is outside the reference
package and uses public APIs throughout. Its tiny checked-in asymmetric control
has four vertices, independently encoded voxel values, a valid zero, one
nonfinite voxel and one medial-wall vertex. Its declared mesh density is
`synthetic-4`; it is not presented as an fsLR32k anatomical example.

Run from the repository root, choosing a writable receipt path:

```sh
sbt "surfaceJVM/Test/runMain scalafim.examples.reference.FslrRouteExample synthetic modules/surface/jvm/src/test/resources/fslr-route-example/manifest.json /tmp/fslr-synthetic-receipt.json"
sbt "surfaceJVM/Test/runMain scalafim.examples.reference.FslrRouteExample real docs/verification/fslr-current-main-20261001-resources.json $HOME/.cache/templateflow /tmp/fslr-real-refusal.json"
```

The first command verifies source, anatomy, cortex mask and both display
surfaces; prepares the source once; maps and inspects each vertex; and attaches
the same mapped object to inflated/very-inflated geometry. The second verifies
all locked resources and emits the real route's typed refusal and explicit
qualification gaps. It does not manufacture a successful anatomical route.

Freshly generated examples are included as
[the synthetic receipt](fslr-current-main-20261001-synthetic.json) and
[the locked real refusal](fslr-current-main-20261001-refusal.json).
Their SHA-256 values are respectively
`5f770d8cecbdb67eb92eb63f964aad73e45d58c81eabe4c767862b1cc2cab187` and
`1649070ff7421e2e1453f08c4e289078578091dc86c70285cae301a983bbcdfc`.
An independent receipt check confirmed values `[0, null, 2, null]`, distinct
`Mapped/NoSupport/Mapped/MedialWall` coverage, display object identity, and the
real receipt's binding to the resource-lock bytes.

Consumer API sequence:

1. Construct an exact `TemplateFrame` and a `FrameDeclaration` with a bounded
   `FrameBasis` and digest-bound `DataAsset` or `AssetProvenance`.
2. Use `DeclaredVolumeReader.readNifti`, `DeclaredSurfaceReader.read` and
   `DeclaredMedialWallReader.read`. The volume reader uses current-main
   `SpaceEvidence`; contradictory header evidence is refused.
3. Construct `VolumeReference`, `CorticalMeshReference`, `SamplingAnatomy` and
   `RouteRequest`; call `SurfaceRoute.admit` with an explicit bridge when needed.
4. Reuse `route.prepare(source)` for `route.map` and `route.inspect`. Preserve
   the receipt and `MappedSurfaceValues.source` with exported results.
5. Attach display geometry with `mapped.onDisplay`; display coordinates never
   determine the sampled values.

The shared model uses current-main `SomeScalarVolume`, `SomeMaskVolume`,
`SomeSampleSpace` and image4s `Affine[D3]`. Equal dimensions and affines do not
establish frame identity. Use a common explicit `WorldSpace` for independently
constructed related grids. RAS millimetres and known world compatibility are
checked at `VolumeReference.make`.

Portable byte admission is `PointMapManifest.fromBytes` followed by
`DeclaredPointMap.fromManifest` with a stage-byte callback. Scala.js supports
those boundaries, numerical mapping and declared GIFTI surface bytes. The
ordinary NIfTI volume reader, cortex-mask byte reader and point-map directory
reader in this handoff are JVM APIs.

Nearest lookup rounds half ties upward and uses half-open support
`[-0.5, dim - 0.5)`. `DepthNearest` samples explicitly declared white/pial
fractions; it is point sampling, not ribbon integration. Receipts distinguish
outside-grid, outside-support, nonfinite, medial-wall and bridge-unavailable
contributions. Zero is valid data. There is no universal missingness percentage
that grants anatomical admission.

## Java 17 and local gates

The allocation helper uses `ThreadMXBean.getCurrentThreadAllocatedBytes` and
represents unsupported, negative or decreasing counters as an explicit
limitation. It always executes the measured body once. A missing counter never
becomes a reported zero allocation. The standalone probe measured a retained
1 MiB allocation on Temurin 17.0.20.1+1 and OpenJDK 25.0.1; the helper's real
allocation and fallback tests also passed in both final JVM runs.

[The gate receipt](fslr-current-main-20261001-gates.json) binds the final
commands, toolchains, source hashes, raw-log hashes, provider artifacts and
example receipts. The tested source input is
`manifest:f6b23ce45097a6f398b30c66ab5ddf775702cc23a6aca45d1139f48e7df60297`,
on base `c05a0811e96295c105633bbad0325790e22377c6`.

- Java 17 `scalafimCompileAll` passed without compiler warnings.
- The existing full repository script passed all 13 bounded batches:
  6,455 tests passed, zero failed, one declared opt-in resource skip.
  Surface tests passed 267 on JVM and 214 on Scala.js; surface-view passed
  68 on each platform.
- The skipped resource test ran separately with `SCALAFIM_JS_TIMING=1` and
  `FullOptStage`: one test passed, `productionMode=true`.
- A fresh Java 25 clean surface recompile and full test run passed 267 surface
  and 16 spatial tests without compiler warnings.
- Both public example modes regenerated their locked receipts on current main.

The FullOpt test used a synthetic 32k hemisphere and dense displacement field.
Admission median was 830.5 ms; prepare-and-map maximum was 201.9 ms over seven
runs, below the unchanged 2 s budget. This is a local synthetic observation,
not a real-route performance qualification.

The final gates used normal dependencies. The isolated CI cache was prepared
with the repository's existing `tools/prepare-pinned-dependencies.sh`, building
Multivar's exact Gale and Resample4s artifacts locally. Provider publication
Scaladoc emitted repeated-classpath flag warnings, retained in its raw log;
final ScalaFIM compiler gates were warning-clean.

Earlier failed attempts remain identifiable in the gate receipt. The initial
fresh-cache run omitted provider preparation. A downstream test also compared
two independently created unresolved grids; the same failure was reproduced
on clean current main. Its test-only repair compares the domain with the actual
source grid and retains rejection of an independent grid. Production alignment
checks were not relaxed.

Independent source review found that known display/foreign-structure GIFTI
metadata could be relabelled as cortical anatomy. The public-reader failure
was reproduced, and the shared boundary now checks geometric type, primary
structure and secondary anatomy. JVM/Scala.js regressions pass; the locked
inflated asset cannot enter the anatomical sampling chain. The new synthetic
VeryInflated fixture's own metadata and lock were corrected; frozen numerical
oracle bytes were not changed.

These are local gates. Fresh hosted CI on a published Git candidate remains
pending. Numerical tests, grey-matter overlap and passing builds do not establish
the missing anatomical registration or pointwise inverse.

PLS Neuro owns migration from its pinned ScalaFIM revision, source-result
provenance, consumer qualification and any default-route decision. This
handoff does not migrate or qualify that consumer.
