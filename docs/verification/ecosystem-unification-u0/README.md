# Unification baseline and composition contracts

Source: `3a2fda43c0a1155832bfb2dc5c4ead1503038190`, 2026-10-16.
The working tree contained only the newer planning journal operations before
U0 documentation. Provider revisions are the immutable pins in that commit's
`build.sbt`; no sibling overrides are used for these ScalaFIM checks.

This is a bounded migration baseline. It does not qualify every algorithm,
native renderer, external asset or downstream application. Commands, full logs,
exit status and tool timing are retained beside this receipt when complete.
The initial sandbox attempt failed before sbt started because its cache lock
was outside the writable checkout; the authorized retry is separate evidence.

## Corpus destinations

The [ownership map](../../plans/ecosystem-ownership.md) owns production
decisions. Tests and resources follow these rules; mixed suites split by the
assertion's owner, preserving their original fixtures and independent oracles.
No fixture is regenerated merely to make migration pass.

| Current corpus/tool | Destination |
| --- | --- |
| `locus-data` tests | locus4s laws for generic ownership/map/quotient behavior; downstream integration tests for explicit registry scope. |
| `response` and `response-laws` | Remain ScalaFIM, except provenance ID/DAG validation moves to locus4s-provenance; retain a cross-consumer lineage test downstream. |
| `provider-spike` | Provider-only composition court, no ScalaFIM imports. Extend with shared frame/mesh and repeated-draw specimens as capabilities land. |
| `image` shared/native/IO tests and resources; `tools/image/*` | Generic array/grid/format behavior to image4s; neuro volume/series and anatomical admission to neuroim4s. Move `WorldSpaceSuite` to spaces and keep image-specific evidence tests in IO. Allocation suites remain platform-specific. |
| `transform` tests, 568 resource files, `tools/transform/*`, `tools/spatial/generate_itk_hdf5_fixtures.py` | neuroim4s transforms, except generic map/solver laws to reframe4s. Keep exact native-tool versions and optional-asset skip policy. |
| `spatial` tests | Split by the complete source partition. Provider map/algebra laws go upstream; volume runtime suites to neuroim4s; surface/hybrid/volume-to-surface suites to neurosurf4s integration; functional/latent scenarios remain ScalaFIM. |
| `surface` tests/resources, `surface-test-fixtures`, `tools/surface/*`, surface ingestion examples | neurosurf4s, except generic topology/kernel laws to mesh4s. Share immutable fixtures or an unpublished fixture artifact; do not copy independent mutable fixture sets. |
| `surface-view*` tests/resources, display examples, all `tools/surface-view/*` | neurosurf4s display/backend owners. Connectivity integration tests remain ScalaFIM. Native correctness/performance gates remain unresolved where already unresolved. |
| `image-view*` tests and `tools/image-view-browser-benchmark.html` | neuroim4s display/backend owners. |
| `atlas` tests/resources and atlas examples | neuroatlas4s core/volume/surface/routing/IO as their dependencies require. Split publication tests into the optional schema adapter. Atlas-to-MVPA example belongs to optional mvpa4s atlas integration. |
| `mvpa` tests, benchmarks, `tools/mvpa/*`, `tools/mvpa-inference/*` | mvpa4s scientific/predictive/measurement owners. Pipeline interpreter contract tests stay ScalaFIM. Preserve adverse qualification reports and experimental labels. |
| `mvpa-fit/dataset/group/artifacts` | ScalaFIM-specific tests remain; generic predictive/measurement assertions move with their implementation. Foundation spike assertions move to provider admission tests before retiring scaffolding. |
| `tools/r-parity/generate_neuroim2_image_parity.R` | neuroim4s (generic image assertions additionally owned by image4s). |
| `tools/r-parity/check_neuroatlas_atlas_parity.R` | neuroatlas4s. |
| `tools/r-parity/generate_neurofunctor_{law,routing}_fixtures.R`, `generate_ribbon_fill_fixture.R`, `surface-fixtures.md` | neurosurf4s integration, retaining generic provider law subsets upstream. |
| `tools/r-parity/generate_mvpa_r_parity_fixtures.R`, `generate_signed_cross_run_rayleigh_fixtures.R`, `mvpa-fixtures.md` | mvpa4s. |
| Other R-parity tools, common receipt tooling, `tools/fixtures`, `tools/scenarios`, motion/HRF/registration/fit tools | Remain ScalaFIM unless a particular provider algorithm is moved; shared receipt tooling is consumed rather than copied. Existing manifests must be updated when a referenced fixture relocates. |

## Three target specimens

1. **Exact domains and repeated draws.** Persist a three-element locus4s
   domain, restore it twice in independent registries, explicitly align it,
   and gather `[2,0]`. Reject an unrelated owner of equal size and a different
   ordered identity. Reject `[2,0,2]` as an injective selection, but accept it
   as a Resample4s draw with three distinct occurrences whose roots are
   `[2,0,2]`. Nested draws must preserve occurrence paths and composition order.
   Current anchors: `DomainIdentityProbeSuite`, `AxisRefSuite`,
   `ReindexingLegSuite`. Gap: `DomainFactory` still supplies a global scope;
   MVPA still materializes response-specific selection/identity.
2. **One volume/mesh frame.** A 2x2x2 grid and an ordered triangle realization
   share exactly one spatial4s RAS/mm frame type. Translate by `(1,2,3)`;
   origin maps to `(1,2,3)` and a scalar field stays on the same ordered mesh
   owner. A wrong frame fails admission; an unavailable inverse direction is
   rejected rather than approximated implicitly. Current anchors:
   `providercontract.ProviderCapabilityCompileContract`, `WorldSpaceSuite`,
   `WorldTransformLinkSuite`, `FramedSurfaceSuite`, `MeshTopologySuite`.
   Gap: the current build still has image4s frame authority and local mesh
   containers. U2/U3 must make the provider-only specimen compile and run;
   this baseline does not pretend it already does.
3. **Response access into MVPA provenance.** Read an explicitly ordered subset
   of an in-memory response, carry its source DAG into identified MVPA evidence,
   and round-trip records with the same IDs, parents, roots and declared
   support. Duplicate roots, duplicate IDs and parents that are unavailable
   in topological order fail. Unknown support remains unknown. A borrowed read
   cannot outlive its scope, and reindexing output does not narrow upstream
   support. Current anchors: `ResponseKernelSuite`, `EvidenceOriginsSuite`,
   dataset observation and native-read policy suites. Gap: provenance is
   currently response-owned. The final specimen is a downstream integration
   test using locus4s-provenance directly; it cannot live in a provider core.

## Workflow gates retained for later cutovers

| Workflow | Existing evidence family and cutover owner |
| --- | --- |
| Dataset → design → fit → contrasts → group | first-level-laws, fit and workflow scenarios; ScalaFIM integration. |
| Dataset/first-level readout → MVPA → prediction/RSA | mvpa-fit readout relations, mvpa-dataset predictive acceptance, mvpa relation/parity suites; ScalaFIM plus mvpa4s. |
| Atlas/locus searchlight → volume/surface measurement → scatter | mvpa-spatial measurement/searchlight parity and atlas quotient/realization suites; optional mvpa4s adapters. |
| GIFTI/FreeSurfer → neuro surface → scalar display → readback | surface IO/placement, raster reference and scene-document/backend conformance suites; neurosurf4s. |
| Shared frame → projection/linked cursor → renderer pick | spatial volume-to-surface/mixed-pullback and surface volume-cursor/picking suites; neurosurf4s volume integration. |
| NIfTI → neuro volume/series → toolkit transform → slice view | native image/NIfTI, transform oracle and image-view suites; neuroim4s. |
| Atlas catalog → realization → query/reduction → route | atlas core/coverage/quotient/realization/provenance/route and IO suites; neuroatlas4s. |

`plsneuro` is the maintained downstream consumer. Its public adapters,
provider-override forwarding and workflow probes must pass at the relevant
cutover; they are not rerun for this documentation baseline. `neuropublish`
and `eidolon` are experimental and do not gate this work.

Known limitations remain explicit: no full-repository release gate here; no
new native/browser visual or performance acceptance; no native-tool or optional
real-asset oracle qualification from unit tests; no provider-only test may
import ScalaFIM. Preserve the unmerged `surface/warm-update-20260921` and its
unresolved correctness/performance evidence. These rules apply even if the
small migration baseline is green.

## Recorded runs

[Machine-readable runs](runs.json) bind the source revision, commands, raw-log
hashes and exit outcomes. Each run has a compressed full log and JSON metadata
in this directory (`gzip -dc NAME.log.gz` reads the log).

| Run | Result |
| --- | --- |
| `identity` | Exit 1 before sbt: sandbox cache-lock restriction. |
| `identity-authorized` | Exit 0: locus-data 26/26, response 16/15, axes/reindex/provenance 17/17 (JVM/JS). |
| `geometry` | Exit 0: image 28/28, transform 7/7, surface 12/12. |
| `composition` | Exit 0: spatial 8/8, atlas 12/12, execution 16/16; provider-spike compiles on both platforms. |
| `display-and-family` | Exit 0: image view 9/9, raster 14/14, voxel-family randomization 12/12. |
| `scene-parity` | Exit 0: 48 Node contract/tool tests, zero skips. This is not a native visual-conformance result. |

Total: 353 Scala test executions, 48 Node tests and two provider-spike compile
gates. No selected test failed. Existing Intaglio build-load warnings report
18 unused documentation `tags` settings; there were no Scala compilation
warnings in the affected source batches. Response has one JVM-only test.
The initial execution/family selectors followed directory names instead of
package names and did not match those suites. `composition` explicitly ran
`scalafim.fmri.mvpa.analysis.LocalExecutionSuite`; `display-and-family` ran
`*VoxelFamilyRandomizationSuite` and the logs confirm all 16 and 12 cases on
both platforms. The final counts include only tests that actually executed.
