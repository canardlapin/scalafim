# Real demo1 interior chain qualification

P7.07 now has a real fMRIPrep chain scenario, `transform.fmriprep-demo1-interior.v1`,
with one clean `Pass` required on JVM and JS. It exercises the actual scanner/BOLD
reference -> T1w -> MNI152NLin6Asym assets on one declared approximately 8 x 11 x 6.5 mm interior patch.
The broader P7.07 ticket remains open for real FSL and real surface commutativity
workflows. This record does not establish registration accuracy or whole-domain
or boundary parity, and does not retire the historical synthetic ITK border caveat.

## Source identity

The demo1 HDF5 pair, BOLD reference and T1w in `neurotransform` match the size and
MD5E annex keys in the public derivative at commit
`1d8407e467d1af0ac8933c0539bbbb0168badab6`. Additional SHA256 identities, local
Git blobs and the derivative paths are recorded in the fixture's
`source-provenance.json`. The derivative describes fMRIPrep 21.0.2, source
ds002748 v1.0.5 and CC0 in its pinned
[dataset description](https://github.com/OpenNeuroDerivatives/ds002748-fmriprep/blob/1d8407e467d1af0ac8933c0539bbbb0168badab6/dataset_description.json).

The exact missing task-rest scanner/T1w text pair was retrieved from the public
S3 derivative, checked against the pinned annex keys and SHA256, and committed
as `scanner_to_t1.txt` / `t1_to_scanner.txt`. fMRIPrep 21.0.2's
[export code](https://github.com/nipreps/fmriprep/blob/21.0.2/fmriprep/workflows/bold/outputs.py)
binds scanner -> T1w to `bold2anat_xfm`; its
[resampling workflow](https://github.com/nipreps/fmriprep/blob/21.0.2/fmriprep/workflows/bold/resampling.py)
orders the standard-space and BOLD/T1w legs consistently with the native command
below. No synthetic affine substitutes for a missing registration.

The original HDF5 sources are not bundled:

| Direction | SHA256 | Bytes |
| --- | --- | ---: |
| T1w -> MNI152NLin6Asym | `44fba8c2aec16da828b20bdd53b7c534f4c80bfc7281a1bc2affc10f8b4eb363` | 74516537 |
| MNI152NLin6Asym -> T1w | `4903d429d8f6dbd30d6b7cdd1b93fac8dd451d34ddcedc4d9d2066bbac4782fe` | 74524486 |

## Native references and support

ANTs `2.6.5.dev1-gfdce4d2` runs in the pinned local image
`sha256:ac096b2f75f67866feb6606fde2a549d395675f83baec9fd17610c9a7053fa18`
(registry digest
`antsx/ants@sha256:59c45f54a1f1dc69134f63bec91a726e41c71c64a16cc21cda0b54526910a3c3`).
The container has no network and reads the original source directory read-only.
Binary double MHA point input/output and `-p 1` avoid lossy CSV point precision;
image references use `--float 0 -u double -n Linear`.

The target is one declared 7 x 9 x 5 interior lattice (1.1 x 1.25 x 1.3 mm
voxels; approximately 8 x 11 x 6.5 mm including voxel support), with 32 deterministic
asymmetric off-grid holdouts. There are 347 template queries, 32 independent T1w
inverse queries and 347 forward-derived T1w queries for native pair closure.
No frozen query is excluded. Source image and field boundaries reject. Crops
retain float32 field sample bits, stage order, affine parameters and centre,
spacing/direction, and the exact integer-shifted origin. The inverse crop covers
the affine-transformed field queries rather than the external input points.

| Native check | Maximum absolute difference |
| --- | ---: |
| Full/cropped forward, scanner chain and independent inverse points | 7.10543e-15 mm |
| Full/cropped real BOLD resampling | 0 |
| Composed/sequential native scanner chain | 0 mm |
| Native image coordinate controls / native point outputs | 4.97380e-14 mm |
| Native supplied forward/inverse pair closure | 0.0233913 mm |

The last row characterizes the native pair; Scala matches its native results,
without an assertion that those source registrations are exact inverses. The
minimum crop support margins exceed 2.14 voxels and the minimum final BOLD source
margin is 10.8765 voxels. `metadata.json` records the lattice and all measurements.

The frozen point tolerance is 1e-6 mm and full/crop equality tolerance is 1e-10 mm.
The BOLD intensity tolerance is 0.01 source units, checked against the global
source-gradient and coordinate-precision bound 0.002221858 before comparing
resampled intensities. The original NIfTI header is preserved in coordinate
controls; ITK source placement uses normalized sform directions and pixdim
spacing. An initial geometry-control attempt was rejected at 3.61412e-6 mm.
`rejected-initial-probe.json` preserves that finding and states the missing raw
initial-output limitation. The accepted recipe kept the original budgets.

The offline fixture manifest closes 26 files (2,609,134 bytes, excluding the
manifest itself), including native outputs, exact crop containers and shared
h5py dumps, metadata, source identities and raw native command receipts. Empty
native stdout is a recorded command receipt, not a substitute for output-byte
checks. The generator refuses changed source identities or lost support.

The cropped native point/image outputs and sequential native control were
compared during generation but were not retained. Their equality measurements
are generation receipts in `metadata.json`; the retained full-native outputs
remain the independent references used by Scala. A follow-up attempt to retain
the extra native outputs was blocked at Docker image inspection, before compute.
`native-extension-blocked.json` records that limit. It does not replace a native
run or claim a new native result.

## Checks and reproduction

The shared suite checks the native point results, actual BOLD intensities and
coordinate controls through typed public transforms and resampling. It checks
the supplied inverse through public `mapPoint`, rejects five convention mutations,
refuses points outside the crop, and checks that reversed typed composition
does not compile. The JVM suite binds both HDF5 crops to their shared dumps through
jHDF and `TransformFiles.load`.

Default offline checks:

```sh
sbt transformJVM/test
sbt transformJS/test
python3 tools/scenarios/validate_manifest.py
sbt scalafimCompileAll
```

The explicit original-container check requires both full SHA-bound files.
It checks point mappings and retained field samples; full-original BOLD
resampling and coordinate controls are separate native-generation receipts,
checked through the offline fixture and shared scenario. It
fails on absent paths or incorrect bytes, parses both complete HDF5 containers,
verifies every retained field value and all other stages against the crops,
and compares the full original chain to the frozen native point results:

```sh
sbt "transformJVM/Test/runMain scalafim.transform.scenarios.Demo1OriginalContainerQualification /path/to/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 /path/to/sub-01_from-MNI152NLin6Asym_to-T1w_mode-image_xfm.h5"
```

It is an additional external-asset gate, not a default test that silently skips.
The qualification receipts distinguish the default offline gates from this gate.

To regenerate with the original `neurotransform/inst/extdata/demo1` source set:

```sh
python3 docs/verification/stp-demo1-20260930/provenance/fetch_metadata.py \
  --source-dir /path/to/neurotransform/inst/extdata/demo1 --out /tmp/demo1-provenance
uv run --python 3.12 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy==2.5.3 \
  python tools/transform/generate_demo1_native_oracle.py \
  --image antsx/ants@sha256:59c45f54a1f1dc69134f63bec91a726e41c71c64a16cc21cda0b54526910a3c3 \
  --source-dir /path/to/neurotransform/inst/extdata/demo1 \
  --provenance-dir /tmp/demo1-provenance
```

The source directory must be inside the verified Git checkout for the provenance
fetcher to record its source commit/blob identities. Generated HDF5/gzip container
bytes may depend on library metadata, so scientific reproduction additionally
checks retained sample bits and native original/crop equality. Exact committed
bytes are always checked by the offline manifest.

`qualification.json` records check receipts and limitations; `source-manifest.json`
binds the reviewed source, fixture and documentation scope to exact bytes.
Five convention mutations check point failures; this suite does not claim
image-mutation coverage or qualification of non-diagonal displacement-field
directions. The 1e-6 mm point budget is a fixed acceptance tolerance, not a claim
of double-precision agreement. The Fray review rederived raw-HDF5 mappings but
did not audit the upstream annex retrieval or run ANTs/sbt.

This is local qualification; no hosted CI, upstream publication or pin adoption
is claimed.
