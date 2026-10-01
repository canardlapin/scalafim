# P7.07 real FSL chain: declared interior

`transform.fsl-real-demo1-interior.v1` checks newly fitted native FSL
`example_func -> highres -> standard` registrations on real, matching images.
The subject is the source-verified CC0 ds002748 v1.0.5 sub-01 task-rest demo1
BOLDref/T1 pair and its matching public masks, produced by fMRIPrep 21.0.2.
The standard is TemplateFlow MNI152NLin2009cAsym res-02 at commit
`15d7c02160f79f5218d2545b4febebeecc11531d`; its MNI/Collins permission licence
and copyright notice accompany the fixture. Subject metadata is pinned at
`1d8407e467d1af0ac8933c0539bbbb0168badab6`.

This is composition and resampling qualification of new real-data fits. It does
not reproduce a historical FEAT registration, measure anatomical registration
accuracy, qualify Jacobians/boundaries/the whole image domain, or settle the
separate surface-chain acceptance. P7.07 remains open.

The recipe masks the real images using their matching public masks, then runs
native six-DOF FLIRT functional-to-T1, twelve-DOF FLIRT T1-to-standard, and FNIRT
with the fitted affine and explicit three-level settings. Native `convertwarp`
composes the prematrix with the complete FNIRT coefficient file. Native
`applywarp` independently samples coordinate ramps and the real functional
image with both direct-prematrix and composed-field routes. Native
`flirt -applyxfm -noresampblur` checks the affine leg separately. Nineteen
successful native command receipts and the installed macOS package identities
are retained. Docker was not restarted or changed.

Both query windows were fixed in the generator before attempt 1 was run, and
remained unchanged thereafter: 693 standard voxels in a 9 x 7 x 11 window starting
at (44, 56, 40) (2 mm spacing, approximately 16 x 12 x 20 mm between extreme centres),
and 315 T1 voxels in a 7 x 9 x 5 window centred using the declared RAS point
(-8.5, -14.5, 9.5). Every frozen query must have native support >=0.999; there is
no error-based query exclusion. The full FNIRT coefficients remain intact.
The functional image crop retains every interpolation neighbourhood with more
than two voxels of margin. All compact native-output/image samples are copied
bit-for-bit from the retained full native files.

The zero-data geometry fixtures retain the **native-input headers**, including
their pixdims and q/sforms, rather than public-source header bytes. The generator
writes these masked native inputs with nibabel: the affines are unchanged but
q/sform codes and tiny pixdim rounding can differ from the public files.
`verify_native_closure.py` independently verifies public-image-times-mask data
bits, every native-input header byte, every compact crop sample and all hashes.
The portable scenario reads these exact native geometries on JVM and JS.

The composed-point/absolute-field bound is 2e-4 mm. The initial FLIRT-only gate
exposed float32 forward-differencing in FSL's `raw_affine_transform`: coefficients
are rounded to float, then the y coordinate advances through repeated additions
from y=0. An independent float32 replay reproduces the discrepancy to about
1.6e-5 mm. The final FLIRT-only bound is derived from the native input matrix,
geometry and frozen indices before inspecting output residuals. It includes
the coefficient-cast bound and (ymax + 4) half-ulps per input axis, mapped through
the absolute source affine, plus the 2e-4 mm base allowance. It must remain below
the fixed 0.001 mm admission cap. The chain gates retain their tighter limit.
Real-image bounds use full-source maximum adjacent differences, inverse-affine
sensitivity and float32 arithmetic; the FLIRT image bound uses its own point
bound consistently. No bound is chosen from an observed scalar residual.
Two tighter, independent guards preserve strictness: the Scala affine must match
the independently generated exact float64 FSL mathematics at all 315 queries to
1e-9 mm, and the native FLIRT coordinate ramps must match the predicted float32
trace to 5e-5 mm. The larger native-resampler allowance therefore does not admit
a corresponding error in Scala's affine interpretation.

The first rejected FNIRT invocation (default masking schedule length mismatch)
and the first Scala compile/type and numerical-gate failures remain in the
evidence records. Final results, exact source identity, review scope and raw-log
hashes are recorded in `qualification.json`.

The final scientific source is
`1ed32a0fa36a8f47e55c46966da51f41bf924c8d`. `transformJVM/test` passed
177/177 tests and `transformJS/test` passed 147/147, with clean `Pass` for this
scenario on each platform. All-module compilation passed without warnings on
both platforms at `5e98559eea099483638be7aebc43437f01de5663`; all production
and build bytes are unchanged through the final scientific source, and the
final test runs compiled the two added guards. The independent closure audit
passed 31 fixture, 32 full-native and 9 public-source hashes, 16,408,499 masked
data values, 16,504 compact crop values and 1,056 native-header bytes.

Fray card 118 retains the independent read-only review and its earlier
objections. Those objections led to the analytic resampler bound and the two
strict guards. A fresh verdict for the final source and receipt candidate is
pending at receipt creation; no approval is inferred from an older revision.
Review does not constitute a separate rerun of native tools or sbt.

Reproduce from separately acquired, hash-matching source files:

```sh
uv run --python 3.12 --with nibabel==5.4.2 --with numpy==2.5.3 \
  python tools/transform/generate_fsl_real_chain_oracle.py \
  --source-dir /path/to/identified-inputs --fsl-dir /path/to/pinned-fsl \
  --work-dir /new/native-evidence --output /new/compact-fixture
uv run --python 3.12 --with nibabel==5.4.2 --with numpy==2.5.3 \
  python docs/verification/stp-fsl-real-20260930/verify_native_closure.py \
  --native-dir /new/native-evidence --fixture-dir /new/compact-fixture \
  --source-dir /path/to/identified-inputs --output /new/closure.json
sbt transformJVM/test
sbt transformJS/test
sbt scalafimCompileAll
```

The FSL course candidate was rejected during the provenance audit because its
[source terms](https://pages.fmrib.ox.ac.uk/fslcourse/website/) limit the data to
education, not research. The owned downloader was stopped; no course data was
processed or committed. Actual source/annex identities and source URLs are in
the fixture's `source-provenance.json`; permissions are supported by the pinned
subject `dataset_description.json` and template `template-LICENSE`. The FSL
arithmetic explanation follows the [official newimage implementation](https://git.fmrib.ox.ac.uk/fsl/newimage/-/blob/master/newimagefns.cc).
