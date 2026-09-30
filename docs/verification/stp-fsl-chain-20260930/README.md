# FSL scenario native composition, 2026-09-30

This bounded P7.07 follow-up starts from local commit
`7989608d32ed8283ed11f5ff3df54c7c7761a23e`. Final qualification uses
`9c656bc79254be9142a8b605d0d4e404b410bd1b`, adopting the concurrent main
integration and the hash-bound ANTs receipt fix. It strengthens the existing
synthetic functional/highres/standard scenario. It does not qualify a new
registration fit or satisfy P7.07's real-data acceptance requirement.

The exact functional NIfTI header and FLIRT matrix are frozen alongside the
historical FNIRT coefficient inputs. Sixteen offline native commands in image
`sha256:3ffbceee2ab631d765c6e2d1d90eedc9f31a33e8c555ec08e85ef6d65c66c1e2`
produce FLIRT coordinate/ramp outputs, a `convertwarp --premat --absout` field,
and coordinate/ramp outputs from both forms of `applywarp`. The native routes
share the pinned FSL implementation; they are independent references for the
Scala candidate, not independent native algorithms. Image identity, commands,
exit status, generator identity and input/output SHA256s are recorded in
`fsl_chain_native/manifest.json` and `commands.json` under the transform oracle
resources. All original input hashes remain unchanged after native execution.

The two native composition routes differ by at most 8.11e-6 mm on coordinates
and 1.53e-5 on the scalar ramp over 1,188 supported standard-grid queries.
Scala acceptance also requires every native highres and standard-grid query to
have support. It retains the existing mathematical registration, convention
mutation failures, materialization coverage, numerical inversion refusals,
Jacobian law and historical native controls. Coordinate gates remain 5e-5 mm;
scalar gates combine the existing spatial-gradient budget with four float32
half-ulps. Supported non-finite values fail rather than being removed.

`rejected-generator-float32` preserves the first native references, generator,
Scala source and failed JVM receipt. The independent matrix guard found a
2.383e-7 mismatch against its unchanged 1e-9 tolerance: NumPy multiplied a
float32 zoom before promoting the mirrored scaled-voxel offset. The corrected
generator promotes stored header scalars before arithmetic and regenerates all
native outputs. Successful native commands alone did not admit the first set.

The missing-native-FLIRT/convertwarp caveat is retired after both JVM and JS
qualification. The spline-versus-finite-difference Jacobian caveat remains
explicit; the scenario still requires its declared `PassWithCaveats` policy.
This follow-up changes no production algorithms, APIs or provider pins.

The fit fixture repair addresses the integration regression reported on Fray
card 89. `ImageMapsSuite` previously constructed a fresh dataset world on each
call to `def dataset`, giving its two adapters different identities. Each test
now reuses one source for fitting and maps. Exact alignment remains required,
and an additional control rejects a different world with identical geometry
and rejects mixed-world parameter maps. Production already preserves the
supplied `DatasetShape.space`, so it needed no change. The peer-reproduced
baseline and exact original source are retained in `fit-identity-baseline.json`
and `ImageMapsSuite.before.scala`. The local focused check passed 6/6; its
historical log filename contains `reproduction` but records the repaired fixture.

Final qualification passed 947 tests: fit JVM 327, transform JVM 166, fit JS 316,
and transform JS 138. `scalafimCompileAll` passed with no warnings or errors;
the scenario manifest validator passed all 35 entries. The five native fixture
manifests have 168 matching file hashes. The complete raw log, actual exit status,
source hashes and numerical observations are retained here in
`qualification.json`, `tested-source-manifest.json` and their named receipts.

`rejected-checkout-missing-ants-log` preserves the failed fresh-checkout run:
FSL passed, but the ANTs manifest required an ignored `native.log`. Commit
`9c656bc7` adds the exact recorded file and a narrow ignore exception. The final
full suites validate that adopted fix. Both this rejected receipt and the first
rejected float32 generator receipt remain byte-for-byte intact.

P7.07 stays open for its broader real-data acceptance. FreeSurfer licensing,
upstream publication/pin adoption and remaining STP integration review are
outside this bounded local follow-up. No publication or foreign-store mutation
is included.
