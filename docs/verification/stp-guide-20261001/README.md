# P8.03 transform guide acceptance

The guide now executes all six conversion directions between FLIRT, ITK text
and LTA, preserving the decoded pullback at every frozen fixture point within
1e-9 mm. The source-semantics table is checked separately at 1e-5 mm. Missing
FSL decode geometry and LTA encode geometry remain typed refusals. These are
affine conversion consistency checks, not native FreeSurfer qualification.

`GuideExamplesSuite` covers registration reading, FSL/FNIRT geometry,
conversion, unavailable/estimated forward maps, linked cursors and warp algebra.
The separate shared `SurfaceVolumeCursorSuite` executes the synthetic MNI peak
to subject voxel and framed surface example on both platforms.

The guide links the accepted bounded real demo1, FSL and surface workflows.
The initial review caught a link to the older synthetic FSL scenario; it was
replaced by the real qualification receipt. Fixed query windows, FSL float32
accumulation and strict independent guards, descriptive real surface route
differences, analytic commutativity bounds and registration/domain limits are
explicit. The pinned inverse solver's limitation is stated for the tested
FNIRT fixture, rather than claiming that every large affine must fail.

Baseline: `debdf34b821e6bf34103dc596dc815f55509b541`.
Test code: `07a3059cf8591dc6d5c549e750070c39bdbdad18`.
Final guide source: `cd0fb116db209d09dfa862b2630162b0eb2bf659`.
Only guide prose changed after the first code commit. All production, build,
fixture and test bytes stayed unchanged through subsequent checks and evidence
packaging. The ordinary reframe4s pin is
`9a4508351d74567147b8ea3221d82db89e5892b0`; no local provider override was used.

Successful local commands, run in separate serialized sbt processes:

```sh
sbt transformJVM/test surfaceViewJVM/test
sbt transformJS/test surfaceViewJS/test
sbt scalafimCompileAll
```

Transform: 177 JVM and 147 Scala.js tests. Surface-view: 68 on each platform.
All three commands exited zero, with no compiler warnings/errors. Raw logs,
actual exit metadata and hashes are retained beside `qualification.json`.
The JVM run began before the documentation correction; its executed test code
is identical to the final source. The final initialization wording is also a
prose-only correction after the checks, supported by reading the exact pinned
provider and the passing fixture-specific refusal suite.

`header-audit.json` independently checks the two fixture NIfTI headers with
nibabel 5.4.2 and NumPy 2.5.3: both forms are active, with maximum matrix
differences of 1.84e-8 and 4.36e-8 mm. `links-and-ci.json` binds all six relative
guide links and confirms the existing pull-request workflow invokes the guide
suites through the bounded repository test script. This is CI configuration
evidence; no hosted CI run is claimed.

Fray card 140 carries the independent source/evidence review; its initial
objection is preserved and the correction is recorded on card 142. Final
acceptance requires a verdict for the evidence-bearing candidate. The subsequent
s2 verdict at Fray sequence 981 approved exact candidate
`a2ec9fea1023b470d3d6c4362bd42a7f02831e84`. It was integrated byte-for-byte as
local main `dadf5973e601c92d5a93fbedbf3d40e65f5b67a7`. `review-final.json` and
`integration-receipt.json` preserve that approval and the checks proving
unrelated staged, working and untracked content stayed unchanged. Review is
source/evidence assessment; it does not claim another execution of the checks.
These subsequent administrative records do not change the guide or test bytes. Mote
`bd-01M39Q5DR9N3XJDXQV59MQ3XNG` remains the authority for closure and ownership.

No native tool was rerun, no provider pin or production code was changed,
and no ticket, publication or release was created. The FreeSurfer license and
upstream native inverse/provider admission gates remain separate.
