# Local image4s / reframe4s / locus4s candidate closure

The preparation receipt describes the local qualification before publication.
Owner-approved [image4s PR #16](https://github.com/canardlapin/image4s/pull/16)
contains the exact tested candidate and is observed merged. The exact reframe4s
candidate is published in [draft PR #3](https://github.com/canardlapin/reframe4s/pull/3).
`publication.json` records verified heads and states; this task performed no merges.
Reframe4s review/merge and clean default-URI qualification remain pending.
Root ScalaFIM pins have not been changed.

## Candidate changes

- image4s candidate `20c9515495e43dff9d17bc1283a8ca1d56355c4a` starts at public main `c9fcab27f21bf60301260e0c084c3a20d279c5e6` and changes only its locus4s pin to `a67bc87c33b5da8a5dc2cad49919c015b59f3050`. It retains merged signed INT8 support.
- reframe4s candidate `292c9bc7e0c026d893a4101560aa9a46e0fbcf04` starts at public main `108028fb7127e71083bb46b5e4c033f831775c0e` and changes its image4s build pin and matching CI checkout pin to the image4s candidate.
- `scalafim-recommended-pins.patch` proposes those two pins and the same locus4s pin. It was saved in TMP only. The isolated ScalaFIM build retained original committed pins and selected all three local candidates explicitly with `JAVA_TOOL_OPTIONS`; this also seeds the existing cache template without pretending unpublished commits are publicly resolvable.

Portable patches and incremental Git bundles are included. Apply each patch on its stated public base; the bundles require those base commits. Upstream candidate authors use the canardlapin skill identity, while the isolated ScalaFIM checkout uses its repository-specific noreply identity.

## Observed validation

`scalafimCompileAll` passed both platforms without compiler warnings, with no scientific adapter changes. Nine provider modules and eight ScalaFIM consumer modules passed 1,708 JVM and 1,498 JS tests, totaling 3,206. Each JS batch has at most three targets and ends with an explicit server shutdown. All command exits are zero.

The consumer gates cover locus-data, image, transform, spatial, surface, motion, atlas and mvpa-spatial. The native atlas suite passed all eight oracle cases inside the full 116-test JVM atlas gate, using the two existing digest-matching TemplateFlow H5 assets.

`atlasJVM/Test/fullClasspath` and `atlasJS/Test/fullClasspath` each contain exactly one image4s core/geometry and one locus4s core/data classes directory, all from the selected candidates. The loaded composite has one image4s build and one locus4s build. Existing direct/transitive Gale source revisions remain unchanged and appear twice; this work does not qualify their unification.

The reframe4s PRD, build graph and symbol ownership checks pass. These are architecture checks, not anatomical registration or performance admission.

## Declared gaps and remaining work

- Twelve JVM surface reference cases skip because external native assets are absent. Two JS surface resource timing cases skip because their opt-in environment flags are unset. No performance claim or broader surface native-fixture admission is made. Skip counts are retained separately from passing counts.
- The locus4s update exposes the released changes where `Relation.identity`, `Relation.tabulate`, and `Relation.converse` return `Either` for allocation/overflow failures. No selected internal callsite uses the old signatures, and compilation needed no adapters. External callers using ScalaFIM's companion reexport need the explicit migration note when these pins land.
- Full upstream release/registration courts beyond the selected provider modules were not run. Upstream CI remains a separate gate.
- Provider commits are local candidates. Publication/owner approval belongs to the root coordinator. After accepted provider commits are public, substitute actual public SHAs in downstream/CI pins and repeat clean-checkout default pinned-URI qualification without local overrides.

## Evidence

`receipt.json` binds run counts and hashes. `logs-and-sources.tar.gz` retains raw logs, all 1,294 tracked source/fixture files in the verified module scopes, manifests, commands, portable patches/bundles and the offline verifier. Native H5 inputs are digest-bound by URL and remain external. Run `python3 verify.py` to verify the saved bundle without sbt.
