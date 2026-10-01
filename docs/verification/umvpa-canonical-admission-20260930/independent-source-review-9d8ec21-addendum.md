# Test-fixture source addendum: PASS

Exact clean source: `9d8ec21454774e840baf41de9dff345e7fd88418`.
Retains the bounded scientific source verdict and limits from independent-source-review-f08f3b.md and the subsequent 6f1474/c254042/eab6e02 addenda.

The complete delta from eab6e02036a073b537f9899083803615463465a5 changes only `modules/mvpa-dataset/shared/src/test/scala/scalafim/fmri/mvpa/dataset/predictive/AlderExposureReadsSuite.scala`. A stable Fixture object now owns the sample/input/response axes and explicitly dependent Observations[samples.Id, inputs.Id] and MultiResponse[samples.Id, responses.Id] members. Tests import one stable instance, preserving the common nominal sample witness rather than losing correlation through an inferred returned tuple. No casts or widened semantic spaces were introduced by the repair.

The training-only negative test now requires precisely TrainingOnlyConstraintUnsupported, strengthening the earlier broad Refused check. Both zero-callback checks remain. The failed wider-read test still requires an actual input callback and checks retained whole-population scope/budget. Constrained-assurance and foreign-reference refusals retain exact errors and zero-callback assertions. The Unknown-origins provenance-only exposure/root identity comparisons, before-callback foreign-reference rejection, and fixed-versus-joint preparation assertions remain unchanged. The provenance helper correction constructs the same intended Domain evidence with properly closed syntax.

Verdict: PASS for this test-source delta, with no scientific assertion weakening or new source blocker found. All production source and the canonical Alder pin are unchanged. The parent reports that dataset production compiled at the predecessor but test compilation failed; this addendum is not an independent build receipt. Fresh canonical JVM/JS dataset and compileAll gates remain separately delegated.

No builds/tests, repository edits, external publication or Mote review registration were performed. Only this requested report was written. Prior source-scope and provider-admission limits remain.

Changed file SHA-256: `a7ed555334ef5181bbafe4205ead4e44927ea949d94235d8e631fdfb803e1463`.
