# Camera and saved-scene review

Reviewed source: `fa188b9283d2864a30aac4cb7b85f701038860d2`; final correction `f87fecf7ade27054229aa55bbd24d499f519dc8a`.

Independent review found two remaining P2 defects: the old serialized `per-surface-cameras` capability was missing, and native camera-only application called the full colour/geometry validator. Both are corrected and the reviewer closed both findings on the final source. Supporting backends declare the capability, scenes infer it from nonempty overrides, and malformed native camera packets are refused before mutation. Camera-only updates retain the resource-key guard and validate camera packets without scanning layer colours.

Earlier review corrections covered CPU fallback camera selection, direct Three paired-plan composition, public legacy program refusal, shared-to-paired transitions/resize, same-key camera edits, and native malformed-camera state preservation. Native transforms and two asymmetric actual JavaFX ray hits have separate receipts. No framebuffer interpolation or timing qualification follows from those hits.

The old September writer at `7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4` and canonical revision-8 writer at `56d0cbcbfa55c7edb8651d19153b9e8c7897d88d` generated the checked-in fixtures. Neither fixture was synthesized by the new reader/writer. The September fixture carries morph fraction 0.25, medial-left override, aspect 2, exact family asset references and the historical camera capability. JVM and JS restore independently specified morphed coordinates. Current revision 9 roundtrips appearance and full family evidence; missing, duplicate, extra and mismatched variants and contradictory hybrid-v8 fields are refused.

Final six-suite evidence: `checks/final-surface-review-04.txt` (272 JVM + 272 JS shared tests, 70 + 70 raster, 63 JavaFX, 57 Three). Earlier failed attempts are retained: a test-only signed-zero string comparison, an overstrict CPU camera validator incompatible with general homogeneous CPU fixtures, and a test attempting a private case-class copy. The final tests use numeric matrix comparisons, separate finite-buffer versus rigid-native validation, and public scene capture. None of these failures was hidden by weakening a scientific admission gate.

Stock affine native sampling, retained dense-colour refinement, real cortical performance and visual/scientific product admission remain outside this recovery's passing engineering checks.
