# PHRF merge to local main — 2026-10-08

Merge parents are main `81c7e1f6` and validated candidate `9915e1ac`.
This preserves the newer corrected-GLS qualification harness/evidence and
lands the original pHRF checkpoint `8992a2bb` and its preceding work.
The merge is clean, and all 32 pre-existing untracked paths remain present.

The production fit/design/dataset code and dependency pins match the validated
candidate. The prior full integration checks (738 fit JVM / 680 fit JS, design,
dataset, MVPA and comparison consumers) remain in the readiness packet.
The final combined tree additionally passes full JVM/JS compilation and **21
scientific-law/simulator tests on each platform**. Compiler warnings are absent;
the existing multiple-main discovery message is recorded separately.

These final gates use the ordinary pull-request profile. The separate GLS
statistical campaign is opt-in and is not rerun or admitted here; Scala.js
reports its empty inactive suite as one ignored entry. The existing AR(2)
statistical failures and the pHRF curvature counterexamples remain in evidence.
No production certificate, calibration, peak-memory or throughput qualification
is claimed, and no related qualification Mote is closed.

`merge.json` binds the exact parents and gate scope; `gates.log.gz` contains
all three completed commands. The original source and main-ready candidate
remain on their preservation branches and in verified local Git bundles.

Concurrent edits appeared in 28 law-test/GLS-analysis files during validation.
The merge commit keeps the staged incoming sources and preserves those edits
unstaged for their owner. The original readiness/reference-decode packet hashes
were verified against the Git index (78 and 26 hashes), rather than formatted
working files. The 26 pHRF Scala formatting changes preserve lexical tokens;
the separate GLS qualification extension and R edit are excluded from this
merge commit.
