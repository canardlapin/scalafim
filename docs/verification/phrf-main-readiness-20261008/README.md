# PHRF main integration review — 2026-10-08

This reviews checkpoint `8992a2bb` against local main `c59c21d8`. The checkpoint
contains eleven PHRF commits since common base `9ba2c57f`, including preparation
and gradient work, recovery/refinement diagnostics, original-observation audits,
the frozen reference bank, compact storage, and explicit reference decoding.

The three-way merge is clean. The combined tree adopts Gale `62c0aeb3` for the
published bounded optimizer and keeps main's newer multivar `edb05de0` pin,
corrected GLS implementation, and response-preparation provenance. No source
conflict resolution or scientific tolerance change was needed. The existing
reference-decode verification packet still verifies all 26 hashes in this tree.

The intended landing is an engineering checkpoint with opt-in diagnostic APIs.
Production shape decoding remains unqualified: the retained default-budget
boundary diagnostic refuses all 16 attempted cases on curvature. The earlier
255/256 fixed-shape amplitude confirmation is not decoded-shape qualification;
the 231.89 MiB retained bank/worker graph is not a peak-memory or complete-owner
admission. Original-equation certification, calibration, bounded continuous
shape work, and B0 throughput remain open. No Mote is closed by this review.

## Validation

**Ready for local main integration as an opt-in engineering checkpoint.**
All completed checks have zero failures and errors. Full compilation passes on
JVM and Scala.js with no compiler warnings; the existing multiple-main discovery
message is recorded separately.

| Suite | JVM passed | Scala.js passed | Existing skips |
| --- | ---: | ---: | --- |
| Fit | 738 | 680 | None |
| Design | 481 | 480 | None |
| Dataset | 76 | 62 | None |
| Targeted scientific laws | 19 | 19 | None |
| MVPA | 468 | 468 | 3 on each platform |
| PHRF comparison | 466 | 243 | 10 opt-in/heavy JVM tests |

The MVPA and comparison test sources are unchanged by this branch. The skips
remain explicit, and no scientific calibration or resource experiment is
claimed through a skipped test. Exact commands and logs are in `validation.json`
and the three compressed logs. No hosted CI run is claimed.
The gate covers repository-wide JVM/JS compilation, full fit/design/dataset
suites, seven affected scientific-law suites, MVPA, and the PHRF comparison
runner on both platforms. Scala.js runs are split into bounded batches.

## Preservation

The original checkpoint is committed on `work/phrf-checkpoint-20261007` and a
verified local incremental Git bundle is saved at
`/Users/bbuchsbaum/code/scala/scalafim/.git/checkpoints/phrf-8992a2bb.bundle`.
It contains checkpoint `8992a2bb` and requires published prerequisite history
at `9ef6a8ed`; the exact ref, byte count and SHA-256 are in `inputs.json`.
The bundle is independent of the temporary checkpoint worktree.

This report and the validated combined source are saved on
`review/phrf-main-ready-20261008`. This review does not move local main or publish to GitHub.
