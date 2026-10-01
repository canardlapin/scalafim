# PHRF-21 bounded tangent-direction repair — 2026-10-01

## Result and scope

The decoder now preserves raw free-Newton stationarity, projects outward occupied-bound components before common scaling, and refuses nonstationary proposals that cannot move in finite representable chart coordinates. `DecodeStatus.Stalled` is appended to the public enum, preserving all seven existing ordinals; it has no fictional budget-exit reason. Coordinates, energy, amplitudes and terminal curvature remain those of the last verified state.

Source commits: `0eb4625b11e5cfb152aa7c287ef8669c0ffbf0a3` (repair plus controls) and `8840dbfcebb53ccf60396b8244b75235eb589bd4` (test-only overflow-fixture correction), based on the independently accepted design `cd60ca6bce73b96b6d3d685ed3abfe2a548cf02d`. Local branch: `work/phrf-tangent-repair-20261001` in `/private/tmp/scalafim-lwu-repair-design-20260930/work`. Shared main and remote remain untouched.

Child `bd-01M3VSY6BYK65WZVWT0D422VRW` owns only the decoder, its new focused shared suite and bounded verification evidence. Objective arithmetic, priors, recovery, SPD/free-mask rules, chart bounds, tolerances, work caps, admission and boundary-inference policy are unchanged. No paired energy comparator, fresh qualification stream or 100k/campaign workload was introduced. PHRF-21 remains open.

## Implementation contract

The previous decoder replaced a nonstationary direction with Stationary when common clipping made its displacement tiny. Coupled curvature can point one component outward from an occupied bound even with an inward gradient, making the common scale zero. The reviewed correction removes outward occupied-bound components using the existing `1e-12` convention. Under the existing gradient-defined free mask and SPD free Hessian, this preserves descent; within that convention it is conservative feasibility handling, not a claim that the coordinate lies exactly on the mathematical boundary.

The `1e-9` stationarity criterion is still checked on the raw free correction. Every finite representable clipped step remains eligible, including one smaller than the criterion. Nonfinite directions/scales/trials, nonpositive scales and numerically unchanged trial coordinates terminate as Stalled before callbacks or counter increments. Signed zeros compare numerically equal. An actually exhausted attempt cap keeps its existing precedence; a hypothetical next proposal is not used to relabel it. Stalled classification precedes ambiguity, boundary, weak and accepted classification. Existing curvature and budget refusals retain their roles.

The new public case requires external exhaustive matches to handle Stalled. In-repository consumers treat it as refusal and preserve available terminal evidence; both-platform compilation/tests cover these consumers. This is a local source/API addition, not a compatibility or release claim.

## Independent controls and retained failures

The exact convex control `x²+xy+y²-x/2-2y`, box `[0,1] × [0,4]`, starts at the unique best bank corner `(0,0)`. Its Newton direction `(-1/3,7/6)` previously clipped to zero. Tangent `(0,7/6)` decreases energy to `-35/36`; constrained optimum `(0,1)` has energy `-1` and gradient `(1/2,0)`. Mirrored upper-bound and independent third-coordinate versions exercise the same contract. A true KKT bank point requires no candidate.

Precision controls check a representable clipped step smaller than `1e-9`, a raw correction exceeding that criterion whose coordinate rounds back to itself, halving to an unchanged coordinate versus actual cap exhaustion, and a finite jet whose tiny positive curvature makes the Newton solve overflow. They assert refusal/status, counters and coherent returned state. The overflow fixture initially selected a genuine boundary optimum because its energy offset did not hide the bank-energy difference; increasing only that synthetic fixture's constant from `1e15` to `1e308` makes the earliest lower node win tied rounded bank energies. Production was unchanged by this fixture correction.

Before production editing, six of seven controls failed and the true-KKT control passed. The exact seven-control source snapshot and failure receipt are archived. Full fit JVM tests subsequently passed 516/516 before the optional eighth control was added; the updated focused suite passed 8/8. Failed sandbox boot-lock, shared dependency classloader-load, initial overflow-fixture and later timeout/filter attempts remain separately named. They are not converted to passing receipts.

## Verification receipts

| Check | JVM | JS | Receipt |
| --- | --- | --- | --- |
| Full fit | 516/516 before the optional eighth control | 463/463, including all eight controls | `repaired-jvm-v1.log`; `tangent-js-controls-v1.log` |
| Updated focused controls | 8/8 | Included in full fit | `tangent-jvm-controls-v3.log` |
| HRF | 252/252 | 252/252 | JVM controls / JS controls receipts |
| HRF laws | 82/82 | 82/82 | JVM controls / JS controls receipts |
| Selected first-level laws | 23/25 initially; two timed-out suites retried serially, 9/9 | 25/25 | `tangent-jvm-timeout-retry-v1.log`; JS controls receipt |
| fMRI workflow | 23/23 | 19/19 | `tangent-jvm-dev-v1.log`; `tangent-workflow-js-v1.log` |
| Existing complete accuracy tests | 2/2 | 2/2 | JVM DEV receipt; `tangent-js-accuracy-link-v2.log` |
| Historical LWU diagnostic replay | 200 records, exit 0 | 200 records, exit 0 | JVM DEV receipt; `tangent-js-dev-v1.log` |

The JVM controls batch exited 1 because of the two timeouts. The JS controls batch exited 1 after the listed successful suites because its subsequent accuracy-filter invocation was unsupported. Their individual passing stages are retained without calling either whole batch a success. The serial JVM retry, JVM workflow/accuracy/replay, JS workflow, final JS accuracy/link and JS Node replay each exited 0. The final JS accuracy/link receipt retains its pre-task dependency-load failure and successful retry separately. Required affected JVM and JS behavior checks passed; no aggregate whole-repository test or lint claim is made.

The first-level parallel JVM batch passed 23/25: only unavailable-audit and LWU diagnostic tests timed out at the default 30 seconds (observed 53.97 and 41.99 seconds). No assertion failure was reported. The serial retry passed all nine tests in those two suites, including both formerly timed-out tests, with the same assertions and time limits; this covers the original 25 unique selected tests together with the 23 initial passes. The shared sbt runner serializes resource use and retains bounded retries for classloader failures before task execution. APFS headroom was measured at 98 GiB before further builds; no storage cleanup occurred.

Scala.js MUnit 1.2.1 does not support the JUnit `--tests` invocation used in the failed accuracy command. The archived read-only `TangentAccuracyOnlySuite.scala` inherits the existing suite and selects exactly its two complete accuracy tests, asserting their names/count. It changes no fixture, seed, assertion, threshold or timeout and excludes the throughput test. An ephemeral Test/unmanagedSources setting supplies this execution selector; library source remains at 8840dbfc. Its separate hash and exact build command bind the auxiliary runtime code. No throughput test or large performance job was launched.

## Historical replay and evidence limits

JVM replay contains all 200 original LWU historical DEV records: all historical noncryptographic fingerprints agree, all 28 available raw/whitened literal response pairs agree, and all 172 Accepted recorded jets pass independent exact-rational full-SPD and existing free-correction checks. Those accepted records expose only a 64-bit rotate/XOR fingerprint, so fingerprint equality is not a word-for-word proof of all accepted response inputs. The audit reports literal comparison coverage separately.

Only SNR .5 voxel 50 changes on JVM. Its coordinates move from `(7.166666666666667,.6029538486747399,0)` to `(6.9578336798255735,.5116978145897664,0)`. SSE decreases by `5.72093096241224`; work changes from `[157,1,0,0,0,0,0]` to `[157,6,0,5,0,4,0]`. Boundary refusal remains. Its saved terminal rho derivative is now positive (`60.9889921293352`) and exact recorded-jet free correction is about `1.15325e-10`, below the unchanged criterion. This checks constrained convergence for the represented objective, not boundary inference or original-family compression accuracy.

JVM LWU admission remains 91/100 and 81/100. Historical Gaussian accuracy tests pass their existing admission/peak/FWHM/amplitude assertions (191/200 and 195/200 admissions at SNR 1/.5); SNR .25 remains explicitly ungated. These are unchanged DEV fixtures, not fresh qualification evidence. The prior separately archived fresh Gaussian qualification is not replayed or superseded.

JS replay likewise contains 200 unchanged historical fingerprints. All 29 available literal response pairs match and all 171 Accepted recorded jets pass the same exact-rational checks. Only SNR .5 voxel 50 changes: coordinates `(6.957833679825566,.5116978145897916,0)`, energy `2723.6537939626587` versus `2729.3747249250705`, and the same work transition as JVM. It remains Boundary. JS LWU admission remains 89/100 and 82/100. Existing JS Gaussian gates pass with 192/200 and 195/200 admissions at SNR 1/.5; SNR .25 is reported at 174/200 and remains ungated.

All four LWU 95/100 gates and the parent milestone remain unmet. No acceptance improvement or fresh qualification is inferred from fixing the false stop. The replay analyzer reports each platform separately and retains the exact before/after records and receipt hashes.

The replay analyzer checks schema lengths, finite terminal fields, returned/terminal amplitude agreement, energy/Hessian coherence, actual work bounds, refusal-input presence/600-word lengths, and independent Accepted recorded-jet SPD/free-correction algebra. These are exported-jet and coherence controls, not independent objective/Hessian derivative qualification or proof of global optima. It compares each platform to its own historical inputs; it does not claim JVM/JS response-word equivalence.

## Reproduction, review and next gate

Artifacts live in [phrf-tangent-repair-20261001](phrf-tangent-repair-20261001/). `source-manifest.json` pins 1,829 tracked module/build files; `source-audit.json` binds the two changed module paths, source patch, decoder and current test hashes. Auxiliary execution-selector and analysis code is archived separately. Exact commands, working directories, actual exits and all raw output—including pre-existing multiple-main discovery warnings—remain in log metadata. No new source compiler warnings were observed in the checked compilation output; this is not a whole-repository lint claim.

`payload-sha256.json` binds this report and all archived payloads. `review.txt` records the independent verdict against the exact report and index hashes; it is outside the index to avoid a cyclic dependency. The JS runtime receipt separately binds the linked diagnostic main hash and inherited selector hash to the successful link/run commands. Evidence files are committed separately from the frozen module source. Generic numerical primitives still belong in Gale; this repair adds none. Next numerical work is the separately reviewed paired-change/error-bound contract, including genuine increase controls and independent Hessian checks. Boundary admission still requires model-specific constrained uncertainty/calibration. Neither limitation is solved by this direction correction.
