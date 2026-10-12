# Independent FGLS simulator/control qualification — 2026-10-12

The replacement simulator passes prospective QA. Known-coefficient,
continuous-censor GLS then passes all eleven unchanged control criteria in
all five cells, on 2,048 independent datasets per cell. All 10,240 decisions
agree across JVM and Scala.js; the largest scaled numerical difference is
6.11e-15. This qualifies the simulator and known controls only. No estimated
candidate was tuned, no confirmation stream was generated, and no new
scientific admission is made.

The prior [failed run-varying confirmation](../fgls-run-varying-20261011/README.md)
and its one-use spend marker are preserved. This packet does not replace or
reinterpret that outcome.

## Prospective protocol

Commit `116bcc3b` froze `qualify.R`, `protocol.json` and `cells.json` before
generation. R 4.3.3 uses L'Ecuyer-CMRG with inversion normals and rejection
sampling. Dataset streams and eight voxel/run substreams are explicitly
jumped; the QA and control phases have separate offsets. Every full six-word
state plus its RNG header is retained in the two compressed stream tables.
The integrity audit finds 81,920 unique noise substreams in each phase and
zero cross-phase overlap.

All five original cell designs and truths are retained, with all four study
voxels. Stationary AR(2) initialization now uses an exact Gaussian pair from
the analytic covariance, followed by fresh innovations. This is a versioned
simulator change from the earlier finite burn-in construction.

The QA family has a fixed conservative ceiling of one million scalar checks
and family alpha 0.01. It tests every innovation and stationary-noise position,
lag-1 through lag-4 normalized pairs, and matched-time voxel/run pairs across
datasets. All 185,184 checks pass. The fixed mean bound is ±0.1266324 and the
variance interval is [0.8311113, 1.1896207]. The old Park–Miller affine-seed
negative control fails: its mean is 0.9828885 and all 2,048 values are positive.
The independent control phase was generated only after QA passed.

## Control and parity evidence

`summary.json` records every interval and all 55 cell/criterion verdicts.
Both platform runs pass 11 tests: ten dense-R-oracle fixtures and one full
10,240-dataset replay. The ordinary fixtures also verify that poisoning
censored observations cannot change the retained-row fit. Full replays
check coefficients, reported variance, null t/F probabilities, signal
coverage probabilities and exact decisions against independently computed
R GLS oracles; all four voxels participate in the fitted study metrics.

The control `receipt.json` deliberately says
`control_fit_results_qualified: false`: it is the pre-fit generation receipt.
The later `summary.json` is the fit qualification. Their distinct roles are
not overwritten.

The input audit passed before and after both platform runs. The fixture
extractor reproduces the checked-in Scala fixture file byte-for-byte. The
analyzer rejects duplicate dataset slots, incomplete inventories, nonfinite
records and incorrect control roles before evaluating margins.

## Custody and replay

`qa/` and `control/` retain source-locked generation receipts, R session
details and complete stream states; `control/` also retains every dense R
oracle. Compressed records contain all 20,480 platform fits. The noise arrays
remain in `/private/tmp/fgls-cmrg-{qa,control}-20261012`; they are not added as
roughly 180 MB of generated repository payload. The frozen generator and
recorded states reproduce them, and the receipts bind their exact bytes.
Use the [replay commands](../../../tools/scenarios/fgls-cmrg/README.md) and
compare every regenerated hash before reusing this qualification evidence.

`source-lock.json` binds the study fixtures, source and analysis at the
tested base `139bbcae5cfec6733af93443c88722a2046839a9`. The original eleven
criteria are unchanged; exact inventory checks were strengthened after the
first analysis, and the same retained records passed the strengthened audit.
The runtime emitted an R locale warning (`LC_CTYPE` fell back to `C`); no
Scala compilation warnings occurred.

Bounded mote `bd-01M4Q4K1EHBP77XTSEHXR5CY4Q` is complete. Its scientific parent
`bd-01M4K6KEPYEMP9XHFT2J0WCYEE` remains open for a separately specified next
candidate/confirmation decision.

The first focused GitHub job rejected formatting of two new test files. The
required scoped scalafmt pass changed layout only. Both formatting checks
and the ten ordinary fixtures pass on JVM and Scala.js after formatting; the
full opt-in packet is skipped in that ordinary-CI run. `format-source-lock.json`
binds the formatted sources; the original `source-lock.json` still binds the
full qualification run. The updated extractor reproduces the formatted
fixture file byte-for-byte.
