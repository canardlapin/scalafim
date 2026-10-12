# Independent CMRG simulator and known controls

`protocol.json`, `cells.json` and `qualify.R` were committed at `116bcc3b`
before generation or fitting. They define a replacement simulator, separate
QA and control streams, fixed QA bounds and the unchanged eleven scientific
control margins. This tool has no candidate-tuning or confirmation mode.

From the repository root, use R 4.3.3 with `jsonlite` and `digest`:

```sh
Rscript tools/scenarios/fgls-cmrg/qualify.R qa /tmp/new-cmrg-qa
Rscript tools/scenarios/fgls-cmrg/qualify.R control /tmp/new-cmrg-control /tmp/new-cmrg-qa/receipt.json
python3 tools/scenarios/fgls-cmrg/verify_inputs.py /tmp/new-cmrg-qa /tmp/new-cmrg-control
```

Both output directories must be new. The control generator refuses a failed
QA receipt or different source hashes. Neither phase redraws failed data.
Each `.f64le` file stores dataset, voxel, time in that order, with time
fastest; each value is a little-endian binary64. Compare regenerated hashes
to the archived receipts before claiming an exact replay.

```sh
SCALAFIM_FGLS_CMRG_CONTROL=/tmp/new-cmrg-control \
SCALAFIM_GLS_STUDY_LOG=/tmp/new-cmrg-results \
python3 tools/build/sbt-warm \
  'firstLevelLawsJVM/testOnly *FglsCmrgSuite' \
  'firstLevelLawsJS/testOnly *FglsCmrgSuite'
python3 tools/scenarios/fgls-cmrg/verify_inputs.py /tmp/new-cmrg-qa /tmp/new-cmrg-control
Rscript tools/scenarios/fgls-cmrg/analyze.R /tmp/new-cmrg-results-jvm.jsonl \
  /tmp/new-cmrg-analysis tools/scenarios/fgls-cmrg/protocol.json /tmp/new-cmrg-results-js.jsonl
```

Use a fresh log prefix and restart an existing sbt-warm server before changing
its environment. The analyzer requires the exact five-cell inventory,
2,048 unique datasets per cell and identical cross-platform decisions.

`fixtures.py CONTROL OUTPUT.scala` deterministically extracts the first two
datasets in every cell and the dense retained-row R GLS oracle. These ten
fixtures run in ordinary JVM and Scala.js CI, including censor-poison tests.
The full 10,240-dataset qualification is opt-in.

The [qualification packet](../../../docs/verification/fgls-cmrg-controls-20261012/README.md)
retains complete stream states, generation receipts, R oracles, all fit
records and platform logs. Full noise arrays are regenerated from the frozen
source and checked against their retained byte hashes.
