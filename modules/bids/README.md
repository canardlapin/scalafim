# scalafim-bids

Cross-compiled JVM/Scala.js BIDS module for `scalafim`.

Package root:

```scala
import scalafim.bids.*
```

This module is the Scala 3 rewrite target for the core of `bidser`: BIDS
filename entities, typed project manifests, query semantics, BIDS URI handling,
dataframe-like TSV tables, and fMRIPrep confound selections. It deliberately
keeps R's S3 surface and mutable registries out of the core. File-system
discovery and image header readers live behind JVM-specific adapters.

Core shared APIs include:

- `BidsName`, `BidsEntities`, `BidsDatatypeSpec`, and `BidsRegistry` for typed
  BIDS filename parsing/rendering.
- `BidsManifest`, `BidsProject`, `BidsQuery`, `BidsScope`, and `MatchMode` for
  immutable project queries, scan selectors, and subject/session/task/run
  summaries.
- `DatasetDescription`, `BidsUri`, and sidecar inheritance through
  `BidsProject.metadata`.
- `BidsProject.participantsTable` for first-class access to every
  `participants.tsv` column while preserving the normalized participant id
  summary in `BidsProject.participants`.
- `BidsMetadataRecord` and `BidsRepetitionTime` for scan-anchored inherited
  metadata and TR inference from `RepetitionTime` or `VolumeTiming`.
- `BidsTable`, `BidsColumn`, `BidsTableFile`, and `BidsEvents` for generic
  BIDS TSV parsing with tab-vs-whitespace sniffing plus attached BIDS
  path/entity metadata where a table is tied to a manifest file.
- `ConfoundSets`, `ConfoundStrategy`, and `ConfoundSelector` for fMRIPrep
  alias/wildcard resolution, missing-value policy, zero-variance/rank
  diagnostics, and PCA-backed strategy reduction.
- `BidsProjectLoader.readConfounds` and `readConfoundStrategy` on the JVM for
  one-shot fMRIPrep TSV discovery, loading, selection, and context-preserving
  results.

JVM adapters currently live under `scalafim.bids.io`:

```scala
import scalafim.bids.*
import scalafim.bids.io.*

val project = BidsProjectLoader.load(java.nio.file.Path.of("/data/study"))
val participants = project.map(_.participantsTable)
val participantsAgain = project.flatMap(BidsProjectLoader.readParticipantsTable)
val anyTable = project.flatMap(BidsProjectLoader.readTable(_, BidsPath("participants.tsv")))
val confoundTables = project.flatMap(BidsProjectLoader.readConfoundTables(_))
val motion6 =
  project.flatMap(p => BidsProjectLoader.readConfounds(p, cvars = ConfoundSets.named("motion6")))
val pcaConfounds =
  project.flatMap(p => BidsProjectLoader.readConfoundStrategy(p, ConfoundStrategy.named("pcabasic80")))
val eventTables = project.flatMap(BidsProjectLoader.readEventTableFiles)
val tr = project.flatMap(_.inferRepetitionTime(subid = "01", task = "rest"))
```

Run it directly with:

```sh
sbt bidsJVM/test
sbt bidsJS/test
```
