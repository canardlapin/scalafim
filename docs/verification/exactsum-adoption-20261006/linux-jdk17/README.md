# Native Linux/JDK17 adoption gate

[Hosted run 37505457633](https://github.com/canardlapin/scalafim/actions/runs/37505457633)
passed 155 AR and 25 compiler budget and kernel basis tests with the published default
Gale 54e73f8e pin and adoption commit 1825d121. The task branch adds only a diagnostic
workflow. No source overrides or scientific policy changes were used.

Actual runtime is Ubuntu 24.04.5 x86_64, Temurin 17.0.20.1+1, sbt 1.11.7 and
Node 24.21.0. The historical JDK 17.0.20+1 request resolved a toolcache directory
named 17.0.20-1 whose binary reports 17.0.20.1+1; this is a JDK 17 compatibility
gate, not proof on the unavailable exact historical binary.

The raw log retains existing linops4s build-definition lint and workflow metadata
warnings caused by the diagnostic workflow. Production Scala compilation emitted
no warnings. A local Rosetta emulation attempt passed 155 AR and 15 budget tests and
was stopped with exit 143 when software Math.fma/BigDecimal made the remaining SVD
gate expensive near its 6 GiB container limit. Its raw logs and thread snapshot
are retained as partial evidence; it is not counted as a completed gate.

The archive contains the exact tested source, workflow, source hashes, host
fingerprint, raw native log, exit 0, run metadata and partial emulation evidence.
`receipt.json` seals the archive SHA256. The original Mac JVM/JS adoption packet
remains unchanged.

Verify the retained gate with `python3 -S verify.py`.
