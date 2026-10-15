# Result artifact export allocation

Mote: `bd-01M43ZTQCPH834TY936D7YYGKD`.

Runwise provenance now reads each run's optional status at the selected position,
defaulting to `Estimable` without constructing a full default vector. Patterned
provenance indexes each child's disjoint voxel axis once and emits records in the
outer selection order. `VoxelFitStatus.aggregate` still determines precedence;
outer exclusions retain their existing order.

The shared suite covers all 64 pairs of status alternatives, absent run statuses,
reordered dense/runwise children, and retained versus excluded source identities.
The JVM allocation suite measures complete `StatMap` construction, including
provenance, payload and axis validation. Fit preparation and three warmups are
outside each measured operation.

At 4096 voxels and two runs without explicit statuses, the restored implementation
allocated 4,443,912 bytes for runwise export and 4,311,648 for patterned export.
Restoring the full-vector status expansion allocated 164,220,680 and 164,547,168
bytes respectively, and both allocation regressions failed. The source was
restored byte-for-byte before rerunning both platforms. Focused gates passed
9 JVM tests and 7 Scala.js tests after restoration.

Commands, allocation observations and the restored source hash are retained in
`receipt.json`; `logs.tar.gz` retains `focused.log`, `mutation.log`,
`restored.log` and the mutation runner executed for these observations. The
receipt records the archive SHA-256. Unpacked logs are also retained locally.
The integrated gate passed 660 fit JVM tests and 603 fit Scala.js tests; the full
results are in `../scalafim-chunk-20261005/receipt.json`. Related HRF, design and
ObservedFamilyAdmission consumer gates also passed on both platforms.
These measurements require supported JVM thread allocation counters and cover
these fixed fixtures; they do not establish general runtime or throughput gains.
