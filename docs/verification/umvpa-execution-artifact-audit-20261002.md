# Adopted execution, artifact and allocation audit, 2026-10-02

Mote packet `bd-01M2BNGZCKC2G6X8ETN0EKE3YD`. Baseline
`32fad7101208bd0a922165e05123ffc5f6f8a657`; isolated UMVPA worktree.
This audit concerns the supported local profiles and declared resource
contract. It does not qualify process peak RSS or reference-workload latency.

## Whole-call measurement

`PatternWholeCallAllocationProbe` invokes the adopted
`TwoStagePatternFit.fit` with an explicitly admitted owned-dense observation
product. Its measured call begins before axis, matrix, source, response,
support, policy and binding preparation. It includes pilot fit, residual
covariance fit, final fit, prediction construction, encoding, actual
`SpatialMeasurementScatter.scatter`, result consumption and retained outputs.
The completed fit, prediction and scatter field remain strongly reachable
after the final counter read.

The 8-by-3 centered Walsh fixture has true forward coefficients
`[2, -1.5, 1]`; each residual column is orthogonal to the target and the others.
The independent scalar forward oracle and singleton spatial scatter oracle
require these coefficients to agree within `1e-6`. The probe also requires
three actual marked fit calls and 24 owned-copy cells. Its checksum is
1,500,000 and all three output objects are retained. This is an actual
finite fit/serving workflow, not a metadata-only allocation sentinel.

The counter is JVM `ThreadMXBean.getThreadAllocatedBytes` on the caller thread.
It measures cumulative allocations, including result construction; it does
not measure maximum simultaneous live bytes, other-thread allocation, native
memory, allocator overhead or RSS. Unsupported counters refuse rather than
printing a fabricated zero. Two warmups precede five checked measurements.

The accepted M3.E1 probe separately measures adopted ordinary relation and
native LORO prediction calls, including preparation and retained results;
its before/after comparison remains scoped to those exact workloads.
Neither probe is a universal peak-memory certificate.

## Resource and failure audit

Independent arithmetic scenarios charge resident source/buffer bytes,
method-owned live workspace, worker costs and retained outputs exactly at
the declared numeric threshold. BigInt overflow and final augmented
precision shape overflow refuse before reads. Strict whole-numeric admission
refuses unknown provider/application/backend costs. Owned-only admission
retains those exclusions in its receipt. The new measured workflow explicitly
checks that its unknown-cost list is nonempty and whole-numeric bytes remain
unavailable. A provider declaration is not an independent memory measurement.

Two-stage direct and copied routes preserve the scientific population,
metric, factors and covariance at explicit `1e-12` comparison tolerance.
Replay, deterministic geometry, structured workspace and operator-call
preflights occur before acquisition or poison reads. Observation products
expire on close; failed reads retain attempts and successful cells. Unsupported
concurrency refuses rather than acquiring invented parallel-safety evidence.

Execution fault scenarios cover acquire, compute, close, before-commit,
control and reduction failures, conflicting retries, omitted units and
canonical-order reduction. New scenarios inject a first pre-commit crash
followed by successful identical retries and a duplicate success: four
attempts close four resources, exactly two contributions yield reduction 18
and exactly two OOF rows. Cancellation after the first commit retains one
committed unit and the exact second missing address, closing only the acquired
resource. The local runtime explicitly refuses durable resume because it
retains no durable committed-unit checkpoint. A new invocation is a fresh
run, not continuation of that cancelled family.

## Persisted profiles and reproducibility

The JVM completed PatternArtifact v1 profile writes little-endian Float64
leaves with digest-pinned metadata and exact shapes/budgets. Separate child
JVM producer and consumer processes reopen the profile without constructing
or invoking a fitter. Restored forward values, exact `Long.MaxValue` seed,
axes, coverage and experimental interpretation are checked. Other scenarios
reject corrupt payloads/metadata and a foreign training axis before reuse.

Prediction round trips retain categorical class order/priors and continuous
target geometry. The v1 profile does not persist an external noise covariance
or Gaussian target covariance; prediction consumers must explicitly supply
them again. Numeric prediction comparisons use declared tolerances. Serialized
binary64 leaf preservation, including signed zero, is a byte-level property;
it does not imply bitwise refitting across JVM/JS or machines. Deterministic
seed assignment and canonical reducer ordering are separately tested laws.

Scala.js supports the shared profile metadata contract; local verified-object
filesystem reopening and durable optimizer checkpoint resume are unsupported
and must refuse explicitly. No fitter resume, automatic upload or new runtime
is introduced by this audit.

## Gate evidence

Evidence root: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`.
`gate132-confirmation-execution-owning.log` passed 549 JVM and 535 Scala.js
tests, plus the complete allocation probe, with no Scala compiler warnings. It used the
local upstream rotation override and included three unfinished rotation
tests; it is integration evidence, not published-pin closure.
Log SHA-256:
`6e921f6d9055f440461f119be09abdcc7bbbcde5fa0967ab595cbd52560c0abe`.

Published-pin gate133 passed 546 JVM and 532 Scala.js tests (core 351 on each,
fit 45 on each, dataset 108 on each, spatial 20 on each, artifacts 12/4,
estimates-IO 10/4). `scalafimCompileAll` passed warning-clean on both platforms.
The rotation draft was held outside the gate and restored afterward; committed
provider pins did not change. Raw log SHA-256:
`3da60ea39def5defafc23443973a94d9f2fd6898b9e8915628e4d5ae3cea46ac`.
The 186-entry `gate133-pinned-sources.json` manifest has SHA-256
`3b4b1937f1bf6701c60627b75cdb4add1bbcf9a5743a0d7988ae648d5a287c69`.

Five pinned whole-call measurements allocated 2,328,512, 2,328,512,
2,328,512, 2,288,456 and 2,283,752 caller-thread bytes. The median is
2,328,512 bytes; the later JIT-dependent reductions are not a claimed speed
or memory improvement. At the median, preparation/fit/serving/scatter/
consumption phases allocate 81,408 / 2,224,400 / 18,144 / 3,568 / 864 bytes;
the remaining 128 bytes are outer call/result bookkeeping. Every measured
run passed both independent oracles and retained all output objects.

The subsequently added explicit profile refusal regression passed the full
archive suites in gate134: 13 JVM and 5 Scala.js tests. Raw log SHA-256:
`6a63233d238861eb9e5d4bc255e75744abd202da92a4824de771840627970386`.
Its source manifest SHA-256 is
`4512599e24356906910ec55b2da07753bb8d2f7e394ceb3e2c5dd9869456dd48`.
The JVM archive child processes emit the existing Java 25 warning about
Scala 3.7.4's `sun.misc.Unsafe` lazy-value implementation. There are no Scala
compiler warnings; this runtime/toolchain warning is retained in the raw logs.

After the final explicit scatter-denominator tolerance edit, gate135 passed
five further whole-call runs with the same checksum and retained outputs,
each allocating 2,328,512 caller-thread bytes. Raw log SHA-256:
`ed45ce4810e4cc1291a09808ea426c31694e04dcb3d1a21ff7969ed543bd8144`.
Final source hashes are bound in the Mote completion record; no scientific
inference or total-process memory claim is transferred from these measurements.
