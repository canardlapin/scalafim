# Independent-process estimate publication controls, 2026-09-30

`IndependentProcessPublicationSuite` starts two owned JVMs directly with the
actual test-classloader URL closure, after verifying the probe resource resolves from that closure. It invokes the JVM at `java.home/bin/java`. A file barrier is only a test
synchronization point: each child seals its own two-cell Core-NIfTI unit before
both call the real `LocalEstimateStore.discover(..., None)` pointer CAS.

The compatible case uses two distinct immutable unit IDs with one identical
catalog. Exactly one initial pointer CAS succeeds and the other returns an
explicit `EstimateError.Conflict`. A separate observer JVM reads the current
pointer. A separate merger JVM reads that pointer digest, constructs a new
collection containing both pre-existing pins, and calls `discover` with the
observed digest. The observer receives the winning label and verifies that the pointer has exactly that one pinned unit; it validates the digest through `LocalEstimateStore.current` and reopens the pinned manifest. A final fresh JVM checks both unit revisions, catalog, values, validity, and the verified current pointer digest. On a passing run, suite receipt lines print bounded (64 KiB per child) captured output, PID, exit, runtime path, and classpath SHA-256. The focused JVM receipt on 2026-09-30 passed both independent-process cases and the four `LocalEstimateCollectionsSuite` controls.

The conflict case gives both children the same unit ID but different immutable
revisions and values. It also requires one success and one explicit conflict.
The final observer can reopen exactly one selected revision; the test does not
invoke an automatic merge or silently accept the loser.

The harness registers every successfully created child before starting another. Its finally blocks attempt to destroy, wait for, and join every registered owned child even when an earlier cleanup fails, then rethrow the first failure with later failures suppressed. Start failures destroy their own just-created process and retain a failed termination wait as suppressed evidence. It retains no production lifecycle changes. They establish no
power-loss or per-flush/fsync/rename fault matrix; that remains a separate
Core conformance requirement.
