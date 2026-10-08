# PR21 protected checks after the stack repair

Run [37545717682](https://github.com/canardlapin/scalafim/actions/runs/37545717682) at `76b2fdbbb696a52dfc1df81c54785a345c869bfe` finished with **failure**.

- Focused scientific gates: 1,929 JVM and 1,869 JS passes, zero failures/errors/skips.
- Coverage: 1,842 passes; every configured module floor passed.
- Full repository: compile succeeded. The previous compiler stack overflow did not recur; phrfComparisonJVM compiled and ran475 tests (464 passed, six failed, five skipped). The run stopped in bounded batch11/16. Across reached batches: 4,748 JVM passes/six failures/22skips and 4,001 JS passes/two skips, zero errors. Later batches were not reached.

All six failing tests are in GlmSingleBridgeSuite. Three force Mac OS X scratch/tool behavior on Linux (two unmount controls and stale-volume sweep). A mount-options test expects Darwin nobrowse/mdutil. The survivor test fails its immediate child ProcessHandle.isAlive assertion after cleanup; a zombie/race cause remains unproven. The registry-failure test expects a thrown exception/device detach. Linux instead returns a typed refusal, and inspection also reveals a rollback gap: LinuxShm.create allocates a directory and calls ScratchRegistry.register before entering its cleanup try, so an injected registration failure leaves that unregistered directory behind.

Published `.jvmopts` includes `-Xss4m`. This job log proves compilation proceeded past the previous failure; it does not independently fingerprint effective ThreadStackSize. Root owns repair and publication; this packet makes no source changes.

LWU accuracy assertions passed. Admission JVM93%/82% and JS93%/81% for SNR1/.5 remains the original reported rho-weak gap below95%; it is not clean95% admission. Source-backed training14×9×7, decode13×9×7 and oracle26×11×9 are distinct grids of the same fixture. The printed3/3 budget line is stale; actual source uses Newton6/jets8/exact2. Raw source/logs are unchanged.

Run `python3 verify.py` to verify the archive and retained inputs.
