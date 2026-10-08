# PR21 protected checks: exact d8 head

Run [37535757067](https://github.com/canardlapin/scalafim/actions/runs/37535757067) at `d8dd45c8f4a01d39c26d57e633798d52ac42ca12` completed with **failure**.

- Focused JVM/JS gates passed: 1,929 JVM and 1,869 JS tests, zero failures/errors.
- Scientific coverage passed: every configured module floor passed; 1,842 test passes.
- Full repository compile succeeded. Test batch 11/16, `phrf-comparison-jvm`, failed while Scala 3.7.4 compiled 52 JVM test sources with `java.lang.StackOverflowError`; that batch executed no tests. The ten earlier batches passed 4,284 JVM and 4,001 JS tests with 19 declared skips and no test failures/errors. Later batches were not reached.

The failure is a compiler stack overflow; these logs do not identify the triggering source expression or prove an effective stack size. Published `.jvmopts` contains `-Xmx6g` only, and the full-test script/workflow do not explicitly configure `-Xss`.

LWU accuracy assertions passed on both platforms. Admission remained below the reported 95% target: JVM 93%/82% and JS 93%/81% for SNR1/.5. This is the existing rho-weak caveat. Basis training14×9×7, decode bank13×9×7, and oracle26×11×9 are different grids in the same published fixture. The printed3/3 budget line is stale reporting; exact source configures Newton6/jets8/exact2. Source and raw logs are preserved unchanged.

Run `python3 verify.py` to verify the archive and each retained input against `receipt.json`. No merge or publication was performed by this monitoring task.
