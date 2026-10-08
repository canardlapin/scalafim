# PR21 protected checks after the bridge repair

Run [37555835270](https://github.com/canardlapin/scalafim/actions/runs/37555835270) at `0ec1a0fc378291b8c405d25ce174dac2edaa144d` finished with **all three required checks successful**. The retained PR snapshot independently confirms that exact head before root's merge action.

| Protected check | Actual passing executions | Declared skips |
| --- | ---: | ---: |
| Full repository | 5,120 JVM + 4,577 JS = 9,697 | 22 JVM + 2 JS |
| Focused scientific gates | 1,929 JVM + 1,869 JS = 3,798 | 0 |
| Scientific coverage | 1,842 JVM | 0 |

All runs report zero failed tests and zero errors. Counts across jobs overlap and are separate executions, not unique tests. Coverage reports every configured module floor passed.

The full run reached all 16 bounded batches. Native comparative controls ran first and passed: JVM 476 total / 471 passed / five optional skips; JS 243 passed. The previous compiler stack failure did not recur. Linux registration rollback, retained group ownership and strict child/watchdog controls passed in the repaired suite.

`inspect_inventory` reports 95 supported alias targets, including two JavaFX tests excluded for a display; four extra example targets produce 97 unique explicit headless test commands across 16 batches (95−2+4). The alias count and actual command count describe different sets. The checker, exact published build/script and inventory output are retained.

The full court's 24 declared skips are five optional comparative dependency/heavy controls, 12 external surface asset cases, five external atlas asset cases and two opt-in Scala.js timing cases. They are retained by name; this CI run does not qualify those omitted external cases. Prior native asset qualification is separate evidence.

Actual runtime logs identify Scala 3.7.4, sbt 1.11.7, Node 24.21.0 and Eclipse Adoptium Java 17.0.20.1. The setup cache directory 17.0.20-1 is not treated as the actual Java patch fingerprint. Published `.jvmopts` includes `-Xss4m`; this run does not independently measure effective ThreadStackSize.

LWU accuracy assertions passed. Admission JVM 93%/82% and JS 93%/81% at SNR 1/.5 remains the original reported rho-weak gap below 95%; successful checks do not establish clean 95% admission. Training 14×9×7, decode 13×9×7 and oracle 26×11×9 are distinct grids of the same frozen fixture. The printed 3 Newton / 3 jets diagnostic is stale reporting; actual source uses Newton 6 / jets 8 / exact 2. Raw source/logs remain unchanged.

Run `python3 verify.py` to verify the archive and retained inputs. Root owns the final preservation check and protected merge; this monitoring task performed no source edits or publication action.
