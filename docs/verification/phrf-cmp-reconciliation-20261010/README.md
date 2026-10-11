# PHRF-CMP reconciliation, 2026-10-10

These records reconcile five PHRF-CMP motes whose code is already on main
(base `8e85ef61`, branch `work/phrf-cmp-reconcile`). Runtimes were recreated
from the checked-in locks in scratch venvs; no repository `.venv` link was
used. No pilot or confirmatory root was used, and no sealed pilot output
exists or was read.

| Mote | Record | Outcome |
| --- | --- | --- |
| v0a `bd-01M3TA9BANBE56NJ45H5M86RHK` | [v0a-truth-generator-tx-gate.md](v0a-truth-generator-tx-gate.md) | close |
| v0b `bd-01M3TA9DAM0SX1NWAH059C88ZP` | [v0b-glmsingle-gate.md](v0b-glmsingle-gate.md) | close |
| S0 `bd-01M3VCSEYDYQSG52JT7F0ZG1B4` | [s0-source-qualification.md](s0-source-qualification.md) | close (receipt credited, spike not rerun) |
| S6 `bd-01M3VSW06P4G26MA7DEC9BZ4WH` | [s6-glmsingle-bridge.md](s6-glmsingle-bridge.md) | open: GLMsingle output-bytes pin fails deterministically on this host |
| S9 `bd-01M3VZTP1V5ZP8WSJQM59QGN38` | [s9-custody-tooling.md](s9-custody-tooling.md) | open: owner binding checklist; tracked-symlink checkout blocker |

The `logs/` directory holds compressed command output. `runtime/` holds the
recreated `pip freeze` outputs. `logs/s6-cpu-probe-suite.scala.txt` is the
temporary probe that was run and then removed from the source tree.

Not claimed anywhere in these records: Linux custody, type-D equivalence and
pilot admission.
