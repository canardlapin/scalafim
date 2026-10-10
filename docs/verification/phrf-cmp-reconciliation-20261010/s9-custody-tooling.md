# PHRF-CMP S9 reconciliation: custody tooling (owner binding pending)

Mote: `bd-01M3VZTP1V5ZP8WSJQM59QGN38`. Date: 2026-10-10. Worktree branch
`work/phrf-cmp-reconcile`, base origin/main `8e85ef61`.

This record reconciles the locked-environment and sealed-format decisions
with the delivered tooling, records a fresh run of the existing custody
suite, and gives the exact checklist the owner must complete before binding.
**S9 stays open.** Binding needs the owner's real public-key fingerprint and
the chosen beacon round and freeze. This session has neither, and it does
not invent placeholders for them.

## Delivered tooling on main

| Item | Path | SHA-256 at `8e85ef61` |
| --- | --- | --- |
| Custody package and tests | `tools/phrf-comparison/custody/` (`phrf_custody/*.py`, `tests/*.py`, fixtures) | Byte-identical to the recovery manifest (`phrf-pilot-integration-20261004/source-manifest.json`). The one exception is `tests/test_round5.py`, which recovery commit `bfa66bd5` itself repaired (the late-ack test captures the spawned PID). |
| Runbook (rev 2) | `docs/plans/phrf-pilot-custody-runbook.md` | `2be454cf6636a476f40415b871249f8ead53da2961851bef110d32d9dc6f16d0` |
| Sealed format `phrf-sealed/1` | `docs/plans/phrf-pilot-sealed-format.md` | `5a9612d8608d3557f2c9a83c03a67d297a952441e740758e36183d26980ff63b` |
| Custody lock | `tools/phrf-comparison/custody/requirements.lock` | `17b9df69cb0eee264027d880a3bae05362e341436a8f072ae458ae5dc1c8f6c3` |
| Normative seeds | `tools/phrf-comparison/generator/phrf_gen/seeds.py` | `c306e56b2e7156162defb7a3ac23efa971759544f45ac27f02906d6ef1130a53` |

All arrived in `bfa66bd5`. The original S9 commits (`77b49dc4`, `6283cd98`,
`f3d435a7`, `fad8b8e7`, `127fdd4d`) are not reachable from main because the
branch was recovered without history.

## Design S9 test obligations (runner design v2.1, section 6)

| Obligation | Test(s) |
| --- | --- |
| Root derivation test vector | `tests/test_drand_root.py::test_root_vector_and_construction`, `test_parity_with_seeds_py`, `test_secret_changes_the_root_and_order_matters` |
| Corrupted signature rejected | `test_corrupted_signature_rejected_structurally`, `test_consistent_but_forged_signature_rejected_by_bls` (BLS via py-ecc) |
| Digest stable and sensitive to one byte | `tests/test_seal.py::test_digest_sensitive_to_one_byte_and_stable` |
| Clean checkout refuses a dirty tree or a gale override | `tests/test_checkout.py::test_dirty_tree_refused`, `test_override_in_environment_refused`, `test_override_in_committed_opts_file_refused`, `test_global_sbt_config_and_home_opts_refused` |
| Denylist over root64 and the 980 stream seeds | `test_denylist_root_hit_including_low32`, `test_denylist_stream_hit_aborts`, `test_pilot_cells_match_generator` |
| Published KATs | `tests/test_kats.py` (RFC 7748 6.1, RFC 5869 A.1/A.3) and the `phrf-sealed/1` golden vectors (`tests/test_vectors.py`) |

## Fresh runs (2026-10-10)

| Run | Result |
| --- | --- |
| Custody venv recreated from `requirements.lock` (Python 3.12.13, macOS arm64), `python -m pytest -q -p no:cacheprovider tests` | **196 passed, 0 skipped**, exit 0, 108 s (`logs/s9-custody-pytest.log.gz`). BLS tests were exercised. This matches the recovery receipt's 196/0. |
| JVM custody interop inside `phrfComparisonJVM/test`, with `PHRF_REQUIRE_INTEROP=1` and `PHRF_CUSTODY_PYTHON` set to the recreated venv | The interop suites ran (mandatory, no skip): `InteropSuite` 5, `SealSuite` 11 and `SealedStoreSuite` 12 passed. The whole module had 473 passed, 2 failed (both the GLMsingle output-bytes pin, unrelated to custody; see `s6-glmsingle-bridge.md`) and 1 opt-in skip (`logs/s6-phrfcomparison-jvm.log.gz`). |

The worktree was clean after the Python run (no `custody.log.jsonl` or other
residue).

## Locked-environment reconciliation

- `requirements.lock` pins only the four top-level packages:
  cryptography 50.0.2, numpy 2.5.3, py-ecc 8.0.0, pytest 9.1.1. It has **no
  hashes**, and the transitive dependencies are unpinned. On 2026-10-10 they
  resolved to cffi 2.1.1, cytoolz 1.2.0, eth-hash 0.8.0, eth-typing 6.0.0,
  eth-utils 6.0.0, iniconfig 2.3.1, packaging 26.3, pluggy 1.6.0,
  pycparser 3.11, pydantic 2.14.0, pydantic_core 2.50.0, Pygments 2.21.0,
  toolz 1.2.0, typing-inspection 0.4.4, typing_extensions 4.16.0. The full
  freeze is in `runtime/custody-freeze-20261010.txt`.
- The S9 receipt makes acceptance of this venv as a v0 environment identity
  an **owner action** (item 4). It has not been accepted. A version-only lock
  does not fix the transitive set. A future resolution can differ from the
  one tested here.
- The stamp (`checkout.tool_hashes`) records the lock's hash, not the resolved
  set. So the owner should either (a) accept the version-only lock with this
  freeze recorded in v0, or (b) require a `--require-hashes` lock like the
  GLMsingle one before v0. This record does not choose.

## Sealed-format reconciliation

- `phrf-sealed/1` (ephemeral X25519 per blob, HKDF-SHA256, AES-256-GCM, size
  buckets, encrypted names, CLOSE record, duplicate rule, write-time
  read-back) is implemented by `phrf_custody/seal.py`. The golden vectors
  passed in the run above.
- Section 8 now carries the **sanctioned GLMsingle watchdog exception**
  approved in the 2026-10-03 S6 review. Its residual limits are stated:
  descendants that call `setsid`, and a SIGKILL of the watchdog alone.
- The JVM writer (S7) is the format's other implementation. S7 is open
  (`bd-01M3VZTKYWQQ7X0H65Y3B38HRQ`, worked elsewhere) and is not assessed here.

## New blocker found: clean checkout refuses current main

`phrf_custody.checkout.check_no_links_or_submodules` refuses any tree with a
tracked symlink (mode 120000). That rule is design 4.2 and runbook step 2.
Main tracks one: **`CLAUDE.md` (120000, blob `47dc3e3d`)**, present since the
initial baseline `7c8358a1`. A direct probe on this worktree returned
`REFUSED: tracked symlink refused` (`logs/s9-checkout-symlink-probe.log`).
Every reviewed SHA cut from main as it stands would therefore be refused at
runbook step 2. The rule was not loosened here. Resolving it is an owner or
coordinator decision (checklist item 6).

## Owner checklist for binding (must all be done by the owner or the named custodian, not by an agent)

1. **Key.** Generate an X25519 key pair **off this machine** (runbook 1.1).
   Hand over only `owner.pub`.
2. **Fingerprint.** The custodian runs `python -m phrf_custody fingerprint
   owner.pub`. The owner confirms the printed string **out-of-band** and
   records it and the date in manifest v0. That exact string goes to every
   `--expect-fingerprint`.
3. **Backups.** Keep two or more independent offline private-key backups. Before
   v0, prove one backup unseals a throwaway `phrf_custody seal` store
   (rehearsal-only command).
4. **Beacon round A.** Choose it in v0 **before v0 is published**, late
   enough that the v0 commit and its review precede
   `1595431050 + 30 * (A - 1)` UTC. v0 must carry `beacon_round`,
   `drand_chain_hash = 8990e7a9aaed2ffed73dbd7092123d6f289930540d7651336225dc172e51b2ce`,
   `secret_commitment` (printed by the session, which **starts before v0 is
   written**), and `seeds_py_sha256 = c306e56b2e7156162defb7a3ac23efa971759544f45ac27f02906d6ef1130a53`.
5. **Freeze.** Pick the reviewed runner SHA (approved by the separate runner
   reviewer; depends on S7 and the S10 freeze and rehearsal) and the full
   40-hex v0 commit.
6. **Resolve the tracked-symlink refusal** above, without weakening the
   checkout rule unless the change is a reviewed amendment.
7. **Custody environment identity.** Accept the version-only custody lock
   with the recorded freeze, or require a hash lock (see above).
8. **Custodian identity.** Confirm that `backlog-worker-20260930` (runbook
   header) is still a non-author of the runner, generator, scorer and
   aggregator.
9. **No-author process audit.** Quiesce all other agent sessions from step 1
   to hand-over. Take start, resume and end `ps`/`lsof` snapshots with their
   hashes in the log (runbook sections 4-5).
10. **Real-run flags and host.** Use `--require-online --ack-timeout 600`
    under `caffeinate -i -s`. The output directory goes inside a `.noindex`
    custodian directory. Keep FileVault on, set `ulimit -c 0`, and exclude
    the directory from Time Machine and sync.
11. **Anchors.** Record `ANCHOR secret-commit`, `ANCHOR v0` and every
    `log head:` line yourself, from the real terminal, off-machine. Confirm at
    the end that the `v0` entry shows `network_checked: true`.
12. **Self-check from the reviewed checkout.** Run the custody suite with
    py-ecc installed and record the counts in the stamp notes (runbook
    section 7).
13. **Copies.** Name two off-machine destinations for the sealed tree,
    `secret.sealed`, the anchored log and `stamp.json` (runbook 2.10).

## Scope limits (not claimed)

These are synthetic tooling checks on macOS arm64. They are **not** an
owner-key-bound run, not a real beacon round, not Linux custody, not pilot
admission, and not a substitute for the S10 rehearsal. Custody is never
inferred from shared OS file permissions (runbook section 0).

## Verdict

The tooling, the runbook and the sealed format are reconciled with main, and
the existing suite passes 196/0. **S9 remains open** until the owner checklist
is complete. Items 6 and 7 are new or still-open decisions found in this
reconciliation.
