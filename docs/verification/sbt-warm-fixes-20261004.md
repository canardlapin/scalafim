# sbt-warm fixes — 2026-10-04

Branch `fix/sbt-warm-20261004`, based on main `2223f13a`. Two commits:
`b1f89b74` (original seven defects) and a follow-up addressing the Opus review
(D1–D7 below). Scope: `tools/build/sbt-warm`, `tools/build/test_sbt_warm.py`,
one AGENTS.md paragraph, and this receipt.

## Defects fixed (b1f89b74)

| # | Defect | Fix |
|---|--------|-----|
| 1 | A pin change `rmtree`'d the base of a live server; `--shutdown` called `prepare_base` first. | Superseded by D1: a pin change never deletes a base. `--shutdown` does not call `prepare_base`. Liveness is a real connect to the unix socket named by `active.json` or under `base/server/*/sock`, so a stale `active.json` no longer reads as "running". |
| 2 | `pin_key` hashed only lines containing `Revision`/`canardlapin/`/`.build`, missing split-line pins (`resample4sRevision`) and reacting to comments. | Key = every 40-hex SHA in `build.sbt` (order of appearance), the sorted set of `canardlapin/<repo>` names, and `project/build.properties` + `project/plugins.sbt`. |
| 3 | A failed first run never snapshotted the template. | After the command loop, snapshot whenever no template exists for the key, `base/staging` exists, and the server is alive (the build loaded), regardless of exit code. |
| 4 | `--help` and unknown flags were passed to sbt after a destructive `prepare_base`. | `-h`/`--help` prints the docstring before any git or filesystem access; unknown options, `--apply` without `--gc`, combined modes, and modes with commands exit 2. Arguments starting with `-` go after `--`. |
| 5 | Non-TTY clients deadlocked in `NetworkTerminal.waitForPending`/`isColorEnabled`. | When stdin is not a TTY the client runs with `TERM=dumb` and `stdin=DEVNULL`. |
| 6 | `clone_tree` used `check=False` and renamed a failed copy into place. | Every copy attempt is checked; on darwin `cp -c -R` (clonefile) falls back to `cp -p -R`; on total failure the `.partial` directory is removed and `RuntimeError` is raised. Clones drop `server/` and `.worktree`. |
| 7 | Bases were never garbage-collected. | `prepare_base` records `base/.worktree`. `--gc` (dry run) / `--gc --apply` remove a base only if it has a record, the record hashes to the base name, the recorded worktree is gone, no socket in the base answers, and (D3) no run holds its worktree lock. |

## Review follow-up

- **D1 — no destructive pin path.** On a key mismatch `prepare_base` logs, rewrites the markers, and keeps the base; sbt staging is keyed by source URI including the revision, so its contents stay valid and a stale base is only disk that `--gc` reclaims. `prepare_base` never probes servers or calls the client, so `bases.lock` is held only for filesystem work. `stop_server` is used only by `--shutdown`; its client call has `timeout=STOP_SECONDS` (a timeout returns 124) and runs without the global lock. `--gc` does not stop servers: the worktree is gone, so the server cannot be addressed through `active.json`; its idle timeout stops it and a later `--gc` collects the base.
- **D2 — old copies of the script stay safe.** `.pin-key` is written with `legacy_pin_key()`, copied verbatim from main's `pin_key`, so the old `marker.read_text() != key` check matches and never `rmtree`s. The new key lives in `.pin-key-v2`. New templates are keyed by the v2 key; a new base seeds from `templates/<v2>` and falls back to `templates/<legacy>`. A test loads main's script from git (`2223f13a:tools/build/sbt-warm`), runs its `prepare_base` on a base touched by the new script after a split-line pin bump, and checks the base survives; it then forces a stale marker to show the old code really deletes, so the check has teeth. On the real checkout, main's script computes `5a8199175db54c39` for this worktree, which is exactly the `.pin-key` the new script wrote.
- **D3 — per-worktree lock.** `run` holds `LOCK_SH` on `HOME/locks/<base-hash>` for its whole duration; `--shutdown` and each `--gc --apply` deletion take `LOCK_EX | LOCK_NB` and refuse when it is held (`--shutdown` exits 1). Tests use real `fcntl` locks across threads: shutdown refused during a run and allowed after; two shared holders coexist while an exclusive claim fails; gc keeps a locked orphan and deletes it once released.
- **D4.** `prepare_base` returns `(base, key)`; the snapshot uses that key. A test edits `build.sbt` mid-run and checks the template is named by the prepared key.
- **D5 — liveness.** A probe failing with anything other than refused/not-found is treated as alive and logs the base path to stop or delete by hand. `--shutdown` consults `active.json` (the only thing `sbt --client` reads); if that file is missing or stale but exactly one socket in the base answers, it is repointed at that socket first, so the shutdown reaches the live server rather than starting a new one.
- **D6 — tests for surviving mutations.** Added: `staging` present in the never-loaded case; `run_client` asserted to pass `stdin=DEVNULL` and `TERM=dumb` to `subprocess.call` without a TTY; `build.properties` change and removal change the key; missing markers keep the base; `server_alive` must consult base sockets. Mutation spot-check (each applied to a copy of the script, suite run against it): dropping `base_server_alive` from `server_alive`, dropping the legacy marker write, dropping the `server_running` snapshot gate, dropping the shared lock, and dropping `TERM=dumb` each fail the suite.

## Evidence

```
$ python3 -W error::ResourceWarning -m unittest tools/build/test_sbt_warm.py
Ran 42 tests in 0.132s
OK
```

Smoke runs on this branch's throwaway worktree, `TERM` unset, stdin `/dev/null`:

- b1f89b74 (54% free): cold `modelJVM/compile` exit 0 in 125.0 s, template
  `38b0bcfcf3b0a984` saved without `server/`/`.worktree`; warm repeat 3.8 s;
  `--status` running; `--shutdown` exit 0, base intact.
- Follow-up (58% free): the base left by b1f89b74 had only a `.pin-key` holding
  the v2-style key — a marker mismatch that the old code would have deleted.
  `modelJVM/compile` exit 0 in 40.1 s (server start; compile 3 s); the base was
  kept (15 staging entries), `.pin-key` became the legacy key
  `5a8199175db54c39` (matches main's script) and `.pin-key-v2` `38b0bcfcf3b0a984`.
  `--status` running; `--shutdown` exit 0; `--status` not running.

## Legacy bases and manual cleanup

The 106 bases that existed before this change have no `.worktree` record, so
`--gc` never collects them. To reclaim one by hand: map it to its worktree
(`python3 -c 'import hashlib,sys; print(hashlib.sha256(sys.argv[1].encode()).hexdigest()[:16])' <worktree path>`
gives the base name), confirm the worktree is gone or that its
`project/target/active.json` names no live socket (`sbt-warm --status` in that
worktree), then `rm -rf ~/.cache/scalafim-sbt/bases/<name>`. Any live worktree
that runs the new script gets a record and becomes gc-eligible once removed.
Old templates under `~/.cache/scalafim-sbt/templates/` are not collected
either; legacy-keyed ones remain useful as seeds (D2).

## Rollout

No action is required after this lands. A pin change no longer deletes a base
(D1), and old copies of the script on other branches see a matching legacy
`.pin-key` (D2), so running `sbt-warm --shutdown` before the next gate is not
needed. The remaining exposure is the old script's own behaviour: an old copy
run in a worktree whose pins differ (under the legacy key) from the last run
still deletes that base, exactly as before this change. A worktree whose base predates this change is adopted in place on its
next run: its markers and `.worktree` are written and the base is kept.
