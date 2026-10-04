# sbt-warm fixes — 2026-10-04

Branch `fix/sbt-warm-20261004`, based on main `2223f13a`. Scope:
`tools/build/sbt-warm`, new `tools/build/test_sbt_warm.py`, one AGENTS.md
paragraph (new `--gc` flag and option rejection), and this receipt.

## Defects fixed

| # | Defect | Fix |
|---|--------|-----|
| 1 | A pin change `rmtree`'d the base of a live server; `--shutdown` called `prepare_base` first. | `prepare_base` calls `stop_server` (graceful `sbt --client shutdown`, then waits up to `SBT_WARM_STOP_SECONDS`) and raises instead of deleting if the server is still alive. Liveness is a real connect to the unix socket named by `active.json` or under `base/server/*/sock`, so a stale `active.json` no longer reads as "running". `--shutdown` no longer calls `prepare_base`. |
| 2 | `pin_key` hashed only lines containing `Revision`/`canardlapin/`/`.build`, missing split-line pins such as `resample4sRevision` and reacting to comments. | Key = every 40-hex SHA in `build.sbt` (order of appearance), the sorted set of `canardlapin/<repo>` names, and `project/build.properties` + `project/plugins.sbt`. |
| 3 | A failed first run never snapshotted the template. | After the command loop (server idle), snapshot whenever no template exists for the key, `base/staging` exists, and the server is alive (the build loaded), regardless of the exit code. |
| 4 | `--help` and unknown flags were passed to sbt after a destructive `prepare_base`. | `-h`/`--help` prints the docstring and exits 0 before any filesystem or git access. Unknown options, `--apply` without `--gc`, combined modes, and modes with commands exit 2. Arguments starting with `-` go after `--`. |
| 5 | Non-TTY clients deadlocked in `NetworkTerminal.waitForPending`/`isColorEnabled`. | When stdin is not a TTY, the client runs with `TERM=dumb` and `stdin=DEVNULL`. |
| 6 | `clone_tree` used `check=False` and renamed a failed copy into place. | Each copy attempt is checked. On darwin it tries `cp -c -R` (clonefile), then falls back to `cp -p -R`. If every attempt fails, the `.partial` directory is removed and `RuntimeError` is raised. Clones drop `server/` and `.worktree`. |
| 7 | Bases were never garbage-collected. | `prepare_base` records the worktree path in `base/.worktree`. `--gc` (a dry run) and `--gc --apply` remove a base only if all of the following hold: it has a `.worktree` record, the record's hash matches the base name, the recorded worktree no longer exists, and no socket in the base accepts connections. Legacy bases without a record are always kept. |

## Evidence

Unit tests (stdlib `unittest`, subprocess and liveness mocked; any unmocked
subprocess call fails the test):

```
$ python3 -m unittest tools/build/test_sbt_warm.py
Ran 25 tests in 0.066s
OK
```

The tests cover the split-line pin bump, comment insensitivity, repository and
plugin changes, `prepare_base` refusing while the server stays alive (and
discarding once it stops), `--help`/`-h` not calling `prepare_base` with a
pending pin change, unknown option rejection with no cache created, `--shutdown`
leaving the base byte-for-byte untouched, clone failure cleanup, the clonefile
fallback, a real copy excluding `server/` and `.worktree`, `--gc` dry run vs.
apply (orphan removed; live-server, live-worktree, legacy and mismatched bases
kept), the template snapshot after a failed second command, no snapshot when
the build never loaded, TTY vs. non-TTY client I/O, a stale `active.json`, and
a real listening socket.

Smoke run on the throwaway worktree for this branch. Memory pressure showed 54%
free before the run. `TERM` was unset and stdin came from `/dev/null`:

- `sbt-warm modelJVM/compile`: cold base (the new key has no template). Exit 0
  in 125.0 s, no deadlock. It logged `saving pin template 38b0bcfcf3b0a984`,
  and the template contains no `server/` or `.worktree`.
- Repeat `modelJVM/compile`: exit 0 in 3.8 s.
- `--status`: `server running`. The socket probe did not disturb the server.
- `--shutdown`: exit 0, then `--status` showed `server not running`. The base
  was intact and contained `.worktree`.
- `--gc` (dry run on the real cache): all 106 existing bases were kept as
  `(no worktree record)`.

## Operational note

The new `pin_key` produces different keys from the old one. The first
`sbt-warm` run in each existing worktree after this lands will therefore stop
that worktree's server gracefully and reseed its base. Seeding uses a template
for the new key if one exists (one now does for the current pins); otherwise
the dependencies are rebuilt cold once. Old templates under
`~/.cache/scalafim-sbt/templates/` are not garbage-collected.
