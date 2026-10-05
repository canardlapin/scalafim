# Development machine handoff

ScalaFIM's Mote tickets and coordination history are versioned in
`.mote/FORMAT.json` and `.mote/ops/*.json`. The immutable operations are the
source of truth. Commit new operations with the related work and fetch/pull
before continuing on another machine. Use the Mote CLI for changes; never edit,
reformat or delete operation files to change tracker state.

`.mote/local/` and `.mote/tmp/` are ignored. They contain workstation identity
and unpublished temporary state, and are not needed to reconstruct tickets.
Historical session PIDs and lease records in the operation log are historical
evidence, not proof that a worker is running on the new host.

## Start on the new machine

```sh
git clone https://github.com/canardlapin/scalafim.git
cd scalafim
mote --version
mkdir -p .mote/local .mote/tmp
mote actor set development-new-machine
mote doctor
mote --json ls --all
mote ready
```

The migration was checked with Mote 0.1.0. Install a compatible Mote CLI; its
schema version must support the committed store. Select your own stable actor
name rather than inheriting a workstation's identity. Current claims and
reservations are TTL leases; inspect them before touching claimed paths.
Git does not carry empty directories, so the `mkdir` step restores the two
ignored directories needed by Mote's layout check.

Formal candidate records can be bound to the original Git repository identity
and landing target. Revalidate or rebind them through the Mote candidate
protocol in the new clone before landing; source-preservation refs do not
change the recorded review or authorization scope.

Follow `AGENTS.md` and `README.md` for toolchain, fixture configuration and
bounded JVM/Scala.js checks. Keep repo-local authoring and GitHub API routing
under `canardlapin`, as specified in `AGENTS.md`; credentials are configured on
the new machine independently and are not part of this handoff.

## Recover retained development work

The migration preserves unintegrated work under remote branches rooted at
`migration/20261005/`. These are source-preservation branches, not acceptance
or scientific qualification. Use
[the exact ref map](verification/machine-migration-20261005/refs.json) to locate
the original branch name, SHA, worktree snapshot or stash.

After a normal clone/fetch, the branches are available as remote-tracking refs:

```sh
git fetch origin
git branch --remotes --list 'origin/migration/20261005/*'
git switch -c recovered-work origin/migration/20261005/branches/work/numerical-ownership-20261003
```

Original local branch tips are under `migration/20261005/branches/`; main is
published as `main`. Preserved dirty worktrees and residual overlays are under
`migration/20261005/preserved/`. A snapshot branch contains the original source
bytes as a commit. It can be checked out directly without resurrecting the old
absolute worktree directory. Original staged patches, status and source
manifests are retained in
[dirty custody](verification/machine-migration-20261005/dirty-custody/) and
[residual custody](verification/machine-migration-20261005/orphan-custody/).
The snapshots and patches preserve working content and original staging intent;
they do not imply that historical candidates can be merged into current main.
Historical patches are stored as `.patch.gz` to preserve their exact bytes;
decompress a selected patch before inspecting or applying it. The
`custody-files.json` manifest records original and transported paths and hashes.

Each of the four stash commits is under `migration/20261005/stashes/`. Its
original message and SHA are in the ref map. To recreate a stash entry, use
`git stash store -m 'restored migration stash' <remote-tracking-ref>`; apply it
only to a suitable base after inspecting its changes.

## Where development stands

- [Consolidation receipt](verification/consolidation-20261005.md): qualified
  local main, retained branches and exact source/platform boundaries.
- [All 102 nonclosed Motes assessed](verification/open-mote-audit-20261005.md):
  current state, fresh-context needs, evidence and next action for every ticket.
- [Migration manifest](verification/machine-migration-20261005/manifest.json):
  exact Mote operation hashes and source-preservation checks.

Temporary runtimes, dependency caches, native installations and some historical
`/private/tmp` evidence paths named in older tickets are not Git artifacts.
Recreate needed environments from the checked-in locks/runbooks and configure
external fixtures explicitly. The committed receipts and preserved source are
portable; no new scientific, native, performance or release qualification is
claimed by moving development.
