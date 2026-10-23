# Development machine handoff

ScalaFIM's Mote tickets and coordination history are versioned in
`.mote/FORMAT.json`, `.mote/ops/*.json`, and `.mote/authority/`. The immutable
operations and authority journals together are the source of truth. Commit
new operations and their admission records with related work and fetch/pull
before continuing on another machine. Use the Mote CLI for changes; never edit,
reformat or delete operation files to change tracker state.

`.mote/local/` and `.mote/tmp/` are ignored. They contain workstation identity
and unpublished temporary state, and are not needed to reconstruct tickets.
Historical session PIDs and lease records in the operation log are historical
evidence, not proof that a worker is running on the new host.

An authority journal is a single writer history, not a distributed merge log.
Stop writes on the old host, commit the complete store, and fetch it before
starting writes on the next host. Never run independently writable replicas
and merge their numbered journals. Journal conflicts or unadmitted operations
require supported recovery; ordinary Git conflict resolution is insufficient.

## Start on the new machine

Clone canonical `main` for a new development machine. The handoff changes landed there at
`2a1af3d003b6f491296b071ee07bd6133801d861`; use the current `main` tip after fetching rather
than treating that historical landing SHA as a moving bootstrap target. The retained
`handoff/20261015-development-machine` branch is historical custody evidence from before the
complete tracker recovery, not a branch to start or merge new work from. See the
[2026-10-15 receipt](verification/development-machine-20261015/README.md) for its retained
worktree dispositions and initial verification. The
[2026-10-16 recovery receipt](verification/mote-recovery-20261016/README.md) records the
restored tracker and modularization epic.

Install Git, JDK 21 (the compiler targets Java 17), sbt 1.11.7, Node.js 22 or
newer, Python 3.11 or newer, and a compatible current Mote CLI. With Rust/Cargo
installed, Mote can be installed with
`cargo install --git https://github.com/bbuchsbaum/mote --rev 4ae95ba97cfef09efa537c2eb21433d36b4ba7be --locked`.
Scala 3.7.4 and build plugins are resolved by sbt. GitHub source dependencies
and artifact repositories require network access on the first build.

```sh
git clone https://github.com/canardlapin/scalafim.git
cd scalafim
git config --local user.name canardlapin
git config --local user.email 307091466+canardlapin@users.noreply.github.com
git config --local github.account canardlapin
./tools/prepare-pinned-dependencies.sh
sbt responseJVM/test responseJS/test
python3 docs/verification/development-machine-20261015/verify-custody.py
```

For GitHub API writes, authenticate the isolated `gh-canardlapin` profile on the
new machine and use `tools/github/gh-repo`. Git transport authentication and API
authentication are independent. Do not copy credentials or absolute credential
helper paths from the old machine. Configure any private-repository transport
under the intended account on the new host.

With GitHub CLI installed, configure that isolated profile independently:

```sh
env -u GH_TOKEN -u GITHUB_TOKEN -u GH_ENTERPRISE_TOKEN -u GITHUB_ENTERPRISE_TOKEN \
  GH_CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/gh-canardlapin" \
  gh auth login --hostname github.com --web --git-protocol https --skip-ssh-key
tools/github/gh-repo auth status
tools/github/gh-repo api user --jq .login
```

Verify that the API login is `canardlapin`. If HTTPS pushes also need that
profile, set a helper only in this clone:

```sh
git config --local credential.helper ''
git config --local --add credential.helper '!tools/github/gh-repo auth git-credential'
```

**Mote was recovered on 2026-10-16.** All 24,943 original operation files and
the existing 75 admission records were preserved. The 943 upstream operations
outside that journal were re-admitted through Mote's native publisher after
the frozen prefix. The original genesis binding and authority fence remain
active. This is a documented recovery admission order, not a claim to have
found the missing original journals. See the
[recovery receipt](verification/mote-recovery-20261016/README.md).

The [2026-10-15 receipt](verification/development-machine-20261015/README.md)
records the earlier blocked state; the recovery receipt supersedes that live
status. A new clone should work with the complete committed store. Bootstrap
its local identity and verify it:

```sh
mote --version
mkdir -p .mote/local .mote/tmp
mote actor set development-new-machine
mote authority status
mote doctor
mote fsck
mote --json ls --all
mote ready
```

Mote reports version 0.1.0 across multiple revisions; the version string alone
does not establish authority compatibility. Its schema and authority support
must match the committed store. Select your own stable actor name rather than
inheriting a workstation's identity. Current claims and reservations are TTL
leases; inspect them before touching claimed paths. Git does not carry empty
directories, so the `mkdir` step restores the ignored layout directories. Do not
carry `MOTE_STORE` or `MOTE_ACTOR` exports from an old session into the new clone.

Formal candidate records can be bound to the original Git repository identity
and landing target. Revalidate or rebind them through the Mote candidate
protocol in the new clone before landing; source-preservation refs do not
change the recorded review or authorization scope.

Follow `AGENTS.md` and `README.md` for toolchain, fixture configuration and
bounded JVM/Scala.js checks. Keep repo-local authoring and GitHub API routing
under `canardlapin`, as specified in `AGENTS.md`; credentials are configured on
the new machine independently and are not part of this handoff.

After the smoke tests, use `sbt scalafimCompileAll` for the portable compile gate
and run relevant module tests on both platforms. On a memory-limited machine,
run a few JVM targets per sbt invocation, then a few JS targets in a separate
invocation; avoid one `scalafimTestAll` run. Native JavaFX checks need a display
and OpenJFX; R/native-tool and external cortex fixtures need their documented
separate setup. They are not prerequisites for the portable smoke test.

## Recover retained development work

The migration preserves unintegrated work under remote branches rooted at
`migration/20261005/`. These are source-preservation branches, not acceptance
or scientific qualification. They are not a queue of pull requests to merge.
Reviewed source was selectively consolidated; unfinished or superseded lines
remain available for bounded ports onto current main. See the
[PR conflict audit](verification/migration-pr-conflicts-20261005.md) for the
archive PR dispositions. Use
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
- [Retained-work recovery ledger](verification/migration-recovery-ledger-20261005.md):
  exact preserved tips, remaining qualification gaps and feature-level acceptance steps.
- [Migration manifest](verification/machine-migration-20261005/manifest.json):
  exact Mote operation hashes and source-preservation checks.

Temporary runtimes, dependency caches, native installations and some historical
`/private/tmp` evidence paths named in older tickets are not Git artifacts.
Recreate needed environments from the checked-in locks/runbooks and configure
external fixtures explicitly. The committed receipts and preserved source are
portable; no new scientific, native, performance or release qualification is
claimed by moving development.
