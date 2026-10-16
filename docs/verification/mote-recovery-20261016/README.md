# Mote recovery, 2026-10-16

ScalaFIM's canonical tracker was repaired from the complete retained operation
content. The recovery starts from handoff commit
`86b3acf1e64d762be99b52ae146fa115b9050356`. The original 24,943 operation files
and 75 authority records were preserved byte for byte. Mote's native publisher
created 943 additional admission records after that frozen prefix. The live
store then passed authority validation, `mote fsck`, `mote ready`, and `mote ls`.
The original genesis binding and authority format 1 remain active.

The [2026-10-15 handoff receipt](../development-machine-20261015/README.md)
describes the earlier failure. This recovery supersedes its blocked live status.
The [machine handoff guide](../../development-machine-handoff.md) contains the
current clone and bootstrap instructions.

## What was recovered

All 943 previously unadmitted files were byte-identical to GitHub main
`ac2ec8b706fce08596a943d79f71a140aad05948`. Of the 232 live upstream branch tips
checked, only the handoff tip contained an authority directory, matching the
retained incomplete journal. That observation does not establish whether other
journals existed on another machine or in older history.

The preserved journal admitted 24,000 operations. Replaying that prefix plus
the 943 extras restores 25 missing historical issues, from 1,286 to 1,311 issues.
Seventy-five issue projections change, including those newly restored issues.
The resulting persistent issue fields and note membership match a diagnostic
filename-ordered replay of all operations. Candidate records and role authority
are unchanged in the prefix-versus-extended replay comparison.

This is a best-effort recovery admission order: the existing prefix remains
fixed, and extras follow in filename order. We did not recover their original
historical admission order. The comparison does not establish every historical
lease or acceptance outcome. Original operation actors, timestamps, and bytes
remain intact; historical leases were not renewed by publishing their bytes.

## How the repair was made

A complete pre-repair backup was retained locally. A separate store was seeded
with the original format, 75 journal records, and their 24,000 admitted operations.
The pinned Mote library's `publish::publish_bytes` admitted each extra operation
using its normal writer lock, durable publication journal, and admission logic.
No admission JSON was constructed by the recovery tool.

The native preview passed its full fsck and exact replay comparison. All 24,943
operation bytes and the original 75 journal records matched the source. The
installed Mote CLI independently validated the preview's authority and hashes.
The preview was placed under a `.mote` directory for the CLI's store-path
convention; this relocation did not change store content.

Under the existing live `.mote/local/publication.lock`, the restore checked
that every live source file still matched the pre-repair snapshot, then restored
only the generated new journal records using no-overwrite atomic links. It
rechecked the original bytes and ran read-only authority validation before
releasing the lock. No live operation was moved, removed, or rewritten. The
authority field was not disabled, and no independently writable journals were
merged.

`restore-receipt.json` records the resulting authority and every new journal
record's SHA-256. `replay-analysis.json` records the bounded replay comparison.
The compressed native-publication logs retain exit status, timings, and the
943-operation result. The publisher used Mote source
`4ae95ba97cfef09efa537c2eb21433d36b4ba7be`, with dependencies fixed by the
archived Cargo lockfile.

## Modularization epic

The exact imported payload is retained as `import-input.json`, with SHA-256
`d850a550cef0f348481e05a73667bd9cd23acaf25a1ecdd5696ee8be2e10063a`.
The canonical import accepted all 348 operations, with zero rejected or skipped.
It created one epic, seven milestone gates, and 25 tasks, all tagged
`ecosystem-unification`, `modularization`, and `dry`. All 33 items and 98 links
match the separately validated import. Titles, bodies, priorities, and tags
match the frozen input. The ready-task filter returns `eco-u0-ownership`.
See `canonical-import-result.json`, `epic-graph.json`, and `verification.json`.

While recovery ran, the planning agent expanded the desired plan to 46 items
(one epic, nine gates, and 36 tasks). Those revised source documents are preserved
unchanged in [ecosystem-unification.md](../../plans/ecosystem-unification.md) and
[ecosystem-unification-mote.json](../../plans/ecosystem-unification-mote.json).
They are a later draft, not the payload used for the 33-item canonical import.
`planning-revision.json` binds that distinction to the draft's hash. Its planning owner is now reconciling the revision in place through Mote: retain the existing IDs and
apply missing issues and changed fields/edges. Do not rerun the full import
against existing IDs or create a second epic. That concurrent expansion is separate from this recovery; the 33-item graph above records the initial import, not a final snapshot of the evolving epic.

Use the tagged view to focus on this work while retaining older history:

```sh
mote show eco-unification
mote ls --tag ecosystem-unification --tag task --ready
mote graph eco-unification
```

The first implementation task is `eco-u0-ownership`. The recovery itself is
tracked by `bd-01M52657DNVWDBZC0XKXKPHJG5`. Older tickets were not bulk-closed
or deleted, and this repair does not qualify or implement their scientific work.

## Independent review

The archived `tool/` contains the exact native-publisher helper, locked dependency
pins, and the guarded one-off restore program. The restore program has fixed
historical paths and snapshot guards; it is evidence, not a new-machine bootstrap
command. A new clone should use the already repaired committed store.

To reproduce the read-only analysis of the historical broken snapshot:

```sh
review_root=$(mktemp -d)
git archive 86b3acf1e64d762be99b52ae146fa115b9050356 .mote | tar -x -C "$review_root"
CARGO_NET_GIT_FETCH_WITH_CLI=true cargo run --locked --release \
  --manifest-path docs/verification/mote-recovery-20261016/tool/Cargo.toml \
  -- analyze "$review_root/.mote" "$review_root/replay.json"
```

The optional full native preview uses the helper's `preview STORE NEW_STORE LIMIT`
command with limit `943` and a new destination ending in the path component
`.mote`. It took about 18 minutes on this host. Run it only in a separate review
copy. No Scala production sources or build pins changed during this recovery.

## Retained warm-update branch

A fresh GitHub check confirmed `surface/warm-update-20260921` at
`7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4` is not merged into main
`ac2ec8b706fce08596a943d79f71a140aad05948`. Its 33 branch-only commits have
no patch-equivalent matches on main, and GitHub lists no PR for this exact head.
The tip remains published directly and under the migration preservation refs;
it is also contained in the retained group-fsLR qualification lineage.

This differs from the recent October surface branches that reached main.
The old retained-layer optimization remains open under Mote
`bd-01M24EQFARZKYMZ71BJP91C9XS`. Its September performance gate and October
dense-palette framebuffer failure are unresolved historical evidence. Current
main lacks its `SurfaceLayerTransaction` API. The existing recovery ledger calls
for a bounded current-base port and native correctness/performance qualification,
not a blanket merge of its older dependency and API tree. See
`warm-update-branch-audit.json`. No implementation or fresh native test is claimed.

## GitHub clone and completion

A new independent GitHub clone of `5cb67982f299e597b27e103f0849ce424cfc72ca`
passed authority validation, doctor, fsck, ready, ls, epic/graph reads and the
historical byte-custody verifier. No inherited `MOTE_STORE` or `MOTE_ACTOR` was
used; the clone stayed clean after setting only its ignored local identity.
See `fresh-github-clone.json`. Its 46-item epic reflects concurrent planning;
dependency reconciliation was still in progress, so that snapshot's ready list
is not a final plan-acceptance result.

The recovery issue was completed through `mote done`, and both recovery
reservations were released: `rv-01M5265P27AJAS67KN0ZZJVS85` and
`rv-01M527472APFQGNM7JTYPHTV12`. All six reserved paths were checked for remaining
leases by this actor; see `reservation-release.json`. The final publication also
includes these completion operations.
