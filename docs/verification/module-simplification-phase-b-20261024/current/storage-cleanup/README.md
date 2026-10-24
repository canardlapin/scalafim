# Retired merged worktrees, 2026-10-24

Six inactive clean ScalaFIM/PLS Neuro worktrees were verified as ancestors of freshly fetched published main, fully archived here, and removed with normal git worktree remove. Existing branch refs are retained. The detached verification checkout points at the same published commit as the retained fast-feedback branch.

This is the physical Passport, volume UUID51B72C66-64BB-3C5F-8853-2219CA2FFA9F. Each subdirectory contains a complete tar.gz (including ignored build artifacts), source-manifest.json with every file hash/type/mode or symlink target, and a timestamped custody.json. Archive members and a fresh source scan matched before removal; archive SHA, clean Git status, HEAD/main ancestry, file bytes, and absence of open handles were checked again immediately before each normal removal. No force removal or branch deletion was used.

See custody-index.json, removals.json and cleanup-summary.json. archive-operation-index.json retains the archiver's original operation report; the current index records subsequent retirement. Per-directory custody timestamps describe the verification instant before retirement.

To inspect historical artifacts, verify the archive SHA from custody.json, then extract into a new quarantine directory. The archived .git file points at a retired worktree registration and must not be used as a live Git checkout. To recreate a usable checkout, run git worktree add --detach <fresh-path> <head> from the corresponding canonical repository, then copy any needed archived ignored artifacts into it. Original commits are on published main and existing branch refs are retained. Do not resume qualification against changed sources using historical outcomes.

Dirty, unpublished and current worktrees were retained. Eleven orphan sbt cache bases were inventoried but not deleted: four source status checks in one base stalled, so broad GC was deferred. The broader Phase C issue remains open.
