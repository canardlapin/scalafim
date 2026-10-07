# Retained work recovery ledger, 2026-10-05

This ledger accounts for all 146 original branch tips and all 166 published preservation refs. Original main is carried by the consolidated handoff rather than a separate archive branch. Twenty-one additional preservation/stash refs carry dirty-source custody. [The complete ledger](migration-recovery-ledger-20261005.json) binds each entry to its SHA, disposition, archival ref, refreshed ancestry, recorded path evidence and next action. Literal references in the 102 audited open Motes are linked where present; missing links are an explicit discovery gap, not permission to discard work.

The current audit establishes preservation, history and the bounded reviewed consolidation. It does not establish that every retained feature is sound or stale. Historical test results and source blobs are useful evidence only for their recorded revision and scope.

## How to decide a remaining feature

1. Identify the intended behavior and authoritative Mote acceptance. Record its exact branch/snapshot SHA, dependency pins, relevant source and tests. Compare the delta from its historical base with current main; do not use the entire old tree as the feature definition.
2. Explain the current replacement where behavior is already adopted or superseded. Cite current APIs, tests and source locations; ancestry and patch equivalence establish different facts. Preserve any useful residual feature as a separate bounded change.
3. Port a useful missing delta in an isolated current-main worktree. Reconcile producer and consumer contracts, domain identity, persisted formats and published dependencies. Preserve unrelated changes and historical failed receipts.
4. Review the exact candidate for types/invariants, numerical/scientific behavior and consumers. Run affected JVM and Scala.js tests, required compilation and independent fixtures/oracles; require native, resource, visual or scientific gates wherever the original acceptance requires them. Old receipts do not fill new-source gaps.
5. Record a SHA-bound Mote disposition with source evidence and terminal checks: already integrated, qualified current-base port, superseded with replacement, or retained experimental/dependency-blocked work. Landing and remote publication remain separate facts.

Until those steps are complete, use **unassessed remaining delta** rather than claiming good, bad or stale. Age and conflict count do not decide usefulness. An automatic choice of either side of every conflict is not this audit.

## What is already established

| Original disposition | Tips | Meaning |
|---|---:|---|
| contained-by-retained | 16 | Overlaps another retained lineage; deduplicate before porting. |
| dependency-blocked | 1 | Preserved with documented limits; see per-tip next action. |
| evidence-retained | 3 | Preserved with documented limits; see per-tip next action. |
| experiment-retained | 3 | Preserved with documented limits; see per-tip next action. |
| integrated-ancestor | 84 | Tip commits are included; existing acceptance limits remain. |
| integration-deferred | 4 | Preserved with documented limits; see per-tip next action. |
| merge-only-history | 1 | Preserved with documented limits; see per-tip next action. |
| patch-equivalent | 4 | Patch correspondence; acceptance still needs review. |
| research-retained | 2 | Preserved with documented limits; see per-tip next action. |
| selectively-integrated | 3 | Only a bounded admitted slice was taken. |
| snapshot-retained | 3 | Preserved with documented limits; see per-tip next action. |
| source-represented | 3 | Blobs appear in main or history; current behavior still needs review. |
| superseded | 3 | Replacement identified in original assessment; preserve any useful residual. |
| superseded-snapshot | 1 | Preserved with documented limits; see per-tip next action. |
| unqualified-for-current-main | 15 | Preserved with documented limits; see per-tip next action. |

## First recovery seams

| Seam | Current known boundary | Authoritative Mote starting point |
|---|---|---|
| Typed atlas / historical domain realizations (#27; C1 snapshot) | Current workflow/spatial/examples and identity contracts require a fresh port. | Existing locus-removal and atlas scopes; audit feature coverage before creating new tickets. |
| GIFTI and historical affine viewer lines (#6/#22/#23; C6) | Later declarations and a bounded affine slice are consolidated; remaining placement/provider/native behavior is unresolved. | `bd-01M37FQGV8ZPT30X37TA4MM8R7`; `bd-01M24EQFARZKYMZ71BJP91C9XS` |
| Historical spatial graph / template-chain lines (#24/#25) | #25 has source-history representation; #24 remains distinct. Compare current route behavior before restoring anything. | `bd-01M39Q2YNKRTJTD8PZ4694DS52`; `bd-01M44W2HXQ5PY3YE33D5W37A04` |
| fsLR court and native/raster research (#29 and related retained lines) | Lattice B4 failure, native/visual/default/resource acceptance remain binding; no blanket admission. | `bd-01M37DVYQ9J6NWE1WEN2SR41CN`; `bd-01M3QQ8EBSWQA0V65NYQATJXB3` |
| Dirty condition/atlas snapshot (#28) | Snapshot preserves unfinished source/evidence. Extract intentional features and keep failed weak-LWU admission intact. | `bd-01M25Q0JY8GA7CJRKZF2934PJ0`; identify the separate atlas feature scope. |
| Numerical ownership override | Unpublished Gale override and S0 measurement do not meet upstream publication or S4/S5 qualification. | `bd-01KXYJPWYV08VFWRMCYYDPZCAV` |
| Estimate export/action alternatives | Current main still lacks some historical schema/backend/status work. Separate adoption from closed-child history. | `bd-01KX6G9B8R86MRBZ9S8K8F5G7V` |

PR #26 is exactly included in the handoff. #19 is a superseded reconciliation candidate. Neither needs a fresh feature port. [PR conflict audit](migration-pr-conflicts-20261005.md) records the current GitHub dispositions.

The [open-Mote assessment](open-mote-audit-20261005.md) retains scientific and integration next actions. Use the [consolidation receipt](consolidation-20261005.md) for the slices actually admitted and exact test evidence; use the JSON ledger to find source custody. No original branch, snapshot or failed receipt was removed by this ledger.
