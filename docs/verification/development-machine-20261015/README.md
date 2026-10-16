# Development machine custody, 2026-10-15

The handoff branch `handoff/20261015-development-machine` starts at published
main `ac2ec8b706fc`. The original local main was `606b49b3e5a6`; a fetch proved
it was 252 commits behind and had no unique commits. It was fast-forwarded.
Every local branch commit was already reachable from a GitHub branch. Main
remains protected; this handoff does not merge or bypass its required gates.

Use the [new-machine guide](../../development-machine-handoff.md) for cloning,
toolchain installation, provider preparation, identity, and JVM/Scala.js checks.

## Retained bytes

- The final loose Mote note `20261015T181148.168576Z-p97309-c0000-rf90e-h861439`
  is committed without alteration. The other 37 loose operations were already
  byte-identical on GitHub main after PR #61.
- All three original surface evidence bundles are retained under `evidence/`.
  The 107,996,059-byte bundle uses ordered 48 MiB parts to stay below GitHub's
  per-file limit. The two smaller bundles are intact tarballs. Their original
  manifests and adverse evidence are preserved; external cortex coordinates
  remain excluded according to those manifests.
- `worktree-overlays/` retains 20 small untracked source and timing files from
  the baseline/parity worktrees. Four baseline source/tool files differ from
  current main; these are historical specimens, not current production code.
  The source worktrees were left untouched. Runtime caches are not transported.
  The historical `run-parity.mjs.gz` is compressed to preserve its exact trailing
  whitespace; decompress it before use.
- `custody.json` binds ordered bundle parts and overlay files to SHA-256.
  `mote-snapshot.json` binds the format, operations, and retained authority files.

Verify all retained bytes after cloning:

```sh
python3 docs/verification/development-machine-20261015/verify-custody.py
```

To reconstruct the large original tarball, concatenate its three parts in name
order into a new temporary file. Its original SHA-256 is
`aa351a33334eb5bddf11bfd06b409a076b207d2d1d9571b522bfd3957b59f141`.
The verifier checks ordered concatenation and every content-addressed object
without extracting files. `originalPath` values are historical provenance;
use the committed relative paths on the new machine.

## Branch and worktree disposition

`worktrees.json` records each existing worktree's exact tip, main ancestry, and
remote preservation refs at the start of this handoff. Recent surface
composition, mapping/visibility, core parity, and qualification branches are
already ancestors of GitHub main. Six existing historical worktree tips remain
unmerged, all preserved under `migration/20261005/branches/`:

| Branch | Remaining decision |
| --- | --- |
| `integration/mvpa-on-exact-spatial` | Identify a useful remaining feature and qualify a bounded port. |
| `spike/face-texel` | Retained experiment; define current native/product acceptance before porting. |
| `spike/lut-encoding` | Contained in the retained face-texel lineage; avoid duplicate ports. |
| `spike/raster-figures` | Retained experiment; qualify useful changes against current APIs. |
| `worktree-agent-a3ebf3564cf9328f3` | Historical spatial/template work; current-base qualification remains open. |
| `worktree-agent-a6daa1f0970060d9c` | Historical numerical-inversion work; consult Mote `bd-01M39Q53JNRCWQG7ZRPWWK74EE`. |

See the [recovery ledger](../migration-recovery-ledger-20261005.md) for feature
admission rules and the older retained branches. Preservation does not establish
that an old branch should be merged. No branch or worktree was deleted.

## Mote recovery blocker

Before updating this checkout, its retained local admission journal replayed
24,000 operations. Published main added 943 operations absent from that journal.
It also carried an authority-enabled `FORMAT.json` while ignoring all journals.
A fresh clone therefore failed with a missing journal; the updated local store
fails with an unadmitted operation. Tracking the retained journal corrects the
missing backup, but cannot establish the missing admission order.

`mote-authority-gap.json` names the exact 943 unadmitted files. The retained
journal has 75 contiguous records; no pending publication or landing journal
was present. All admitted operation bytes remain available. The installed Mote
source used for diagnosis was `4ae95ba97cfef09efa537c2eb21433d36b4ba7be` in
`bbuchsbaum/mote` (CLI version 0.1.0). Its authority protocol requires backing
up `FORMAT.json`, `ops/`, and `authority/` together and forbids manually merging
independently writable journals.

Restore the matching complete authority history from its publishing machine,
or obtain a supported upstream recovery procedure for these independently
published operations. Do not disable the authority field, hand-edit journals,
or run an older writer. `mote doctor`, `mote fsck`, and tracker writes are
blocked until that recovery succeeds. Creating a handoff ticket was attempted
and refused by this fence; no ticket was fabricated outside the CLI.

The final historical note refers to pending PR #61 checks. GitHub now shows
PR #61 merged; that note is preserved as historical evidence, not current
status. Full surface parity/scientific qualification remains separate work.

## Verification

The independent clone fetched published handoff commit
`9c7904488b8f03f4f943f496d60e744de0b07722` from GitHub. The documented provider
bootstrap passed, as did `sbt responseJVM/test responseJS/test` (16 JVM and 15
Scala.js tests). The byte verifier passed for all three bundles, 20 overlay
files, and 25,019 Mote files. The GitHub wrapper checks passed for isolated
profile selection, removal of ambient token overrides, invocation outside the
repository, and refusal without the configured account.

`verification.json` records commands, exit codes, environment, warning lines,
log hashes, and the failed tracker checks. Full logs are retained under
`verification-logs/`; compressed logs preserve their original bytes. The clone
used this host's dependency caches and ran sbt on Homebrew Java 25.0.1 with
Node 26.7.0. This was not an empty-cache installation or a JDK 17 qualification.
Upstream documentation/unused-setting and existing-artifact warnings are
retained; no warning-clean full build is claimed.

Byte custody, source smoke tests, and tracker reconstruction are separate
results. A source smoke pass does not resolve the Mote authority gap or replace
the full required CI suites. Subsequent handoff edits only add documentation
and verification receipts; production sources and build pins remain unchanged.
