# C0 fresh-stream non-use: independent audit

**Verdict: the three intended roots are conservatively unobserved within the auditable ScalaFIM C0 execution ledger.** This is sufficient to describe their status as unobserved for the next authorization decision only within that stated history. It is not an absolute claim about deleted, unlogged, external, or other-machine execution.

The auditable details are in [c0-fresh-stream-nonuse-independent-findings.json](c0-fresh-stream-nonuse-independent-findings.json), SHA-256 `d2c168ad0034df65c81222386f281eb05b1bf64a8370d905352d5e81573ee574`. Its 33-log catalog is bound by SHA-256 `55b41c16d51452e34e1f71f60dcb296d87763005e313ec2df3f8f8f253bc4fba`.

## Fixed stream contract

The frozen protocol (`c0-repair-freeze/sources/docs/verification/condition-c0-qualification.md:91-100`) fixes the prospective roots as SNR 1: `7000930201`, SNR .5: `7000930102`, and reported-only SNR .25: `7000930103`. `runProspectiveStudy` is the only current prospective constructor and passes exactly those three roots into `runStudy`. Each call to `directCohort` constructs a new `scala.util.Random(seed)` once and consumes that generator voxel-major; no global continuation, seed arithmetic, worker offset, or block-dependent seed derivation is present.

The roots are distinct 64-bit literals. On the JVM, their `java.util.Random` 48-bit initialized states are respectively `0x47fa551f4`, `0x47fa5515b`, and `0x47fa5515a`; none aliases either another proposed root or the retired `7000930101` state `0x47fa55158`. This JVM calculation is an alias check, not a claim that JVM and Scala.js generate bit-identical streams. Unique roots and the one-fresh-generator-per-cell construction are the relevant overlap boundary on both platforms.

The Mote decision `20260930T034615.314728Z-p2341-c0000-r1666-h101b5e` records that the one-voxel smoke inspected `7000930101`; the whole root is therefore retired to development. Its raw smoke receipts expose suite status and timing but no seed or per-draw trace, so this audit deliberately does not reconstruct or minimize its draw count. The old root remains excluded even though its JVM initialized state is distinct from the replacements.

## Historical receipt ledger

I read every pre-existing `ROOT/logs/c0-*.log` and matching metadata **except** the active `c0-frozen200-jvm.log` pair. Parent reported that frozen-development work was in progress; I did not inspect its output or any future result. The ledger contains 33 raw logs: 23 exit 0 and 10 exit 1. Its parseable records are 112 cells, 144 voxel terminal records, 56 paired records, 10 measurements, and four oracle-adequacy records.

Every parseable seeded record is development-rooted: 156 records for seed 101 and 156 for seed 102. The 144 indexed voxel terminal records split 72/72 between those roots. No parseable record contains `7000930201`, `7000930102`, or `7000930103`; no recorded command contains any of those literals or invokes `study fresh`. The source IDs in the historical records are only `unfrozen-dev`, the old freeze `bbf30ff4bd169861999eddb39ccd349b7949c495bbebd1706f864f89b291fe53`, and the repair freeze `451be51b9abca9c127c9d8d2f012d7ef871f262922ec5c68ae3f7f06513e023d`.

The failed/opaque cases remain accounted for:

- `c0-qualification-smoke-r1.log` and `...smoke-r2.log` both exit 0 with one JVM and one JS suite test but no JSON seed trace. The Mote decision, rather than an inferred string hit, is the basis for retiring `7000930101`.
- The filtered r1, the r2 260.51-second MUnit timeout, failed direct-oracle receipts, and compile failures contain no fresh-root literal or parseable fresh result. Their scheduled development paths use roots 101/102; none carries a prospective `study fresh` invocation.
- The ten measurement JSON records omit their seed by schema, but the frozen `runQualified` entry explicitly receives `101L`; they cannot be attributed to a prospective root.

Mote PHRF-21 history confirms the sequence: the original planned SNR-1 root was replaced before qualification after smoke contamination, while `.5` and `.25` remained frozen and uninspected. The Fray C0 packets and both old and repaired frozen protocols consistently record that no fresh command was launched. These coordination records supply the smoke fact and contemporaneous intent; raw receipt records supply the executed ledger.

## Scope and remaining requirement

No further machine-readable evidence is required to make the bounded statement “unobserved in the recorded ScalaFIM C0 execution history.” A stronger, global claim cannot be established from this workspace without an external execution ledger or a responsible custodian’s attestation covering unlogged/deleted/other-machine runs. If the fresh gate requires global historical non-use, that attestation remains genuinely required; it should be attached to the later reviewed fresh freeze rather than inferred from this audit.

This audit neither authorizes a fresh run nor reads the active frozen-200 result. It leaves the prospective source/provider/protocol review, frozen-200 receipts, scientific gates, and all other C0 qualification boundaries unchanged.
