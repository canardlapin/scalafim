# Linux bridge custody and portable controls

The protected run at 76b2fdbb confirmed the compiler stack repair, then failed
six GlmSingleBridgeSuite controls in the first comparative JVM batch. Focused
and coverage checks passed. Exact failed-native receipts are retained locally
under protected-37545717682; that full run did not reach the remaining batches.

## Production repairs

- A Linux scratch directory is rolled back and verified if registration fails.
  Setup refusal is retained; cleanup failure takes ScratchResidue precedence.
- Removing a Linux RAM directory no longer discards ownership of a surviving
  process group. Verified termination clears group ownership; shutdown retries
  and verifies the real group kill before unregistering a released scratch.
- Normal and shutdown termination share bounded conservative pgrep checks:
  every member, including zombies, counts, and unknown status remains a survivor.
  Reporting hooks run before the saved interrupt is restored.

## Controls

Darwin retains its real RAM mounts and security checks. Linux tests its actual
/dev/shm tmpfs and 0700/nosuid/nodev properties. The Darwin teardown-fault state
machine has explicit portable doubles on Linux; these do not admit real Darwin
mount/security claims. No tests are excluded or newly skipped.

The noKill control first requires a genuinely running group and retained
registry ownership, then real shutdown must terminate it. Post-shutdown tests
inspect PID/start identity and independently reject running group members.
Production remains conservative about zombies. An interrupt-consuming report
hook cannot swallow the original flag. All capture/assertion paths run cleanup.

Two watchdog fixtures now clear their environment just like production. This
avoids Darwin Perl aborting on inherited C.UTF-8; exact 137 exit and the two-second
parent-death bound remain. Standalone clean-environment controls terminate in
0.086–0.098 seconds. The production watchdog script is unchanged.

## Qualification

- Final Linux JDK17 lifecycle suite: OK (35 tests), exit 0; parent-death 0.037 seconds,
  no surviving captured processes. This executes Scala 3.7.4 classes compiled
  locally on JDK21, in the recorded Linux JDK17 image.
- Full macOS JVM comparison: 476 total, 466 passed, 10 existing optional skips, no
  failures/errors, exit 0.
- Full Scala.js comparison: 243 passed, no failures/errors, exit 0.
- Inventory: all 95 supported targets in 16 bounded batches; five checker tests pass.
  The two comparative batches run first for prompt compiler/platform feedback.

Compressed logs and receipt.json bind source/runtime/image/counts. The original
46-path/e1cfa089 preservation check and all three protected checks must pass at
the new published head before the ordinary main merge. Main is not merged by
this local qualification. Scientific limits and the recorded weak-LWU admission
caveat remain as qualified in the publication packet.
