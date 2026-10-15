# Warm sbt reconfiguration resource contract

Mote: `bd-01M47MBK0K3VPJ490HDJTQ6Y3C`.

The ordinary warm-server default remains 3 GB. Changing heap, processor count,
or idle timeout on a client cannot change the running JVM. The wrapper now
records its requested startup profile and refuses reuse when that profile
differs or cannot be established. `--status` shows the requested and recorded
startup profiles. `--shutdown` remains available regardless of a mismatch.

The record is tied to the active Unix socket's URI, device, inode and change
timestamp, so a server restarted at the same URI does not inherit an old
record. Metadata is excluded from templates and written atomically after an
exclusively serialized cold startup. Ordinary warm commands retain shared
worktree locks. Shutdown cannot interrupt an active command.

These records describe wrapper launch requests. They do not replace checking
actual JVM flags, and they do not track arbitrary JVM/environment overrides.
The receipt records measured effective flags separately.

## Why reconfiguration needs more memory

The pinned graph has 350,211 settings. sbt 1.11.7 applies `set` by rebuilding
the whole settings graph and indexes while the previous state remains live.
This establishes transient coexistence of old and new graphs; no permanent
memory leak is established.

The external builds declare JVM, JS, Native, documentation, benchmarks and
qualification projects beyond the subset ScalaFIM executes. Different pinned
Gale and image4s revisions are also loaded. Those revisions cannot be removed
without compatibility qualification. Provider pin consolidation is owned by
Mote `bd-01M43MQB7S7X8TK76CE391CJPJ`; this slice preserves all pins, projects
and scientific behavior.

## Evidence and limits

In an isolated checkout, the 3 GB server loaded and reloaded successfully but
the first settings change failed in 42.9 seconds. The client reported up to
98.7% GC time with 0.02 GB free; JVM GC logs recorded repeated full compaction
and GC overhead-limit events. A fresh 4 GB server completed reload and two
successive settings changes (about 32 seconds each), with no sbt GC-pressure
warnings. Both profiles were verified using actual JVM flags.

Use `SBT_WARM_HEAP=4g` explicitly for settings reconfiguration. Stop the existing
server first and retain the same profile until shutting it down. The wrapper
does not silently enlarge a running server or automatically change defaults.

`receipt.json` and `logs-and-sources.tar.gz` retain isolated measurements,
raw GC/client logs, observed JVM flags, sampled RSS, wrapper tests, the
independent provider/settings audit and final JVM/JS gates. The settings
probe uses a harmless test environment variable, requires no atlas assets,
and changes only session settings.

RSS includes memory outside the heap. Its sampled maxima are not guaranteed
full-lifetime peaks. A supported profile applies to this pinned build and the
measured commands; it is not a guarantee for unlimited reconfiguration or all
test campaigns. Keep large Scala.js campaigns bounded as described in
`AGENTS.md`.

Final-source validation passed **57 Python tests**, **386 HRF JVM tests** and
**386 HRF JS tests**, with no compiler warnings. Live CLI checks confirmed
matched reuse, requested/recorded status, mismatch refusal before base changes,
unknown-startup refusal, shutdown independent of requested heap, and recovery
of malformed active metadata through a known live base socket. All experiment
servers were stopped.

Two host-harness postconditions were corrected: socket shutdown can precede
JVM process exit briefly, and sbt deletes `active.json` during shutdown. Those
instrumentation assertions occurred after passing command outcomes; the
retained notes and bounded final checks distinguish them from product failures.
The final wrapper was independently tested after the defensive parser repair.

An exclusive cold-start contender can conservatively wait until the first
owner’s command batch finishes before rechecking the live server. Fresh warm
clients retain shared execution once startup metadata has been recorded.

A server dying between a warm check and client connection can still cause
sbt's client to restart under the shared lock. The next wrapper invocation
refuses the stale socket identity. Serialization covers wrapper-observed
cold starts, not every external death/restart race. Malformed active records
remain unknown without crashing; shutdown repairs them only when exactly one
known live base socket makes the server addressable.
