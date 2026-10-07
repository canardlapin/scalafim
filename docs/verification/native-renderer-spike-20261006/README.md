# JOGL native color kernel and typed-plan fixture handoff

Mote: `bd-01M3QQ8EBSWQA0V65NYQATJXB3` remains open for full provider qualification.

Public JOGL/GlueGen 2.6.0 on macOS ARM64, JDK 21 and the Apple M2 Pro produced
**48/48 passing frames and 0/32 face-permutation failures** under the unchanged
two-channel-level independent affine-color oracle. The observed driver reports
`Apple`, `Apple M2 Pro`, `4.1 Metal - 89.3`; the requested GL3 API runs on a
hardware GL4.1 context. Native RGBA8 context readback passed.

The JVM fixture producer consumes real `SurfaceRenderPlan` public mesh/layer
buffers after original vertex/face identity and supported-scope checks.
Its 48 binaries match the frozen originals byte-for-byte. Both surface-view
test gates passed **68 tests**, with no compiler warnings. The final replay
from an empty directory consumed these emitted binaries, checked runtime
digests, and passed both oracle self-check and all 48 color fixtures.

## Scientific scope

This proves the first bounded color-kernel and fixture-handoff gate. It does not
admit a complete renderer or consumer. The pixel-parallel projection is the
same declared fixture isolation; application camera fitting is excluded. Only
one opaque unlit mesh/layer/pass is supported. Boundary vertex IDs are copied
as oracle metadata after unchanged vertex ordinals have been proved.

No shared scientific policy, fixture, tolerance, mask or production admission
was changed. The shader interpolates original RGBA directly; four-sample AA is
resolved through the public OpenGL framebuffer API, without pixel correction,
atlas sampling or private runtime patches. The oracle retains internal edges
and the prospective channel budget.

Full qualification still requires a typed provider/runtime support surface,
bilateral beta/FIR, at least 1,200 same-face picks per admitted scenario,
alternating updates, cancellation/disposal, production-width resource gates,
and a named human reviewer of the specified visual matrix. Other platforms and
packaged consumers require separate admission.

## Setup corrections retained

The initial direct Java launch timed out before context/readback; the documented
public NEWT main-thread launcher succeeded. An initial compile used the public
constant on the wrong GL interface, corrected before any color frames. The first
replay omitted the required self-check directory argument; its native/color
commands passed but the harness correctly failed overall. The final replay
uses the corrected harness from an empty directory and all commands pass.
These development attempts remain separate from accepted evidence.

See `receipt.json`, the retained archive and the `plan-producer` packet for source
and runtime hashes, commands, raw images/binaries and exact scope. Replay
instructions are in [the tool README](../../../tools/surface/native-renderer-spike/README.md).

The launcher rationale follows the upstream [JogAmp FAQ](https://jogamp.org/wiki/index.php/Jogl_FAQ);
actual behavior is bound to the exact 2.6.0 runtime jars and this measured host.
