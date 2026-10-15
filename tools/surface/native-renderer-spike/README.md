# Native surface color spike

This bounded probe uses the public JOGL/GlueGen 2.6.0 macOS ARM64 jars and
unmodified native libraries. It is an investigation tool, not a production
surface renderer. Public NEWT `MainThread` supplies macOS event dispatch.

The context probe requests hardware GL3, checks a native framebuffer readback,
and records the observed API/vendor/renderer/capabilities. The color probe draws
original indexed triangles into RGBA8 buffers with either zero or four samples,
using a GLSL 150 affine vertex-color shader with centroid interpolation. It
disables lighting, depth, blending, dithering and sRGB conversion. It resolves
multisampling through the standard framebuffer API and reads pixels unchanged.
It does not use a JavaFX atlas or apply image/color corrections.

The fixed projection maps original pixel coordinates to the 256×256 viewport;
compiled application camera fitting is excluded. The input format is the frozen,
bounded, opaque, unlit v23 oracle fixture format. Arbitrary scenes are unsupported.

## Replay

Use a JDK 21 installation and an empty task directory on macOS ARM64:

```sh
python3 tools/surface/native-renderer-spike/run.py --work /private/tmp/my-native-spike
```

The runner checks the four runtime jars against `runtime.lock.json`, compiles
the Java probes, checks the context, renders the unchanged 48 originals from
the retained stock-Metal archive, and installs pinned NumPy/Pillow in an isolated
environment. It then runs the existing oracle self-check and color oracle.
It exits on setup, native or oracle failure; it does not choose another backend.

For the typed `SurfaceRenderPlan` handoff, first extract the archived originals
to a separate input directory and run the JVM test-scope producer:

```sh
python3 tools/build/sbt-warm 'surfaceViewJVM/Test/runMain scalafim.surface.view.SurfacePlanAffineFixtureProbe /path/to/originals /path/to/plan-inputs'
python3 tools/surface/native-renderer-spike/run.py --work /private/tmp/my-plan-spike --fixtures /path/to/plan-inputs
```

The producer compiles each real typed plan and checks original vertex/face
ordinals, positions, RGBA, triangle corners, draw-pass resource addresses and
the limited plan scope before emitting public mesh/layer buffers. The runner
requires all emitted binaries to match the frozen originals exactly.

## Remaining qualification

A passing result is **color-kernel evidence only**. A complete typed provider,
application camera, normals/lighting, missingness, thresholds, multilayer alpha,
cortical occlusion, legends, picking, export, interactive updates, cancellation,
disposal, production-width resources and device-scale visual review are pending.
The production JavaFX `SamplerUnqualified` decision is unaffected.

[Evidence](../../../docs/verification/native-renderer-spike-20261006/README.md)
records the exact host, raw frames, original binaries, producer and shader
sources, runtime digests and the unchanged oracle/plan hashes.
