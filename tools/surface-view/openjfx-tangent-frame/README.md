# OpenJFX 24.0.1 tangent-frame repair

ScalaFIM issue `bd-01M5ABGAP2ZP15KWQM5B0ZSQSY`.

An ordinary `POINT_NORMAL_TEXCOORD` tetrahedron can render black in lateral
views even though its self-illumination texture contains the correct colors.
The Java-only `PublicEmissionControl` reproduces this on stock OpenJFX 24.0.1
and on the previously admitted affine-atlas compatibility runtime. It has no
ScalaFIM or consumer dependencies, additional lights, or custom shaders.

The defect is in OpenJFX's `MeshUtil.fixTSpace`: projecting a face UV derivative
onto a supplied vertex normal's tangent plane can produce a zero vector. The
subsequent angle and normalization divisions produce a nonfinite vertex
quaternion. Native lighting then contaminates even a black-diffuse,
black-specular, self-illuminated material with NaNs. Switching off the implicit
headlight isolated this failure during diagnosis; that is not the repair.

`MeshUtil.patch` preserves the supplied normal and any surviving projected UV
derivative, completing an orthonormal frame when a derivative collapses. If
both collapse, it projects the least aligned coordinate axis. The ordinary
nondegenerate path is unchanged. Mesh positions, indices, UVs, colors, picking
identities, camera transforms, and scientific lighting policy are unchanged.

This is an explicit, process-wide **native runtime patch**, not a ScalaFIM
numeric implementation or a silently installed runtime. The builder checks
the original source and macOS ARM module hashes and the patched class rejects
runtime versions other than `24.0.1+4`. Do not apply it to ScalaFIM's default
JavaFX 21 runtime. Original OpenJFX copyright and license files are retained.

## Build and verify

Use the original Maven `javafx-graphics-24.0.1-sources.jar` and the macOS ARM
`javafx-base` and `javafx-graphics` 24.0.1 JARs, with JDK 22 or newer:

```sh
python3 tools/surface-view/openjfx-tangent-frame/build.py \
  --sources /path/to/javafx-graphics-24.0.1-sources.jar \
  --module-path /path/to/javafx-base-24.0.1-mac-aarch64.jar:/path/to/javafx-graphics-24.0.1-mac-aarch64.jar \
  --javac /path/to/jdk/bin/javac \
  --output /fresh/output/directory \
  --verify-native
```

The builder preserves the stock failure and requires the same CPU and native
tests to pass with the patch. CPU assertions cover 24 signed-normal and
collapsed-derivative combinations: finite quaternions, unit frame vectors,
orthogonality, and preservation of the normal direction. Native assertions
require the exact initial orange and updated blue texture colors, substantial
interior coverage, and zero black interior pixels. `--verify-native` requires
a native ES2 display; omit it for a CPU-only check. Every command, exit status,
and full log is retained. A receipt is written only after required checks pass.

The output JAR contains only the patched `MeshUtil.class`, its source and
patch, and legal notices. The archive is deterministic for the same compiler
and inputs. Load it explicitly:

```text
--patch-module javafx.graphics=/path/to/openjfx-24.0.1-tangent-frame.jar
```

When using the existing affine-atlas component, combine both paths in the
same argument; the two components replace different classes:

```text
--patch-module javafx.graphics=/path/to/openjfx-24.0.1-tangent-frame.jar:/path/to/openjfx-24.0.1-affine-atlas-compat.jar
```

Keep the existing `-Dscalafim.javafx.affineAtlas=true` policy and all other
runtime inputs unchanged. A consumer must explicitly incorporate this new
artifact and qualify its resulting runtime closure before publication.

## Evidence

The independent public control had 41,328 black interior pixels and zero
expected-color pixels on stock and admitted JavaFX. The repaired component
had 41,328 exact expected-color pixels and zero black pixels in both frames.
Both unchanged consumer transaction probes (pre-optimization `5ba8f266` and
optimized `1cb1440f`) passed with the repair.

Rendering correctness does not establish the separate 4 ms publication gate.
The repaired optimized-consumer diagnostics observed 6.076625 ms and 7.444292 ms
maximum retained publications; these adverse observations are retained. No timing threshold
was changed. See `verification.json` for bounded local evidence and artifact
hashes; no hosted CI, publication, or deployment is implied.

Upstream source: [OpenJFX MeshUtil](https://github.com/openjdk/jfx24u/blob/e998e3105951459e3076026242df8c31e2775a49/modules/javafx.graphics/src/main/java/com/sun/prism/impl/MeshUtil.java).

The source-level `surfaceViewJVM/test` and `surfaceViewJS/test` gates both stop
at 55 errors for missing pinned Intaglio APIs (`ScalarMapping`, `LegendTitle`,
`ScalarMappingError`) in unchanged Compile sources. The JavaFX module gate was
not reached. These results are not represented as passing Scala module tests.
The final reproducer was separately compiled with Scala 3.7.4 and `-Werror`.
Native picking passed 28 cases and 30,479 intersections against the raster
oracle (maximum barycentric error 8.692281e-8); see the recorded constructor
compatibility adaptation in `verification.json`.

`plsneuro-integration.patch` incorporates the repair into the consumer's
existing affine-atlas component and manifest. It was apply-checked and rebuilt
offline. All seven pre-existing native classes are byte-identical; only
`MeshUtil.class` is added. This avoids changing unrelated provider behavior.
The integration patch also updates both consumer hash pins to the exact new
component and requires material, shader, and tangent classes to share that
verified origin. The original guard's rejection of the new component is
retained as adverse evidence. The updated guard accepts the repaired component,
rejects the old component, and passes the original native consumer transaction
probe. The rebuilt component and receipts are in the evidence directory recorded
in `verification.json`. Applying the patch to the consumer checkout and updating
its application runtime receipt remain explicit adoption steps.

## Pull request scope

This self-contained runtime repair is based on current ScalaFIM `main`; it
changes no compiled Scala library code or default runtime. The consumer-facing
qualification above used the frozen provider candidate `fb53f187c` and admitted
consumer closures. In particular, the recorded 55-error JVM/JS source builds
refer to that historical candidate, not to this PR's `main` base.

`verification/JavaFxEmissionReproducer.scala` preserves the exact provider
specimen used for qualification. It uses retained-atlas APIs from the frozen
candidate that are not yet in `main`, so it is retained outside module source
roots. The standalone Java controls and patch builder have no ScalaFIM or
consumer classpath dependency. Consumer adoption changes are supplied as an
explicit patch; this PR does not apply them to another repository.
