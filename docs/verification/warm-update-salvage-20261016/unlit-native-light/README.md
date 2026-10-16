# JavaFX unlit surface repair — 2026-10-16

The strengthened PLSNeuro transaction probe exposed a black finite scalar map.
A direct ScalaFIM reproduction at source `0c1205e12c26ce25e61f5c3faa85728304205af9`
then reproduced the failure on stock OpenJFX 21.0.5, without PLSNeuro.
The texture, source normals, UVs and material binding were valid.

The repair gives each self-illuminated mesh group an explicit ambient light.
This suppresses JavaFX's automatic point light inside the actual mesh-owning
SubScene. The light moves with its mesh group through viewport reparenting.
NativePhong Lit removes it; Unlit restores it. WorldVertexLambert retains it
because its lighting is already computed in world coordinates and its native
material remains self-illuminated. Source geometry, normals, UVs, atlas data,
scientific lighting policy and resource keys are unchanged.

## Diagnosis and controls

A four-face tetrahedron and a four-face planar fan use the same 16x4 varying-color
LegacyTriangle atlas. Lateral tetra rendering was black, while dorsal tetra and
correctly viewed dorsal planar rendering were colored. Different faces were
visible: the lateral camera exposes an axis-aligned face, while the dorsal
camera exposes the sloped face. This was not a singular camera matrix.

Direct invocation of stock OpenJFX 24.0.1's `MeshUtil.computeTBNNormalized`,
`fixTSpace` and `buildQuat` reproduced six non-finite face-corner quaternions on
the three coordinate faces, and finite quaternions on the sloped face. The
supplied area-weighted vertex normals can be parallel to a generated tangent
or bitangent. `fixTSpace` projects that vector to zero and divides by its length.
Finite input normals and a nonzero UV determinant do not prevent this case.
The standalone Java reproduction and output are retained in the manifest.

Controls retained, including adverse and invalid evidence:

- Stock OpenJFX 21 and the frozen OpenJFX 24 runtime both render the lateral
  tetra black. Opting into the existing mipmap/centroid patch does not repair it.
- The first planar controls were edge-on and entirely white; they are invalid
  evidence about texture sampling. The corrected dorsal planar control passes.
- Replacing only the native normals with one uniform diagonal normal passes;
  this was diagnostic only and is absent from the repair.
- Adding an ambient light to the outer composition SubScene does not reach
  the nested 3D SubScene. That negative control has invalid light scope.
- Adding the ambient light inside the mesh-owning SubScene passes initial,
  retained-color and camera-only checks with the original normals unchanged.

Claude Opus 5.5 was consulted with a standalone synthetic example and no
repository access. Its returned model identity was `claude-opus-5-5`.
It independently identified the face-visibility/tangent-degeneracy explanation.
The response is advice, not acceptance evidence. Its suggestion that these
normals might be an indexing error does not apply here: they are the compiler's
area-weighted vertex normals. No flat-normal substitution or normal perturbation
was adopted. The synthetic question and response are preserved.

## Verification

The first unit attempt passed 65/66 tests. The new two-surface fixture had not
selected bilateral layout and compiled only its initial single surface. After
that fixture correction, the JavaFX suite passes 66/66, shared surface JVM
279/279 and shared surface Scala.js 279/279. Both attempts and exit metadata are
retained. Production compilation is warning-clean; the raw logs retain the
existing JDK native-access/Unsafe runtime notices.

The final native regression covers the public backend's lateral tetra, dorsal
tetra and dorsal planar rendering, retained recoloring, camera-only changes,
and material restoration. All three cases pass, including three real JavaFX pulses in Lit before
restoring Unlit on the same backend. The existing correctness probe passes
(mask IoU 0.997911, mean interior channel error 1.207267, landmark error 0), and
the existing composition probe passes 4,144 scientific identity picks.
[Source digests](source-digests.json) bind these development gates to the exact
three source files. Consumer acceptance remains separate and pending.

The underlying stock JavaFX tangent conversion is unchanged. This repair makes
the self-illuminated path independent of its automatic point-light calculation;
it does not qualify degenerate meshes under native Phong lighting. No patched
JavaFX runtime is required for the repaired path.

Raw evidence is gzip-compressed with hashes of both raw and compressed bytes in
[manifest.json](manifest.json). Development diagnostics are not fresh-checkout
consumer admission receipts.
