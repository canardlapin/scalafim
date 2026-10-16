# Independent source review

Reviewer: `scene_v9_recovery`, read-only. Final verdict: approved.

The ambient light is a sibling of each surface's mesh views, so it stays inside
its mesh-owning SubScene through viewport reparenting. NativePhong Lit removes
it; Unlit and WorldVertexLambert keep it consistently with the native material.
No geometry, normal, UV, atlas or resource-key mutation was added.

The native regression uses the same public backend for Lit-to-Unlit restoration,
with three actual FX pulses before each next step. It checks mesh/atlas identity
and exact points, normals, faces and texture coordinates. No light injection,
normal replacement or diagnostic backend construction remains in the test.

Reviewed SHA-256 values:

- `JavaFxSurfaceProbe.scala`: `464d03aeac84e4386545799a68a00d0e6adb6c5a6669e712329f1d9affc78c05`
- `JavaFxSurfaceProbeSuite.scala`: `22e71f7a2a8958294cc2fcf8be19ca1d940a9deafff57f8af7228af8f8be1e94`
- `JavaFxFourFaceColorProbe.scala`: `9919c3d4d923aa6f6f8f21a86c1f21c981db3cbff3619c330ebbc23645efada6`

The parent checked these digests, actual exit receipts and the resulting lateral
image. The reviewer did not rerun tests. NativePhong Lit on tangent-degenerate
meshes remains an underlying JavaFX limitation outside this Unlit repair.
