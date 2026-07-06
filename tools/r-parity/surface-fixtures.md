# Surface Fixture Parity Notes

The surface module uses small synthetic fixtures instead of bundling large
neuroimaging assets. The fixtures are derived from neurosurf concepts, not from
licensed data files.

| Fixture | Location | Purpose |
| --- | --- | --- |
| Tetrahedron | `SurfaceTestFixtures.tetraMesh` | Closed mesh for core geometry, topology, and geodesic distances. |
| Two-triangle sheet | `SurfaceTestFixtures.sheetGeometry` | Contiguous parcels and boundary-contact counts. |
| Disconnected triangles | `SurfaceTestFixtures.disconnectedGeometry` | Fragmented parcel policies and unreachable geodesics. |
| Spherical points | `SurfaceTestFixtures.sphericalTopology` | Spherical distance clamping and finite great-circle distances. |
| Skewed star | `SurfaceTestFixtures.skewedCentroidGeometry` | Divergence between Euclidean centroid vertex and geodesic medoid. |
| FreeSurfer ASCII | `modules/surface/jvm/src/test/resources/surface/mini_lh_smoothwm.asc` | SUMA/FreeSurfer-style ASCII geometry with 0-based faces. |
| GIFTI ASCII | `modules/surface/jvm/src/test/resources/surface/tetra_lh_midthickness.surf.gii` | POINTSET/TRIANGLE arrays plus POINTSET affine extraction. |
