# Mesh lookup and sphere ownership

This is a bounded `eco-u3-kernels` slice. It does not close U3 or the eco epic.

| Owner | Responsibility |
| --- | --- |
| mesh4s geometry | Exact nearest-vertex lookup over a domain-owned point field; positive radial face lookup over an admitted realization; origin-centered sphere checks/projection. |
| mesh4s topology | Ordered connectivity and vertex/face domains; orientation, duplicate, boundary, disconnected and nonmanifold admission. No lookup repairs or renumbers inputs. |
| spatial4s | Coordinate frames and frame-owned query points. Sharing connectivity does not declare frame alignment. |
| ScalaFIM surface | Registered-coordinate interpretation, neuro defaults (radius 100, ratio tolerance 1.001), sparse plan construction, weight cutoff 1e-10, normalization, and nearest fallback when a valid realization has no radial face hit. |

Barycentric planning now refuses degenerate geometry through
`SurfaceError.InvalidGeometry`. Nearest planning does not require nondegenerate
faces. The upstream sphere projection retains its exact point-field owner and
frame. ScalaFIM still uses its existing raw-storage chart construction when
replacing mesh coordinates; a projection does not invent registration evidence.

Nearest ties select the lowest vertex ordinal. Radial lookup selects the greatest
minimum barycentric weight, then lowest face ordinal; it is not a first-hit
visibility query. Rays are normalized by maximum absolute component. The
normalized determinant refusal threshold is 1e-15. Weights down to -1e-6 are
admitted, clamped, and normalized. The BVH bounds include this boundary envelope.
A zero direction has no hit. Geometry tolerances are explicit here; template and
hemisphere policy remains downstream.

## Remaining work

- mesh4s `b9e0553` is published and public mesh-pin surface JVM/JS gates pass.
  Its candidate/local/tracking/remote SHA parity is recorded in the receipt.
- The preceding neuroatlas4s volume candidate remains separately awaiting its
  existing publication answer. Preserve its qualified checkout and receipts.
- Continue generic geodesic extraction under the existing `eco-u3-kernels` issue.
  Its custom nonnegative edge-cost contract is broader than the strict intrinsic
  triangle-metric contract. Decide the appropriate reusable solver owner without
  silently strengthening admissibility or duplicating a generic graph solver.
- The geodesic cache and repeated-target defects were subsequently reproduced
  and repaired at `78acb531d`; see the
  [geodesic correctness receipt](../geodesic-correctness-20261020/README.md).
  Retain those witnesses through the solver cutover.
- Complete the task's remaining acceptance and required independent review before
  declaring U3 or the eco epic complete.

No hosted CI, fresh native visual acceptance, speedup, or general allocation
claim is made. Existing smoothing scale/allocation output in the geometry log
qualifies smoothing only, not these new lookup APIs.
