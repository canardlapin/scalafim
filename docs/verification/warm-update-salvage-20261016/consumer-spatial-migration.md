# Consumer spatial migration contract

A read-only independent review on 2026-10-16 confirmed a scientific identity boundary, not merely renamed image APIs. The reviewer inspected PLS `2e0aac8e90300fd5a87d5b0d79d3bb9b78ecd1bf`, ScalaFIM recovery `bf202d75b83f94297083947db47518b18480c320`, and the original provider `7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4`. No execution evidence is claimed for this review.

Old native indices use `x + dx*y + dx*dy*z`; current canonical ordinals use `(x*dy+y)*dz+z`. The invariant is that every saved feature continues to denote the same voxel coordinate, physical point, and value. A permutation can preserve numerical summaries while corrupting spatial interpretation.

## Required implementation

- Keep legacy `grid-v1` identities, selections, numerical payloads and inspection `participant-source.v3` fingerprints unchanged. Result v1–v8 and inspection v1 decoding must retain their original validations and checksums.
- Add an explicit legacy/canonical geometry encoding and checked feature-index ↔ canonical-ordinal conversions in `ResultGeometry`. Preserve support in feature-selection order; never sort it. Centralize scatter/gather there.
- New results need `grid-v2` based on `GridDomain.canonicalDomainKey(shape.grid)`, including the full fingerprint. Retain and serialize admitted frame/world identity, exact affine bits, dimensions, unit, convention and layout. Do not reconstruct new geometry solely from an analysis label and affine.
- Add result v9 and inspection v2 with explicit optional-capability handling. Numeric version comparisons alone would incorrectly require every v9 result to contain effects. New inspection fingerprints must include the new geometry identity.
- Legacy files lack anatomical world evidence. Give each restored legacy geometry one fresh unresolved world, reused by its related runtime maps. Keep the old analysis label for presentation/checksums. It cannot authorize overlay, template equivalence or cross-artifact alignment.
- `SpatialResultMap.includedVoxels` remains native canonical support. Convert explicitly at `ParticipantExplorerData.map/cohortMap`, `RegionExtractor`, `ParticipantSimilaritySource.order`, cursor-to-profile lookup in `ParticipantExplorerPane`, `FirProfilePane`, surface masks and reverse lookup. Inspect any saved view/region indices too.
- Version spatial compatibility/cache receipts that currently omit world identity. Preserve refusal when two equal-looking grids have different declared worlds.

Current provider constructors are `SomeNeuroVolume.copyFromCanonicalArray` and `NeuroVolume.copyContinuousFromCanonicalArray`; the old public `NeuroVol` alias must not be restored to obscure this boundary.

## Discriminating acceptance evidence

Freeze a legacy artifact on a 2×3×4 grid with a nontrivial affine and ordered support `[1, 6, 4, 23]`. Its expected canonical support is `[12, 1, 8, 23]`. Attach distinct values and check coordinates, physical points, excluded NaNs, cursor profiles, region extraction and surface masks. Existing saved-result fixtures are all 3×1×1 and do not discriminate these conventions.

Check unchanged legacy payload digests, grid identity, inspection fingerprint, selections and values; retain corruption/mismatch refusals. Reserialization does not promise a byte-identical ZIP archive. New world/grid records must roundtrip across JVM processes; changing frame identity or layout must fail alignment. NIfTI tests need independently specified disk values to detect accidental double permutation.

This document specifies pending consumer work. It does not admit a new provider pin or establish consumer qualification.
