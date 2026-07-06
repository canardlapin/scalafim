# Neurothresh-Style Spatial Inference

This plan maps the useful computational core in `~/code/neurothresh` into
ScalaFIM without porting the R package surface. The goal is a typed,
cross-platform thresholding layer for statistic maps: LR-MFT set scoring,
hierarchical/octree search, resampling-based FWER control, TFCE, cluster-FDR,
and baseline RFT where the assumptions are explicit.

## What neurothresh contributes

The durable shape is:

```text
statistic map + mask + optional priors/parcels + null draws -> spatial inference result
```

The first-class ideas worth preserving are:

- prior-weighted set statistics for focal and diffuse evidence;
- deterministic-by-index null draws consumed by several inference methods;
- Westfall-Young step-down and single-step maxT correction;
- octree and parcel-seeded region search for localized LR-MFT inference;
- TFCE as a transform plus max-null calibration;
- cluster-level FDR/FWER over connected components;
- statistic canonicalization to Z-equivalent maps.

The parts not worth carrying over directly are S3 result objects, list-shaped
method options, Rcpp-specific boundaries, `future.apply` orchestration, and
plot/report helpers in the core. Those should become typed values, pure kernels,
and later adapters.

## Home

Create a new cross-compiled module:

```text
modules/threshold
name := "scalafim-fmri-threshold"
package scalafim.fmri.threshold
dependsOn(image, linalg)
```

This should not live in `image`: masks, volumes, connected components, kernels,
and spatial indexing are data/geometry primitives, while neurothresh is
statistical inference over those primitives. It should not live in `group`:
the same thresholding methods apply to first-level contrast maps, group maps,
MVPA searchlight maps, surface/parcel reductions, and simulated fields.

Dependency direction matters. `threshold` can depend on `image` for
`NeuroVol`, `Mask`, `ClusteredNeuroVol`, `ConnComp`, `Kernel3D`, and
`NeuroSpace`, and on `linalg` for primitive matrix/vector helpers. It must not
depend on `group`; later `group` code can consume `threshold` results or share
generic multiple-testing helpers by moving those helpers down, not by creating a
cycle.

## Core types

The public API should start from existing ScalaFIM values instead of inventing a
new image model:

```scala
import scalafim.image.*
import scalafim.fmri.threshold.*

val z: NeuroVol[Double] = ???
val mask: NeuroVol[Boolean] = ???

val result =
  HierScan.run(
    stat = z,
    mask = Some(mask),
    config = HierScanConfig(alpha = Alpha.unsafe(0.05))
  )
```

Proposed nouns:

| Type | Role |
|---|---|
| `Alpha`, `QValue`, `Kappa`, `PermutationCount` | opaque checked scalars for user-facing inference settings. |
| `Tail` | closed enum: `Positive`, `Negative`, `TwoSided`. |
| `StatKind` / `CanonicalStat` | Z, t(df), and negative-log10-p canonicalization to Z-equivalent maps. |
| `MaskedField` | compact mask-space view of a `NeuroVol[Double]`: full indices, coordinates, values. |
| `PriorWeights` | checked mask-space priors, including uniform and mixed-prior construction. |
| `Region` | a stable id plus mask-space indices and optional bounding box/prior mass. |
| `RegionTree` / `RegionNode` | immutable octree or parcel-seeded hierarchy with scores and tests. |
| `EvidenceScore` | sealed score choice: `SoftMax(kappa)`, `Diffuse`, `Omnibus(kappaGrid)`. |
| `NullDraw` | `draw(b): Either[ThresholdError, Array[Double]]`, deterministic by permutation index. |
| `ThresholdResult` | typed output: method, reject mask, corrected p-values where available, threshold, clusters/regions, and params. |
| `ThresholdError` | error ADT for empty masks, shape mismatch, invalid alpha/q/kappa, non-finite maps, bad null draws, and unsupported method assumptions. |

`ThresholdResult` should keep method-specific payloads typed rather than using a
free-form map. Examples: `HierScanResult`, `TfceResult`, `ClusterFdrResult`, and
`RftResult` can share a small trait exposing `reject`, `pValues`, `mask`, and
`method`.

## Algorithms

### Set scoring

Port the core of `R/scoring.R` first:

- `ScoreSet.softMax(indices, z, priors, kappa)` using log-sum-exp;
- `ScoreSet.diffuse(indices, z, priors)` returning `score` and `effectiveN`;
- `ScoreSet.omnibus(indices, z, priors, kappaGrid)` returning the best channel.

Implementation should use primitive arrays and `while` loops in the inner path.
The public entry points can accept `Region`, `MaskedField`, and `PriorWeights`.

### Multiple testing

Port `R/stepdown.R` as a small pure kernel:

- `WestfallYoung.stepDown(observed, nullMatrix, alpha)`;
- `MaxT.singleStep(observed, nullMatrix, alpha)`;
- `MaxNull.pValues(observed, maxNull)` and `MaxNull.threshold(maxNull, alpha)`.

Use a compact row-major null matrix representation, probably `DoubleMatrix`, so
the implementation can stay portable and testable.

### Octree and hierarchical LR-MFT

`R/octree.R`, `R/hier_scan.R`, and the Rcpp helpers become an allocation-aware
Scala octree:

- build a `MaskedField` once from volume, mask, and tail;
- compute parent bounding boxes and split into up to eight non-empty children;
- keep child prior mass and variance constants with the split;
- score all children in one pass through the parent region;
- recurse with either Westfall-Young step-down or explicit alpha spending.

Parcel seeding should accept `ClusteredNeuroVol` or atlas-derived
`VolumeAtlas` output later. The core contract should only require a sequence of
initial `Region`s so atlas remains an adapter, not a dependency.

### TFCE

Port `R/tfce.R` after connected-component reuse is in place:

- `Tfce.transform(stat, mask, config)` using `ConnComp.connComp3D`;
- `Tfce.fwer(stat, nullDraws, config)` using max-null calibration;
- two-sided support by evaluating positive and negative tails and taking the
  voxelwise maximum.

The first implementation can use threshold stepping exactly like the R code.
Optimized sweep-line/component-update variants are a later performance slice.

### Cluster FDR/FWER and RFT

`cluster_fdr` is a natural next slice because `image` already has
`ClusteredNeuroVol.fromThreshold` and `ConnComp`. The portable core should
support:

- cluster-forming threshold by tail;
- cluster tables with id, extent, uncorrected p, adjusted p/q, and tail;
- BH/BY correction over clusters;
- permutation cluster p-values from `NullDraw`.

RFT peak/cluster methods are useful baselines, but they should land after the
permutation-backed methods. The API must make smoothness/FWHM and field
assumptions explicit instead of silently treating them as ordinary thresholds.

### Canonicalization and null factories

Canonicalization can live in `threshold` because it is an inference concern:

- `Z`: identity plus one/two-sided p-values;
- `T(df)`: probability integral transform to Z-equivalent values;
- `NegLog10P`: p-to-Z with optional sign map.

Null factories should be split:

- core `threshold`: `NullDraw`, validation, voxel sign-flip, Monte Carlo smooth
  field draws if they remain dependency-free;
- adapters later: subject sign-flip, two-sample label permutation, and GLM nulls
  from `dataset`, `fit`, or `group` values.

## First Implementation Slice

1. Add `modules/threshold` to `build.sbt`, root aggregates, `compileAll`, and
   `testAll`.
2. Add README and core ADTs: `Alpha`, `QValue`, `Kappa`, `Tail`,
   `ThresholdError`, `MaskedField`, `PriorWeights`, and `ThresholdResult`.
3. Implement set scoring and Westfall-Young/maxT kernels.
4. Add octree split and bounding-box utilities over mask-space indices.
5. Add focused JVM+JS tests for invariants, edge cases, and hand-computable
   examples.
6. Add an R parity fixture generator under `tools/r-parity/` using
   `~/code/neurothresh`, then fixture-backed tests for scoring, stepdown, and
   octree splits.

That slice gives `group` and `fit` a real target without committing to the full
neurothresh method family all at once.

## Later Slices

1. `HierScan.run` with whole-mask octree descent and optional parcel seeds.
2. TFCE transform and permutation FWER.
3. Cluster-FDR and permutation cluster p-values.
4. RFT peak/cluster baselines with explicit smoothness types.
5. Canonicalization to Z-equivalent volumes.
6. LISA bilateral filtering and scale-space localized energy after smoothing
   and mask-normalized filtering are stable.
7. Surface/parcel adapters once `surface` and `atlas` payload integration is
   mature.
8. JVM-only acceleration or streaming null evaluation only after the portable
   kernels are tested.

## Parity and Verification

The R reference files to anchor first are:

- `~/code/neurothresh/R/scoring.R`
- `~/code/neurothresh/R/stepdown.R`
- `~/code/neurothresh/R/octree.R`
- `~/code/neurothresh/R/hier_scan.R`
- `~/code/neurothresh/R/tfce.R`
- `~/code/neurothresh/R/cluster_fdr.R`
- `~/code/neurothresh/R/canonicalize.R`

Every shared feature must pass both platforms:

```sh
sbt thresholdJVM/test
sbt thresholdJS/test
sbt testAll
```

Acceptance criteria for the first slice:

- no JVM-only dependencies in shared code;
- invalid settings and shape mismatches return `ThresholdError`;
- scoring and stepdown fixtures match R within explicit tolerances;
- octree split fixtures match R child assignment and bounding boxes;
- all public constructors preserve valid instances after construction;
- no plotting, reporting, file IO, or worker orchestration in the core module.
