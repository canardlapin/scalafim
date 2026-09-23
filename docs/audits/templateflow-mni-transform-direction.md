# Open finding: TemplateFlow MNI152NLin6Asym ↔ MNI152NLin2009cAsym transform direction

- **Status:** open. Recorded 2026-09-23 for further investigation. **Not reported
  upstream**; that is held by maintainer decision.
- **Affects:** any volume→surface or point route that uses TemplateFlow's
  6Asym ↔ 2009c transforms. In particular, the fsLR 32k route for
  MNI152NLin2009cAsym group results (`bd-01M35BHNDCHM6YXCKYX0544TP3`,
  `docs/plans/fslr32k-exact-route-blockers.md`).
- **Mirror:** the same finding is recorded in templateflow4s, which quarantines
  the suspect asset.

## Summary

TemplateFlow publishes two ITK composite transforms whose names say they run in
opposite directions. Measured as point maps, **both behave as
MNI152NLin2009cAsym → MNI152NLin6Asym**.

- **`tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5`** is
  consistent with its name. A `mode-image` transform resamples 6Asym images onto
  2009c, so as a point map it takes 2009c points to 6Asym points.
- **`tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5`** should be
  the reverse, a 6Asym → 2009c point map. It is not. It moves points the same way
  as its twin.

Using the second file to move 6Asym-frame geometry (e.g. fsLR surfaces) into
2009c would displace it *away* from 2009c anatomy, by 1.6 mm at the median and
up to about 4.8 mm.

## Inputs

All inputs are from the TemplateFlow S3 archive, fetched 2026-09-23.

| File | SHA-256 |
|---|---|
| `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5` | `2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe` |
| `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5` | `2a19853bc99ebd1d711d230ea4a4dc4db3b9a60f8b50b9181f93b44036dfbd0f` |
| `tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii` | `036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af` |
| `tpl-fsLR_den-32k_hemi-R_midthickness.surf.gii` | `9d2cef05096c433b134870456abebe7ff201cdda60ce5741d7b794018eeccda7` |
| `tpl-fsLR_hemi-L_den-32k_desc-nomedialwall_dparc.label.gii` | `4ac9199dab151ccdc2a35bdddb5bac4f4907da7dfebd90807c09b82e2eb9d512` |
| `tpl-fsLR_hemi-R_den-32k_desc-nomedialwall_dparc.label.gii` | `698f46b399f5a89829f83cc697e32dc8841031fbf31ffef75aaaa0c4a16c3015` |
| `tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz` | `662b18e83dddc554b19c621d9750af3454b54d4e103df03633eacced3884805a` |

Tools: Python 3.14.7, nibabel 5.4.2, nitransforms 25.1.0, h5py 3.16.0,
numpy 2.5.3.

nitransforms interpolates the displacement field with cubic B-splines in float32,
whereas ITK interpolates trilinearly. On these vertices it differs from SimpleITK
2.5.6 (reference ITK semantics) by at most 0.04 mm. That is two orders of
magnitude below the effects reported here, so the conclusions do not depend on
the choice of tool. Qualification oracles nevertheless use SimpleITK.

To reproduce, run:

```
python tools/fslr-qualification/transform_direction_evidence.py --assets DIR
```

## Evidence

### 1. Structure

Both files are `CompositeTransform_double_3_3` with two stages:
`AffineTransform_double_3_3` (12 parameters) and
`DisplacementFieldTransform_double_3_3` (25 590 063 parameters).

- **Affine centre:** identical fixed parameters in both, (0.0693, 19.2088, 2.15).
- **Affine matrices:** diagonals ≈ (0.984, 0.986, 0.969) and
  (0.985, 0.986, 0.972). They are nearly the same, not reciprocal. An inverse
  pair would show ≈0.985 against ≈1.015.
- **Displacement grid:** identical in both files: 193×229×193, LPS origin
  (96, 132, −78), spacing 1 mm, direction diag(−1, −1, 1). This is the 2009c
  res-01 grid.
- **Dataset names:** the parameter datasets are spelled `TranformParameters` and
  `TranformFixedParameters` (sic).

### 2. Composition

The two files are composed on the 64 984 fsLR 32k midthickness vertices, the
reverse-named map applied after the forward-named map.

| Quantity | Median | p95 | p99 | Max |
|---|---|---|---|---|
| One-way displacement (mm) | 1.50–1.62 | 2.87–3.05 | — | 4.64–4.82 |
| Round-trip error (mm) | 2.99 | — | 6.57 | 8.10 |

The round-trip error is about twice the one-way displacement. The two maps
compound instead of cancelling.

### 3. Anatomy (direction control)

One instrument scores every candidate placement: the 2009c res-01 GM
probability map, sampled by nearest voxel at the 59 412 cortical vertices (the
medial wall is excluded by `desc-nomedialwall`).

| Placement of fsLR vertices | Mean GM p | Fraction p > 0.5 |
|---|---|---|
| Raw (hypothesis: already in 2009c) | 0.670 | 0.743 |
| `2009cAsym_from-6Asym`, applied as a point map | 0.618 | 0.675 |
| `6Asym_from-2009cAsym`, applied as a point map | 0.616 | 0.672 |
| `2009cAsym_from-6Asym`, **numerically inverted** | **0.703** | **0.794** |
| `6Asym_from-2009cAsym`, **numerically inverted** | **0.705** | **0.799** |

The inversion is a fixed-point solve of T(y) = x, 12 iterations. The residual is
≤ 0.0004 mm and ≤ 0.003 mm respectively.

Interpretation:

1. The fsLR surfaces are in MNI152NLin6Asym. Moving them into 2009c with the
   correct map improves the GM fit over leaving them in place.
2. Both files move points the same way: applied directly, both make the fit
   worse, and inverted, both make it better.

This is consistent with the structural evidence in section 1.

## How ScalaFIM uses this

- **Transform file.** Routes that move 6Asym-frame geometry into 2009c use
  `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym`. Its behaviour matches its
  name. It is applied through its **per-vertex inverse**, with the residual
  recorded as evidence.
- **Direction control.** The GM-map test above is a qualification gate. The
  bridged placement must beat raw placement, and the direction-reversed
  placement must fail.
- **Quarantine.** `tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym` is not used.
  templateflow4s quarantines it until the questions below are resolved.

## Open questions (for the later investigation)

1. Is the `6Asym_from-2009cAsym` file a mislabelled copy, or a re-run of the
   same registration direction? Its OS/ITK build metadata differs from its
   twin's (a different kernel string), so it is not a byte copy.
2. Do ANTs/ITK consumers (`antsApplyTransforms` / `antsApplyTransformsToPoints`)
   behave identically? This needs checking with ANTs itself, not only
   nitransforms.
3. Does a documented TemplateFlow or ANTs convention explain the naming? For
   example, is the file intended for a different application mode?
4. Are the other `_from-` pairs in TemplateFlow affected?
