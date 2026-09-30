#!/usr/bin/env python3
"""Freeze independent native-inverse coordinate tables for the STP U6 court.

Run with:
  uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with numpy==2.5.3 \
    --with fslpy==3.29.1 python tools/transform/generate_native_inverse_coordinate_oracles.py

The ANTs field stores LPS vector components while NIfTI affines are RAS.  The
table deliberately exposes both sides of that conversion.  FSL is decoded by
fslpy's documented ``readFnirt`` API with the original target image as both
source and reference, because this control has coincident endpoint geometry.
"""

from __future__ import annotations

import hashlib
import json
from pathlib import Path

import SimpleITK as sitk
import nibabel as nib
import numpy as np
from fsl.transform.fnirt import readFnirt
from fsl.data.image import Image as FslImage

import oracle_common as oc


ROOT = Path(__file__).resolve().parents[2]
ORACLE = ROOT / "modules/transform/shared/src/test/resources/scalafim/transform/oracle"
OUT = ORACLE / "native_inverse_coordinates"
ANTS = ORACLE / "ants_native"
FSL = ORACLE / "fsl6_native"
FSL_TARGET = ORACLE / "neurotransform/fsl_dense_oracle/left_left_relative/target.nii.gz"
CHARACTERIZATION = Path("/private/tmp/scalafim-stp-finish-20260929/ants-native-inverse-characterization.json")


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def cropped_nodes(image: nib.spatialimages.SpatialImage) -> np.ndarray:
    shape = image.shape[:3]
    indices = np.stack(np.meshgrid(*(np.arange(2, n - 2) for n in shape), indexing="ij"), axis=-1)
    return indices.reshape(-1, 3).astype(np.float64)


def off_node_indices(shape: tuple[int, int, int]) -> np.ndarray:
    # One-voxel interior support, with non-integral coordinates and probes near
    # every face. These are fixed independently of the random fixture producer.
    lo = np.array([1.0, 1.0, 1.0])
    hi = np.asarray(shape, dtype=np.float64) - 2.0
    return np.array([
        [1.25, 2.5, 3.75], [hi[0] - .25, 2.4, 4.1],
        [2.2, hi[1] - .35, 3.3], [2.1, 3.2, hi[2] - .45],
        [1.4, hi[1] - .6, hi[2] - .7], [hi[0] - .7, 1.5, hi[2] - .8],
        [hi[0] - .8, hi[1] - .9, 1.6], [3.125, 4.375, 5.625],
        [4.2, 6.6, 7.1], [5.7, 3.3, 8.2], [6.4, 7.3, 2.8], [7.6, 5.2, 9.4],
    ], dtype=np.float64)


def ras(image: nib.spatialimages.SpatialImage, voxels: np.ndarray) -> np.ndarray:
    return nib.affines.apply_affine(image.affine, voxels)


def ants_transform(field_path: Path) -> sitk.DisplacementFieldTransform:
    image = nib.load(str(field_path))
    values = np.asarray(image.dataobj, dtype=np.float64)
    if values.shape[-2:] != (1, 3):
        raise RuntimeError(f"unexpected ANTs field shape {values.shape}")
    # nibabel gives x,y,z,1,component. SimpleITK wants z,y,x,component.
    field = sitk.GetImageFromArray(values[..., 0, :].transpose(2, 1, 0, 3), isVector=True)
    # NIfTI affine is RAS; ANTs displacement images use LPS physical space.
    lps_affine = np.diag([-1.0, -1.0, 1.0, 1.0]) @ image.affine
    linear = lps_affine[:3, :3]
    spacing = np.linalg.norm(linear, axis=0)
    direction = linear / spacing
    field.SetOrigin(tuple(lps_affine[:3, 3]))
    field.SetSpacing(tuple(spacing))
    field.SetDirection(tuple(direction.ravel()))
    return sitk.DisplacementFieldTransform(field)


def ants_rows(indices: np.ndarray) -> list[list[float]]:
    moving = nib.load(str(ANTS / "moving.nii.gz"))
    points_ras = ras(moving, indices)
    transform = ants_transform(ANTS / "syn_0InverseWarp.nii.gz")
    points_lps = points_ras * np.array([-1.0, -1.0, 1.0])
    mapped_lps = np.asarray([transform.TransformPoint(tuple(point)) for point in points_lps])
    mapped_ras = mapped_lps * np.array([-1.0, -1.0, 1.0])
    return [[*index, *before, *after] for index, before, after in zip(indices, points_ras, mapped_ras)]


def fsl_rows(indices: np.ndarray) -> list[list[float]]:
    target = nib.load(str(FSL_TARGET))
    points_ras = ras(target, indices)
    # This native invwarp is intentionally the coincident-geometry control.
    fsl_target = FslImage(str(FSL_TARGET))
    inverse = readFnirt(str(FSL / "invwarp_dense_matched_geometry.nii.gz"), fsl_target, fsl_target, defType="relative")
    # ``DeformationField.transform`` deliberately rounds to a field voxel.
    # That is a correct native-node query, but the fixed off-node probes need
    # the same trilinear field evaluation as the FNIRT dense field.  Keep
    # fslpy's readFnirt decode and its FSL/world matrices authoritative, then
    # interpolate its decoded relative FSL displacement explicitly.
    fsl = nib.affines.apply_affine(fsl_target.getAffine("voxel", "fsl"), indices)

    def trilinear(values: np.ndarray, coordinate: np.ndarray) -> np.ndarray:
        lower = np.floor(coordinate).astype(int)
        fraction = coordinate - lower
        upper = lower + 1
        if np.any(lower < 0) or np.any(upper >= np.asarray(values.shape[:3])):
            raise RuntimeError(f"off-node probe outside trilinear field support: {coordinate}")
        result = np.zeros(3, dtype=np.float64)
        for dx in (0, 1):
            for dy in (0, 1):
                for dz in (0, 1):
                    weight = (fraction[0] if dx else 1 - fraction[0]) * (fraction[1] if dy else 1 - fraction[1]) * (fraction[2] if dz else 1 - fraction[2])
                    result += weight * inverse.data[lower[0] + dx, lower[1] + dy, lower[2] + dz, :]
        return result

    mapped_fsl = np.asarray([coordinate + trilinear(inverse.data, voxel) for coordinate, voxel in zip(fsl, indices)])
    mapped_ras = nib.affines.apply_affine(fsl_target.getAffine("fsl", "world"), mapped_fsl)
    if not np.isfinite(mapped_ras).all():
        raise RuntimeError("fslpy returned non-finite coordinates for declared probes")
    return [[*index, *before, *after] for index, before, after in zip(indices, points_ras, mapped_ras)]


def main() -> None:
    OUT.mkdir(parents=True, exist_ok=True)
    ants_image = nib.load(str(ANTS / "moving.nii.gz"))
    fsl_image = nib.load(str(FSL_TARGET))
    ants_nodes = cropped_nodes(ants_image)
    fsl_nodes = cropped_nodes(fsl_image)
    ants_off = off_node_indices(ants_image.shape[:3])
    fsl_off = off_node_indices(fsl_image.shape[:3])
    header = ["i", "j", "k", "x_ras", "y_ras", "z_ras", "native_x_ras", "native_y_ras", "native_z_ras"]
    oc.write_table(str(OUT / "ants_inverse_nodes.tsv"), header, ants_rows(ants_nodes))
    oc.write_table(str(OUT / "ants_inverse_off_nodes.tsv"), header, ants_rows(ants_off))
    oc.write_table(str(OUT / "fsl_inverse_nodes.tsv"), header, fsl_rows(fsl_nodes))
    oc.write_table(str(OUT / "fsl_inverse_off_nodes.tsv"), header, fsl_rows(fsl_off))
    characterization = json.loads(CHARACTERIZATION.read_text())
    expected = {"ants_original_cropped_nodes": len(ants_nodes), "fsl_original_cropped_nodes": len(fsl_nodes), "off_node_probes_per_tool": len(ants_off)}
    payload = {
        "kind": "reference-implementation",
        "tools": {"SimpleITK": sitk.Version_VersionString(), "nibabel": nib.__version__, "numpy": np.__version__, "fslpy": "3.29.1"},
        "expected_counts": expected,
        "ants_coordinate_convention": "NIfTI affine/query table are RAS; ANTs vector field and SimpleITK transform are LPS; both query and result are flipped on x,y before table storage.",
        "fsl_coordinate_convention": "fslpy readFnirt(invwarp, src=target, ref=target, defType=relative) decodes the native relative FSL field. Node values are float32 fixture values accumulated in float64 trilinear arithmetic before fslpy's FSL-to-world matrix; this is needed because fslpy DeformationField.transform intentionally rounds its query to a voxel.",
        "input_sha256": {"ants/syn_0Warp.nii.gz": sha256(ANTS / "syn_0Warp.nii.gz"), "ants/syn_0InverseWarp.nii.gz": sha256(ANTS / "syn_0InverseWarp.nii.gz"), "fsl/invwarp_dense_matched_geometry.nii.gz": sha256(FSL / "invwarp_dense_matched_geometry.nii.gz"), "fsl/target.nii.gz": sha256(FSL_TARGET)},
        "ants_native_inverse_composition_characterization": characterization,
    }
    (OUT / "metadata.json").write_text(json.dumps(payload, indent=2) + "\n")
    oc.write_manifest(str(OUT), generator=__file__, tools=payload["tools"], commands=["uv run --with SimpleITK==2.5.6 --with nibabel==5.4.2 --with numpy==2.5.3 --with fslpy==3.29.1 python tools/transform/generate_native_inverse_coordinate_oracles.py"], notes="Independent native decoder tables. The ANTs pair's independently characterized native composition closure is recorded as evidence, not an estimate residual gate.", kind="reference-implementation")


if __name__ == "__main__":
    main()
