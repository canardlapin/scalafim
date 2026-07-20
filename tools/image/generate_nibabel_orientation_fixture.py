#!/usr/bin/env python3
"""Generate the independent NIfTI reslicing oracle used by NiftiResliceOracleSuite.

The checked-in NIfTI is deliberately oblique, anisotropic, sheared, reflected,
and has conflicting qform/sform transforms. Expected pixels are produced by
nibabel.processing.resample_from_to after reloading the persisted file, so the
Scala test does not derive its answers from ScalaFIM geometry code.
"""

from __future__ import annotations

import csv
import itertools
from pathlib import Path

import nibabel as nib
import numpy as np
from nibabel.processing import resample_from_to


SHAPE = (4, 5, 6)
SPACING = (0.9, 1.1)
AFFINE = np.array(
    [
        [0.25, -1.60, 0.30, 12.0],
        [1.10, 0.20, 0.15, -7.0],
        [0.10, 0.35, -2.20, 20.0],
        [0.00, 0.00, 0.00, 1.0],
    ],
    dtype=np.float64,
)
QFORM = np.array(
    [
        [1.2, 0.0, 0.0, -50.0],
        [0.0, 1.3, 0.0, 60.0],
        [0.0, 0.0, 1.4, -70.0],
        [0.0, 0.0, 0.0, 1.0],
    ],
    dtype=np.float64,
)
PLANES = {
    "Sagittal": (np.array([0.0, -1.0, 0.0]), np.array([0.0, 0.0, 1.0])),
    "Coronal": (np.array([1.0, 0.0, 0.0]), np.array([0.0, 0.0, 1.0])),
    "Axial": (np.array([1.0, 0.0, 0.0]), np.array([0.0, 1.0, 0.0])),
}
CONVENTIONS = ("PatientLeftOnLeft", "PatientRightOnLeft")


def source_values() -> np.ndarray:
    values = np.empty(SHAPE, dtype=np.float64)
    for x, y, z in itertools.product(*(range(size) for size in SHAPE)):
        values[x, y, z] = 0.25 + 100.0 * x + 10.0 * y + z
    return values


def boundary_corners(affine: np.ndarray) -> np.ndarray:
    corners = []
    bounds = [(-0.5, size - 0.5) for size in SHAPE]
    for x, y, z in itertools.product(*bounds):
        corners.append(nib.affines.apply_affine(affine, (x, y, z)))
    return np.asarray(corners)


def target_grid(
    affine: np.ndarray,
    cursor: np.ndarray,
    plane: str,
    convention: str,
) -> tuple[tuple[int, int, int], np.ndarray]:
    screen_right, screen_up = PLANES[plane]
    if plane != "Sagittal" and convention == "PatientRightOnLeft":
        screen_right = -screen_right

    corners = boundary_corners(affine)
    offsets = corners - cursor
    horizontal = offsets @ screen_right
    vertical = offsets @ screen_up
    width = max(1, int(np.ceil((horizontal.max() - horizontal.min()) / SPACING[0])))
    height = max(1, int(np.ceil((vertical.max() - vertical.min()) / SPACING[1])))
    top_left = (
        cursor
        + screen_right * (horizontal.min() + 0.5 * SPACING[0])
        + screen_up * (vertical.max() - 0.5 * SPACING[1])
    )
    target_affine = np.eye(4, dtype=np.float64)
    target_affine[:3, 0] = screen_right * SPACING[0]
    target_affine[:3, 1] = -screen_up * SPACING[1]
    target_affine[:3, 2] = np.cross(screen_right, screen_up)
    target_affine[:3, 3] = top_left
    return (width, height, 1), target_affine


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    output = root / "modules/image/jvm/src/test/resources/scalafim/image/io"
    output.mkdir(parents=True, exist_ok=True)
    nifti_path = output / "nibabel-oblique.nii"
    table_path = output / "nibabel-oblique-slices.tsv"

    image = nib.Nifti1Image(source_values(), AFFINE)
    image.set_qform(QFORM, code=1)
    image.set_sform(AFFINE, code=2)
    nib.save(image, nifti_path)

    persisted = nib.load(nifti_path)
    cursor = nib.affines.apply_affine(persisted.affine, (1.25, 2.10, 2.60))
    axis_codes = nib.aff2axcodes(persisted.affine)
    axis_code_fields = "\t".join(axis_codes)

    with table_path.open("w", newline="", encoding="utf-8") as handle:
        handle.write(f"# generator\tnibabel-{nib.__version__}\n")
        handle.write(f"# source_axis_codes\t{axis_code_fields}\n")
        handle.write(f"# cursor_world\t{cursor[0]:.17g}\t{cursor[1]:.17g}\t{cursor[2]:.17g}\n")
        handle.write(f"# pixel_spacing\t{SPACING[0]:.17g}\t{SPACING[1]:.17g}\n")
        writer = csv.writer(handle, delimiter="\t", lineterminator="\n")
        writer.writerow(
            (
                "plane",
                "convention",
                "width",
                "height",
                "column",
                "row",
                "world_x",
                "world_y",
                "world_z",
                "nearest",
                "linear",
            )
        )
        for plane in PLANES:
            for convention in CONVENTIONS:
                shape, target_affine = target_grid(persisted.affine, cursor, plane, convention)
                nearest = resample_from_to(
                    persisted,
                    (shape, target_affine),
                    order=0,
                    mode="grid-constant",
                    cval=-1.0,
                ).get_fdata(dtype=np.float64)
                linear = resample_from_to(
                    persisted,
                    (shape, target_affine),
                    order=1,
                    mode="grid-constant",
                    cval=-1.0,
                ).get_fdata(dtype=np.float64)
                for row in range(shape[1]):
                    for column in range(shape[0]):
                        world = nib.affines.apply_affine(target_affine, (column, row, 0.0))
                        writer.writerow(
                            (
                                plane,
                                convention,
                                shape[0],
                                shape[1],
                                column,
                                row,
                                f"{world[0]:.17g}",
                                f"{world[1]:.17g}",
                                f"{world[2]:.17g}",
                                f"{nearest[column, row, 0]:.17g}",
                                f"{linear[column, row, 0]:.17g}",
                            )
                        )

    print(f"wrote {nifti_path.relative_to(root)}")
    print(f"wrote {table_path.relative_to(root)}")
    print(f"axis codes: {axis_codes}")
    print(f"cursor: {tuple(cursor)}")


if __name__ == "__main__":
    main()
