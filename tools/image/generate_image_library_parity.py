#!/usr/bin/env python3
"""Generate the nibabel half of ScalaFIM's basic image-library parity fixture.

The fixture is deliberately asymmetric in every dimension.  Values encode their
logical (x, y, z, time) coordinate, which makes any NIfTI/Ravel storage-order
confusion visible.  Run the neuroim2 generator afterwards to obtain an
independent oracle over the exact persisted NIfTI file.

From the repository root:

  uv run --python 3.12 --with 'nibabel==5.2.1' --with 'numpy==2.2.6' \
    python tools/image/generate_image_library_parity.py
  Rscript tools/r-parity/generate_neuroim2_image_parity.R
"""

from __future__ import annotations

import csv
import itertools
from pathlib import Path

import nibabel as nib
import numpy as np


SHAPE = (2, 3, 4, 3)
TEMPORAL_SPACING_SECONDS = 1.5
AFFINE = np.array(
    [
        [0.0, -3.0, 0.0, 10.0],
        [2.0, 0.0, 0.0, -20.0],
        [0.0, 0.0, 4.0, 30.0],
        [0.0, 0.0, 0.0, 1.0],
    ],
    dtype=np.float64,
)


def source_values() -> np.ndarray:
    values = np.empty(SHAPE, dtype=np.float32)
    for x, y, z, time in itertools.product(*(range(size) for size in SHAPE)):
        values[x, y, z, time] = 0.25 + 1000.0 * time + 100.0 * x + 10.0 * y + z
    return values


def nifti_ordinal(x: int, y: int, z: int, time: int) -> int:
    nx, ny, nz, _ = SHAPE
    return x + nx * (y + ny * (z + nz * time))


def ravel_ordinal(x: int, y: int, z: int, time: int) -> int:
    _, ny, nz, nt = SHAPE
    return time + nt * (z + nz * (y + ny * x))


def fields(values: np.ndarray) -> str:
    return "\t".join(f"{float(value):.17g}" for value in values.reshape(-1))


def main() -> None:
    root = Path(__file__).resolve().parents[2]
    output = root / "modules/image/jvm/src/test/resources/scalafim/image/io"
    output.mkdir(parents=True, exist_ok=True)
    nifti_path = output / "nibabel-neuroim2-basic-4d.nii"
    table_path = output / "nibabel-basic-4d.tsv"

    image = nib.Nifti1Image(source_values(), AFFINE)
    image.header.set_data_dtype(np.float32)
    image.header.set_xyzt_units("mm", "sec")
    image.header["pixdim"][4] = TEMPORAL_SPACING_SECONDS
    image.set_qform(AFFINE, code=1)
    image.set_sform(AFFINE, code=2)
    nib.save(image, nifti_path)

    persisted = nib.load(nifti_path)
    data = persisted.get_fdata(dtype=np.float64)
    if tuple(persisted.shape) != SHAPE:
        raise RuntimeError(f"persisted shape changed: {persisted.shape}")
    if not np.array_equal(data, source_values().astype(np.float64)):
        raise RuntimeError("persisted coordinate values changed")

    with table_path.open("w", newline="", encoding="utf-8") as handle:
        handle.write("# fixture_version\timage-library-parity-v1\n")
        handle.write(f"# generator\tnibabel-{nib.__version__}\n")
        handle.write(f"# source\t{nifti_path.name}\n")
        handle.write("# shape\t" + "\t".join(str(value) for value in SHAPE) + "\n")
        handle.write("# axis_codes\t" + "\t".join(nib.aff2axcodes(persisted.affine)) + "\n")
        handle.write(f"# affine_row_major\t{fields(persisted.affine)}\n")
        handle.write(
            f"# temporal_spacing_seconds\t{persisted.header.get_zooms()[3]:.17g}\n"
        )
        writer = csv.writer(handle, delimiter="\t", lineterminator="\n")
        writer.writerow(
            (
                "x",
                "y",
                "z",
                "time",
                "nifti_ordinal0",
                "ravel_ordinal0",
                "value",
                "world_x",
                "world_y",
                "world_z",
            )
        )
        for x, y, z, time in itertools.product(*(range(size) for size in SHAPE)):
            world = nib.affines.apply_affine(persisted.affine, (x, y, z))
            writer.writerow(
                (
                    x,
                    y,
                    z,
                    time,
                    nifti_ordinal(x, y, z, time),
                    ravel_ordinal(x, y, z, time),
                    f"{data[x, y, z, time]:.17g}",
                    f"{world[0]:.17g}",
                    f"{world[1]:.17g}",
                    f"{world[2]:.17g}",
                )
            )

    print(f"wrote {nifti_path.relative_to(root)}")
    print(f"wrote {table_path.relative_to(root)}")


if __name__ == "__main__":
    main()
