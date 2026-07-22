#!/usr/bin/env python3
"""Generate and verify the NIfTI -> Zarr -> BIDS/NIfTI scalar oracle matrix."""

from __future__ import annotations

import argparse
import json
from pathlib import Path

import nibabel as nib
import numpy as np
import zarr


SCALARS = {
    "uint8": np.dtype("uint8"),
    "int16": np.dtype("int16"),
    "int32": np.dtype("int32"),
    "float32": np.dtype("float32"),
    "float64": np.dtype("float64"),
}
PINNED_ZARR_VERSION = "3.2.1"
PINNED_NIBABEL_VERSION = "5.2.1"
PINNED_NUMPY_VERSION = "2.2.6"
SHAPE = (3, 2, 2, 2)
AFFINE = np.array(
    [
        [1.75, 0.10, 0.00, 11.0],
        [0.00, 2.25, 0.15, -7.0],
        [0.05, 0.00, 2.75, 4.0],
        [0.00, 0.00, 0.00, 1.0],
    ],
    dtype=np.float64,
)


def values(dtype: np.dtype) -> np.ndarray:
    index = np.arange(np.prod(SHAPE), dtype=np.float64).reshape(SHAPE)
    if dtype == np.dtype("uint8"):
        result = index
    elif dtype == np.dtype("int16"):
        result = index - 12
    elif dtype == np.dtype("int32"):
        result = index * 100003 - 900001
    else:
        result = index * 0.125 - 1.75
    return result.astype(dtype)


def write_inputs(root: Path) -> None:
    root.mkdir(parents=True, exist_ok=True)
    for name, dtype in SCALARS.items():
        image = nib.Nifti1Image(values(dtype), AFFINE)
        image.set_qform(AFFINE, code=1)
        image.set_sform(AFFINE, code=2)
        image.header.set_xyzt_units("mm", "sec")
        image.header["pixdim"][4] = 1.25
        nib.save(image, root / f"{name}.nii")


def verify_outputs(input_root: Path, output_root: Path) -> None:
    for name, dtype in SCALARS.items():
        source = nib.load(input_root / f"{name}.nii")
        source_raw = np.asanyarray(source.dataobj.get_unscaled())
        expected_canonical = np.transpose(source_raw, (3, 2, 1, 0))

        revision_root = output_root / f"{name}.zarr"
        canonical_path = revision_root / "canonical"
        canonical = zarr.open_array(canonical_path, mode="r")
        canonical_raw = np.asarray(canonical[:])
        if canonical_raw.dtype != dtype:
            raise AssertionError(
                f"{name}: canonical Zarr expected dtype {dtype}, "
                f"found {canonical_raw.dtype}"
            )
        if tuple(canonical_raw.shape) != tuple(reversed(SHAPE)):
            raise AssertionError(
                f"{name}: canonical Zarr expected t-z-y-x shape "
                f"{tuple(reversed(SHAPE))}, found {canonical_raw.shape}"
            )
        np.testing.assert_array_equal(canonical_raw, expected_canonical)

        metadata = json.loads((canonical_path / "zarr.json").read_text())
        if metadata.get("dimension_names") != ["t", "z", "y", "x"]:
            raise AssertionError(
                f"{name}: canonical Zarr axes are not t-z-y-x: "
                f"{metadata.get('dimension_names')}"
            )
        manifest = json.loads((revision_root / "neuroarchive.json").read_text())
        expected_relative = f"sub-01/func/sub-01_task-rest_acq-{name}_bold.nii"
        if manifest["source"]["relative_path"] != expected_relative:
            raise AssertionError(f"{name}: incorrect BIDS source join {manifest['source']}")
        if manifest["acquisition_id"] != f"sub-01_task-rest_acq-{name}_bold":
            raise AssertionError(f"{name}: incorrect acquisition identity {manifest}")
        canonical_manifest = manifest["canonical"]
        if canonical_manifest["shape"] != list(reversed(SHAPE)):
            raise AssertionError(f"{name}: incorrect canonical manifest shape {manifest}")
        if canonical_manifest["axes"] != ["t", "z", "y", "x"]:
            raise AssertionError(f"{name}: incorrect canonical manifest axes {manifest}")
        calibration = canonical_manifest["calibration"]
        canonical_physical = (
            canonical_raw.astype(np.float64) * calibration["scale"]
            + calibration["offset"]
        )
        expected_physical = np.transpose(
            np.asanyarray(source.dataobj).astype(np.float64),
            (3, 2, 1, 0),
        )
        np.testing.assert_allclose(
            canonical_physical,
            expected_physical,
            rtol=0.0,
            atol=0.0,
        )
        np.testing.assert_allclose(
            np.asarray(canonical_manifest["geometry"]["voxel_to_world"]).reshape(4, 4),
            AFFINE,
            rtol=0.0,
            atol=1e-5,
        )
        timing = canonical_manifest["timing"]
        if timing != {
            "count": SHAPE[3],
            "kind": "regular",
            "origin": 0.0,
            "step": 1.25,
            "units": "second",
        }:
            raise AssertionError(f"{name}: incorrect canonical timing {timing}")

        bids_root = output_root / f"{name}-bids"
        exported_path = bids_root / "sub-01" / "func" / f"sub-01_task-rest_acq-{name}_bold.nii"
        exported = nib.load(exported_path)
        np.testing.assert_array_equal(
            np.asanyarray(exported.dataobj.get_unscaled()),
            source_raw,
        )
        np.testing.assert_allclose(
            np.asanyarray(exported.dataobj),
            np.asanyarray(source.dataobj),
            rtol=0.0,
            atol=0.0,
        )
        if exported.get_data_dtype() != dtype:
            raise AssertionError(f"{name}: expected dtype {dtype}, found {exported.get_data_dtype()}")
        np.testing.assert_allclose(exported.affine, AFFINE, rtol=0.0, atol=1e-5)
        if tuple(exported.shape) != SHAPE:
            raise AssertionError(f"{name}: expected shape {SHAPE}, found {exported.shape}")

        sidecar = json.loads(exported_path.with_suffix(".json").read_text())
        if sidecar["RepetitionTime"] != 1.25:
            raise AssertionError(f"{name}: incorrect repetition time {sidecar}")
        if sidecar["TaskName"] != "rest":
            raise AssertionError(f"{name}: incorrect task join {sidecar}")
        description = json.loads((bids_root / "dataset_description.json").read_text())
        if description["BIDSVersion"] != "1.11.1":
            raise AssertionError(f"{name}: incorrect BIDS version {description}")
        participants = (bids_root / "participants.tsv").read_text().splitlines()
        if participants != ["participant_id", "sub-01"]:
            raise AssertionError(f"{name}: incorrect participants table {participants}")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("mode", choices=("write-inputs", "verify-outputs"))
    parser.add_argument("first", type=Path)
    parser.add_argument("second", nargs="?", type=Path)
    arguments = parser.parse_args()
    versions = {
        "zarr": (zarr.__version__, PINNED_ZARR_VERSION),
        "nibabel": (nib.__version__, PINNED_NIBABEL_VERSION),
        "numpy": (np.__version__, PINNED_NUMPY_VERSION),
    }
    for package, (actual, expected) in versions.items():
        if actual != expected:
            parser.error(f"expected {package} {expected}, found {actual}")
    if arguments.mode == "write-inputs":
        if arguments.second is not None:
            parser.error("write-inputs accepts one root")
        write_inputs(arguments.first)
    else:
        if arguments.second is None:
            parser.error("verify-outputs requires input and output roots")
        verify_outputs(arguments.first, arguments.second)


if __name__ == "__main__":
    main()
