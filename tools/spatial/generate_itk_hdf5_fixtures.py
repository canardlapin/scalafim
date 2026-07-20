#!/usr/bin/env python3
"""Generate compact, independently executable ITK HDF5 transform fixtures."""

from __future__ import annotations

import argparse
import hashlib
import json
import shutil
import tempfile
from pathlib import Path

import h5py
import numpy as np
import SimpleITK as sitk


ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OUTPUT = (
    ROOT
    / "modules"
    / "spatial"
    / "jvm"
    / "src"
    / "test"
    / "resources"
    / "scalafim"
    / "spatial"
    / "io"
    / "itk-hdf5"
)


def displacement(
    size: tuple[int, int, int],
    origin: tuple[float, float, float],
    spacing: tuple[float, float, float],
    direction: tuple[float, ...],
    value_at,
) -> sitk.DisplacementFieldTransform:
    image = sitk.Image(size, sitk.sitkVectorFloat64, 3)
    image.SetOrigin(origin)
    image.SetSpacing(spacing)
    image.SetDirection(direction)
    for z in range(size[2]):
        for y in range(size[1]):
            for x in range(size[0]):
                image[x, y, z] = tuple(float(v) for v in value_at(x, y, z))
    return sitk.DisplacementFieldTransform(image)


def lps_to_ras(point) -> list[float]:
    return [-float(point[0]), -float(point[1]), float(point[2])]


def physical_point(origin, spacing, direction, index) -> tuple[float, float, float]:
    matrix = np.asarray(direction, dtype=float).reshape(3, 3)
    return tuple(np.asarray(origin) + matrix @ (np.asarray(spacing) * np.asarray(index)))


def write_composite_fixture(output: Path) -> dict:
    size = (4, 3, 2)
    origin = (10.0, -20.0, 30.0)
    spacing = (2.0, 3.0, 4.0)
    direction = (0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    warp = displacement(
        size,
        origin,
        spacing,
        direction,
        lambda x, y, z: (1.0 + 0.25 * x, -0.5 + 0.2 * y, 0.75 + 0.1 * z),
    )
    affine = sitk.AffineTransform(3)
    affine.SetMatrix((1.25, 0.1, 0.0, -0.05, 0.9, 0.0, 0.0, 0.0, 1.1))
    affine.SetCenter((11.0, -17.0, 34.0))
    affine.SetTranslation((2.0, -1.5, 0.5))

    composite = sitk.CompositeTransform(3)
    composite.AddTransform(affine)
    composite.AddTransform(warp)
    path = output / "composite_affine_displacement_double.h5"
    sitk.WriteTransform(composite, str(path))

    lps_points = [
        physical_point(origin, spacing, direction, (0.5, 0.5, 0.25)),
        physical_point(origin, spacing, direction, (1.25, 1.0, 0.5)),
        physical_point(origin, spacing, direction, (2.0, 1.5, 0.75)),
    ]
    return {
        "fixture": path.name,
        "itk_component_order": [
            "AffineTransform_double_3_3",
            "DisplacementFieldTransform_double_3_3",
        ],
        "application_order": "last component first",
        "points": [
            {
                "input_ras": lps_to_ras(point),
                "output_ras": lps_to_ras(composite.TransformPoint(point)),
            }
            for point in lps_points
        ],
    }


def write_constant_pair(output: Path) -> dict:
    size = (5, 2, 2)
    geometry = (
        (0.0, 0.0, 0.0),
        (1.0, 1.0, 1.0),
        (1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0),
    )

    def write(name: str, shift: float):
        transform = displacement(size, geometry[0], geometry[1], geometry[2], lambda _x, _y, _z: (shift, 0.0, 0.0))
        composite = sitk.CompositeTransform(3)
        composite.AddTransform(transform)
        sitk.WriteTransform(composite, str(output / name))
        return composite

    forward = write("pullback_plus_one.h5", 1.0)
    inverse = write("pullback_minus_one.h5", -1.0)
    points = [(0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (2.5, 0.5, 0.5)]
    return {
        "forward_fixture": "pullback_plus_one.h5",
        "inverse_fixture": "pullback_minus_one.h5",
        "points": [
            {
                "input_ras": lps_to_ras(point),
                "forward_ras": lps_to_ras(forward.TransformPoint(point)),
                "roundtrip_ras": lps_to_ras(inverse.TransformPoint(forward.TransformPoint(point))),
            }
            for point in points
        ],
    }


def write_affine_only(output: Path) -> dict:
    affine = sitk.AffineTransform(3)
    affine.SetMatrix((1.1, 0.15, 0.0, 0.0, 0.85, 0.1, 0.0, 0.0, 1.2))
    affine.SetCenter((3.0, -4.0, 2.0))
    affine.SetTranslation((1.0, 2.0, -0.5))
    composite = sitk.CompositeTransform(3)
    composite.AddTransform(affine)
    path = output / "affine_only_forward_double.h5"
    sitk.WriteTransform(composite, str(path))
    points = [(0.0, 0.0, 0.0), (2.0, -1.0, 3.0)]
    return {
        "fixture": path.name,
        "forward_points": [
            {
                "input_ras": lps_to_ras(point),
                "output_ras": lps_to_ras(composite.TransformPoint(point)),
            }
            for point in points
        ],
    }


def rewrite_as_legacy_float(source: Path, destination: Path) -> None:
    shutil.copyfile(source, destination)
    string_type = h5py.string_dtype(encoding="ascii")
    with h5py.File(destination, "r+") as hdf:
        for group in hdf["TransformGroup"].values():
            transform_type = group["TransformType"][0]
            if isinstance(transform_type, bytes):
                transform_type = transform_type.decode("ascii")
            del group["TransformType"]
            group.create_dataset("TransformType", data=np.asarray([transform_type.replace("_double_", "_float_")], dtype=string_type))
            for canonical, legacy in (
                ("TransformParameters", "TranformParameters"),
                ("TransformFixedParameters", "TranformFixedParameters"),
            ):
                if canonical in group:
                    values = np.asarray(group[canonical], dtype=np.float32)
                    del group[canonical]
                    group.create_dataset(legacy, data=values)


def write_failure_fixtures(output: Path) -> None:
    string_type = h5py.string_dtype(encoding="ascii")
    with h5py.File(output / "unsupported_bspline.h5", "w") as hdf:
        transforms = hdf.create_group("TransformGroup")
        group = transforms.create_group("0")
        group.create_dataset("TransformType", data=np.asarray(["BSplineTransform_double_3_3"], dtype=string_type))
        group.create_dataset("TransformParameters", data=np.zeros(24, dtype=np.float64))
        group.create_dataset("TransformFixedParameters", data=np.zeros(18, dtype=np.float64))

    with h5py.File(output / "malformed_missing_fixed.h5", "w") as hdf:
        transforms = hdf.create_group("TransformGroup")
        group = transforms.create_group("0")
        group.create_dataset("TransformType", data=np.asarray(["AffineTransform_double_3_3"], dtype=string_type))
        group.create_dataset(
            "TransformParameters",
            data=np.asarray([1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0]),
        )


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def hdf5_semantic_sha256(path: Path) -> str:
    digest = hashlib.sha256()

    def add(name, node):
        digest.update(name.encode("utf-8"))
        if isinstance(node, h5py.Group):
            digest.update(b"group")
        else:
            values = node[()]
            digest.update(str(node.shape).encode("ascii"))
            digest.update(str(node.dtype).encode("ascii"))
            if node.dtype.kind in ("O", "S", "U"):
                flat = np.asarray(values).reshape(-1).tolist()
                normalized = [value.decode("utf-8") if isinstance(value, bytes) else str(value) for value in flat]
                digest.update(json.dumps(normalized, separators=(",", ":")).encode("utf-8"))
            else:
                digest.update(np.ascontiguousarray(values).tobytes())

    with h5py.File(path, "r") as hdf:
        hdf.visititems(add)
    return digest.hexdigest()


def generate(output: Path) -> None:
    output.mkdir(parents=True, exist_ok=True)
    oracle = {
        "generator": f"SimpleITK {sitk.Version_VersionString()} / ITK {sitk.Version_ITKVersionString()}",
        "coordinate_contract": "fixtures execute in ITK LPS; stored oracle points are converted to RAS by diag(-1,-1,1)",
        "composite": write_composite_fixture(output),
        "constant_pair": write_constant_pair(output),
        "affine_only": write_affine_only(output),
    }
    rewrite_as_legacy_float(
        output / "composite_affine_displacement_double.h5",
        output / "composite_affine_displacement_legacy_float.h5",
    )
    write_failure_fixtures(output)
    (output / "oracles.json").write_text(json.dumps(oracle, indent=2, sort_keys=True) + "\n")
    rows = ["case\tin_x\tin_y\tin_z\tout_x\tout_y\tout_z"]
    for point in oracle["composite"]["points"]:
        values = point["input_ras"] + point["output_ras"]
        rows.append("\t".join(["composite"] + [format(value, ".17g") for value in values]))
    for point in oracle["constant_pair"]["points"]:
        values = point["input_ras"] + point["forward_ras"]
        rows.append("\t".join(["constant-forward"] + [format(value, ".17g") for value in values]))
        roundtrip = point["forward_ras"] + point["roundtrip_ras"]
        rows.append("\t".join(["constant-inverse"] + [format(value, ".17g") for value in roundtrip]))
    for point in oracle["affine_only"]["forward_points"]:
        values = point["input_ras"] + point["output_ras"]
        rows.append("\t".join(["affine-forward"] + [format(value, ".17g") for value in values]))
    (output / "point_oracles.tsv").write_text("\n".join(rows) + "\n")
    files = sorted(path for path in output.iterdir() if path.is_file() and path.name != "manifest.json")
    manifest = {
        path.name: (hdf5_semantic_sha256(path) if path.suffix == ".h5" else sha256(path))
        for path in files
    }
    (output / "manifest.json").write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n")


def check(output: Path) -> None:
    expected_manifest = json.loads((output / "manifest.json").read_text())
    with tempfile.TemporaryDirectory(prefix="scalafim-itk-hdf5-") as temp:
        generated = Path(temp)
        generate(generated)
        actual_manifest = json.loads((generated / "manifest.json").read_text())
    if actual_manifest != expected_manifest:
        raise SystemExit("ITK HDF5 fixtures are stale; rerun tools/spatial/generate_itk_hdf5_fixtures.py")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    if args.check:
        check(args.output)
    else:
        generate(args.output)


if __name__ == "__main__":
    main()
