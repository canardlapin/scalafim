#!/usr/bin/env python3
"""STP P4.03: ANTs CLI point oracles, including native SyN forward/inverse containers.

The tiny registration is a fixture producer, not an accuracy benchmark. Run with
uv run --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy==2.5.3 python \
  tools/transform/generate_ants_native_oracle.py --image antsx/ants@sha256:...
"""

from __future__ import annotations

import argparse
import csv
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile

import h5py
import nibabel as nib
import numpy as np

import oracle_common as oc


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--image", required=True, help="Immutable Docker image ID or repository digest")
    parser.add_argument("--out", type=Path, default=Path(oc.ORACLE_ROOT) / "ants_native")
    args = parser.parse_args()
    if "@sha256:" not in args.image and not args.image.startswith("sha256:"):
        parser.error("--image must identify immutable bytes with sha256")
    args.out.mkdir(parents=True, exist_ok=True)
    image = json.loads(subprocess.check_output(["docker", "image", "inspect", args.image]))[0]
    commands = []
    with tempfile.TemporaryDirectory(prefix="scalafim-ants-oracle-", dir="/private/tmp") as temporary:
        work = Path(temporary)
        shape = (13, 15, 17)
        angle = 0.19
        affine = np.eye(4)
        affine[:3, :3] = np.array([[np.cos(angle), -np.sin(angle), 0], [np.sin(angle), np.cos(angle), 0], [0, 0, 1]]) @ np.diag([1.2, 1.5, 1.8])
        affine[:3, 3] = -affine[:3, :3] @ ((np.array(shape) - 1) / 2) + [2, -3, 1]
        ijk = np.stack(np.meshgrid(*[np.arange(n) for n in shape], indexing="ij"), axis=-1)
        xyz = ijk @ affine[:3, :3].T + affine[:3, 3]

        def phantom(p):
            x, y, z = np.moveaxis(p, -1, 0)
            return np.exp(-((x - 2) ** 2 / 18 + (y + 3) ** 2 / 32 + (z - 1) ** 2 / 45)) + 0.6 * np.exp(-((x + 3) ** 2 / 8 + (y - 2) ** 2 / 12 + (z + 4) ** 2 / 16))

        for name, values in [("fixed.nii.gz", phantom(xyz)), ("moving.nii.gz", phantom(xyz + [0.4, -0.3, 0.2]))]:
            volume = nib.Nifti1Image(values.astype(np.float32), affine)
            volume.set_qform(affine, 1)
            volume.set_sform(affine, 1)
            nib.save(volume, work / name)
        # At least twelve off-centre physical points in the native field's interior, expressed in ANTs LPS.
        indices = np.array(oc.asymmetric_points(tuple(n - 4 for n in shape))) + 2
        ras = indices @ affine[:3, :3].T + affine[:3, 3]
        lps = ras * [-1, -1, 1]
        with (work / "queries.csv").open("w") as handle:
            handle.write("x,y,z,t\n")
            np.savetxt(handle, np.column_stack([lps, np.zeros(len(lps))]), delimiter=",", fmt="%.17g")
        linear = ["affine", "euler", "euler_zyx", "versor_rigid", "similarity", "scale_skew_versor"]
        for name in linear:
            shutil.copyfile(Path(oc.ORACLE_ROOT) / "itk_linear" / f"{name}.mat", work / f"{name}.mat")
        commands.append(["antsRegistration", "--version"])
        commands.append([
            "antsRegistration", "--dimensionality", "3", "--float", "0", "--output", "[syn_,warped.nii.gz]",
            "--interpolation", "Linear", "--write-composite-transform", "1", "--collapse-output-transforms", "1",
            "--transform", "SyN[0.1,3,0]", "--metric", "CC[fixed.nii.gz,moving.nii.gz,1,2]",
            "--convergence", "[3,1e-6,2]", "--shrink-factors", "1", "--smoothing-sigmas", "0vox",
        ])
        # --write-composite-transform emits HDF5 only. Have ANTs itself materialize each composite as a 5D field.
        for name, reference in [("syn_0Warp", "fixed.nii.gz"), ("syn_0InverseWarp", "moving.nii.gz")]:
            composite = "syn_InverseComposite.h5" if "Inverse" in name else "syn_Composite.h5"
            commands.append(["antsApplyTransforms", "-d", "3", "-r", reference, "-o", f"[{name}.nii.gz,1]", "-t", composite])
        cases = linear + ["syn_0Warp", "syn_0InverseWarp", "syn_Composite", "syn_InverseComposite"]
        filenames = [f"{name}.mat" for name in linear] + ["syn_0Warp.nii.gz", "syn_0InverseWarp.nii.gz", "syn_Composite.h5", "syn_InverseComposite.h5"]
        for key, filename in zip(cases, filenames):
            commands.append(["antsApplyTransformsToPoints", "-d", "3", "-i", "queries.csv", "-o", f"{key}.points.csv", "-t", filename])
        script = work / "native.sh"
        script.write_text("#!/bin/bash\nset -euo pipefail\nexport ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1 OMP_NUM_THREADS=1\ncd /work\n" + "\n".join(shlex.join(c) for c in commands) + "\n")
        run = ["docker", "run", "--rm", "--network", "none", "--platform", "linux/amd64", "--cpus", "2", "--memory", "1g", "--entrypoint", "/bin/bash", "-v", f"{work}:/work", args.image, "/work/native.sh"]
        with (args.out / "native.log").open("w") as log:
            result = subprocess.run(run, stdout=log, stderr=subprocess.STDOUT, timeout=300)
        if result.returncode:
            raise RuntimeError(f"ANTs command failed: see {args.out / 'native.log'}")
        rows = []
        for key, filename in zip(cases, filenames):
            shutil.copyfile(work / filename, args.out / filename)
            with (work / f"{key}.points.csv").open() as handle:
                mapped = list(csv.DictReader(handle))
            if len(mapped) != len(lps):
                raise RuntimeError(f"ANTs returned {len(mapped)} points for {key}, expected {len(lps)}")
            for before, after in zip(lps, mapped):
                rows.append([key, *before, *[float(after[axis]) for axis in ["x", "y", "z"]]])
        for name in ["fixed.nii.gz", "moving.nii.gz", "queries.csv"]:
            shutil.copyfile(work / name, args.out / name)
        for name in ["syn_0Warp.nii.gz", "syn_0InverseWarp.nii.gz"]:
            field = nib.load(args.out / name)
            if field.shape != (*shape, 1, 3) or field.header.get_intent()[0] != "vector":
                raise RuntimeError(f"ANTs field has unexpected shape/intent: {name}, {field.shape}, {field.header.get_intent()}")
        # The decoded shared HDF5 fixtures must be tied back to the actual container by JVM tests.
        for name in ["syn_Composite", "syn_InverseComposite"]:
            lines = []
            with h5py.File(args.out / f"{name}.h5", "r") as h5:
                group = h5["TransformGroup"]
                for index in sorted(group, key=int):
                    node = group[index]
                    kind = node["TransformType"][()].ravel()[0].decode()
                    values = lambda key: " ".join(repr(float(v)) for v in np.asarray(node[key]).ravel()) if key in node else ""
                    lines.append("\t".join([index, kind, values("TransformParameters"), values("TransformFixedParameters")]))
            (args.out / f"{name}.components.txt").write_text("\n".join(lines) + "\n")
        oc.write_table(str(args.out / "points.tsv"), ["key", "x", "y", "z", "tx", "ty", "tz"], rows)
        version = next((line.strip() for line in (args.out / "native.log").read_text().splitlines() if line.startswith("ANTs Version:")), None)
        if version is None or "2.6." not in version:
            raise RuntimeError(f"expected ANTs 2.6, observed {version}")
        oc.write_manifest(str(args.out), generator=__file__, tools={"ANTs": version, "image_id": image["Id"], "image_digests": image.get("RepoDigests", []), "nibabel": nib.__version__, "h5py": h5py.__version__, "numpy": np.__version__}, commands=[shlex.join(c) for c in commands], notes="Synthetic SyN fixture producer (3 iterations; no registration-accuracy claim). Native ANTs 2.6 writes 5D vector-intent forward/inverse warp files and HDF5 Composite/InverseComposite. All expected coordinates are antsApplyTransformsToPoints LPS output at 12 asymmetric interior physical points. Linear .mat inputs are the already-qualified ITK parameterisation fixtures; their shear, centres and rotations break symmetry. Native field grids are oblique and anisotropic, without unsupported non-orthonormal direction cosines. HDF5 dumps are h5py decodes of the exact native files; JVM container tests bind them to shared interpretation tests.")
    print(f"ANTs native oracle: {len(cases)} cases, {len(rows)} point comparisons, {args.out}")


if __name__ == "__main__":
    main()
