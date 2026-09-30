#!/usr/bin/env python3
"""P7.07: native ANTs references for an interior of the real demo1 registration chain.

uv run --python 3.12 --with nibabel==5.4.2 --with h5py==3.16.0 --with numpy==2.5.3 \
  python tools/transform/generate_demo1_native_oracle.py --image sha256:... \
  --source-dir /path/to/neurotransform/inst/extdata/demo1 --provenance-dir /path/to/verified-metadata

The original transforms are read-only inputs. Cropping retains exact float32 field
samples, affine parameters/centres and every interpolation neighbourhood of the
frozen comparison set. Native original-versus-crop equality is a required gate,
not an assumption. This does not qualify boundaries or the whole image domain.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import shlex
import shutil
import subprocess
import tempfile
import zlib

import h5py
import nibabel as nib
import numpy as np

import oracle_common as oc

FORWARD = "sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5"
INVERSE = "sub-01_from-MNI152NLin6Asym_to-T1w_mode-image_xfm.h5"
SCANNER = "sub-01_task-rest_from-scanner_to-T1w_mode-image_xfm.txt"
SCANNER_INVERSE = "sub-01_task-rest_from-T1w_to-scanner_mode-image_xfm.txt"
POINT_BUDGET = 1e-6  # mm, native double computation and binary double point IO
CROP_BUDGET = 1e-10  # mm, unchanged samples and exact integer crop-origin shift
INTENSITY_BUDGET = 0.01  # native float32 output plus bounded coordinate perturbation
SHAPE = (7, 9, 5)
AFFINE = np.diag([1.1, 1.25, 1.3, 1.0])
AFFINE[:3, 3] = [-7.75, -14.25, 18.5]


def write_points(path, xyz):
    matrix = np.column_stack([xyz, np.zeros(len(xyz))])
    header = ("ObjectType = Image\nNDims = 2\nBinaryData = True\n"
              "BinaryDataByteOrderMSB = False\nCompressedData = False\n"
              f"DimSize = {len(xyz)} 4\nElementType = MET_DOUBLE\nElementDataFile = LOCAL\n")
    path.write_bytes(header.encode() + matrix.T.astype("<f8").tobytes())


def read_points(path):
    raw = path.read_bytes()
    marker = b"ElementDataFile = LOCAL\n"
    end = raw.index(marker) + len(marker)
    fields = dict(line.split(" = ", 1) for line in raw[:end].decode().splitlines())
    if fields["ElementType"] != "MET_DOUBLE" or fields.get("BinaryDataByteOrderMSB", "False") != "False":
        raise RuntimeError(f"Unexpected native point encoding: {fields}")
    payload = zlib.decompress(raw[end:]) if fields.get("CompressedData") == "True" else raw[end:]
    rows, columns = map(int, fields["DimSize"].split())
    matrix = np.frombuffer(payload, dtype="<f8").reshape(columns, rows).T
    if columns != 4 or not np.isfinite(matrix).all():
        raise RuntimeError(f"Invalid native points: {path}")
    return matrix[:, :3].copy()


def save_image(path, values, affine, dtype):
    image = nib.Nifti1Image(np.asarray(values, dtype=dtype), affine)
    image.set_qform(affine, 1)
    image.set_sform(affine, 1)
    image.header.set_xyzt_units("mm")
    nib.save(image, path)


def crop_transform(original, output, queries):
    with h5py.File(original) as source:
        stages = source["TransformGroup"]
        field_key = next(key for key in stages if b"DisplacementField" in stages[key]["TransformType"][()].ravel()[0])
        field = stages[field_key]
        fixed = field["TransformFixedParameters"][()].copy()
        size = fixed[:3].astype(int)
        direction = fixed[9:].reshape(3, 3)
        basis = direction @ np.diag(fixed[6:9])
        indices = np.linalg.solve(basis, (queries - fixed[3:6]).T).T
        start = np.floor(indices.min(axis=0)).astype(int) - 2
        stop = np.ceil(indices.max(axis=0)).astype(int) + 3
        if (start < 0).any() or (stop > size).any():
            raise RuntimeError("Frozen comparison set is not strictly interior to the original field")
        values = field["TransformParameters"][()].reshape(*size[::-1], 3)
        crop = values[start[2]:stop[2], start[1]:stop[1], start[0]:stop[0], :].copy()
        copied = crop.reshape(-1)
        if copied.dtype != np.dtype("float32") or not np.isfinite(copied).all():
            raise RuntimeError("Original demo1 field must be finite float32")
        fixed[:3] = stop - start
        fixed[3:6] += basis @ start
        with h5py.File(output, "w") as target:
            for key in source:
                if key != "TransformGroup":
                    source.copy(key, target)
            target_group = target.create_group("TransformGroup")
            for key, node in stages.items():
                target_node = target_group.create_group(key)
                for name, dataset in node.items():
                    if key == field_key and name == "TransformParameters":
                        target_node.create_dataset(name, data=copied, compression="gzip")
                    elif key == field_key and name == "TransformFixedParameters":
                        target_node.create_dataset(name, data=fixed)
                    else:
                        node.copy(name, target_node)
                for name, value in node.attrs.items():
                    target_node.attrs[name] = value
            for name, value in source.attrs.items():
                target.attrs[name] = value
        with h5py.File(output) as target:
            retained = target["TransformGroup"][field_key]["TransformParameters"][()]
            if not np.array_equal(retained.view("uint32"), copied.view("uint32")):
                raise RuntimeError("Cropping changed field sample bits")
        margin = np.minimum(indices - start, stop - 1 - indices).min()
        return dict(start=start.tolist(), shape=(stop-start).tolist(), original_shape=size.tolist(),
                    origin_lps=fixed[3:6].tolist(), minimum_query_margin_voxels=float(margin),
                    retained_values=len(copied), retained_float32_bits_exact=True)


def affine_queries(original, xyz):
    with h5py.File(original) as source:
        node = next(node for node in source["TransformGroup"].values()
                    if b"AffineTransform" in node["TransformType"][()].ravel()[0])
        parameters = node["TransformParameters"][()].astype(float)
        centre = node["TransformFixedParameters"][()].astype(float)
    matrix = parameters[:9].reshape(3, 3)
    return (xyz-centre) @ matrix.T + centre + parameters[9:]


def dump_transform(path, output):
    lines = []
    with h5py.File(path) as source:
        for key in sorted(source["TransformGroup"], key=int):
            node = source["TransformGroup"][key]
            kind = node["TransformType"][()].ravel()[0].decode()
            values = lambda name: " ".join(repr(float(v)) for v in node[name][()].ravel()) if name in node else ""
            lines.append("\t".join([key, kind, values("TransformParameters"), values("TransformFixedParameters")]))
    output.write_text("\n".join(lines) + "\n")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--image", required=True)
    parser.add_argument("--source-dir", type=Path, required=True)
    parser.add_argument("--provenance-dir", type=Path, required=True)
    parser.add_argument("--out", type=Path, default=Path(oc.ORACLE_ROOT)/"demo1_native")
    args = parser.parse_args()
    if not args.image.startswith("sha256:") and "@sha256:" not in args.image:
        parser.error("--image must identify immutable bytes")
    provenance = json.loads((args.provenance_dir/"source-provenance.json").read_text())
    if provenance["upstream_commit"] != "1d8407e467d1af0ac8933c0539bbbb0168badab6" or provenance["license"] != "CC0":
        raise RuntimeError("Expected pinned, identified public derivative")
    for name, record in provenance["assets"].items():
        path = (args.provenance_dir if name.endswith(".txt") else args.source_dir)/name
        if path.stat().st_size != record["size"] or oc.sha256_file(path) != record["sha256"]:
            raise RuntimeError(f"Source identity mismatch: {name}")
    args.out.mkdir(parents=True, exist_ok=True)
    image = json.loads(subprocess.check_output(["docker", "image", "inspect", args.image]))[0]
    commands = []
    indices = np.array([(i,j,k) for i in range(SHAPE[0]) for j in range(SHAPE[1]) for k in range(SHAPE[2])])
    holdouts = np.array(oc.asymmetric_points(SHAPE, count=32, seed=20260930))
    template_ras = np.concatenate([indices, holdouts]) @ AFFINE[:3,:3].T + AFFINE[:3,3]
    inverse_ras = np.array(oc.asymmetric_points(SHAPE, count=32, seed=20261001)) @ np.diag([1.1,1.25,1.3]) + [-8.3,-9.1,18.4]
    flip = np.array([-1.0,-1.0,1.0])
    template = template_ras * flip
    inverse = inverse_ras * flip
    with tempfile.TemporaryDirectory(prefix="scalafim-demo1-native-", dir="/private/tmp") as temporary:
        work = Path(temporary)
        for name, target in [(SCANNER,"scanner_to_t1.txt"),(SCANNER_INVERSE,"t1_to_scanner.txt")]:
            shutil.copyfile(args.provenance_dir/name, work/target)
        shutil.copyfile(args.source_dir/"sub-01_boldref.nii.gz", work/"boldref.nii.gz")
        write_points(work/"template.mha", template)
        write_points(work/"inverse.mha", inverse)
        save_image(work/"target.nii.gz", np.zeros(SHAPE), AFFINE, np.float64)
        bold = nib.load(work/"boldref.nii.gz")
        bold_values = bold.get_fdata()
        ijk = np.stack(np.meshgrid(*[np.arange(n) for n in bold.shape], indexing="ij"), axis=-1)
        xyz = ijk @ bold.affine[:3,:3].T + bold.affine[:3,3]
        for axis in range(3):
            save_image(work/f"source_coord_{axis}.nii.gz", xyz[...,axis], bold.affine, np.float64)

        def points(input_name, output_name, transforms):
            return ["antsApplyTransformsToPoints","-d","3","-p","1","-i",input_name,"-o",output_name] + [v for t in transforms for v in ["-t",t]]

        def resample(input_name, output_name, transforms):
            return ["antsApplyTransforms","-d","3","--float","0","-i",input_name,"-r","target.nii.gz","-o",output_name,"-n","Linear"] + [v for t in transforms for v in ["-t",t]]

        def run(batch, label):
            commands.extend(batch)
            script = "#!/bin/bash\nset -euo pipefail\nexport ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1 OMP_NUM_THREADS=1\ncd /work\n" + "\n".join(shlex.join(c) for c in batch) + "\n"
            (work/f"{label}.sh").write_text(script)
            shutil.copyfile(work/f"{label}.sh", args.out/f"{label}.sh")
            command = ["docker","run","--rm","--network","none","--platform","linux/amd64","--cpus","2","--memory","1g","--entrypoint","/bin/bash","-v",f"{work}:/work","-v",f"{args.source_dir.resolve()}:/input:ro",args.image,f"/work/{label}.sh"]
            with (args.out/f"{label}.stdout.txt").open("w") as log:
                result = subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, timeout=300)
            if result.returncode:
                raise RuntimeError(f"Native {label} failed: {result.returncode}")

        forward = f"/input/{FORWARD}"
        reverse = f"/input/{INVERSE}"
        run([
            ["antsRegistration","--version"],
            points("template.mha","original_t1.mha",[forward]),
            points("original_t1.mha","original_bold_sequential.mha",["scanner_to_t1.txt"]),
            points("template.mha","original_bold.mha",[forward,"scanner_to_t1.txt"]),
            points("inverse.mha","original_inverse.mha",[reverse]),
            points("original_t1.mha","original_roundtrip.mha",[reverse]),
            resample("boldref.nii.gz","original_bold_warp.nii.gz",[forward,"scanner_to_t1.txt"]),
            *[resample(f"source_coord_{axis}.nii.gz",f"original_coord_{axis}.nii.gz",[forward,"scanner_to_t1.txt"]) for axis in range(3)],
        ], "original")
        t1 = read_points(work/"original_t1.mha")
        scanner = read_points(work/"original_bold.mha")
        sequential = read_points(work/"original_bold_sequential.mha")
        chain_error = float(np.max(np.abs(scanner-sequential)))
        if chain_error > CROP_BUDGET:
            raise RuntimeError(f"Native chain order disagrees with sequential stages: {chain_error}")
        forward_crop = crop_transform(args.source_dir/FORWARD, work/"t1_to_template.h5", template)
        reverse_queries = np.concatenate([inverse,t1])
        reverse_crop = crop_transform(args.source_dir/INVERSE, work/"template_to_t1.h5", affine_queries(args.source_dir/INVERSE, reverse_queries))
        scanner_indices = nib.affines.apply_affine(np.linalg.inv(bold.affine), scanner*flip)
        margin = float(np.minimum(scanner_indices, np.array(bold.shape)-1-scanner_indices).min())
        if margin < 1.0:
            raise RuntimeError(f"Frozen comparisons leave original BOLD interpolation support: {margin}")
        # Freeze the intensity budget against source gradients before examining the resampled intensities.
        max_gradient = sum(float(np.max(np.abs(np.diff(bold_values,axis=axis)))) * float(np.abs(np.linalg.inv(bold.affine[:3,:3])[axis]).sum()) for axis in range(3))
        output_rounding = float(np.max(np.abs(bold_values))) * np.finfo(np.float32).eps
        justified_bound = max_gradient*POINT_BUDGET + output_rounding + 1e-8
        if justified_bound > INTENSITY_BUDGET:
            raise RuntimeError(f"Frozen intensity budget is not justified by source/precision bound: {justified_bound}")
        run([
            points("template.mha","crop_t1.mha",["t1_to_template.h5"]),
            points("template.mha","crop_bold.mha",["t1_to_template.h5","scanner_to_t1.txt"]),
            points("inverse.mha","crop_inverse.mha",["template_to_t1.h5"]),
            points("original_t1.mha","crop_roundtrip.mha",["template_to_t1.h5"]),
            resample("boldref.nii.gz","crop_bold_warp.nii.gz",["t1_to_template.h5","scanner_to_t1.txt"]),
        ], "crop")
        equality = {}
        for key in ["t1","bold","inverse","roundtrip"]:
            delta = float(np.max(np.abs(read_points(work/f"original_{key}.mha")-read_points(work/f"crop_{key}.mha"))))
            equality[key] = delta
            if delta > CROP_BUDGET:
                raise RuntimeError(f"Original/cropped native {key} maps differ: {delta}")
        native_values = nib.load(work/"original_bold_warp.nii.gz").get_fdata()
        crop_values = nib.load(work/"crop_bold_warp.nii.gz").get_fdata()
        intensity_equality = float(np.max(np.abs(native_values-crop_values)))
        if intensity_equality != 0.0 or not np.isfinite(native_values).all():
            raise RuntimeError(f"Cropping changed native intensity output: {intensity_equality}")
        coordinates = np.stack([nib.load(work/f"original_coord_{axis}.nii.gz").get_fdata() for axis in range(3)],axis=-1)
        point_image_error = float(np.max(np.abs(coordinates.reshape(-1,3)-scanner[:len(indices)]*flip)))
        if point_image_error > POINT_BUDGET:
            raise RuntimeError(f"Native points and image pullback disagree: {point_image_error}")
        files = ["boldref.nii.gz","scanner_to_t1.txt","t1_to_scanner.txt","target.nii.gz","template.mha","inverse.mha","t1_to_template.h5","template_to_t1.h5", "original_t1.mha","original_bold.mha","original_inverse.mha","original_roundtrip.mha","original_bold_warp.nii.gz"]
        files += [f"original_coord_{axis}.nii.gz" for axis in range(3)]
        for name in files:
            shutil.copyfile(work/name,args.out/name)
        for key in ["t1_to_template","template_to_t1"]:
            dump_transform(work/f"{key}.h5",args.out/f"{key}.components.txt")
        table = []
        for key, inputs in [("t1",template),("bold",template),("inverse",inverse),("roundtrip",t1)]:
            outputs = read_points(work/f"original_{key}.mha")
            table.extend([[key,*p,*q] for p,q in zip(inputs*flip,outputs*flip)])
        oc.write_table(str(args.out/"points.tsv"),["key","x","y","z","tx","ty","tz"],table)
        (args.out/"source-provenance.json").write_text(json.dumps(provenance,indent=2)+"\n")
        shutil.copyfile(args.provenance_dir/"dataset_description.json",args.out/"dataset_description.json")
        metadata = dict(scope="real-demo1-interior-chain", target_shape=SHAPE, target_index_to_ras=AFFINE.tolist(), grid_queries=len(indices), off_grid_holdouts=len(holdouts), inverse_queries=len(inverse), point_budget_mm=POINT_BUDGET, crop_equality_budget_mm=CROP_BUDGET, intensity_budget=INTENSITY_BUDGET, source_gradient_precision_bound=justified_bound, minimum_bold_source_margin_voxels=margin, forward_crop=forward_crop, inverse_crop=reverse_crop, original_crop_point_maxima_mm=equality, original_crop_intensity_maximum=intensity_equality, native_sequential_chain_maximum_mm=chain_error, native_point_image_maximum_mm=point_image_error, native_roundtrip_maximum_mm=float(np.max(np.abs(read_points(work/"original_roundtrip.mha")-template))), forbidden_claims=["whole-domain registration accuracy","ITK border parity","all P7.07 workflows qualified"])
        (args.out/"metadata.json").write_text(json.dumps(metadata,indent=2)+"\n")
        version = next(line.strip() for line in (args.out/"original.stdout.txt").read_text().splitlines() if line.startswith("ANTs Version:"))
        oc.write_manifest(str(args.out),generator=__file__,tools={"ANTs":version,"image_id":image["Id"],"image_digests":image.get("RepoDigests",[]),"numpy":np.__version__,"nibabel":nib.__version__,"h5py":h5py.__version__},commands=[shlex.join(command) for command in commands],notes="Exact real ds002748 task-rest demo1 assets from pinned CC0 fMRIPrep21.0.2 derivative. Original containers produce all reference outputs. Every frozen comparison is retained; no output-dependent exclusions. Original/cropped fields agree natively in the declared interior, and actual BOLD values plus coordinate ramps exercise the complete scanner/boldref -> T1w -> MNI152NLin6Asym pullback. Binary double MHA avoids lossy CSV precision. No whole-domain, registration-accuracy, surface-chain or boundary qualification.")
    print(json.dumps(metadata,indent=2))


if __name__ == "__main__":
    main()
