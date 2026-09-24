"""Generate small native FNIRT coefficient-file fixtures; never uses neurotransform.

Writes inst/extdata/fsl_coef_oracle. Set NEUROTRANSFORM_ORACLE_OUT to write
elsewhere (e.g. to verify reproducibility) and NEUROTRANSFORM_ORACLE_WORK to
choose the Docker-shared work directory.
"""
from pathlib import Path
import hashlib
import json
import os
import shutil
import sys
import tempfile

import numpy as np

sys.path.insert(0, str(Path(__file__).parent / "visual_qa"))
from run import ROOT, IMAGES, save, world_grid, supporting_images, native  # noqa: E402

out = Path(os.environ.get("NEUROTRANSFORM_ORACLE_OUT", ROOT / "inst/extdata/fsl_coef_oracle"))
out.mkdir(parents=True, exist_ok=True)
work = Path(os.environ.get("NEUROTRANSFORM_ORACLE_WORK", tempfile.gettempdir())) / "neurotransform-fsl-coef-oracle"
work.mkdir(parents=True, exist_ok=True)
records, cases = [], []


def geometry(shape, spacing, angle, hand, origin):
    """Oblique voxel->RAS affine about z; 'left' = negative determinant."""
    c, s = np.cos(angle), np.sin(angle)
    a = np.eye(4)
    a[:3, :3] = np.array([[c, -s, 0], [s, c, 0], [0, 0, 1]]) @ np.diag(spacing)
    a[:3, 3] = -a[:3, :3] @ ((np.array(shape) - 1) / 2) + origin
    if hand == "left":
        a[:3, 3] += a[:3, 0] * (shape[0] - 1)
        a[:3, 0] *= -1
    return a


def vox_to_fsl(affine, shape):
    sp = np.sqrt((affine[:3, :3] ** 2).sum(0))
    m = np.diag(list(sp) + [1.0])
    if np.linalg.det(affine[:3, :3]) > 0:
        m[0, 0], m[0, 3] = -sp[0], (shape[0] - 1) * sp[0]
    return m


def rot(axis, t):
    c, s = np.cos(t), np.sin(t)
    i, j = [a for a in range(3) if a != axis]
    r = np.eye(3)
    r[i, i], r[i, j], r[j, i], r[j, j] = c, -s, s, c
    return r


def content(p):
    """Asymmetric smooth blobs sized for ~30 mm fields of view."""
    x, y, z = np.moveaxis(p, -1, 0)
    a = 1000 * np.exp(-((x / 9) ** 2 + (y / 11) ** 2 + (z / 8) ** 2))
    a += 450 * np.exp(-(((x - 6) / 4) ** 2 + ((y + 5) / 5) ** 2 + ((z - 3) / 4) ** 2))
    a += 300 * np.exp(-(((x + 5) / 3) ** 2 + ((y - 6) / 4) ** 2 + ((z + 4) / 3) ** 2))
    return a


# Per-case grids: dimensions, spacing, obliquity, origin and knot spacing all differ;
# knot spacings include N not divisible by ksp and one ksp == 1 axis per aff/noaff pair.
REF_SHAPES = [
    (10, 12, 9),
    (11, 10, 12),
    (9, 12, 10),
    (12, 11, 9),
    (10, 11, 11),
    (11, 9, 10),
    (9, 12, 11),
    (12, 10, 10),
]
REF_SPACING = [
    (2.2, 2.5, 3.0),
    (2.6, 2.3, 2.4),
    (2.4, 2.1, 2.8),
    (2.0, 2.7, 2.5),
    (2.5, 2.4, 2.2),
    (2.3, 2.8, 2.6),
    (2.7, 2.2, 2.3),
    (2.1, 2.6, 2.7),
]
KSP = [
    (3, 4, 2),
    (4, 3, 3),
    (2, 5, 3),
    (5, 2, 4),
    (3, 3, 1),
    (4, 2, 3),
    (3, 4, 5),
    (1, 3, 2),
]

def make_case(src_hand, ref_hand, with_aff, n, order):
    cid = f"src{src_hand}_ref{ref_hand}_{'aff' if with_aff else 'noaff'}"
    if order == 2:
        cid += "_quad"
    folder = work / cid
    if folder.exists():
        shutil.rmtree(folder)
    folder.mkdir(parents=True)
    rs, rsp, ksp = REF_SHAPES[n], REF_SPACING[n], KSP[n]
    ss = (rs[0] + 4, rs[1] + 3, rs[2] + 5)
    ssp = tuple(round(v * f, 3) for v, f in zip(rsp, (1.07, 0.93, 1.05)))
    ta = geometry(
        rs, rsp, 0.13 + 0.02 * n, ref_hand, np.array([1.0, -1.0, 2.0]) + 0.3 * n
    )
    sa = geometry(
        ss,
        ssp,
        -0.21 + 0.03 * n,
        src_hand,
        np.array([-2.0, 1.5, -1.0]) - 0.2 * n,
    )
    ps = world_grid(ss, sa)
    warp = np.stack(
        [
            1.6 * np.sin(ps[..., 1] / 7),
            1.3 * np.cos(ps[..., 2] / 6),
            1.1 * np.sin(ps[..., 0] / 8),
        ],
        axis=-1,
    )
    save(folder / "source.nii.gz", content(ps + warp), sa)
    save(folder / "target.nii.gz", content(world_grid(rs, ta)), ta)
    supporting_images(folder, folder / "source.nii.gz")
    warpres = ",".join(f"{k * v:.6g}" for k, v in zip(ksp, rsp))
    fnirt = [
        "fnirt",
        "--ref=target.nii.gz",
        "--in=source.nii.gz",
        "--cout=coef.nii.gz",
        f"--warpres={warpres}",
        "--subsamp=1",
        "--miter=4",
        "--lambda=30",
        "--infwhm=0",
        "--reffwhm=0",
        "--estint=1",
        "--applyrefmask=1",
        "--applyinmask=1",
        "--intmod=global_linear",
        f"--splineorder={order}",
    ]
    premat = None
    if with_aff:
        # World-aligning FLIRT matrix (source FSL -> target FSL) times a small
        # rotation/anisotropic scaling/translation, so A is far from identity.
        M = (
            vox_to_fsl(ta, rs)
            @ np.linalg.inv(ta)
            @ sa
            @ np.linalg.inv(vox_to_fsl(sa, ss))
        )
        E = np.eye(4)
        E[:3, :3] = rot(2, 0.06) @ rot(0, -0.04) @ np.diag([1.04, 0.97, 1.02])
        E[:3, 3] = [1.3, -0.8, 0.6]
        premat = E @ M
        np.savetxt(folder / "premat.mat", premat, fmt="%.10f")
        fnirt.append("--aff=premat.mat")
    commands = [
        fnirt,
        [
            "fnirtfileutils",
            "--in=coef.nii.gz",
            "--ref=target.nii.gz",
            "--out=field_noaff.nii.gz",
        ],
        [
            "fnirtfileutils",
            "--in=coef.nii.gz",
            "--ref=target.nii.gz",
            "--out=field_aff.nii.gz",
            "--withaff",
        ],
    ]
    for name in ["source", "coord0", "coord1", "coord2", "support"]:
        commands.append(
            [
                "applywarp",
                f"--in={name}.nii.gz",
                "--ref=target.nii.gz",
                "--warp=coef.nii.gz",
                f"--out=native_{name}.nii.gz",
                "--interp=trilinear",
            ]
        )
    native(work, "FSL", cid, commands, records)
    dest = out / cid
    dest.mkdir(exist_ok=True)
    names = [
        "source",
        "target",
        "coef",
        "field_noaff",
        "field_aff",
        "native_source",
        "native_coord0",
        "native_coord1",
        "native_coord2",
        "native_support",
    ]
    for name in names:
        shutil.copyfile(folder / (name + ".nii.gz"), dest / (name + ".nii.gz"))
    if with_aff:
        shutil.copyfile(folder / "premat.mat", dest / "premat.mat")
    cases.append(
        {
            "id": cid,
            "source_hand": src_hand,
            "reference_hand": ref_hand,
            "with_aff": with_aff,
            "reference_dim": list(rs),
            "source_dim": list(ss),
            "knot_spacing_vox": list(ksp),
            "warpres_mm": warpres,
            "spline_order": order,
        }
    )


n = 0
for src_hand in ("left", "right"):
    for ref_hand in ("left", "right"):
        for with_aff in (False, True):
            make_case(src_hand, ref_hand, with_aff, n, 3)
            n += 1

# Quadratic splines (--splineorder=2, intent 2009) on two of the cubic grids:
# both reference handedness, with and without --aff, and a knot spacing of 1.
for src_hand, ref_hand, with_aff, n in (("left", "right", True, 3), ("right", "left", False, 4)):
    make_case(src_hand, ref_hand, with_aff, n, 2)

manifest = {
    "producer": IMAGES["FSL"],
    "version": "5.0.9",
    "cases": cases,
    "commands": records,
    "semantics": {
        "coef": "fnirt --cout, cubic spline (intent 2007), or quadratic (intent 2009) in *_quad cases; sform = --aff FLIRT matrix (identity if none)",
        "field_noaff": "fnirtfileutils relative field, spline displacement only (target FSL mm)",
        "field_aff": "fnirtfileutils --withaff relative field: inv(A) @ target_FSL + d - target_FSL",
        "native_coordK": "applywarp --warp=coef of source RAS coordinate ramp K (trilinear)",
        "native_support": "applywarp of the two-voxel source-interior indicator; ==1 where ramps are exact",
    },
    "generator_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
    "hashes": {
        str(p.relative_to(out)): hashlib.sha256(p.read_bytes()).hexdigest()
        for p in sorted(out.glob("*/*"))
        if p.suffix in (".gz", ".mat")
    },
}
(out / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
print(out)
