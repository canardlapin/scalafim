"""Generate small native FSL regression fixtures; never uses neurotransform."""
from pathlib import Path
import hashlib
import json
import os
import shutil
import sys
import tempfile
import numpy as np
import nibabel as nib

sys.path.insert(0, str(Path(__file__).parent / "visual_qa"))
from run import ROOT, IMAGES, save, world_grid, phantom, supporting_images, native, apply_commands

# Set NEUROTRANSFORM_ORACLE_OUT to verify reproducibility without replacing fixtures.
out = Path(os.environ.get("NEUROTRANSFORM_ORACLE_OUT", ROOT / "inst/extdata/fsl_dense_oracle"))
out.mkdir(parents=True, exist_ok=True)
# The work directory must be shareable with Docker; override if the default is not.
work = Path(os.environ.get("NEUROTRANSFORM_ORACLE_WORK", tempfile.gettempdir())) / "neurotransform-fsl-dense-oracle"
work.mkdir(parents=True, exist_ok=True)
records, cases = [], []


def geometry(shape, spacing, angle, hand, origin):
    c, s = np.cos(angle), np.sin(angle)
    a = np.eye(4)
    a[:3,:3] = np.array([[c,-s,0],[s,c,0],[0,0,1]]) @ np.diag(spacing)
    a[:3,3] = -a[:3,:3] @ ((np.array(shape)-1)/2) + origin
    if hand == "left":
        a[:3,3] += a[:3,0]*(shape[0]-1)
        a[:3,0] *= -1
    return a


for src_hand in ("left", "right"):
    for ref_hand in ("left", "right"):
        for representation in ("relative", "absolute"):
            cid = f"{src_hand}_{ref_hand}_{representation}"
            folder = work/cid
            folder.mkdir(exist_ok=True)
            sa = geometry((19,21,23),(1.8,1.6,1.7),.19,src_hand,np.array([1.,-2.,3.]))
            ta = geometry((11,13,15),(1.2,1.3,1.5),-.11,ref_hand,np.array([-1.,1.,-2.]))
            save(folder/"source.nii.gz",phantom((19,21,23),sa),sa)
            save(folder/"target.nii.gz",np.zeros((11,13,15)),ta)
            p = world_grid((11,13,15),ta)
            d = np.stack([.5*np.sin(p[...,1]/8),.4*np.cos(p[...,0]/7),.3*np.sin(p[...,2]/9)],axis=-1)
            save(folder/"seed.nii.gz",d,ta)
            supporting_images(folder,folder/"source.nii.gz")
            commands = [["convertwarp","--ref=target.nii.gz","--warp1=seed.nii.gz","--rel",
                         "--relout" if representation=="relative" else "--absout","--out=warp.nii.gz"]]
            native(work,"FSL",cid,commands+apply_commands("FSL","warp.nii.gz","warp",
                   "rel" if representation=="relative" else "abs"),records)
            dest=out/cid
            dest.mkdir(exist_ok=True)
            for name in ["source","target","warp","native_source","native_coord0","native_coord1",
                         "native_coord2","native_support"]:
                shutil.copyfile(folder/(name+".nii.gz"),dest/(name+".nii.gz"))
            cases.append({"id":cid,"representation":representation})

manifest={"producer":IMAGES["FSL"],"version":"5.0.9","cases":cases,"commands":records,
          "generator_sha256":hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
          "hashes":{str(p.relative_to(out)):hashlib.sha256(p.read_bytes()).hexdigest()
                    for p in out.glob("*/*.nii.gz")}}
(out/"manifest.json").write_text(json.dumps(manifest,indent=2)+"\n")
print(out)
