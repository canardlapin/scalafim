import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import urllib.parse
import urllib.request

parser = argparse.ArgumentParser(description="Verify demo1 assets against a pinned public derivative and fetch its two small scanner transforms.")
parser.add_argument("--source-dir", type=Path, required=True)
parser.add_argument("--out", type=Path, required=True)
args = parser.parse_args()
OUT = args.out
OUT.mkdir(parents=True, exist_ok=True)
COMMIT = "1d8407e467d1af0ac8933c0539bbbb0168badab6"
BASE = f"https://raw.githubusercontent.com/OpenNeuroDerivatives/ds002748-fmriprep/{COMMIT}/"
LOCAL = args.source_dir.resolve()

def fetch(url):
    with urllib.request.urlopen(url, timeout=30) as response:
        return response.read()

records = []
for name in ["dataset_description.json", "README.md"]:
    data = fetch(BASE + name)
    (OUT / name).write_bytes(data)
    records.append(dict(path=name, url=BASE + name, sha256=hashlib.sha256(data).hexdigest()))

links = {}
for directory in ["anat", "func"]:
    url = f"https://api.github.com/repos/OpenNeuroDerivatives/ds002748-fmriprep/contents/sub-01/{directory}?ref={COMMIT}"
    data = fetch(url)
    (OUT / f"upstream-{directory}.json").write_bytes(data)
    entries = json.loads(data)
    for entry in entries:
        name = entry["name"]
        if (directory == "anat" and (name.endswith("T1w.nii.gz") or name.endswith(".h5"))) or (directory == "func" and (name == "sub-01_task-rest_boldref.nii.gz" or (name.startswith("sub-01_task-rest_from-") and name.endswith(".txt")))):
            target = fetch(BASE + entry["path"]).decode().strip()
            links[name] = dict(path=entry["path"], target=target, url=BASE + entry["path"])

assets = {}
for path in LOCAL.iterdir():
    if path.suffix != ".h5" and path.name not in ["sub-01_T1w.nii.gz", "sub-01_boldref.nii.gz"]:
        continue
    data = path.read_bytes()
    md5 = hashlib.md5(data).hexdigest()
    matches = [(name, value) for name, value in links.items() if f"--{md5}." in value["target"] and f"-s{len(data)}--" in value["target"]]
    if len(matches) != 1:
        raise RuntimeError(f"Expected exactly one upstream annex match for {path.name}: {matches}")
    upstream_name, upstream = matches[0]
    blob = subprocess.check_output(["git", "-C", str(LOCAL.parents[2]), "rev-parse", f"HEAD:inst/extdata/demo1/{path.name}"], text=True).strip()
    assets[path.name] = dict(size=len(data), md5=md5, sha256=hashlib.sha256(data).hexdigest(), git_blob=blob, upstream_name=upstream_name, **upstream)

for name, expected in [("sub-01_task-rest_from-scanner_to-T1w_mode-image_xfm.txt", (237,"7a6c6423a86b4f2f222ed7c036150ce2")), ("sub-01_task-rest_from-T1w_to-scanner_mode-image_xfm.txt", (235,"e1644fcd5c7c33c6207399f669cb0a8d"))]:
    prefix = "fmriprep/ds002748-fmriprep/sub-01/func/" + name
    url = "https://openneuro-derivatives.s3.amazonaws.com/" + prefix
    data = fetch(url)
    if (len(data), hashlib.md5(data).hexdigest()) != expected:
        raise RuntimeError(f"Unexpected immutable annex payload for {name}")
    (OUT / name).write_bytes(data)
    assets[name] = dict(size=len(data), md5=expected[1], sha256=hashlib.sha256(data).hexdigest(), payload_url=url, **links[name])

source = dict(upstream_repository="https://github.com/OpenNeuroDerivatives/ds002748-fmriprep", upstream_commit=COMMIT, local_repository="neurotransform", local_commit=subprocess.check_output(["git", "-C", str(LOCAL.parents[2]), "rev-parse", "HEAD"],text=True).strip(), license=json.loads((OUT/"dataset_description.json").read_text())["License"], assets=assets, metadata=records)
(OUT / "source-provenance.json").write_text(json.dumps(source, indent=2) + "\n")
print(json.dumps({"upstream_commit":COMMIT,"license":source["license"],"verified_assets":list(assets)},indent=2))
