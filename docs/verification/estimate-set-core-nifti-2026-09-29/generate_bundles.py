"""Generate complete estimate bundles without importing image4s or ScalaFIM.

Only Python's standard library encodes the metadata and NIfTI-1 bytes. The
development schema is deliberately explicit here so a codec change has an
independent reader fixture to meet.
"""

from pathlib import Path
import hashlib
import json
import struct


ROOT = Path(__file__).resolve().parents[3]
OUTPUT = ROOT / "modules/estimates-io/jvm/src/test/resources/estimate-golden/bundles"
DATASET = "00000000-0000-4000-8000-000000000051"
MODEL = "00000000-0000-4000-8000-000000000052"
UNIT = "00000000-0000-4000-8000-000000000053"
REVISION = "00000000-0000-4000-8000-000000000054"
PREFIX = f"units/{REVISION}"


def encoded(value):
    return (json.dumps(value, indent=2, ensure_ascii=False) + "\n").encode("utf-8")


def document(kind, content):
    return encoded({"ProfileVersion": "0.2.0", "Schema": "scalafim-estimates-development-1",
                    "DocumentKind": kind, "Content": content})


def nifti(dtype, bits, payload):
    header = bytearray(352)
    struct.pack_into("<i", header, 0, 348)
    struct.pack_into("<8h", header, 40, 4, 2, 1, 1, 2, 1, 1, 1)
    struct.pack_into("<h", header, 70, dtype)
    struct.pack_into("<h", header, 72, bits)
    struct.pack_into("<8f", header, 76, *([1.0] * 8))
    struct.pack_into("<f", header, 108, 352.0)
    struct.pack_into("<f", header, 112, 1.0)
    header[123] = 2
    struct.pack_into("<h", header, 254, 1)
    for offset, row in ((280, (1., 0., 0., 0.)), (296, (0., 1., 0., 0.)),
                        (312, (0., 0., 1., 0.))):
        struct.pack_into("<4f", header, offset, *row)
    header[344:348] = b"n+1\0"
    return bytes(header) + payload


def build(case):
    root = OUTPUT / case

    def save(path, data):
        full = root / path
        full.parent.mkdir(parents=True, exist_ok=True)
        full.write_bytes(data)
        return {"Path": path, "SHA256": hashlib.sha256(data).hexdigest(), "Bytes": len(data)}

    statistic = case == "statistic-only"
    deficient = case == "deficient-rank"
    product_id = "z-statistic" if statistic else "effect"
    catalog = {"model": MODEL, "entries": [
        {"id": name, "label": name, "kind": "Hypothesis" if statistic else "Coefficient",
         "units": "signal", "normalization": "unit", "definition": name}
        for name in ("A", "B")
    ]}
    catalog_ref = save(f"{PREFIX}/estimands.json", document("catalog", catalog))
    estimands_ref = save(f"{PREFIX}/estimands.tsv", b"index\testimand_id\n0\tA\n1\tB\n")
    observations_ref = save(f"{PREFIX}/observations.tsv", b"index\tobservation_id\n0\trow\n")
    values = (1., 2., 3., 4.) if statistic else (2., 4., 6., 8.)
    validity = (0, 0, 0, 0) if statistic else (0, 3, 0, 0)
    values_ref = save(f"{PREFIX}/values.nii", nifti(64, 64, struct.pack("<4d", *values)))
    validity_ref = save(f"{PREFIX}/validity.nii", nifti(2, 8, bytes(validity)))
    estimability = {"$type": "Unknown", "reason": "synthetic fixture"}
    if deficient:
        basis_ref = save("evidence/estimable-subspace.tsv", b"column\tcomponent\nA\t1\nB\t0\n")
        estimability = {"$type": "Subspace", "columns": ["A", "B"], "basis": basis_ref,
                        "rank": 1, "tolerance": 1e-8, "method": "synthetic SVD"}
    unknown = {"$type": "Unknown", "reason": "synthetic fixture"}
    content = {
        "dataset": DATASET, "unit": UNIT, "revision": REVISION,
        "domain": {"Dimensions": [2, 1, 1],
                   "VoxelToWorldRASMillimetres": [1, 0, 0, 0, 0, 1, 0, 0,
                                                    0, 0, 1, 0, 0, 0, 0, 1],
                   "WorldFrame": "scanner", "Support": [0, 1]},
        "observations": [{"id": "row", "participant": {"dataset": DATASET, "label": "01"},
                          "acquisitions": ["run-1"]}],
        "bindings": [],
        "products": [{"id": product_id,
                      "kind": {"$type": "Statistic", "kind": "Z"} if statistic else "Effect",
                      "precision": "Float64", "observations": ["row"],
                      "targets": {"$type": "Scalar", "ids": ["A", "B"]},
                      "pooling": "Run", "units": "dimensionless" if statistic else "signal"}],
        "outcomes": [[product_id, {"$type": "Available", "product": product_id}]],
        "estimability": estimability,
        "provenance": {"producer": "python-stdlib", "version": "1", "executionId": case,
                       "estimator": unknown, "noise": unknown, "nuisance": unknown,
                       "runCombination": unknown, "scans": [], "inputs": []},
        "Catalog": catalog_ref,
        "Representations": [{"product": product_id, "observation": "row",
                             "values": values_ref, "validity": validity_ref,
                             "precision": "Float64", "slope": 1, "intercept": 0,
                             "volumeOrder": ["A", "B"], "selectedTransform": "scanner-sform"}],
        "Tables": {"estimands": estimands_ref, "observations": observations_ref},
        "ModelRevisionId": MODEL,
    }
    if statistic:
        content["statistics"] = [{"product": product_id, "distribution": "Normal",
                                  "tail": "TwoSided", "nullValue": 0,
                                  "effect": None, "standardError": None}]
    manifest = save(f"{PREFIX}/estimates.json", document("unit", content))
    print(case, manifest["SHA256"], flush=True)


for fixture_case in ("effects-only", "statistic-only", "deficient-rank"):
    build(fixture_case)
