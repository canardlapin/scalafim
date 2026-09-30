"""Independent literal Core-3 fixture: Python stdlib only, no Scala encoder."""
import gzip
import hashlib
import json
import pathlib
import struct

ROOT = pathlib.Path(__file__).resolve().parent
REV = "00000000-0000-4000-8000-000000000104"
PREFIX = f"units/{REV}"
DATASET = "00000000-0000-4000-8000-000000000101"
MODEL = "00000000-0000-4000-8000-000000000102"
UNIT = "00000000-0000-4000-8000-000000000103"
AFFINE = [-2, 0, 0, 8, 0, 3, 0, -4, 0, 0, 4, 2, 0, 0, 0, 1]
FIT = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
HA = [0, 1, 2, 3, 4, 2, 2, 2, 2, 2]
HB = [0, 1, 2, 2, 2, 5, 6, 7, 8, 9]
PLANES = [{"$type": "Fit", "observation": "row"},
          {"$type": "Hypothesis", "observation": "row", "hypothesisId": "hA"},
          {"$type": "Hypothesis", "observation": "row", "hypothesisId": "hB"}]


def put(name, data):
    path = ROOT / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(data)
    return {"Path": name, "SHA256": hashlib.sha256(data).hexdigest(), "Bytes": len(data)}


def literal(name, value):
    return put(name, (json.dumps(value, indent=2, ensure_ascii=False) + "\n").encode())


def document(kind, content, schema="scalafim-estimates-core-nifti-1", wire="1.0.0"):
    return {"ProfileVersion": "0.2.0", "Schema": schema, "DocumentKind": kind,
            "Content": content, "WireVersion": wire}


def nifti(data, dtype, planes, dims=(10, 1, 1), slope=1, singleton=False):
    h = bytearray(352)
    struct.pack_into("<i", h, 0, 348)
    shape = [3 if singleton else 4, *dims, planes, 1, 1, 1]
    struct.pack_into("<8h", h, 40, *shape)
    struct.pack_into("<hh", h, 70, dtype, 8 if dtype == 2 else 64)
    struct.pack_into("<8f", h, 76, 1, 2, 3, 4, 1, 1, 1, 1)
    struct.pack_into("<3f", h, 108, 352, slope, 0)
    h[123] = 2  # millimetres, unknown time units
    struct.pack_into("<h", h, 254, 1)  # scanner sform, qform uncoded
    for row in range(3):
        struct.pack_into("<4f", h, 280 + row * 16, *AFFINE[row * 4:row * 4 + 4])
    h[344:348] = b"n+1\0"
    return bytes(h) + (bytes(data) if dtype == 2 else struct.pack("<" + "d" * len(data), *data))


def main():
    unknown = {"$type": "Unknown", "reason": "synthetic independent fixture"}
    entries = [{"id": e, "label": e, "kind": "Coefficient" if e in ("A", "B") else "Hypothesis",
                "units": "signal" if e in ("A", "B") else "dimensionless",
                "normalization": "explicit", "definition": e} for e in ("A", "B", "hA", "hB")]
    catalog = literal(f"{PREFIX}/estimands.json", document("catalog", {"model": MODEL, "entries": entries}))
    estimands = put(f"{PREFIX}/estimands.tsv", b"index\testimand_id\n0\tA\n1\tB\n2\thA\n3\thB\n")
    observations = put(f"{PREFIX}/observations.tsv", b"index\tobservation_id\n0\trow\n")
    products, representations = [], []
    for product, ids in (("effect", ["A", "B"]), ("t", ["hA", "hB"])):
        products.append({"id": product, "kind": "Effect" if product == "effect" else {"$type": "Statistic", "kind": "T"},
                         "precision": "Float64", "observations": ["row"],
                         "targets": {"$type": "Scalar", "ids": ids}, "pooling": "Run",
                         "units": "signal" if product == "effect" else "dimensionless"})
        values = put(f"{PREFIX}/{product}-values.nii", nifti(list(range(10)) + list(range(100, 110)), 64, 2))
        # NotComputed is deliberately independent of explanatory status.
        validity = put(f"{PREFIX}/{product}-validity.nii", nifti(([1] + [5] * 9) * 2, 2, 2))
        representations.append({"Tag": "Nifti", "Content": {"product": product, "observation": "row",
            "values": values, "validity": validity, "precision": "Float64", "slope": 1, "intercept": 0,
            "volumeOrder": ids, "selectedTransform": "scanner-sform", "pairOrder": [],
            "qformAlternativeFrame": None, "storedDatatype": "Float64"}})
    payload = nifti(FIT + HA + HB, 2, 3)
    status = put(f"{PREFIX}/status.nii", payload)
    put(f"{PREFIX}/status.nii.gz", gzip.compress(payload, mtime=0))
    put(f"{PREFIX}/single-3d.nii", nifti(FIT, 2, 1, singleton=True))
    bindings = [{"estimand": e, "columnIds": ["task", "nuisance"], "rows": 1, "weights": weights}
                for e, weights in (("A", [1, 0]), ("B", [0, 1]), ("hA", [1, -1]), ("hB", [2, 1]))]
    evidence = {"coefficients": [{"observation": "row", "columns": ["task", "nuisance"],
                                 "inferableColumns": ["task"], "scopeLabel": "task only",
                                 "method": {"$type": "Known", "description": "conditional bootstrap: draws=17 seed=23"},
                                 "conditioning": {"$type": "Unknown", "reason": "learned response subspace not retained by this materialized result"}}],
                "planes": PLANES}
    content = {"dataset": DATASET, "unit": UNIT, "revision": REV,
               "domain": {"Dimensions": [10, 1, 1], "VoxelToWorldRASMillimetres": AFFINE,
                          "WorldFrame": "scanner", "Support": list(range(1, 10))},
               "observations": [{"id": "row", "participant": {"dataset": DATASET, "label": "01"}, "acquisitions": ["run-1"]}],
               "bindings": bindings, "products": products,
               "outcomes": [[p["id"], {"$type": "Available", "product": p["id"]}] for p in products],
               "estimability": unknown, "provenance": {"producer": "python-stdlib", "version": "1", "executionId": "inference-evidence",
                 "estimator": unknown, "noise": unknown, "nuisance": unknown, "runCombination": unknown, "scans": [], "inputs": []},
               "covariance": [], "statistics": [{"product": "t", "distribution": unknown, "tail": None, "nullValue": None, "effect": None, "standardError": None}],
               "degreesOfFreedom": [], "marginalUncertainty": [], "Catalog": catalog,
               "Representations": representations, "Tables": {"estimands": estimands, "observations": observations},
               "ModelRevisionId": MODEL, "inferenceEvidence": evidence,
               "InferenceStatus": {"Tag": "UInt8Nifti", "Content": {"file": status, "planes": PLANES}}}
    literal(f"{PREFIX}/estimates.json", document("unit", content, "scalafim-estimates-core-nifti-3", "3.0.0"))
    literal("expected.json", {"planes": PLANES, "codes": [FIT, HA, HB], "permutedSamples": [9, 2, 0, 4],
                              "permutedPlanes": [2, 0, 1], "expectedPermuted": [9, 2, 0, 2, 9, 2, 0, 4, 2, 2, 0, 4],
                              "statusBytes": 382, "statusCoverageBytes": 30})
    hashes = [f"{hashlib.sha256(p.read_bytes()).hexdigest()}  {p.relative_to(ROOT)}\n"
              for p in sorted(ROOT.rglob("*")) if p.is_file() and p.name != "SHA256SUMS"]
    (ROOT / "SHA256SUMS").write_text("".join(hashes))


if __name__ == "__main__":
    main()
