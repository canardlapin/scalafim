"""Author a complete Core-NIfTI-1 specimen from literal Python data.

Only Python's standard library is used. The metadata dictionaries, axis order,
statistical link and NIfTI header bytes are specified here, independently of
the ScalaFIM encoder and image4s writer.
"""

from pathlib import Path
import hashlib
import json
import struct


OUTPUT = (Path(__file__).resolve().parents[3] /
          "modules/estimates-io/jvm/src/test/resources/estimate-golden/core-literal")
DATASET = "00000000-0000-4000-8000-000000000081"
MODEL = "00000000-0000-4000-8000-000000000082"
UNIT = "00000000-0000-4000-8000-000000000083"
REVISION = "00000000-0000-4000-8000-000000000084"
PREFIX = f"units/{REVISION}"


def json_bytes(value):
    return (json.dumps(value, indent=2, ensure_ascii=False) + "\n").encode("utf-8")


def document(kind, content):
    return json_bytes({"ProfileVersion": "0.2.0",
                       "Schema": "scalafim-estimates-core-nifti-1",
                       "DocumentKind": kind, "Content": content,
                       "WireVersion": "1.0.0"})


def nifti(dtype, bitpix, payload):
    header = bytearray(352)
    struct.pack_into("<i", header, 0, 348)
    struct.pack_into("<8h", header, 40, 3, 2, 1, 1, 1, 1, 1, 1)
    struct.pack_into("<h", header, 70, dtype)
    struct.pack_into("<h", header, 72, bitpix)
    struct.pack_into("<8f", header, 76, *([1.0] * 8))
    struct.pack_into("<f", header, 108, 352.0)
    struct.pack_into("<f", header, 112, 1.0)
    header[123] = 2  # spatial millimetres, unknown fourth-axis temporal units
    struct.pack_into("<h", header, 254, 1)  # scanner sform
    for offset, row in ((280, (1., 0., 0., 0.)),
                        (296, (0., 1., 0., 0.)),
                        (312, (0., 0., 1., 0.))):
        struct.pack_into("<4f", header, offset, *row)
    header[344:348] = b"n+1\0"
    return bytes(header) + payload


def save(name, content):
    path = OUTPUT / name
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)
    return {"Path": name, "SHA256": hashlib.sha256(content).hexdigest(),
            "Bytes": len(content)}


catalog = {"model": MODEL, "entries": [
    {"id": "effect-A", "label": "A effect", "kind": "Coefficient",
     "units": "percent-signal", "normalization": "unit", "definition": "A"},
    {"id": "hypothesis-A", "label": "A versus zero", "kind": "Hypothesis",
     "units": "dimensionless", "normalization": "none", "definition": "A = 0"},
]}
catalog_ref = save(f"{PREFIX}/estimands.json", document("catalog", catalog))
estimands_ref = save(f"{PREFIX}/estimands.tsv",
                     b"index\testimand_id\n0\teffect-A\n1\thypothesis-A\n")
observations_ref = save(f"{PREFIX}/observations.tsv",
                       b"index\tobservation_id\n0\trow-1\n")

representations = []
for product, target, values, validity in (
    ("effect", "effect-A", (2.0, 4.0), (0, 3)),
    ("t-statistic", "hypothesis-A", (1.5, -2.0), (0, 0)),
):
    stem = "effect" if product == "effect" else "t"
    values_ref = save(f"{PREFIX}/{stem}-values.nii",
                      nifti(64, 64, struct.pack("<2d", *values)))
    validity_ref = save(f"{PREFIX}/{stem}-validity.nii",
                        nifti(2, 8, bytes(validity)))
    representations.append({"product": product, "observation": "row-1",
                            "values": values_ref, "validity": validity_ref,
                            "precision": "Float64", "slope": 1, "intercept": 0,
                            "volumeOrder": [target],
                            "selectedTransform": "scanner-sform",
                            "pairOrder": [], "qformAlternativeFrame": None,
                            "storedDatatype": "Float64"})

unknown = {"$type": "Unknown", "reason": "independent Core literal"}
products = [
    {"id": "effect", "kind": "Effect", "precision": "Float64",
     "observations": ["row-1"], "targets": {"$type": "Scalar", "ids": ["effect-A"]},
     "pooling": "Run", "units": "percent-signal"},
    {"id": "t-statistic", "kind": {"$type": "Statistic", "kind": "T"},
     "precision": "Float64", "observations": ["row-1"],
     "targets": {"$type": "Scalar", "ids": ["hypothesis-A"]},
     "pooling": "Run", "units": "dimensionless"},
]
reference_df = {"role": "Reference", "value": {"$type": "Scalar", "value": 9},
                "method": "literal nine residual degrees", "approximate": False}
content = {
    "dataset": DATASET, "unit": UNIT, "revision": REVISION,
    "domain": {"Dimensions": [2, 1, 1],
               "VoxelToWorldRASMillimetres": [1, 0, 0, 0, 0, 1, 0, 0,
                                                0, 0, 1, 0, 0, 0, 0, 1],
               "WorldFrame": "scanner", "Support": [0, 1]},
    "observations": [{"id": "row-1", "participant": {"dataset": DATASET, "label": "01"},
                      "acquisitions": ["run-1"]}],
    "bindings": [], "products": products,
    "outcomes": [[p["id"], {"$type": "Available", "product": p["id"]}]
                 for p in products],
    "estimability": unknown,
    "provenance": {"producer": "python-stdlib-literal", "version": "1",
                   "executionId": "core-literal", "estimator": unknown,
                   "noise": unknown, "nuisance": unknown,
                   "runCombination": unknown, "scans": [], "inputs": []},
    "covariance": [],
    "statistics": [{"product": "t-statistic",
                    "distribution": {"$type": "StudentT", "df": reference_df},
                    "tail": "TwoSided", "nullValue": 0,
                    "effect": {"product": "effect", "correspondence": {
                        "$type": "Known", "mapping": [{"hypothesis": "hypothesis-A",
                                                        "targets": ["effect-A"]}]}},
                    "standardError": None}],
    "degreesOfFreedom": [reference_df], "marginalUncertainty": [],
    "Catalog": catalog_ref, "Representations": representations,
    "Tables": {"estimands": estimands_ref, "observations": observations_ref},
    "ModelRevisionId": MODEL,
}
manifest = save(f"{PREFIX}/estimates.json", document("unit", content))
for ref in [catalog_ref, estimands_ref, observations_ref, *(
        item for representation in representations
        for item in (representation["values"], representation["validity"])), manifest]:
    print(ref["Path"], ref["Bytes"], ref["SHA256"])
