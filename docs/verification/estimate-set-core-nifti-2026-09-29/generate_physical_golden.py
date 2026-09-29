"""Independent NIfTI-1 byte producer for the tiny estimate-axis fixture.

This script uses only Python's standard library and writes the public NIfTI-1
header fields directly. It never calls image4s or ScalaFIM.
"""

from pathlib import Path
import hashlib
import struct


ROOT = Path(__file__).resolve().parents[3]
OUTPUT = ROOT / "modules/estimates-io/jvm/src/test/resources/estimate-golden"
OUTPUT.mkdir(parents=True, exist_ok=True)


def nifti(dtype: int, bits: int, payload: bytes) -> bytes:
    header = bytearray(352)
    struct.pack_into("<i", header, 0, 348)
    struct.pack_into("<8h", header, 40, 4, 2, 1, 1, 2, 1, 1, 1)
    struct.pack_into("<h", header, 70, dtype)
    struct.pack_into("<h", header, 72, bits)
    struct.pack_into("<8f", header, 76, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
    struct.pack_into("<f", header, 108, 352.0)
    struct.pack_into("<f", header, 112, 1.0)
    struct.pack_into("<f", header, 116, 0.0)
    header[123] = 2  # spatial millimetres; no temporal unit on the estimand axis
    struct.pack_into("<f", header, 136, 0.0)
    struct.pack_into("<h", header, 254, 1)  # scanner sform
    struct.pack_into("<4f", header, 280, 1.0, 0.0, 0.0, 0.0)
    struct.pack_into("<4f", header, 296, 0.0, 1.0, 0.0, 0.0)
    struct.pack_into("<4f", header, 312, 0.0, 0.0, 1.0, 0.0)
    header[344:348] = b"n+1\0"
    return bytes(header) + payload


fixtures = {
    "values.nii": nifti(64, 64, struct.pack("<4d", 2.0, 4.0, 6.0, 8.0)),
    "validity.nii": nifti(2, 8, bytes((0, 3, 0, 0))),
}
for name, content in fixtures.items():
    path = OUTPUT / name
    path.write_bytes(content)
    print(f"{name} bytes={len(content)} sha256={hashlib.sha256(content).hexdigest()}")
