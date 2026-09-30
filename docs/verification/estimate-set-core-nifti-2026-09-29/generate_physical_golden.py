"""Independent NIfTI-1 byte producer for the tiny estimate-axis fixture.

This script uses only Python's standard library and writes the public NIfTI-1
header fields directly. It never calls image4s or ScalaFIM.
"""

from pathlib import Path
import hashlib
import gzip
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


def singleton_3d_big_endian(dtype: int, bits: int, payload: bytes, slope: float, intercept: float) -> bytes:
    header = bytearray(352)
    struct.pack_into(">i", header, 0, 348)
    struct.pack_into(">8h", header, 40, 3, 2, 1, 1, 1, 1, 1, 1)
    struct.pack_into(">h", header, 70, dtype)
    struct.pack_into(">h", header, 72, bits)
    struct.pack_into(">8f", header, 76, 1.0, 2.0, 3.0, 4.0, 1.0, 1.0, 1.0, 1.0)
    struct.pack_into(">f", header, 108, 352.0)
    struct.pack_into(">f", header, 112, slope)
    struct.pack_into(">f", header, 116, intercept)
    header[123] = 2  # millimetres, no temporal unit
    struct.pack_into(">h", header, 254, 1)
    struct.pack_into(">4f", header, 280, -2.0, 0.0, 0.0, 8.0)
    struct.pack_into(">4f", header, 296, 0.0, 3.0, 0.0, -4.0)
    struct.pack_into(">4f", header, 312, 0.0, 0.0, 4.0, 2.0)
    header[344:348] = b"n+1\0"
    return bytes(header) + payload


fixtures = {
    "values.nii": nifti(64, 64, struct.pack("<4d", 2.0, 4.0, 6.0, 8.0)),
    "validity.nii": nifti(2, 8, bytes((0, 3, 0, 0))),
    "values-f64-fraction.nii": nifti(64, 64, struct.pack("<4d", 0.1, 0.1, 6.0, 8.0)),
    "values-3d-be-scaled.nii": singleton_3d_big_endian(16, 32, struct.pack(">2f", 1.5, -2.0), 2.0, 1.0),
    "values-3d-be-fraction.nii": singleton_3d_big_endian(16, 32, struct.pack(">2f", 1.5, -2.0), 0.1, 0.0),
    "validity-3d.nii": singleton_3d_big_endian(2, 8, bytes((0, 0)), 1.0, 0.0),
}
fixtures["values-3d-be-scaled.nii.gz"] = gzip.compress(fixtures["values-3d-be-scaled.nii"], mtime=0)
fixtures["validity-3d.nii.gz"] = gzip.compress(fixtures["validity-3d.nii"], mtime=0)
for name, content in fixtures.items():
    path = OUTPUT / name
    path.write_bytes(content)
    print(f"{name} bytes={len(content)} sha256={hashlib.sha256(content).hexdigest()}")
