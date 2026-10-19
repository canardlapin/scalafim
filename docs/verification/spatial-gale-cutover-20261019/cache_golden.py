"""Independent encoding of the ASCII cache-v1 fixture used by the JVM test."""

import hashlib
import math
import struct


def cache_bytes():
    payload = bytearray()

    def integer(value):
        payload.extend(struct.pack(">i", value))

    def utf(value):
        # All fixture strings are ASCII, so Java writeUTF uses these same bytes.
        encoded = value.encode("ascii")
        payload.extend(struct.pack(">H", len(encoded)))
        payload.extend(encoded)

    def provenance():
        integer(2)
        utf("func-to-t1w")
        utf("t1w-to-mni")
        utf("Anatomical")
        utf("Trilinear")
        payload.append(1)
        integer(2)
        integer(3)
        integer(1)
        payload.append(1)
        utf("volume-affine-v1")

    utf("scalafim-spatial-triplets")
    integer(1)
    utf("func")
    utf("mni")
    provenance()
    integer(2)
    integer(4)
    provenance()
    integer(2)
    integer(4)
    integer(3)
    for row, col, value in [(0, 1, math.pi), (0, 3, -0.0), (1, 0, 0.125)]:
        integer(row)
        integer(col)
        payload.extend(struct.pack(">d", value))
    return bytes(payload)


if __name__ == "__main__":
    data = cache_bytes()
    digest = hashlib.sha256(data).hexdigest()
    assert len(data) == 278
    assert digest == "43b3ffab2af5f15ea4133baf23a2b9b519bcffb7df8a416b4afaa0c58532c50a"
    print(f"{digest}  {len(data)} bytes")
