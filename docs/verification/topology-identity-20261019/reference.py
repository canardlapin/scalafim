"""Independent legacy identity calculation; Python stdlib, no ScalaFIM imports.

Run with python3 reference.py. Byte order and face order are part of the protocol.
These constants pin compatibility, not collision resistance of the Murmur key.
"""
import hashlib
import json
import struct

MASK = 0xFFFFFFFF


def rotl(value, bits):
    return ((value << bits) | (value >> (32 - bits))) & MASK


def mix(hash_value, value):
    value = (value * 0xCC9E2D51) & MASK
    value = (rotl(value, 15) * 0x1B873593) & MASK
    return (rotl(hash_value ^ value, 13) * 5 + 0xE6546B64) & MASK


def finalize(hash_value, count):
    hash_value ^= count
    hash_value ^= hash_value >> 16
    hash_value = (hash_value * 0x85EBCA6B) & MASK
    hash_value ^= hash_value >> 13
    hash_value = (hash_value * 0xC2B2AE35) & MASK
    return hash_value ^ (hash_value >> 16)


def topology(vertices, faces):
    high = mix(0x3C074A61, vertices)
    low = mix(0x1B873593, len(faces))
    for index, value in enumerate(faces):
        high = mix(high, value)
        low = mix(low, value ^ index)
    key = (finalize(high, len(faces) + 1) << 32) | finalize(low, len(faces) + 1)
    return {"hex": f"{key:016x}", "decimal": str(key if key < 2**63 else key - 2**64)}


def string(value):
    encoded = value.encode("utf-8")
    return struct.pack("<i", len(encoded)) + encoded


def surface(hemisphere, faces):
    return (b"NPUDOM1\0" + string("org.neuropublish.domain/surface-vertices")
            + string("1") + string("fsaverage6") + string(hemisphere)
            + struct.pack("<qq", 4, 2) + struct.pack("<6i", *faces))


def fingerprints(faces):
    left, right = surface("left", faces), surface("right", faces)
    bilateral = (b"SCALAFIM-BILATERAL-SURFACE1\0" + string("fsaverage6")
                 + struct.pack("<q", len(left)) + left
                 + struct.pack("<q", len(right)) + right)
    return {name: hashlib.sha256(value).hexdigest()
            for name, value in [("left", left), ("right", right), ("bilateral", bilateral)]}


if __name__ == "__main__":
    quad = [0, 1, 2, 0, 2, 3]
    reordered = [0, 2, 3, 0, 1, 2]
    print(json.dumps({
        "quad": topology(4, quad),
        "reorderedQuad": topology(4, reordered),
        "disconnectedBanks": topology(6, [0, 1, 2, 3, 4, 5]),
        "quadSurface": fingerprints(quad),
        "reorderedQuadSurface": fingerprints(reordered),
    }, indent=2))
