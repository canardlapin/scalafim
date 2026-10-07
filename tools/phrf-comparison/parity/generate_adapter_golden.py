"""Regenerate the shared S1 adapter-output golden with the generator's Python lock.

Uses the frozen adapter and committed HARNESS input with a deterministic wrapper
double. This qualifies serialization and parsing, not GLMsingle estimation.
"""
import hashlib
from pathlib import Path
import struct
import sys
import tempfile

import numpy as np

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "tools/phrf-comparison/glmsingle"))
import from_generator as fg


def runner(data, onsets, conds, **kw):
    voxels = data[0].shape[0]
    trials = sum(len(o) for o in onsets)
    return dict(
        d_beta_psc=np.arange(voxels * trials).reshape(voxels, trials) / 1024.0,
        d_HRFindex=np.zeros(voxels),
        d_R2=np.arange(voxels),
        d_FRACvalue=np.ones(voxels),
        meanvol=np.full(voxels, 100.0),
        noisepool=np.arange(voxels) >= 3,
        noisepool_size=voxels - 3,
        pcnum=0,
        hrf_library=np.ones((20, 2)),
        trial_order=np.arange(trials),
        trial_run=np.concatenate([np.full(len(o), r) for r, o in enumerate(onsets)]),
        trial_cond=np.concatenate(conds),
        trial_vol=np.concatenate(onsets) / kw["tr"],
        meta=dict(
            onset_quantization_max_abs_shift_s=[0.0] * len(onsets),
            types=kw["types"], threads=kw["threads"], seed=kw["seed"],
            glmsingle_commit="deterministic-wrapper-double", versions={},
            log_tail="", cpu_s=0.0, wall_s=0.0,
        ),
    )


with tempfile.TemporaryDirectory() as scratch:
    fixture = ROOT / "modules/phrf-comparison/jvm/src/test/resources/fixtures/T-TX-fast__d0000.npz"
    output, _, _ = fg.bridge(str(fixture), scratch, runner=runner)
    blob = Path(output).read_bytes()
    arrays = fg.strict_npz_read(blob, allowed=("<f8",))
    parts = []
    for name, array in sorted(arrays.items()):
        shape = "Vector(" + ", ".join(map(str, array.shape)) + ")"
        bits = [format(struct.unpack(">Q", struct.pack(">d", x))[0], "x") for x in array.flat]
        parts.append(f"{name}=f8{shape}:" + ",".join(bits))
    fingerprint = hashlib.sha256(";".join(parts).encode()).hexdigest()
    chunks = [blob.hex()[i:i + 100] for i in range(0, len(blob.hex()), 100)]
    text = "\n".join([
        "package scalafim.phrfcmp.ingest",
        "",
        "/** Frozen adapter output with a deterministic wrapper double; see generate_adapter_golden.py. */",
        "object AdapterGoldenBytes:",
        f'  val NpzSha256: String = "{hashlib.sha256(blob).hexdigest()}"',
        f'  val FingerprintSha256: String = "{fingerprint}"',
        "",
        "  private val hex: String =",
        " +\n".join(f'    "{chunk}"' for chunk in chunks),
        "",
        "  def npz: Array[Byte] = hex.grouped(2).map(h => Integer.parseInt(h, 16).toByte).toArray",
        "",
    ])
    target = ROOT / "modules/phrf-comparison/shared/src/test/scala/scalafim/phrfcmp/ingest/AdapterGoldenBytes.scala"
    target.write_text(text)
    print(f"{len(blob)} bytes; sha256={hashlib.sha256(blob).hexdigest()}; fingerprint={fingerprint}")
