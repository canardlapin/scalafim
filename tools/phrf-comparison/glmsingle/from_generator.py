"""Bridge from a phrf_gen trial dataset (T-G cell) to the pinned GLMsingle wrapper (slice S6).

v0-frozen (pilot-runner design v2.1, sections 1.1, 1.3, 2.2, F8).  The adaptor

1. verifies the generator manifest (schema, whole-file and per-member SHA-256, dtype/shape,
   denylist flags, stream seeds recomputed from the root) and REFUSES unless the cell is a trial
   cell with ``tr_aligned_onsets`` true;
2. builds GLMsingle's TR-resolution design (the wrapper's ``build_design``; onsets must lie on
   the TR grid, so the realized quantization is exactly zero);
3. adds +100.0 to every sample of every scored voxel (``y``) AND every pool voxel (``y_pool``),
   for the GLMsingle input only; the generator arrays are never modified;
4. calls ``run_glmsingle`` in-process: ``types="BD"``, ``threads=1``, ``seed`` = the ``audit``
   stream seed of (root, cell, dataset), under a wall-clock timeout;
5. converts betas to data units in float64 from GLMsingle's own ``meanvol`` and the HRF it chose:
   ``beta_data = beta_psc / 100 * |meanvol| * peak(h_used)``;
6. writes ``<id>.glmsingle.npz`` (STORED, float64 arrays only, sorted members, fixed
   timestamps, .npy v1.0) and ``<id>.glmsingle.meta.json`` (sidecar).

Output arrays (all ``<f8``; V scored voxels, Vp pool voxels, N trials), in GLMsingle's
CHRONOLOGICAL trial order.  Use ``trial_event_index`` to return to generator event order:
``out_in_event_order[:, trial_event_index[k]] = out_chrono[:, k]``.

  d_beta_data (V,N)   type-D betas in data units (what E-trial is scored on)
  d_beta_psc  (V,N)   GLMsingle percent-signal-change betas, as returned
  d_HRFindex  (V)     0-based HRF library index
  d_R2        (V)     percent
  d_FRACvalue (V)     fractional ridge value
  meanvol     (V)     GLMsingle's per-voxel mean (includes the +100)
  noisepool   (V+Vp)  0/1 noise-pool membership, scored voxels first
  hrf_library (20,L)  peak-1 HRF library, TR-sampled
  trial_event_index, trial_run, trial_cond, trial_vol   (N), chronological

CLI:  python from_generator.py DATASET.npz [--out DIR] [--timeout S] [--expect-generator-sha H]
Exit codes: 0 ok, 2 refusal, 3 GLMsingle failure, 4 timeout.
"""
from __future__ import annotations

import os

# Single-threaded numerics must be requested before numpy/numba load (belt and braces with
# threadpool_limits inside the wrapper).
for _v in (
    "OMP_NUM_THREADS",
    "OPENBLAS_NUM_THREADS",
    "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS",
    "NUMBA_NUM_THREADS",
):
    os.environ.setdefault(_v, "1")

import argparse  # noqa: E402
import ast  # noqa: E402
import hashlib  # noqa: E402
import io  # noqa: E402
import json  # noqa: E402
import signal  # noqa: E402
import struct  # noqa: E402
import sys  # noqa: E402
import zipfile  # noqa: E402

import numpy as np  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
GENERATOR_DIR = os.path.normpath(os.path.join(HERE, "..", "generator"))
BASELINE = 100.0
SCHEMA = "phrf-gen-npz-1"
OUT_SCHEMA = "phrf-glmsingle-npz-1"
_FIXED_DATE = (1980, 1, 1, 0, 0, 0)
DEFAULT_TIMEOUT_S = 900.0


class Refusal(Exception):
    """Input does not satisfy the bridge contract (exit 2)."""


class GlmSingleTimeout(BaseException):
    """The wrapper exceeded the wall-clock limit (exit 4)."""


class StrictNpzError(Exception):
    """Output is not readable by the strict S1 reader."""


# ---------------------------------------------------------------- hashing and strict npz


def sha256_bytes(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


def sha256_file(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def npy_bytes(a: np.ndarray) -> bytes:
    buf = io.BytesIO()
    np.lib.format.write_array(
        buf, np.ascontiguousarray(a), version=(1, 0), allow_pickle=False
    )
    return buf.getvalue()


def npz_bytes(arrays: dict) -> bytes:
    """Byte-deterministic zip: sorted members, STORED, fixed timestamps (generator convention)."""
    buf = io.BytesIO()
    with zipfile.ZipFile(buf, "w", zipfile.ZIP_STORED) as z:
        for name in sorted(arrays):
            zi = zipfile.ZipInfo(name + ".npy", date_time=_FIXED_DATE)
            zi.compress_type = zipfile.ZIP_STORED
            zi.external_attr = 0o644 << 16
            z.writestr(zi, npy_bytes(arrays[name]))
    return buf.getvalue()


def strict_npz_read(path_or_bytes, allowed=("<f8", "<i4")):
    """Mirror of the S1 strict reader. Returns {name: ndarray}; raises StrictNpzError.

    Refuses: non-STORED members, non-.npy member names, .npy versions other than 1.0,
    fortran order, dtypes outside ``allowed`` (so no strings/objects/bool), shape/length
    mismatch, trailing bytes, duplicate members.
    """
    src = (
        io.BytesIO(path_or_bytes)
        if isinstance(path_or_bytes, (bytes, bytearray))
        else path_or_bytes
    )
    out = {}
    try:
        zf = zipfile.ZipFile(src)
    except zipfile.BadZipFile as e:
        raise StrictNpzError(f"not a zip: {e}")
    with zf:
        infos = zf.infolist()
        names = [i.filename for i in infos]
        if len(set(names)) != len(names):
            raise StrictNpzError("duplicate members")
        for zi in infos:
            if zi.compress_type != zipfile.ZIP_STORED:
                raise StrictNpzError(
                    f"{zi.filename}: compressed member (type {zi.compress_type})"
                )
            if not zi.filename.endswith(".npy"):
                raise StrictNpzError(f"{zi.filename}: not a .npy member")
            if zi.flag_bits & 0x1:
                raise StrictNpzError(f"{zi.filename}: encrypted")
            raw = zf.read(zi)
            if len(raw) != zi.file_size:
                raise StrictNpzError(f"{zi.filename}: size mismatch")
            if raw[:6] != b"\x93NUMPY":
                raise StrictNpzError(f"{zi.filename}: bad magic")
            if raw[6:8] != b"\x01\x00":
                raise StrictNpzError(
                    f"{zi.filename}: .npy version {tuple(raw[6:8])} (v1.0 only)"
                )
            (hlen,) = struct.unpack("<H", raw[8:10])
            header = raw[10 : 10 + hlen].decode("latin1")
            try:
                d = ast.literal_eval(header)
            except Exception as e:  # noqa: BLE001
                raise StrictNpzError(f"{zi.filename}: unparsable header: {e}")
            if set(d) != {"descr", "fortran_order", "shape"}:
                raise StrictNpzError(
                    f"{zi.filename}: unexpected header keys {sorted(d)}"
                )
            if d["fortran_order"] is not False:
                raise StrictNpzError(f"{zi.filename}: fortran order")
            if d["descr"] not in allowed:
                raise StrictNpzError(
                    f"{zi.filename}: dtype {d['descr']!r} not in {allowed}"
                )
            shape = tuple(d["shape"])
            if not all(isinstance(s, int) and s >= 0 for s in shape):
                raise StrictNpzError(f"{zi.filename}: bad shape {shape}")
            count = int(np.prod(shape, dtype=np.int64)) if shape else 1
            item = np.dtype(d["descr"]).itemsize
            body = raw[10 + hlen :]
            if len(body) != count * item:
                raise StrictNpzError(
                    f"{zi.filename}: payload {len(body)} B != {count}*{item}"
                )
            out[zi.filename[:-4]] = (
                np.frombuffer(body, dtype=d["descr"]).reshape(shape).copy()
            )
    return out


# ---------------------------------------------------------------- verification


def _seeds_module():
    if GENERATOR_DIR not in sys.path:
        sys.path.insert(0, GENERATOR_DIR)
    from phrf_gen import seeds  # normative stream chaining

    return seeds


def _load_and_verify(
    npz_path: str,
    manifest_path: str | None = None,
    *,
    expect_generator_sha: str | None = None,
    expect_n_pool: int | None = None,
    expect_n_voxels: int | None = None,
    allow_roots=("harness", "pilot"),
):
    """Verify the generator dataset against its manifest; return (arrays, manifest, hashes)."""
    manifest_path = manifest_path or npz_path[:-4] + ".manifest.json"
    if not os.path.isfile(manifest_path):
        raise Refusal(f"manifest not found: {manifest_path}")
    raw_man = open(manifest_path, "rb").read()
    man = json.loads(raw_man)
    blob = open(npz_path, "rb").read()

    if man.get("schema") != SCHEMA:
        raise Refusal(f"schema {man.get('schema')!r} != {SCHEMA!r}")
    if sha256_bytes(blob) != man["npz_sha256"]:
        raise Refusal("npz sha256 does not match manifest npz_sha256")
    if os.path.basename(npz_path) != man["file"]:
        raise Refusal(
            f"file name {os.path.basename(npz_path)!r} != manifest file {man['file']!r}"
        )
    if man["root_kind"] not in allow_roots:
        raise Refusal(f"root_kind {man['root_kind']!r} not allowed")
    if man["root_denylisted"]:
        raise Refusal("root is denylisted")
    bad = [p for p, c in man["stream_denylist_check"].items() if c["hit"]]
    if bad:
        raise Refusal(f"denylisted stream seeds: {bad}")
    if expect_generator_sha and man["generator_code_sha256"] != expect_generator_sha:
        raise Refusal("generator_code_sha256 differs from the frozen value")

    cell = man["cell"]
    if cell.get("kind") != "trial":
        raise Refusal(f"cell kind {cell.get('kind')!r} is not 'trial'")
    if cell.get("tr_aligned_onsets") is not True:
        raise Refusal(f"cell {cell.get('cell_id')} does not have tr_aligned_onsets set")

    # Strict read (generator output is STORED f8/i4 only) and per-member hashes.
    arrays = strict_npz_read(blob, allowed=("<f8", "<i4"))
    with zipfile.ZipFile(io.BytesIO(blob)) as z:
        for name, spec in man["arrays"].items():
            if name not in arrays:
                raise Refusal(f"array {name!r} missing")
            if sha256_bytes(z.read(name + ".npy")) != spec["npy_sha256"]:
                raise Refusal(f"array {name!r}: member sha256 mismatch")
            if list(arrays[name].shape) != spec["shape"]:
                raise Refusal(
                    f"array {name!r}: shape {arrays[name].shape} != {spec['shape']}"
                )
            if str(arrays[name].dtype) != spec["dtype"]:
                raise Refusal(
                    f"array {name!r}: dtype {arrays[name].dtype} != {spec['dtype']}"
                )
        if set(arrays) != set(man["arrays"]):
            raise Refusal("array set differs from manifest")

    # Seeds: recompute every recorded stream from the root; derive the audit stream.
    seeds = _seeds_module()
    root = int(man["root_hex"], 16)
    cid, ds = cell["cell_id"], int(man["dataset"])
    for p, s in man["streams"].items():
        if seeds.stream_seed(root, cid, ds, p) != s:
            raise Refusal(f"stream {p!r} seed does not follow from root_hex")
    audit = seeds.stream_seed(root, cid, ds, "audit")
    if seeds.denylist_hit(audit):
        raise Refusal("audit stream seed is denylisted")
    if man["streams"].get("audit", audit) != audit:
        raise Refusal("manifest audit seed mismatch")

    n_pool = int(man["n_pool"])
    n_vox = int(man["n_voxels"])
    if expect_n_voxels is not None and n_vox != expect_n_voxels:
        raise Refusal(f"n_voxels {n_vox} != expected {expect_n_voxels}")
    if expect_n_pool is not None and n_pool != expect_n_pool:
        raise Refusal(f"n_pool {n_pool} != expected {expect_n_pool}")
    if n_pool < 1:
        raise Refusal("no pool voxels: GLMsingle type D needs a noise pool")
    tr = float(cell["tr"])
    # L1: manifest vs array shapes and sample spacing, before anything runs
    if arrays["y"].shape[0] != n_vox:
        raise Refusal(f"y has {arrays['y'].shape[0]} voxels, manifest n_voxels {n_vox}")
    if arrays["y_pool"].shape[0] != n_pool:
        raise Refusal(f"y_pool has {arrays['y_pool'].shape[0]} voxels, manifest n_pool {n_pool}")
    T = arrays["y"].shape[1]
    if arrays["y_pool"].shape[1] != T or int(man["n_samples"]) != T:
        raise Refusal("sample count differs between y, y_pool and manifest n_samples")
    run_id, st = arrays["run_id"], arrays["sample_time"]
    for r in np.unique(run_id):
        t = st[run_id == r]
        if t[0] != 0 or not np.allclose(np.diff(t), tr, rtol=0, atol=1e-9):
            raise Refusal(f"sample_time in run {r} is not n*tr with tr={tr}")
    # L2: every event run exists; every condition occurs in every run
    runs = set(int(r) for r in np.unique(run_id))
    ev_runs = set(int(r) for r in np.unique(arrays["ev_run"]))
    if not ev_runs <= runs:
        raise Refusal(f"ev_run values {sorted(ev_runs - runs)} absent from run_id")
    conds = set(int(c) for c in np.unique(arrays["ev_cond"]))
    for r in sorted(runs):
        miss = conds - set(int(c) for c in arrays["ev_cond"][arrays["ev_run"] == r])
        if miss:
            raise Refusal(f"run {r} lacks conditions {sorted(miss)}")
    ons = arrays["ev_onset"]
    if not np.all(np.abs(ons / tr - np.round(ons / tr)) < 1e-9):
        raise Refusal(
            "ev_onset is not on the TR grid although tr_aligned_onsets is set"
        )
    hashes = dict(
        input_npz_sha256=man["npz_sha256"],
        input_manifest_sha256=sha256_bytes(raw_man),
        audit_seed=audit,
    )
    return arrays, man, hashes


def load_and_verify(npz_path, manifest_path=None, **kw):
    """Typed wrapper: malformed or incomplete manifests are refusals, not crashes."""
    try:
        return _load_and_verify(npz_path, manifest_path, **kw)
    except (KeyError, TypeError, ValueError, IndexError) as e:
        raise Refusal(f"malformed manifest or arrays: {type(e).__name__}: {e}")


# ---------------------------------------------------------------- GLMsingle input / output


def build_runs(arrays: dict, tr: float, baseline: float = BASELINE):
    """Per-run (V+Vp, T_r) float64 data with +baseline everywhere; onsets and conditions.

    Scored voxels come first, pool voxels after.  Also returns the generator event index of
    each trial in the wrapper's concatenation order.
    """
    y, yp = arrays["y"], arrays["y_pool"]
    run_id, ev_run = arrays["run_id"], arrays["ev_run"]
    data, onsets, conds, ev_index = [], [], [], []
    for r in sorted(np.unique(run_id)):
        cols = np.where(run_id == r)[0]
        data.append(np.vstack([y[:, cols] + baseline, yp[:, cols] + baseline]))
        ev = np.where(ev_run == r)[0]
        onsets.append(arrays["ev_onset"][ev].astype(float))
        conds.append(arrays["ev_cond"][ev].astype(int))
        ev_index.append(ev)
    return data, onsets, conds, np.concatenate(ev_index)


def to_data_units(psc, meanvol, hrf_library, hrf_index):
    """beta_data = beta_psc / 100 * |meanvol| * peak(h_used), all in float64."""
    psc = np.asarray(psc, dtype=np.float64)
    mv = np.abs(np.asarray(meanvol, dtype=np.float64))
    peaks = np.asarray(hrf_library, dtype=np.float64).max(axis=1)
    return psc / 100.0 * mv[:, None] * peaks[np.asarray(hrf_index, dtype=int)][:, None]


def _with_timeout(seconds, fn, *a, **kw):
    if seconds is None:
        return fn(*a, **kw)

    def _h(signum, frame):
        raise GlmSingleTimeout(f"wrapper exceeded {seconds} s")

    old = signal.signal(signal.SIGALRM, _h)
    signal.setitimer(signal.ITIMER_REAL, float(seconds))
    try:
        return fn(*a, **kw)
    finally:
        signal.setitimer(signal.ITIMER_REAL, 0)
        signal.signal(signal.SIGALRM, old)


def _default_runner():
    if HERE not in sys.path:
        sys.path.insert(0, HERE)
    from run_glmsingle import run_glmsingle

    return run_glmsingle


def bridge(
    npz_path: str,
    out_dir: str,
    *,
    timeout_s: float | None = DEFAULT_TIMEOUT_S,
    expect_generator_sha: str | None = None,
    expect_n_pool: int | None = None,
    expect_n_voxels: int | None = None,
    runner=None,
    baseline: float = BASELINE,
    allow_roots=("harness", "pilot"),
):
    """Run the whole bridge for one dataset. Returns (npz_out_path, meta_out_path, sidecar)."""
    arrays, man, hashes = load_and_verify(
        npz_path,
        expect_generator_sha=expect_generator_sha,
        expect_n_pool=expect_n_pool,
        expect_n_voxels=expect_n_voxels,
        allow_roots=allow_roots,
    )
    cell = man["cell"]
    tr = float(cell["tr"])
    V = int(man["n_voxels"])
    Vp = int(man["n_pool"])
    data, onsets, conds, ev_index = build_runs(arrays, tr, baseline)
    runner = runner or _default_runner()
    stimdur = (
        float(np.max(arrays["ev_duration"])) if arrays["ev_duration"].size else 0.0
    )
    stimdur = (
        stimdur if stimdur > 0 else tr
    )  # delta events: one-volume stimulus, as in v0b

    wrapper_path = os.path.join(HERE, "run_glmsingle.py")
    res = _with_timeout(
        timeout_s,
        runner,
        data,
        onsets,
        conds,
        tr=tr,
        stimdur=stimdur,
        seed=hashes["audit_seed"],
        types="BD",
        threads=1,
    )
    meta = dict(res["meta"])
    shifts = meta.get("onset_quantization_max_abs_shift_s", [])
    if any(abs(s) > 1e-9 for s in shifts):
        raise Refusal(f"nonzero onset quantization {shifts} for an aligned cell")

    hi = np.asarray(res["d_HRFindex"]).astype(int)
    lib = np.asarray(res["hrf_library"], dtype=np.float64)
    mv = np.asarray(res["meanvol"], dtype=np.float64)
    psc = np.asarray(res["d_beta_psc"], dtype=np.float64)
    if psc.shape[0] != V + Vp:
        raise Refusal(f"GLMsingle returned {psc.shape[0]} voxels, expected {V + Vp}")
    sl = slice(0, V)
    beta_data = to_data_units(psc[sl], mv[sl], lib, hi[sl])
    f8 = lambda a: np.ascontiguousarray(np.asarray(a), dtype="<f8")  # noqa: E731
    out = {
        "d_beta_data": f8(beta_data),
        "d_beta_psc": f8(psc[sl]),
        "d_HRFindex": f8(hi[sl]),
        "d_R2": f8(np.asarray(res["d_R2"])[sl]),
        "d_FRACvalue": f8(np.asarray(res["d_FRACvalue"])[sl]),
        "meanvol": f8(mv[sl]),
        "noisepool": f8(np.asarray(res["noisepool"]).astype(float)),
        "hrf_library": f8(lib),
        "trial_event_index": f8(ev_index[np.asarray(res["trial_order"]).astype(int)]),
        "trial_run": f8(res["trial_run"]),
        "trial_cond": f8(res["trial_cond"]),
        "trial_vol": f8(res["trial_vol"]),
    }
    blob = npz_bytes(out)
    strict_npz_read(blob, allowed=("<f8",))  # self-check before anything is written

    stem = man["file"][:-4]
    os.makedirs(out_dir, exist_ok=True)
    npz_out = os.path.join(out_dir, stem + ".glmsingle.npz")
    meta_out = os.path.join(out_dir, stem + ".glmsingle.meta.json")
    sidecar = {
        "schema": OUT_SCHEMA,
        "input": {
            "file": man["file"],
            "npz_sha256": hashes["input_npz_sha256"],
            "manifest_sha256": hashes["input_manifest_sha256"],
            "generator_code_sha256": man["generator_code_sha256"],
            "root_kind": man["root_kind"],
            "root_hex": man["root_hex"],
            "cell_id": cell["cell_id"],
            "dataset": int(man["dataset"]),
        },
        "output": {
            "npz_sha256": sha256_bytes(blob),
            "arrays": {
                k: {"dtype": "<f8", "shape": list(v.shape)}
                for k, v in sorted(out.items())
            },
            "trial_order": "chronological (GLMsingle); event order via trial_event_index",
        },
        "meta": {
            "baseline_added": baseline,
            "baseline_applied_to": "scored and pool voxels",
            "n_scored_voxels": V,
            "n_pool_voxels_requested": Vp,
            "realized_pool_size": int(res["noisepool_size"]),
            "pcnum": int(res["pcnum"]),
            "HRFindex": [int(i) for i in hi[sl]],
            "R2": [float(x) for x in out["d_R2"]],
            "FRACvalue": [float(x) for x in out["d_FRACvalue"]],
            "types": meta["types"],
            "threads": meta["threads"],
            "seed": int(meta["seed"]),
            "seed_stream": "audit",
            "timeout_s": timeout_s,
            "tr": tr,
            "stimdur": stimdur,
            "glmsingle_commit": meta["glmsingle_commit"],
            "versions": meta["versions"],
            "onset_quantization_max_abs_shift_s": shifts,
            "wrapper_sha256": sha256_file(wrapper_path),
            "from_generator_sha256": sha256_file(os.path.abspath(__file__)),
            "log_tail": meta["log_tail"],
        },
        "timing": {"cpu_s": meta["cpu_s"], "wall_s": meta["wall_s"]},
    }
    tmp = npz_out + ".tmp"
    with open(tmp, "wb") as f:
        f.write(blob)
    os.replace(tmp, npz_out)
    with open(meta_out, "w") as f:
        json.dump(sidecar, f, indent=2, sort_keys=True)
        f.write("\n")
    return npz_out, meta_out, sidecar


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("dataset", help="generator <CELL>__dNNNN.npz (manifest alongside)")
    ap.add_argument("--out", default=".")
    ap.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT_S)
    ap.add_argument("--expect-generator-sha", default=None)
    ap.add_argument("--expect-n-pool", type=int, default=None)
    ap.add_argument("--expect-n-voxels", type=int, default=None)
    a = ap.parse_args(argv)
    try:
        npz_out, meta_out, sc = bridge(
            a.dataset,
            a.out,
            timeout_s=a.timeout,
            expect_generator_sha=a.expect_generator_sha,
            expect_n_pool=a.expect_n_pool,
            expect_n_voxels=a.expect_n_voxels,
        )
    except (Refusal, StrictNpzError) as e:
        print(f"REFUSED: {e}", file=sys.stderr)
        return 2
    except GlmSingleTimeout as e:
        print(f"TIMEOUT: {e}", file=sys.stderr)
        return 4
    except Exception as e:  # noqa: BLE001
        print(f"FAILED: {type(e).__name__}: {e}", file=sys.stderr)
        return 3
    print(
        json.dumps(
            {
                "npz": npz_out,
                "meta": meta_out,
                "npz_sha256": sc["output"]["npz_sha256"],
                "timing": sc["timing"],
            }
        )
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
