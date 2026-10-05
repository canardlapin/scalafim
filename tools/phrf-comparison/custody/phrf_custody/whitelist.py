"""Whitelist schema check and release (design 3.1, 3.2 item 3; review F3-F5).

Only the fields below may leave custody.  The checker is strict: any extra, missing or mistyped
key at any level is rejected, so a mean, median, per-cell rate, per-dataset value or any other
comparative quantity cannot pass.  Error messages name only a fixed top-level section, never a key
or value found in the candidate (key names are an exfiltration channel).

Candidate (aggregator output) vs emitted form
---------------------------------------------
Candidate: each gating pair is ``{"sigma": x, "df": n}``.  ``pool_candidate`` emits ONE pooled
``df`` = the minimum over all pairs (F4; refused below DF_MIN = 14) and bare sigmas.  ICC entries
are ``{"icc": x | "degenerate", "ucl80": u | "degenerate"}`` (F3): ``ucl80`` is the F-based 80 %
upper limit, truncated at 0; "degenerate" is allowed only for the coverage indicator, and for both
fields together.  Floats are quantized to 5 significant figures (F5): sigma and ucl80 are rounded
UP, everything else to nearest; validation runs after rounding.
"""
from __future__ import annotations

import hashlib
import json
import math
from decimal import ROUND_CEILING, ROUND_HALF_EVEN, Decimal
from pathlib import Path

SCHEMA = "phrf-pilot-whitelist/2"
SIG_FIGS = 5
DF_MIN = 14
COND_CELLS = ("C-TX-.5", "C-TS-1", "C-TS-.5", "C-TG-.5")
COND_COMPS = ("CAN", "INF3", "FIR")
TRIAL_CELLS = ("T-TX-fast", "T-TX-jit", "T-TS-fast")
TRIAL_COMPS = ("LSA", "LSS", "rLSS")
GLMS_CELLS = ("T-TX-fast", "T-TX-jit")
GLMS_COMP = "GLMs-D"
ICC_CELL = "C-TG-.5"
ICC_FIELDS = ("e_peak_rel_error", "tau_error", "coverage_1sigma")
DEGENERATE = "degenerate"
METHODS = ("PHRF", "CAN", "INF3", "FIR", "LSA", "LSS", "rLSS", "GLMs-D")
TIMING_KEYS = ("a_trial_ml_throughput", "b_trial_preparation", "c_alpha_cache", "d_glmsingle", "e_cold_condition")
SIGMA_SECTIONS = ("sigma_condition", "sigma_trial", "sigma_trial_glms")
TOP = ("schema", "df") + SIGMA_SECTIONS + ("icc", "pooled_rates", "timing")


class WhitelistError(Exception):
    """``section`` is a fixed top-level schema name or '$'; no candidate-derived text is included."""

    def __init__(self, path: str):
        top = path.split(".")[0].split("[")[0]
        self.section = top if top in TOP else "$"
        super().__init__(f"whitelist rejected in section {self.section}")


def _num(x, path, lo=None, hi=None):
    if isinstance(x, bool) or not isinstance(x, (int, float)) or not math.isfinite(x):
        raise WhitelistError(path)
    if (lo is not None and x < lo) or (hi is not None and x > hi):
        raise WhitelistError(path)


def _int(x, path, lo):
    if isinstance(x, bool) or not isinstance(x, int) or x < lo:
        raise WhitelistError(path)


def _keys(d, expected, path):
    if not isinstance(d, dict) or set(d) != set(expected):
        raise WhitelistError(path)


def _sigma_block(d, cells, comps, path):
    _keys(d, cells, path)
    for c in cells:
        _keys(d[c], comps, path)
        for k in comps:
            _num(d[c][k], path, lo=0)


def validate(w) -> None:
    """Raise WhitelistError unless ``w`` is exactly the emitted whitelist structure."""
    _keys(w, TOP, "$")
    if w["schema"] != SCHEMA:
        raise WhitelistError("schema")
    _int(w["df"], "df", DF_MIN)
    _sigma_block(w["sigma_condition"], COND_CELLS, COND_COMPS, "sigma_condition")
    _sigma_block(w["sigma_trial"], TRIAL_CELLS, TRIAL_COMPS, "sigma_trial")
    _sigma_block(w["sigma_trial_glms"], GLMS_CELLS, (GLMS_COMP,), "sigma_trial_glms")
    _keys(w["icc"], (ICC_CELL,), "icc")
    _keys(w["icc"][ICC_CELL], ICC_FIELDS, "icc")
    for k in ICC_FIELDS:
        e = w["icc"][ICC_CELL][k]
        _keys(e, ("icc", "ucl80"), "icc")
        deg = (e["icc"] == DEGENERATE, e["ucl80"] == DEGENERATE)
        if deg[0] != deg[1] or (deg[0] and k != "coverage_1sigma"):
            raise WhitelistError("icc")
        if not deg[0]:
            _num(e["icc"], "icc", -1, 1)
            _num(e["ucl80"], "icc", 0, 1)
            if e["icc"] > e["ucl80"]:
                raise WhitelistError("icc")
    _keys(w["pooled_rates"], METHODS, "pooled_rates")
    for m in METHODS:
        e = w["pooled_rates"][m]
        _keys(e, ("refusal_rate", "failure_rate", "denominator"), "pooled_rates")
        _num(e["refusal_rate"], "pooled_rates", 0, 1)
        _num(e["failure_rate"], "pooled_rates", 0, 1)
        _int(e["denominator"], "pooled_rates", 1)
    _keys(w["timing"], TIMING_KEYS, "timing")
    for k in TIMING_KEYS:
        e = w["timing"][k]
        if k == "c_alpha_cache":
            _keys(e, ("status", "flatness_ratio"), "timing")
            if e["status"] != "not possible through the public API":
                raise WhitelistError("timing")
            _num(e["flatness_ratio"], "timing", 0)
        else:
            _keys(e, ("median_core_s", "min_core_s", "max_core_s"), "timing")
            for f in e:
                _num(e[f], "timing", 0)
            if not e["min_core_s"] <= e["median_core_s"] <= e["max_core_s"]:
                raise WhitelistError("timing")


def pool_candidate(c) -> dict:
    """Candidate (per-pair sigma+df) -> emitted structure with one pooled df.  Refuses df < DF_MIN."""
    if not isinstance(c, dict) or "df" in c or not all(k in c for k in SIGMA_SECTIONS):
        raise WhitelistError("$")
    dfs, out = [], {k: v for k, v in c.items() if k not in SIGMA_SECTIONS}
    for sec in SIGMA_SECTIONS:
        blk = c[sec]
        if not isinstance(blk, dict):
            raise WhitelistError(sec)
        out[sec] = {}
        for cell, comps in blk.items():
            if not isinstance(comps, dict):
                raise WhitelistError(sec)
            out[sec][cell] = {}
            for comp, e in comps.items():
                if not isinstance(e, dict) or set(e) != {"sigma", "df"}:
                    raise WhitelistError(sec)
                _int(e["df"], sec, 1)
                dfs.append(e["df"])
                out[sec][cell][comp] = e["sigma"]
    if not dfs or min(dfs) < DF_MIN:
        raise WhitelistError("df")
    out["df"] = min(dfs)
    return out


def _q(x, up):
    d = Decimal(repr(float(x)))
    q = Decimal(1).scaleb(d.adjusted() - (SIG_FIGS - 1))
    return float(d.quantize(q, rounding=ROUND_CEILING if up else ROUND_HALF_EVEN))


def quantize(w, _path=()):
    """5-significant-figure copy of ``w``; sigma and ucl80 round up, other floats to nearest."""
    if isinstance(w, dict):
        return {k: quantize(v, _path + (k,)) for k, v in w.items()}
    if isinstance(w, float) and math.isfinite(w) and w != 0.0:
        up = (_path and _path[0] in SIGMA_SECTIONS) or (_path and _path[-1] == "ucl80")
        return _q(w, bool(up))
    return w


def canonical_bytes(w) -> bytes:
    return json.dumps(quantize(w), sort_keys=True, separators=(",", ":"), allow_nan=False).encode()


def release(candidate, out_path: Path) -> str:
    """Pool df, quantize, validate (after rounding), write canonical JSON; return its SHA-256.
    Nothing is written on failure."""
    w = quantize(pool_candidate(candidate))
    validate(w)
    b = canonical_bytes(w) + b"\n"
    Path(out_path).write_bytes(b)
    return hashlib.sha256(b).hexdigest()
