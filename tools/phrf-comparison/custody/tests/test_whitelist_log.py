import copy
import hashlib
import json
import multiprocessing
import os

import pytest

from phrf_custody import log, whitelist as W


def good():
    sig = lambda cells, comps: {c: {k: {"sigma": 0.3100049, "df": 19} for k in comps} for c in cells}
    icc = {k: {"icc": 0.2, "ucl80": 0.35} for k in W.ICC_FIELDS}
    return {
        "schema": W.SCHEMA,
        "sigma_condition": sig(W.COND_CELLS, W.COND_COMPS),
        "sigma_trial": sig(W.TRIAL_CELLS, W.TRIAL_COMPS),
        "sigma_trial_glms": sig(W.GLMS_CELLS, (W.GLMS_COMP,)),
        "icc": {W.ICC_CELL: icc},
        "pooled_rates": {m: {"refusal_rate": 0.01, "failure_rate": 0.0, "denominator": 140} for m in W.METHODS},
        "timing": {**{k: {"median_core_s": 1.0, "min_core_s": 0.5, "max_core_s": 2.0} for k in W.TIMING_KEYS if k != "c_alpha_cache"},
                   "c_alpha_cache": {"status": "not possible through the public API", "flatness_ratio": 1.02}},
    }


def emitted(c=None):
    return W.quantize(W.pool_candidate(c or good()))


def test_good_passes_counts_and_single_pooled_df():
    w = emitted(); W.validate(w)
    assert w["df"] == 19 and "df" not in str(w["sigma_trial"])
    assert sum(len(v) for v in w["sigma_condition"].values()) == 12
    assert sum(len(v) for v in w["sigma_trial"].values()) == 9
    assert sum(len(v) for v in w["sigma_trial_glms"].values()) == 2


def test_df_is_minimum_over_pairs_and_below_14_refused():
    c = good(); c["sigma_trial"]["T-TX-fast"]["LSA"]["df"] = 15
    assert emitted(c)["df"] == 15
    c["sigma_trial"]["T-TX-fast"]["LSA"]["df"] = 13
    with pytest.raises(W.WhitelistError) as e:
        W.pool_candidate(c)
    assert e.value.section == "df"


def test_sigma_rounds_up_others_to_nearest_five_sig_figs():
    c = good()
    c["sigma_condition"]["C-TX-.5"]["CAN"]["sigma"] = 0.3100049   # up -> 0.31001
    c["sigma_condition"]["C-TX-.5"]["INF3"]["sigma"] = 0.31000001  # up -> 0.31001
    c["icc"][W.ICC_CELL]["tau_error"] = {"icc": 0.123456, "ucl80": 0.2000001}
    c["timing"]["b_trial_preparation"]["median_core_s"] = 1.000049
    w = emitted(c)
    assert w["sigma_condition"]["C-TX-.5"]["CAN"] == 0.31001 and w["sigma_condition"]["C-TX-.5"]["INF3"] == 0.31001
    assert w["icc"][W.ICC_CELL]["tau_error"] == {"icc": 0.12346, "ucl80": 0.20001}
    assert w["timing"]["b_trial_preparation"]["median_core_s"] == 1.0
    assert W.canonical_bytes(w) == W.canonical_bytes(json.loads(W.canonical_bytes(w)))  # idempotent
    assert W.quantize({"sigma_trial": {"a": 123456.0}})["sigma_trial"]["a"] == 123460.0


def test_validation_runs_after_rounding(tmp_path):
    # unrounded median > max would fail; after rounding all three are equal, which is valid, and
    # the output carries no trace of the sub-resolution ordering
    c = good()
    c["timing"]["b_trial_preparation"] = {"median_core_s": 1.000004, "min_core_s": 1.000001, "max_core_s": 1.000002}
    W.release(c, tmp_path / "w.json")
    t = json.loads((tmp_path / "w.json").read_text())["timing"]["b_trial_preparation"]
    assert t == {"median_core_s": 1.0, "min_core_s": 1.0, "max_core_s": 1.0}
    c["timing"]["b_trial_preparation"] = {"median_core_s": 1.5, "min_core_s": 1.0, "max_core_s": 1.2}
    with pytest.raises(W.WhitelistError):
        W.release(c, tmp_path / "w2.json")
    assert not (tmp_path / "w2.json").exists()


def test_icc_degenerate_only_for_coverage_and_with_ucl():
    c = good(); c["icc"][W.ICC_CELL]["coverage_1sigma"] = {"icc": "degenerate", "ucl80": "degenerate"}
    W.validate(emitted(c))
    for field, e in (("tau_error", {"icc": "degenerate", "ucl80": "degenerate"}),
                     ("coverage_1sigma", {"icc": "degenerate", "ucl80": 0.3}),
                     ("coverage_1sigma", {"icc": 0.1, "ucl80": "degenerate"}),
                     ("coverage_1sigma", {"icc": 0.1, "ucl80": -0.1}),   # truncated at 0
                     ("coverage_1sigma", {"icc": 0.1}),
                     ("coverage_1sigma", {"icc": 0.1, "ucl80": 0.2, "p": 0.5})):
        c = good(); c["icc"][W.ICC_CELL][field] = e
        with pytest.raises(W.WhitelistError):
            W.validate(emitted(c))


def mutate(f):
    c = good(); f(c); return c


BAD = {
    "top extra": lambda w: w.update(mean_difference=0.1),
    "missing top": lambda w: w.pop("timing"),
    "extra comparative field in pair": lambda w: w["sigma_condition"]["C-TX-.5"]["CAN"].update(mean=0.1),
    "extra pair": lambda w: w["sigma_condition"]["C-TX-.5"].update(LSA={"sigma": 1, "df": 30}),
    "extra cell": lambda w: w["sigma_trial"].update({"T-TG-fast": {}}),
    "per-cell rate": lambda w: w["pooled_rates"]["PHRF"].update(per_cell={"C-TX-.5": 0.1}),
    "icc paired differences": lambda w: w["icc"][W.ICC_CELL].update(paired_diff_icc=0.1),
    "icc extra cell": lambda w: w["icc"].update({"C-TX-.5": {k: 0 for k in W.ICC_FIELDS}}),
    "accuracy median": lambda w: w["timing"]["a_trial_ml_throughput"].update(accuracy_median=0.5),
    "per-dataset list": lambda w: w["sigma_trial"]["T-TX-fast"]["LSA"].update(per_dataset=[1, 2]),
    "nan": lambda w: w["sigma_trial"]["T-TX-fast"]["LSA"].update(sigma=float("nan")),
    "negative sigma": lambda w: w["sigma_trial"]["T-TX-fast"]["LSA"].update(sigma=-1.0),
    "float df": lambda w: w["sigma_trial"]["T-TX-fast"]["LSA"].update(df=19.5),
    "bool df": lambda w: w["sigma_trial"]["T-TX-fast"]["LSA"].update(df=True),
    "rate >1": lambda w: w["pooled_rates"]["LSS"].update(failure_rate=1.5),
    "icc >1": lambda w: w["icc"][W.ICC_CELL]["tau_error"].update(icc=1.5),
    "string number": lambda w: w["icc"][W.ICC_CELL]["tau_error"].update(icc="0.2"),
    "timing order": lambda w: w["timing"]["b_trial_preparation"].update(median_core_s=9.0),
    "wrong schema": lambda w: w.update(schema="x"),
    "alpha status": lambda w: w["timing"]["c_alpha_cache"].update(status="possible"),
    "not object": lambda w: w.update(icc=[1]),
    "emitted-form df smuggled": lambda w: w.update(df=99),
}


@pytest.mark.parametrize("name", BAD)
def test_rejects(name, tmp_path):
    out = tmp_path / "w.json"
    with pytest.raises(W.WhitelistError):
        W.release(mutate(BAD[name]), out)
    assert not out.exists()


def test_error_text_names_only_fixed_sections():
    secretish = "leaky_key_name_0.8731"
    c = good(); c["sigma_trial"]["T-TX-fast"]["LSA"][secretish] = 0.8731
    with pytest.raises(W.WhitelistError) as e:
        W.release(c, "/nonexistent/x")
    assert secretish not in str(e.value) and "0.8731" not in str(e.value) and e.value.section == "sigma_trial"
    c = good(); c[secretish] = 1
    with pytest.raises(W.WhitelistError) as e:
        W.release(c, "/nonexistent/x")
    assert secretish not in str(e.value) and e.value.section == "$"


def test_release_hash_and_determinism(tmp_path):
    out = tmp_path / "pilot-whitelist.json"
    h = W.release(good(), out)
    assert hashlib.sha256(out.read_bytes()).hexdigest() == h
    assert json.loads(out.read_text()) == emitted()
    assert W.release(copy.deepcopy(good()), tmp_path / "again.json") == h


# ---- custody log ----

def test_custody_log_chain_and_anchor(tmp_path):
    p = tmp_path / "l.jsonl"
    log.append(p, "a", {"x": 1}, now=1); e = log.append(p, "b", {}, now=2)
    anchor = log.head(p)
    assert anchor == e["entry_sha256"] and log.verify(p, expected_head=anchor, expected_count=2) == 2
    log.append(p, "c", {}, now=3)
    with pytest.raises(ValueError):
        log.verify(p, expected_head=anchor)
    lines = p.read_text().splitlines()
    p.write_text("\n".join(lines[:2]) + "\n")  # truncation: chain fine, anchor catches it
    assert log.verify(p) == 2
    with pytest.raises(ValueError):
        log.verify(p, expected_count=3)
    p.write_text(lines[0].replace('"x": 1', '"x": 2') + "\n" + lines[1] + "\n")
    with pytest.raises(ValueError):
        log.verify(p)


def _append_many(path, tag, n):
    for i in range(n):
        log.append(path, "e", {"w": tag, "i": i}, now=1)


def test_custody_log_concurrent_writers_keep_chain(tmp_path):
    p = tmp_path / "l.jsonl"
    ps = [multiprocessing.get_context("fork").Process(target=_append_many, args=(p, t, 15)) for t in range(4)]
    [x.start() for x in ps]; [x.join() for x in ps]
    assert log.verify(p) == 60
