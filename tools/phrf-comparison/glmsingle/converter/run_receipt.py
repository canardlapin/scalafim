"""Run the converter tests and write converter/from_generator_converter.json."""
import hashlib, json, os, sys, unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import test_from_generator as t  # noqa: E402


def pilot_size_runs():
    """Harness-root, pilot-size (40 scored, 4000 pool) end-to-end runs; hashes and timing."""
    import dataclasses, shutil, tempfile
    import from_generator as fg
    from phrf_gen.cells import CELLS
    from phrf_gen.io import write_dataset
    from phrf_gen.seeds import HARNESS_ROOT
    tmp = tempfile.mkdtemp(prefix="fg_pilot_size_")
    rows = []
    for cid in ("T-TX-fast", "T-TX-jit"):
        cell = dataclasses.replace(CELLS[cid], n_voxels=40, n_pool=4000)
        m = write_dataset(cell, HARNESS_ROOT, "harness", 0, tmp)
        _, _, sc = fg.bridge(os.path.join(tmp, m["file"]), os.path.join(tmp, "out"),
                             expect_n_pool=4000, expect_n_voxels=40)
        rows.append(dict(dataset=m["file"], root_kind="harness", input_npz_sha256=sc["input"]["npz_sha256"],
                         output_npz_sha256=sc["output"]["npz_sha256"],
                         realized_pool_size=sc["meta"]["realized_pool_size"], pcnum=sc["meta"]["pcnum"],
                         timing=sc["timing"]))
    shutil.rmtree(tmp, ignore_errors=True)
    return rows


def main():
    suite = unittest.defaultTestLoader.loadTestsFromModule(t)
    res = unittest.TextTestRunner(verbosity=2).run(suite)
    glm = os.path.dirname(HERE)
    sha = lambda p: hashlib.sha256(open(p, "rb").read()).hexdigest()
    rec = dict(
        schema="phrf-s6-converter-receipt-1", tests_run=res.testsRun,
        failures=[str(f[0]) for f in res.failures], errors=[str(e[0]) for e in res.errors],
        passed=res.wasSuccessful(),
        from_generator_sha256=sha(os.path.join(glm, "from_generator.py")),
        run_glmsingle_sha256=sha(os.path.join(glm, "run_glmsingle.py")),
        evidence=t.EVIDENCE)
    if res.wasSuccessful():
        rec["pilot_size_harness_runs"] = pilot_size_runs()
    json.dump(rec, open(os.path.join(HERE, "from_generator_converter.json"), "w"), indent=2, sort_keys=True)
    open(os.path.join(HERE, "from_generator_converter.json"), "a").write("\n")
    return 0 if res.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
