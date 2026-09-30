"""Independent test-only h5py namespace fixtures; exclusive creation, no adapter/JNI."""
from pathlib import Path
import hashlib
import json
import sys

import h5py
import numpy as np


def author(root):
    root.mkdir(exist_ok=False)
    outcomes = {
        "good1": True, "good2": True, "reversed": True,
        "third": False, "missing": False, "empty": False, "wrongname": False,
        "group": False, "nested": False, "cycle": False, "namedtype": False,
        "hardalias": False, "soft": False, "danglingsoft": False,
        "external": False, "danglingexternal": False,
        "longforeign": False, "manyforeign": False,
    }

    def dataset(f, name):
        dtype = "u1" if name == "validity" else "<f8"
        f.create_dataset(name, data=np.arange(6, dtype=dtype).reshape(1, 2, 3), chunks=(1, 1, 3))

    target = root / "external-target.h5"
    with h5py.File(target, "x") as f:
        dataset(f, "values")
    for case in outcomes:
        with h5py.File(root / (case + ".h5"), "x") as f:
            if case == "empty":
                continue
            if case == "reversed":
                dataset(f, "validity")
                dataset(f, "values")
                continue
            if case in ("group", "nested", "cycle"):
                g = f.create_group("values")
                if case == "nested":
                    dataset(g, "hidden")
                if case == "cycle":
                    g["self"] = g
            elif case == "namedtype":
                f["values"] = np.dtype("<f8")
            elif case in ("soft", "danglingsoft"):
                f["values"] = h5py.SoftLink("/validity" if case == "soft" else "/absent")
            elif case in ("external", "danglingexternal"):
                f["values"] = h5py.ExternalLink(target.name if case == "external" else "absent.h5", "/values")
            else:
                dataset(f, "values")
            if case in ("good1", "missing"):
                continue
            if case == "hardalias":
                f["validity"] = f["values"]
            else:
                dataset(f, "wrong" if case == "wrongname" else "validity")
            if case == "third":
                dataset(f, "hidden")
            if case == "longforeign":
                f["z" * 65536] = f["values"]
            if case == "manyforeign":
                for i in range(10000):
                    f["foreign_%05d" % i] = f["values"]

    facts = {}
    for case, accepted in outcomes.items():
        path = root / (case + ".h5")
        with h5py.File(path, "r") as f:
            count = len(f)
            # Inspect only fixed names. Never resolve soft/external links, even in this oracle.
            objects = {}
            for name in ("values", "validity"):
                link = f.get(name, getlink=True)
                fact = {"link": type(link).__name__}
                if isinstance(link, h5py.HardLink):
                    obj = f[name]
                    fact.update(kind=type(obj).__name__, rc=h5py.h5o.get_info(obj.id).rc)
                objects[name] = fact
            if accepted:
                assert count == (1 if case == "good1" else 2)
                for name in (("values",) if case == "good1" else ("values", "validity")):
                    assert objects[name] == dict(link="HardLink", kind="Dataset", rc=1)
            if case == "hardalias":
                assert count == 2 and objects["values"]["rc"] == objects["validity"]["rc"] == 2
            if case in ("soft", "danglingsoft", "external", "danglingexternal"):
                assert count == 2 and objects["values"]["link"] != "HardLink"
            if case == "manyforeign":
                assert count == 10002
            facts[case] = dict(accepted=accepted, root_links=count, objects=objects,
                               bytes=path.stat().st_size, sha256=hashlib.sha256(path.read_bytes()).hexdigest())
    receipt = dict(python=sys.executable, h5py=h5py.__version__, numpy=np.__version__,
                   hdf5=h5py.version.hdf5_version, fixtures=facts)
    (root / "oracle.json").write_text(json.dumps(receipt, indent=2) + "\n")
    print(json.dumps(receipt, sort_keys=True))


if __name__ == "__main__":
    author(Path(sys.argv[1]))
