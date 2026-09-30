"""Independent literal physical writer/checker. No Scala expectations or JNI."""
import json, pathlib, sys
import h5py
import numpy as np
mode, expected_path, root = sys.argv[1:]
expected = json.loads(pathlib.Path(expected_path).read_text())
root = pathlib.Path(root)
if mode.startswith("write"):
    root.mkdir(parents=True, exist_ok=True)
    for name, record in expected.items():
        with h5py.File(root / (name + ".h5"), "x") as file:
            file.create_dataset("values", data=np.array(record["values"], dtype=record["dtype"]), chunks=tuple(record["chunks"]))
            file.create_dataset("validity", data=np.array(record["validity"], dtype="u1"), chunks=tuple(record["chunks"]))
    if mode != "write":
        with h5py.File(root / "effect.h5", "r+") as file:
            if mode == "write-extra": file.create_dataset("foreign", data=[1])
            elif mode == "write-group": file.create_group("foreign")
            elif mode == "write-soft":
                del file["validity"]
                file["validity"] = h5py.SoftLink("/values")
            elif mode == "write-code": file["validity"][0,0,0] = 255
            elif mode == "write-support": file["validity"][0,0,1] = 0
            elif mode == "write-nan": file["values"][0,0,0] = np.nan
            elif mode in ("write-chunks", "write-filter", "write-dtype", "write-shape"):
                data = file["values"][...]
                del file["values"]
                kwargs = {"chunks":(1,1,2) if mode == "write-chunks" else (1,1,5)}
                if mode == "write-filter": kwargs["compression"] = "gzip"
                if mode == "write-dtype": data = data.astype("<f8")
                if mode == "write-shape":
                    data = data[:,:,:4]
                    kwargs["chunks"] = (1,1,4)
                file.create_dataset("values", data=data, **kwargs)
            else: raise AssertionError(mode)
else:
    assert mode == "check"
    for name, record in expected.items():
        with h5py.File(root / (name + ".h5"), "r") as file:
            assert set(file.keys()) == {"values", "validity"}
            for dataset, key, dtype in [("values", "values", record["dtype"]), ("validity", "validity", "|u1")]:
                stored = file[dataset]
                assert stored.dtype.str == dtype, (name, dataset, stored.dtype.str)
                assert stored.shape == tuple(record["shape"])
                assert stored.maxshape == stored.shape
                assert stored.chunks == tuple(record["chunks"])
                assert stored.compression is None and not stored.shuffle and not stored.fletcher32 and stored.scaleoffset is None
                np.testing.assert_array_equal(stored[...], np.array(record[key], dtype=dtype))
                print(json.dumps({"product":name,"dataset":dataset,"dtype":stored.dtype.str,"shape":stored.shape,"chunks":stored.chunks,"all_cells":stored[...].tolist()},ensure_ascii=False))
print("H5PY_LITERAL_" + mode.upper() + "_PASS products=4 cells=76")
