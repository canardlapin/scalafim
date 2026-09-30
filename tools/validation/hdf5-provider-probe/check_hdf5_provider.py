#!/usr/bin/env python3
"""Independent h5py oracle; no JVM implementation or generated expected-value file."""
import argparse
import json
import sys
from pathlib import Path
import h5py
import numpy as np

if not __debug__:
    raise RuntimeError('UnsupportedRuntime: Python optimization disables oracle assertions')


def inspect(d, dtype, shape, chunks, deflate):
    assert d.dtype == np.dtype(dtype), (d.name, d.dtype, dtype)
    # get_order is authoritative even when NumPy renders native little endian as '='.
    assert d.id.get_type().get_order() == h5py.h5t.ORDER_LE, d.name
    assert d.shape == shape and d.maxshape == shape and d.chunks == chunks, d.name
    p = d.id.get_create_plist()
    filters = [p.get_filter(i)[0] for i in range(p.get_nfilters())]
    assert filters == ([h5py.h5z.FILTER_DEFLATE] if deflate else []), (d.name, filters)
    if deflate:
        assert p.get_filter(0)[2] == (4,), p.get_filter(0)


def small(path):
    cells = 0
    with h5py.File(path, 'r', rdcc_nbytes=1048576) as f:
        assert set(f) == {t + '_' + z for t in ('f32', 'f64', 'u8', 'valid') for z in ('none', 'deflate')}
        x, y, z = np.indices((7, 11, 5), dtype=np.int64)
        for name in f:
            kind = name.split('_')[0]
            dtype = {'f32': '<f4', 'f64': '<f8', 'u8': 'u1', 'valid': 'u1'}[kind]
            d = f[name]
            inspect(d, dtype, (7, 11, 5), (3, 4, 2), name.endswith('deflate'))
            # Explicit physical x,y,z coordinates, independent array broadcasting.
            expected = (x * 1000 + y * 10 + z + 0.25) if kind in ('f32', 'f64') else (
                (x * 47 + y * 19 + z * 5) % 256 if kind == 'u8' else (x + 2*y + 3*z) % 4)
            np.testing.assert_array_equal(d[...], expected.astype(dtype))
            cells += d.size
    return {'check': 'independent-small', 'datasets': 8, 'every_cell_checked': cells,
            'dtype_endianness_shape_chunks_filter_ids': 'pass', 'validity_codes': [0, 1, 2, 3]}


def literal(path):
    values = [-7.5, 0, 1.25, 99.5, -0.125, 8192]
    byte_values = [0, 255, 128, 1, 2, 3]
    with h5py.File(path, 'x', rdcc_nbytes=1048576) as f:
        for name, dtype, vals in [('f32', '<f4', values), ('f64', '<f8', values), ('u8', 'u1', byte_values)]:
            f.create_dataset(name, data=np.array(vals, dtype=dtype).reshape((2, 3)), chunks=(1, 2), compression='gzip', compression_opts=4)
    return {'check': 'python-authored-literal', 'cells': 18, 'literal_values': values, 'literal_bytes': byte_values}


def big(path, rows):
    checked = 0
    with h5py.File(path, 'r', rdcc_nbytes=1048576, rdcc_nslots=521) as f:
        assert set(f) == {'values', 'validity'}
        inspect(f['values'], '<f8', (rows, 131072), (1, 32768), False)
        inspect(f['validity'], 'u1', (rows, 131072), (1, 32768), False)
        for row in range(rows):
            for first in range(0, 131072, 65536):
                col = np.arange(first, first+65536, dtype=np.int64)
                expected = row * 1048576.0 + col * 0.125 - 17.25
                np.testing.assert_array_equal(f['values'][row, first:first+65536], expected)
                np.testing.assert_array_equal(f['validity'][row, first:first+65536], ((row+col) % 4).astype('u1'))
                checked += 65536
    return {'check': 'independent-big', 'rows': rows, 'every_value_cell_checked': checked,
            'every_validity_cell_checked': checked, 'block_elements': 65536}


def main():
    p = argparse.ArgumentParser()
    p.add_argument('mode', choices=['small', 'literal', 'big'])
    p.add_argument('path', type=Path)
    p.add_argument('--rows', type=int, default=128)
    a = p.parse_args()
    result = small(a.path) if a.mode == 'small' else literal(a.path) if a.mode == 'literal' else big(a.path, a.rows)
    result.update(python=sys.executable, python_version=sys.version, h5py=h5py.__version__,
                  hdf5=h5py.version.hdf5_version, numpy=np.__version__, path=str(a.path))
    print(json.dumps(result, sort_keys=True))


if __name__ == '__main__':
    main()
