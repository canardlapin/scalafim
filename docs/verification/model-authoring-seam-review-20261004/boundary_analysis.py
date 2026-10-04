import json, math
import numpy as np

maximum_int = 2**31 - 1
requested = math.ceil(1.0 / 1e-20)
effective_if_saturated = 1.0 / maximum_int
assert requested > maximum_int
assert effective_if_saturated > 1e-20
values = np.array([1e-200, 3e-200])
assert np.all(values != 0.0)
assert np.all(values * values == 0.0)
x = np.column_stack(([1e-18, 0.0, 0.0], [0.0, 1.0, 0.0]))
y = np.array([1.0, 1.0, 1.0])
u, s, vt = np.linalg.svd(x, full_matrices=False)
cutoff = max(x.shape) * s[0] * np.finfo(float).eps
kept = s > cutoff
beta = vt[kept, :].T @ ((u[:, kept].T @ y) / s[kept])
residual = y - x @ beta
cosine = np.dot(np.array([1.0, 0.0, 0.0]), residual) / np.linalg.norm(residual)
assert abs(cosine - 1.0 / math.sqrt(2.0)) < 1e-15
print(json.dumps({
    'claim': 'Independent bounded arithmetic and NumPy toy analysis; not Scala or Gale execution',
    'quadrature': {'requested_intervals': requested, 'int_capacity': maximum_int, 'effective_step_if_saturated': effective_if_saturated},
    'underflow': {'nonzero_entries': values.tolist(), 'squared_entries': (values * values).tolist()},
    'reference_cutoff': {'singular_values': s.tolist(), 'cutoff': float(cutoff), 'retained': kept.tolist(), 'residual': residual.tolist(), 'cosine_with_earlier_column': float(cosine)}
}, indent=2))
