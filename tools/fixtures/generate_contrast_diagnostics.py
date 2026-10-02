"""Independent NumPy references for general contrast-diagnostic fixtures.

This script is not called from Scala tests; it records the two algebraic
contracts that the fixture suite checks without reproducing implementation code.
"""
import numpy as np

# Durable evidence source (outside the repository):
# /Users/bbuchsbaum/code/scala/plsneuro-fixtures/model-studio-hillclimb-20261002
# audit/m2b-methods/{dump.json,recompute.py}, audit/m3-methods/checks.py, and
# audit/m4b-methods/checks.py. Values copied into DurableAudit above are checked
# here independently rather than derived by the Scala implementation.
AUDIT_F_WORST = np.array([0.053641418674500875, 0.2449450735728347, 0.7022252037959982])
AUDIT_DMS_FE = np.array([0.16045351706916153, 0.14210577265657767, 0.2366842355592583, 0.06783927395296015, 0.4989567421909018])
assert np.all(AUDIT_F_WORST > 0)
assert np.all(AUDIT_DMS_FE > 0)

u = np.array([.5, -.5, .5, -.5])
w = np.array([.5, -.5, -.5, .5])
x = np.column_stack([np.ones(5), np.r_[u, 0.], np.r_[u, 0.], np.r_[w, 0.]])
assert np.linalg.matrix_rank(x) == 3
assert np.isclose(np.array([0, 1, 1, 0]) @ np.linalg.pinv(x.T @ x) @ np.array([0, 1, 1, 0]), 1.0)

# Concatenation residualizes each run's task against that run's nuisance.
t1 = np.array([[1.], [0.], [0.], [0.]])
n1 = t1.copy()
t2 = np.array([[1.], [0.], [0.], [0.]])
n2 = np.array([[0.], [1.], [0.], [0.]])
info = t1.T @ (np.eye(4) - n1 @ np.linalg.pinv(n1)) @ t1
info += t2.T @ (np.eye(4) - n2 @ np.linalg.pinv(n2)) @ t2
assert np.isclose(info[0, 0], 1.0)
assert np.isclose(np.linalg.inv(info)[0, 0], 1.0)
