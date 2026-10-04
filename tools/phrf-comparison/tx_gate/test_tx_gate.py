import numpy as np
import pytest

import tx_gate as G

T = G.T


def test_grid_and_zero_padding():
    assert len(T) == 481 and T[0] == 0 and abs(T[-1] - 48.0) < 1e-12


def test_cascade_matches_literal_formula():
    p, u, rho = 0.5, 0.2, 0.3
    lit = p**3 * T**2 * np.exp(-p * T) / 2 - rho * u**4 * T**3 * np.exp(-u * T) / 6
    assert np.allclose(G.cascade_shape(p, u, rho), lit, atol=1e-15)
    assert abs(G.cascade_shape(0.5, 0.2, 0.0)[40] - 0.5 * 2.0**2 / 2 * np.exp(-2.0)) < 1e-15  # t=4, x=2


def test_cascade_area_and_peak():
    h = G.cascade_shape(0.5, 0.2, 0.3)
    assert abs(np.trapezoid(h, T) - (1 - 0.3)) < 5e-3  # signed integral 1 - rho (tail truncation at 48 s)
    assert abs(T[np.argmax(G.cascade_shape(0.5, 0.2, 0.0))] - 4.0) < 1e-9  # Erlang-3 mode 2/k


def test_self_distance_zero_and_scale_invariance():
    g = G.gauss_shape(5.0, 1.5)
    assert G.rel_dist(g, g) < 1e-7
    assert G.rel_dist(-3.7 * g, g) < 1e-7  # closed-form amplitude, sign free
    assert G.rel_dist(g, G.gauss_shape(20.0, 1.0)) > 0.99  # essentially orthogonal


@pytest.fixture(scope="module")
def grids():
    return (G.build_grid(G.gauss_from_chart, G.GAUSS_LO, G.GAUSS_HI),
            G.build_grid(G.cascade_from_chart, G.CAS_LO, G.CAS_HI))


def test_fit_recovers_in_chart_shapes(grids):
    (gm, gG, gG2), (cm, cG, cG2) = grids
    h = G.gauss_shape(5.3, 1.37)  # off-grid member of the Gaussian chart
    d, dg = G.chart_distance(h, G.gauss_from_chart, G.GAUSS_LO, G.GAUSS_HI, gm, gG, gG2)
    assert d < 1e-5 and d <= dg
    h = G.cascade_shape(0.43, 0.43 * 0.27, 0.41)
    d, dg = G.chart_distance(h, G.cascade_from_chart, G.CAS_LO, G.CAS_HI, cm, cG, cG2)
    assert d < 1e-4 and d <= dg


def test_inf3_contains_can_and_derivatives():
    can, tder, disp = G.can_shapes()
    B = np.stack([can, tder, disp])
    assert abs(T[np.argmax(can)] - 5.0) < 0.05
    assert G.span_distance(can, B) < 1e-10 and G.span_distance(0.3 * can + 2 * tder - disp, B) < 1e-10
    # temporal derivative agrees with a central difference of the canonical
    fd = np.gradient(can, 0.1)
    assert np.max(np.abs(fd[5:-5] - tder[5:-5])) < 2e-3
    assert G.rel_dist(can, can) < 1e-12


def test_sobol_set_properties():
    pts, sums, seen = G.tx_points()
    assert pts.shape == (1024, 7) and seen >= 1024
    for s in sums:
        for k, (lo, hi) in G.K.TX_RANGES.items():
            assert lo <= s[k] <= hi
    pts2, _, _ = G.tx_points()
    assert np.array_equal(pts, pts2)  # deterministic, no seed
