"""Independent small-matrix checks for the profile-HRF contextual review.

These checks are not the proposal's referenced validation script, a ScalaFIM
implementation test, or a scientific validation. Run with NumPy and SciPy:
    OPENBLAS_NUM_THREADS=1 python profile_hrf_context_checks.py
"""
from __future__ import annotations
import json
import numpy as np
from scipy.linalg import block_diag, cho_factor, cho_solve


def solve_spd(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    return cho_solve(cho_factor(a, lower=True, check_finite=True), b)


def logdet_spd(a: np.ndarray) -> float:
    cf, _ = cho_factor(a, lower=True)
    return float(2 * np.log(np.diag(cf)).sum())


def main() -> None:
    rng = np.random.default_rng(20260910)
    T, N, C = 110, 21, 3
    t = np.arange(T, dtype=float)
    onsets = np.sort(rng.uniform(1, 85, N))
    lag = t[:, None] - onsets[None, :]
    X = np.where(lag >= 0, np.exp(-0.5 * ((lag - 5) / 1.5) ** 2), 0)
    F = np.column_stack([np.ones(T), np.linspace(-1, 1, T)])
    M = np.eye(C)[np.arange(N) % C]
    counts = M.T @ M
    P = np.eye(N) - M @ np.linalg.solve(counts, M.T)
    lam = 0.7
    y = X @ rng.normal(size=N) + F @ np.array([0.2, -0.1]) + 0.15 * rng.normal(size=T)
    Z = np.column_stack([F, X @ M])

    def release(x: np.ndarray, penalty: float):
        s = np.eye(T) + x @ x.T / penalty
        z = np.column_stack([F, x @ M])
        rz, ry = solve_spd(s, z), solve_spd(s, y)
        c = solve_spd(z.T @ rz, z.T @ ry)
        q = ry - rz @ c
        a = M @ c[F.shape[1]:] + x.T @ q / penalty
        energy = float(q @ q + penalty * np.linalg.norm(P @ a) ** 2)
        return a, c, energy, s

    a, c, E, S = release(X, lam)
    D = np.column_stack([F, X])
    normal = D.T @ D + block_diag(np.zeros((F.shape[1], F.shape[1])), lam * P)
    direct = solve_spd(normal, D.T @ y)
    rel = float(np.linalg.norm(a - direct[F.shape[1]:]) / np.linalg.norm(a))
    K = np.eye(T) + X @ P @ X.T / lam
    G = X.T @ X + lam * np.eye(N)
    invsqrt_counts = np.diag(1 / np.sqrt(np.diag(counts)))
    small_positive = lam * invsqrt_counts @ M.T @ solve_spd(G, M) @ invsqrt_counts
    determinant_error = abs(logdet_spd(K) - logdet_spd(S) - logdet_spd(small_positive))

    scale = 2.3
    scaled_a, _, E_compensated, _ = release(scale * X, scale**2 * lam)
    _, _, E_fixed_lambda, _ = release(scale * X, lam)
    normalization_equivalence = float(np.linalg.norm(a - scale * scaled_a) / np.linalg.norm(a))

    # Add a regularized, condition-centered stimulus feature model for amplitudes.
    Phi = P @ rng.normal(size=(N, 4))
    Gamma = np.diag([0.4, 0.7, 1.1, 1.4])
    Zf = np.column_stack([F, X @ M, X @ Phi])
    fixed_penalty = block_diag(np.zeros((F.shape[1] + C, F.shape[1] + C)), Gamma)
    RZf, Ry = solve_spd(S, Zf), solve_spd(S, y)
    cf = solve_spd(Zf.T @ RZf + fixed_penalty, Zf.T @ Ry)
    qf = Ry - RZf @ cf
    uf = X.T @ qf / lam
    objective_release = float(qf @ qf + lam * (uf @ uf) + cf @ fixed_penalty @ cf)
    # Independent solve with u restricted to an orthonormal contrast basis.
    eigvals, eigvecs = np.linalg.eigh(P)
    Q = eigvecs[:, eigvals > 0.5]
    Df = np.column_stack([Zf, X @ Q])
    penalty_f = block_diag(fixed_penalty, lam * np.eye(N - C))
    df = solve_spd(Df.T @ Df + penalty_f, Df.T @ y)
    residual_f = y - Df @ df
    objective_direct = float(residual_f @ residual_f + df @ penalty_f @ df)

    # Fixed-shape conditional readout: covariance of errors does not disappear.
    QF = np.linalg.qr(F, mode='reduced')[0]
    A = X - QF @ (QF.T @ X)
    L = solve_spd(A.T @ A + lam * P, A.T)
    rho = 0.8
    Sigma_cross = rho * (L @ L.T)  # unit temporal noise covariance
    # For equal true amplitudes, equal readouts and unit marginal noise variance:
    noise_only_far_distance = float(2 * np.sum(L * L))
    noise_only_near_distance = (1 - rho) * noise_only_far_distance
    results = {
        'original_vs_release_amplitude_relative_error': rel,
        'positive_small_determinant_identity_absolute_error': float(determinant_error),
        'normalization_with_rescaled_lambda_amplitude_relative_error': normalization_equivalence,
        'normalization_with_rescaled_lambda_energy_absolute_error': abs(E - E_compensated),
        'energy_original': E,
        'energy_scaled_kernel_fixed_lambda': E_fixed_lambda,
        'stimulus_extension_objective_absolute_error': abs(objective_release - objective_direct),
        'stimulus_extension_centering_max_absolute_error': float(np.max(np.abs(M.T @ uf))),
        'conditional_cross_voxel_covariance_frobenius_norm': float(np.linalg.norm(Sigma_cross)),
        'noise_only_near_vs_far_squared_distance_ratio_rho_0_8': noise_only_near_distance / noise_only_far_distance,
    }
    assert rel < 1e-10
    assert determinant_error < 1e-10
    assert normalization_equivalence < 1e-10
    assert abs(E - E_compensated) < 1e-10
    assert abs(objective_release - objective_direct) < 1e-10
    assert np.max(np.abs(M.T @ uf)) < 1e-10
    print(json.dumps(results, indent=2))


if __name__ == '__main__':
    main()
