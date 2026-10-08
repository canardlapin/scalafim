#!/usr/bin/env python3
"""Offline independent same-basis recovery diagnostic. Never PHRF qualification.

Requires NumPy/SciPy; not a dependency of Scala builds or tests. Uses dense
conditional normal equations for optimization and augmented QR for final audits.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor, as_completed
import hashlib
import itertools
import json
import os
from pathlib import Path
import platform
import time

# Set before importing numerical libraries; outer parallelism is explicit.
for name in ('OPENBLAS_NUM_THREADS', 'OMP_NUM_THREADS', 'MKL_NUM_THREADS', 'VECLIB_MAXIMUM_THREADS'):
    os.environ[name] = '1'
import numpy as np
import scipy
from scipy.linalg import cho_factor, cho_solve, lstsq, qr
from scipy.optimize import minimize
from scipy.special import expit


class Reference:
    def __init__(self, directory):
        self.directory = directory
        self.metadata = json.loads((directory / 'input.json').read_text())
        def read(name):
            shape = self.metadata['arrays'][name]
            return np.fromfile(directory / (name + '.bin'), dtype='>f8').astype(np.float64).reshape(shape)
        self.rows, self.trials, self.rank = (self.metadata[k] for k in ('rows', 'trials', 'rank'))
        self.expanded = read('expanded').reshape(self.rows, self.rank, self.trials)
        self.f = read('nuisance')
        self.q, _ = qr(self.f, mode='economic')
        expanded = self.expanded.reshape(self.rows, -1)
        projected = expanded - self.q @ (self.q.T @ expanded)
        self.projected = projected.reshape(self.rows, self.rank, self.trials).transpose(1, 0, 2).copy().reshape(self.rank, -1)
        self.phi, self.lags = read('phi'), read('lags').ravel()
        self.clean, self.noise = read('clean'), read('unit-noise')
        self.lower, self.upper = (np.array(self.metadata[k]) for k in ('lower', 'upper'))
        self.width = self.upper - self.lower
        membership = np.eye(3)[np.arange(self.trials) % 3]
        self.penalty = np.eye(self.trials) - membership @ np.diag(1 / membership.sum(axis=0)) @ membership.T
        self.truth = np.array(self.metadata['truth'])
        self.starts = [np.full(3, .5)] + [np.array(p) for p in itertools.product((.25, .75), repeat=3)]
        # Cross-language value/gradient audit at a point used only for validation.
        actual = self.coefficients(self.truth).ravel()
        expected = np.array(self.metadata['truthCoefficientJet'])
        self.coefficient_error = float(np.max(np.abs(actual - expected)))
        if self.coefficient_error > 1e-12:
            raise ValueError(f'independent coefficient jet mismatch: {self.coefficient_error}')

    def coefficients(self, theta):
        p, q, rho = np.exp(theta[0]), expit(theta[1]), theta[2]
        u = p * q
        t = self.lags
        gp = .5 * p**3 * t**2 * np.exp(-p * t)
        gu = (u**4 / 6) * t**3 * np.exp(-u * t)
        gp_a, gu_a = gp * (3 - p*t), gu * (4 - u*t)
        jets = np.stack((gp-rho*gu, gp_a-rho*gu_a, -rho*(1-q)*gu_a, -gu))
        return jets @ self.phi.T

    def problem(self, ratio, voxel):
        y = self.clean[:, voxel] + ratio * self.noise[:, voxel]
        yp = y - self.q @ (self.q.T @ y)
        scale = self.rows * self.metadata['signalRms'][voxel]**2
        def evaluate(unit, full=False):
            theta = self.lower + self.width * unit
            jets = (self.coefficients(theta) @ self.projected).reshape(4, self.rows, self.trials)
            x = jets[0]
            h = x.T @ x + self.penalty
            a = cho_solve(cho_factor(h, lower=True, check_finite=False), x.T @ yp, check_finite=False)
            residual = yp - x @ a
            deviations = self.penalty @ a
            energy = residual @ residual + deviations @ deviations
            gradient = np.array([-2 * residual @ (dx @ a) for dx in jets[1:]])
            if full:
                return float(energy), gradient, a
            return float(energy / scale), gradient * self.width / scale
        return y, scale, evaluate

    @staticmethod
    def projected_gradient(unit, gradient):
        g = gradient.copy()
        g[(unit <= 1e-7) & (g > 0)] = 0
        g[(unit >= 1-1e-7) & (g < 0)] = 0
        return float(np.max(np.abs(g)))

    def solve(self, ratio, voxel):
        started = time.monotonic()
        y, scale, evaluate = self.problem(ratio, voxel)
        runs = []
        for start in self.starts:
            result = minimize(evaluate, start, jac=True, bounds=[(0., 1.)]*3, method='L-BFGS-B',
                              options={'maxiter': 300, 'maxfun': 1200, 'maxls': 40, 'ftol': 1e-14, 'gtol': 1e-8})
            energy, gradient = evaluate(result.x)
            runs.append({'start': start.tolist(), 'unit': result.x.tolist(), 'energyScaled': energy,
                         'projectedGradient': self.projected_gradient(result.x, gradient),
                         'optimizerSuccess': bool(result.success), 'message': str(result.message),
                         'iterations': int(result.nit), 'evaluations': int(result.nfev)})
        best = min(runs, key=lambda r: r['energyScaled'])
        unit = np.array(best['unit'])
        theta = self.lower + self.width * unit
        energy, gradient, amplitudes = evaluate(unit, full=True)
        # Central differences of the analytic gradient, even at chart boundaries:
        # Cascade's formula is defined nearby, but only in-chart solutions are searched.
        hessians = []
        for step in (2e-4, 1e-4):
            h = np.column_stack([(evaluate(unit + np.eye(3)[j]*step)[1] -
                                  evaluate(unit - np.eye(3)[j]*step)[1])/(2*step) for j in range(3)])
            hessians.append((h+h.T)/2)
        eigenvalues = np.linalg.eigvalsh(hessians[-1])
        # Independent augmented QR, including nuisance, at the returned shape.
        c = self.coefficients(theta)[0]
        x = np.einsum('p,tpn->tn', c, self.expanded)
        augmented = np.block([[x, self.f], [self.penalty, np.zeros((self.trials, self.f.shape[1]))]])
        rhs = np.r_[y, np.zeros(self.trials)]
        solved, _, rank, _ = lstsq(augmented, rhs, cond=1e-12, lapack_driver='gelsy')
        residual = rhs - augmented @ solved
        qr_energy = float(residual @ residual)
        amplitude_error = float(np.max(np.abs(amplitudes - solved[:self.trials])))
        if rank != self.trials + self.f.shape[1] or amplitude_error > 1e-7 or abs(qr_energy-energy)/scale > 1e-10:
            raise ValueError(f'QR disagreement: voxel={voxel}, ratio={ratio}, rank={rank}, amplitude={amplitude_error}')
        stage = next(s for s in self.metadata['stages'] if s['noiseRatio'] == ratio)
        decoded = stage['decoder'][voxel]
        decoded_unit = (np.array(decoded['coordinates']) - self.lower)/self.width
        decoder_energy, _ = evaluate(decoded_unit)
        truth_unit = (self.truth - self.lower)/self.width
        truth_energy, truth_gradient, _ = evaluate(truth_unit, full=True)
        truth_gradient_error = float(np.max(np.abs(truth_gradient - np.array(decoded['truthGradient']))))
        # Scale-aware energy/gradient checks against Scala's separate banded implementation.
        scala_energy_error = abs(decoder_energy*scale - decoded['energy']) / scale
        if scala_energy_error > 1e-9 or abs(truth_energy-decoded['truthEnergy'])/scale > 1e-9 or truth_gradient_error/scale > 1e-9:
            raise ValueError(f'Scala/dense disagreement: voxel={voxel}, ratio={ratio}, energy={scala_energy_error}, gradient={truth_gradient_error/scale}')
        boundary = bool(np.any((unit <= 1e-5) | (unit >= 1-1e-5)))
        return {'noiseRatio': ratio, 'voxel': voxel, 'coordinates': theta.tolist(),
                'energy': energy, 'energyScaled': energy/scale, 'energyAtTruthScaled': truth_energy/scale,
                'shapeErrorInChartWidths': float(np.max(np.abs(unit-truth_unit))),
                'boundary': boundary, 'projectedGradient': best['projectedGradient'],
                'curvatureEigenvaluesScaled': eigenvalues.tolist(),
                'curvatureStepDifference': float(np.max(np.abs(hessians[0]-hessians[1]))),
                'interiorStationaryPositive': bool(not boundary and best['projectedGradient'] <= 1e-6 and eigenvalues[0] > 1e-8),
                'qrRank': int(rank), 'qrEnergy': qr_energy, 'qrAmplitudeMaxError': amplitude_error,
                'scalaEnergyScaledError': scala_energy_error, 'scalaTruthGradientScaledError': truth_gradient_error/scale,
                'decoderStatus': decoded['status'], 'decoderCoordinates': decoded['coordinates'],
                'decoderEnergyGapScaled': decoder_energy-energy/scale,
                'starts': runs, 'seconds': time.monotonic()-started}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('input', type=Path)
    parser.add_argument('output', type=Path)
    parser.add_argument('--workers', type=int, default=4)
    parser.add_argument('--voxels', type=int)
    parser.add_argument('--scala-fixture', type=Path)
    args = parser.parse_args()
    reference = Reference(args.input)
    count = args.voxels or reference.metadata['voxels']
    if not 1 <= count <= reference.metadata['voxels']:
        parser.error('voxel count outside exported input')
    result = {'format': 'phrf-recovery-reference/1', 'qualification': 'not-admitted',
              'numpy': np.__version__, 'scipy': scipy.__version__, 'python': platform.python_version(),
              'workers': args.workers, 'coefficientJetMaxError': reference.coefficient_error,
              'inputSha256': {p.name: hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(args.input.glob('*')) if p.is_file()},
              'reference': 'dense conditional Cholesky + nine interior L-BFGS-B starts; final independent augmented QR',
              'limitations': ['same prepared basis; no original-family certificate', 'multistart is not proof of global optimum',
                              'matched condition-mean signals, not a random-effects calibration cohort'], 'records': []}
    started = time.monotonic()
    args.output.parent.mkdir(parents=True, exist_ok=True)
    def persist():
        result['seconds'] = time.monotonic()-started
        result['records'].sort(key=lambda r: (r['noiseRatio'], r['voxel']))
        args.output.write_text(json.dumps(result, indent=2, allow_nan=False)+'\n')
    with ThreadPoolExecutor(max_workers=args.workers) as pool:
        jobs = {pool.submit(reference.solve, ratio, v): (ratio, v)
                for ratio in reference.metadata['noiseRatios'] for v in range(count)}
        for future in as_completed(jobs):
            record = future.result()
            result['records'].append(record)
            persist()
            print(f"ratio={record['noiseRatio']} voxel={record['voxel']} error={record['shapeErrorInChartWidths']:.3g} "
                  f"boundary={record['boundary']} stationary+PD={record['interiorStationaryPositive']} "
                  f"decoder={record['decoderStatus']} gap={record['decoderEnergyGapScaled']:.3g}", flush=True)
    result['complete'] = True
    persist()
    if args.scala_fixture:
        write_scala_fixture(result['records'], args.scala_fixture)


def write_scala_fixture(records, path):
    lines = ['package scalafim.fmri.laws.profile', '',
             '/** Generated by tools/verification/phrf-recovery/reference.py; same-basis dense SciPy/QR oracle. */',
             'object TrialRecoveryOracle:',
             '  final case class Case(noiseRatio: Double, voxel: Int, coordinates: Vector[Double], energy: Double)',
             '  val cases: Vector[Case] = Vector(']
    for index, row in enumerate(records):
        values = ', '.join(repr(float(v)) for v in row['coordinates'])
        comma = ',' if index + 1 < len(records) else ''
        lines.append(f"    Case({float(row['noiseRatio'])}, {row['voxel']}, Vector({values}), {float(row['energy'])}){comma}")
    lines.append('  )')
    path.write_text('\n'.join(lines)+'\n')


if __name__ == '__main__':
    main()
