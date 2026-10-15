#!/usr/bin/env python3
"""Independent scalar formula fixtures for the existing summary conventions.

No R statistical behavior is introduced by the admission change. These goldens
preserve scalar Gaussian energy quadrature and raw LWU attained summaries;
LwuFamilySuite additionally compares the raw formula with the library kernel.
"""
import json
import math

TAIL_INPUTS = [(5.5, 1.7, 8.0, 0.3, 4.0), (6.0, 2.0, 6.0, 0.5, 4.0), (3.0, 0.8, 3.0, 0.7, 1.0)]
LWU_INPUTS = [(6.0, 2.0, 0.4, 32.0), (5.5, 1.7, 0.35, 32.0), (6.0, 2.0, 0.0, 32.0), (6.0, 2.0, 0.4, 7.005)]

def tail(tau, sd, horizon, step, extent):
    count = max(2, math.ceil(extent * horizon / step) + 1)
    total = tail_mass = 0.0
    for index in range(count):
        lag = index * step
        mass = (0.5 if index in (0, count - 1) else 1.0) * math.exp(-((lag - tau) / sd) ** 2)
        total += mass
        if lag > horizon:
            tail_mass += mass
    return tail_mass / total

def lwu(tau, sd, rho, horizon):
    times = [index * 0.01 for index in range(math.floor(horizon / 0.01) + 1)]
    values = [math.exp(-((lag - tau) / sd) ** 2 / 2.0) - rho * math.exp(-((lag - tau - 2.0 * sd) / (1.6 * sd)) ** 2 / 2.0) for lag in times]
    peak = max(range(len(values)), key=values.__getitem__)
    trough = min(range(len(values)), key=values.__getitem__)
    half = values[peak] / 2.0
    left = right = peak
    while left > 0 and values[left] > half:
        left -= 1
    while right < len(values) - 1 and values[right] > half:
        right += 1
    ratio = -values[trough] / values[peak] if values[peak] > 0.0 and values[trough] < 0.0 else None
    return dict(peak=times[peak], fwhm=times[right] - times[left], undershoot=ratio)

print(json.dumps(dict(tail=[dict(inputs=values, relative_energy=tail(*values)) for values in TAIL_INPUTS], lwu=[dict(inputs=values, summary=lwu(*values)) for values in LWU_INPUTS]), indent=2))
