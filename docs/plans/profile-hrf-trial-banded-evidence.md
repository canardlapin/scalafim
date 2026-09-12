# TrialBanded backend evidence (PHRF-07)

Date: 2026-09-12. Scope: the shared trial-level ridge-and-release backend,
including its dense numerical laws and the B0 checkpoint that decides whether
the finite-state PHRF-10 backend must be activated. This is engineering and
same-model numerical evidence, not PHRF-14 scientific calibration or the
PHRF-29 executor integration.

## Estimand and implementation

For trial design `X(theta)`, nuisance `F`, one-hot trial-to-condition map `M`,
and unnormalised-kernel penalty `lambda`, the backend minimizes

`||y - F gamma - X a||^2 + lambda ||a - M beta||^2`.

It prepares `m(m+1)/2` lower-banded cross-basis Gram blocks without retaining
the dense expanded design. At each of eight reference shapes it constructs
`A = X'X + lambda I`, factors the packed band with Gale, and eliminates trial
coefficients against `Z = [F, X M]`. Immutable factors and the response-
independent jets of `Wc = A^-1 C` and `H = Z'Z - C'Wc` are shared by all
workers. Each worker retains only primitive response statistics and solve
scratch; `newWorker()` does not rebuild factors.

The analytic response jet differentiates `A wb = X'y`. First derivatives
solve `A wb_p = b_p - A_p wb`; mixed/pure second derivatives solve
`A wb_pq = b_pq - A_pq wb - A_p wb_q - A_q wb_p`. The corresponding
product-rule jets of `y'y - b'wb` and `Z'y - b'Wc` feed the common
`ProfileReduction`. Precomputing `Wc` makes the voxel loop one RHS per jet
component rather than one response plus every release column.

The constrained random-effects ML determinant is
`log|A| - (N-C) log(lambda) + log|M' A^-1 M| - log|M'M|`. It is evaluated from
the banded factor and a dense `C x C` release, never from a `T x T` covariance.
The conditional readout returns unnormalised condition means and all signed
trial amplitudes. Final normalization and public query contracts remain owned
by PHRF-11.

## Independent numerical evidence

`TrialBandedSuite` compares the backend with independently assembled dense
augmented normal equations and a dense full-data covariance. Its five laws
cover packed cross-basis blocks, band solves, profiled energy, condition and
trial amplitudes, finite-difference gradients/Hessians, the ML determinant,
unequal and singleton conditions, coincident trials, trial permutation,
`lambda = 1e-6` and `1e4`, nuisance alias refusal, two-run boundaries, and
shared AR whitening with exact run resets. The same suite passes on the JVM
and Scala.js.

Work counters are also executable contracts. The complete prepared workload
records exactly eight bank values, two analytic jets, one amplitude correction,
and 30 one-RHS band solves per voxel. Exact readout additionally counts one
factor and its two preparation solves: at B0 this is 32 calls and 42 RHS per
voxel. Invalid nodes are refused rather than charged as completed work.

## Frozen B0 checkpoint

The executable freeze is recorded in
[profile-hrf-cohorts.md](profile-hrf-cohorts.md#trial-cohort-b0-unchanged-from-revision-1).
The final batch used the frozen Apple M3 Max host, macOS 14.3, OpenJDK 22,
Node 26.7.0, sbt 1.11.7, the merged Gale pin `099832ff`, one warmed sbt JVM,
and eight workers sharing one immutable reference bank. Input is bounded to
256 voxels; the full Float32 `N x V` output is converted and consumed as a
stream rather than retained. Setup and input acquisition are reported
separately from measured compute.

The one-shot checkpoint runner performs a 32-voxel warmup. PHRF-15 still owns
five-run distributions and end-to-end IO qualification; these complete cells
are the earlier PHRF-07 activation decision required by the plan.

### N = 300 absolute and ratio cells

All rows use the identical dense schedule, responses and 120,000,000-byte
Float32 output workload.

| Workload | Compute | Setup | Retained engine | Conservative live | Work per voxel | Ratio to fixed |
| --- | ---: | ---: | ---: | ---: | --- | ---: |
| Fixed shape | 3.358 s | 7.374 s | 19.063 MiB | 33.968 MiB | 1 correction, 2 solves | 1.00x |
| One reference | 12.155 s | 8.370 s | 19.063 MiB | 33.968 MiB | 1 value, 1 jet, 1 correction, 13 solves | 3.62x |
| Complete prepared | 30.422 s | 6.871 s | 19.063 MiB | 33.968 MiB | 8 values, 2 jets, 1 correction, 30 solves | 9.06x |
| Complete exact readout | 48.198 s | 7.346 s | 21.124 MiB | 36.029 MiB | prepared work plus 1 exact factor; 32 solves / 42 RHS | 14.35x |

The prepared backend passes the absolute `<= 120 s`, engine `<= 256 MiB`,
one-reference `<= 12x`, and complete/two-reference `<= 24x` gates. Including
setup, prepared and exact-readout elapsed times are 37.293 s and 55.544 s.
The exact mode therefore passes PHRF-07's full measured engineering gate and
may remain an explicit opt-in mode; it does not become the default and carries
no scientific-calibration claim.

### Bandwidth crossover and N stress

| N | V | Schedule | Bandwidth | Compute | Setup | Retained engine | Conservative live | Float32 output |
| ---: | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 300 | 100,000 | dense uniform | 40 | 30.422 s | 6.871 s | 19.063 MiB | 33.968 MiB | 120 MB |
| 300 | 10,000 | regular | 26 | 1.839 s | 9.424 s | 14.483 MiB | 29.388 MiB | 12 MB |
| 1,200 | 10,000 | dense uniform | 126 | 31.136 s | 28.102 s | 187.493 MiB | 243.596 MiB | 48 MB |
| 1,200 | 10,000 | regular | 106 | 30.341 s | 28.684 s | 161.318 MiB | 217.421 MiB | 48 MB |

At N=300, normalizing the dense full cell to 10,000 voxels gives 3.042 s, so
the regular schedule is 1.65x faster. At N=1,200 the narrower band saves
26.175 MiB but the one-shot runtime difference is only 1.03x and is not
treated as a stable throughput effect. The N=1,200 cell is the declared
10,000-voxel stress/scaling case; no unmeasured 100,000-voxel pass is inferred.

The retained-engine estimate includes preparation, all eight immutable
references, eight worker workspaces, response encodings, trial/Float32 output
scratch, and—for exact mode—one live value-reference construction with its
multi-RHS builders per worker. The 1.172 MiB retained input block is reported
separately. The conservative-live column additionally retains that input and
the caller's dense expanded basis design, even though the backend itself does
not retain the design after preparation. These primitive-array bounds exclude
VM/object headers. The separately calculated N=1,200 dense preparation peak is
178.931 MiB; the conservative runtime bound remains 12.404 MiB below the
256 MiB cap. `/usr/bin/time -l` reported a 3.614 GiB maximum RSS for the entire
seven-cell sbt batch; that includes sbt, the compiler, dependency builds and
the JVM heap and is not substituted for engine live state.

## Decision and remaining boundaries

The primary dense B0 geometry passes both runtime and memory gates, including
the full exact-readout option. PHRF-10 therefore remains deferred; no
finite-state implementation is activated by this checkpoint. The N=1,200
stress result remains visible for later scaling work and is not promoted into
an unmeasured full-scale claim.

Sparse support storage, shared whitening, run-reset behavior and exact shape
readout are present. PHRF-11 still owns normalized/public trial query output;
PHRF-14 owns calibration; PHRF-15 owns repeated performance qualification;
and PHRF-29 owns executor attachment. Those boundaries are not claimed here.

## Validation commands

```text
sbt -Dsbt.supershell=false "firstLevelLawsJVM/testOnly scalafim.fmri.laws.profile.TrialBandedSuite"
sbt -Dsbt.supershell=false "firstLevelLawsJS/testOnly scalafim.fmri.laws.profile.TrialBandedSuite"
sbt -Dsbt.supershell=false scalafimCompileAll
sbt -Dsbt.supershell=false "firstLevelLawsJVM/test"
sbt -Dsbt.supershell=false "firstLevelLawsJS/test"
sbt -Dsbt.supershell=false "fitBenchJVM/compile"
sbt -Dsbt.supershell=false "fitBenchJVM/runMain scalafim.fmri.fit.profile.TrialBandedBenchmark 300 100000 dense complete 8"
```

The checkpoint class also exposes JMH phase methods for projection, fixed
shape, one reference, complete prepared, complete exact and exact-only
readout. The reported table uses the one-shot runner so exact work receipts,
streamed output bytes and engine-memory accounting accompany wall time.

The final full gates passed warning-clean: `scalafimCompileAll`, 56/56 JVM
first-level laws and 56/56 Scala.js first-level laws. The existing LWU SNR 0.5
admission shortfall was printed as reported-but-not-gated and is unchanged by
this backend work.
