# Why the Scala.js recovery sweep is slower

The uninstrumented 16-voxel noiseless test took 432.774 seconds on Scala.js
versus 103.516 on JVM (4.18x). Ratio 0.1 took 388.818 versus 89.957 (4.32x).
These are diagnostic test timings on the same host, not controlled throughput
benchmarks. Both execute the public search/readout and the same assertions;
the number of search calls can differ with floating-point trajectory decisions.

## Live CPU samples

The busy Node process was verified as a child of this worktree's sbt server,
at approximately one full CPU, before attaching the localhost V8 inspector.
Two 20-second profiles were captured during noise ratios 0.5 and 1. Neither
capture paused the debugger or changed model state. Later JS test timings
therefore include sampling overhead. The final JVM output receipt is unaffected.

First sample, self time (excluding called methods):

| Work | Sample share | Owner |
| --- | ---: | --- |
| Weighted assembly of derivative bands (`addBlock`) | 33.18% | ScalaFIM `TrialBandedObjective.reference` |
| Band by derivative-solution products (`bandMatrixProduct`) | 32.87% | ScalaFIM `TrialBandedObjective.reference` |
| Remaining reference-construction body | about 8.3% | ScalaFIM |
| Banded triangular solves | 6.51% | Gale `BandedCholesky.run` |
| Garbage collector | 0.16% | V8 |

Raw profiles and function-level shares are retained in `js-live*.cpuprofile.gz`
and `js-profile*-summary.json`. Inclusive optimizer time includes the entire
objective; it must not be mistaken for time spent doing BFGS updates. This sample
does not implicate the tiny three-dimensional BFGS update or GC as the primary
cost. It also does not establish how JVM distributes its time.

The second sample corroborates the first: 30.03% band assembly, 33.85% band
products, 6.66% Gale banded solves and 0.11% GC.

The loaded file is the `test-fastopt/main.js` bundle. Its emitted hot loops use
Scala `Array[Double]` wrappers around `Float64Array` with checked `get`/`set`,
null checks and integer-index conversions. The band product recomputes symmetric
packed indices separately for each of nine right-hand sides. Gale's internal
`DoubleArray` uses direct `Float64Array` access. These observations motivate
measurements; they do not by themselves prove the source of the whole JVM/JS gap.

## Bounded kernel replay

`kernel-replay.mjs` copies the two emitted fastopt kernel bodies and provides
equivalent checked-array helpers. It compares them with direct typed-array
access and a row/band traversal shared across all nine right-hand sides.
Dimensions match B0: N=300, bandwidth 40, rank 10, ten jet components and nine
right-hand sides. Arrays contain deterministic synthetic finite values, not a
fitted voxel. All variants preserve the summation order for each output entry;
the observed maximum numerical difference is zero.

After 20 warmup calls per method, seven alternating-order measurements each
execute 20 calls. Median results from `kernel-replay.json`:

| Kernel | Current emitted loop | Direct typed arrays | Batched right-hand sides |
| --- | ---: | ---: | ---: |
| Derivative-band assembly | 8.042 ms | 7.412 ms (1.09x) | Not tested |
| One band by matrix product | 0.620 ms | 0.232 ms (2.68x) | 0.193 ms (3.22x) |

This is an isolated replay, with the main sweep running concurrently. Its small
wrapper scaffold and synthetic data do not reproduce every V8 optimization
decision in the original bundle. A 3.22x kernel improvement is not a 3.22x fit
improvement. Optimizing only the sampled one-third band-product share by that
factor would imply roughly 1.29x overall under an unchanged time distribution;
that is an Amdahl estimate, not a measured fit speedup.

## Concrete next changes

1. Use full optimization for performance investigations, then profile that
   bundle before choosing an upstream API change. The experiment below already
   removes a substantial part of the observed gap through build mode alone.
2. Evaluate a Gale operation for symmetric lower-packed band by dense block
   multiplication with validated layouts, non-aliasing output, portable kernels
   and both-platform tests. Traverse a band once for all right-hand sides and
   keep the validated inner loop on Gale storage. Preserve per-entry summation
   order initially. This is generic algebra and belongs upstream, while PHRF's
   derivative policy stays in ScalaFIM. Existing Gale `Banded` uses a different
   storage layout, so merely replacing calls is not a zero-copy solution.
3. Expose a charged value/gradient-only PHRF oracle for search. In three
   dimensions a full jet carries value, three first derivatives and six second
   derivatives. BFGS currently pays for all ten, including derivative-band
   assembly and differentiated solves, although it consumes only four.
   Keep the full jet for terminal scientific admission. This targets both
   platforms and more of the measured work than changing array access alone.
4. Recheck the retained counterexamples, all reference objectives, refusals,
   work counters and end-to-end timings after each change; then consider a
   staged search budget. Do not infer qualification from a kernel speedup.

## Full optimization experiment

The retained ratio-0.1 batch (all 16 voxels, including the strong-signal
counterexample and augmented-QR readout check) passes in all three cases:

| Runtime | Test seconds |
| --- | ---: |
| JVM | 89.957 |
| Scala.js fastopt | 388.818 |
| Scala.js fullOpt | 156.107 |

FullOpt is 2.49x faster than fastopt in this diagnostic, and 1.74x the JVM time.
The earlier 4.32x gap is therefore substantially affected by build mode. These
are single test timings without controlled repetitions or normalized oracle
counts. FullOpt starts a fresh fixture, while the fastopt ratio-0.1 test follows
the noiseless test and reuses preparation. The kernel replay and live profiles
above are fastopt evidence; their speedups must not be applied to fullOpt without
remeasuring. No production algorithm or default linker setting changed.

This MUnit version's Scala.js runner rejects both regex and glob command-line
case filters. The experiment instead used a temporary two-line `munitTests`
selector on the existing suite; `fullopt-probe-source.scala` preserves that
source. The complete shared suite was restored byte-for-byte to its already
JVM/JS-tested hash, and both test compilations plus restored FastOptStage were
checked afterward. `fullopt-comparison.json` and the logs bind the measurement,
the temporary selector and restoration. Initial unsupported-filter attempts
failed before running any numerical test and are retained separately.

Scala.js documents [full optimization and selecting `FullOptStage`](https://www.scala-js.org/doc/project/building.html#full-optimize).

Reproduce the standalone replay with:

```sh
node docs/verification/phrf-refinement-20261007/kernel-replay.mjs /tmp/kernel-replay.json
```

`node-cpu-profile.mjs` is the retained inspector client. It checks the target PID
before sampling through the localhost inspector and disconnects afterward.
The capture follows the official [Node CPU profiler API](https://nodejs.org/api/inspector.html#cpu-profiler).
