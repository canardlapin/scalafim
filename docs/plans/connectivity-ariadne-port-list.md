# Ariadne Connectivity Port List

This list preserves the ordered port strategy from the Ariadne review. The goal
is not to mirror Ariadne's R surface. The goal is to lift the durable analysis
semantics into ScalaFIM's typed connectivity algebra, with Scala-native ordering
by default and Ariadne ordering only for explicit parity fixtures.

## Priority Order

1. ETS / PETS / DETS kernels plus event-weighted correlation.
   - Crisp primitive-array kernels with strong Ariadne fixtures.
   - Unlocks event-centric and dynamic analyses without adding heavy solvers.

2. Partial correlation: ridge precision, then PC-regression approximation.
   - High analytic value and a clean static-estimator fit.
   - Keep graphical lasso out until a portable optimizer exists.

3. Connectivity-set inference: edgewise GLM, PC-MANOVA, and kernel-machine
   summaries.
   - Turns subject connectivity matrices into group and trait analyses.
   - Port as typed design/model/test objects, not R formula/list wrappers.

4. Dynamic low-risk layer: dynamic containers, sliding-window correlation,
   EWMA correlation stacks, and instantaneous outer-product stacks.
   - Builds the dynamic backbone before HMM, TVGL, or TrajIC.

5. Lagged/directed correlation and delay alignment.

6. Phase metrics from supplied phases: PLV, phase-winding flow, phase slip,
   DPLI, and PGF.

7. AEC lag cube and Welch/multiband coherence, after portable FFT and analytic
   signal support are settled.

8. Entropy kernels and low-rank ETS features.

9. Graph diffusion, heat-kernel shrinkage, and light Ricci geometry.

10. TrajIC trajectory decomposition.

11. HMM over ETS or TrajIC features.

12. PTE/MPTE, TVGL, SRLC, PID/HOI, and reporting/visualization surfaces.
    These are later advanced layers, not structural-core shapers.

## Porting Rules

- Keep shared connectivity code portable across JVM and Scala.js.
- Preserve mathematical contracts as typed constructors and ADTs, not string
  dispatch.
- Put hot numeric kernels in row-major `Array[Double]` loops.
- Add analytic, metamorphic, and adversarial tests for each ported analysis.
- Keep the connectivity core free of atlas, BIDS, plotting, JVM IO, and native
  optimizer dependencies.
