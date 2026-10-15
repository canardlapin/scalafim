# Family summary admission verification

Mote `bd-01M47BDR7J7FKGXESZFY9J370P` bounds parametric tail diagnostics and
LWU attained summaries before scalar sampling allocation or evaluation.

Run `python3 docs/verification/family-summary-budget-20261005/verify.py` from an
exclusive shared-worktree build slot. The runner uses `tools/build/sbt-warm`,
runs focused HRF and KernelBasis consumer suites on JVM and Scala.js, then
checks two safe mutations. It restores source in `finally`, compares all owned
source/test hashes, removes temporary selectors, and retains a receipt and
logs/source snapshots archive. Concurrent source changes invalidate the receipt.

The cap mutant admits one extra sample, at most 1,000,001 samples and 2,000,002
Double array cells. The last-time mutant uses only three samples. Both fail on
both platforms; restored selectors and production family suites pass.

`fixtures.py` regenerates `fixtures.json` using independent scalar Gaussian and
LWU formulas in Python's standard library. Shared Scala tests contain these
numeric goldens. Existing LWU family tests also compare raw evaluations with
the library kernel. This change preserves the existing quadrature and attained
summary conventions; it introduces no R statistical algorithm or claimed R run.

Admission permits 1,000,000 samples, one scalar value evaluation per sample,
and two Double cells per sample. No basis or jet arrays are allocated by these
summary methods. Shape points are checked against the smart-constructed chart.
Custom family evaluation internals are outside this budget; callback validation
failures and nonfinite values/energy have typed errors.

Tail extent must be finite and >= 1. Its legacy ceil/minimum-two grid can
overshoot the requested endpoint, even at extent 1; final sampled times must
remain finite. LWU retains its 0.01-second floor grid, including horizons below
one step and partial final steps. Requests are refused rather than coarsened.

KernelBasis.compile preflights the tail diagnostic before sampled allocation
and propagates its typed errors from certification. Overall node, jet, matrix,
SVD and repeated held-out evaluation budgets remain a separate follow-up.
