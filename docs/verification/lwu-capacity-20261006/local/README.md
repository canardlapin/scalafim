# Local LWU capacity qualification

The compiler capacity failure is fixed through explicit per-compilation admission. Defaults remain 2 million array cells and 100 billion spectral work units; the frozen LWU caller requests 3 million and 128 billion. Fixed limits of 16 million total storage cells, 20 million training units and 100 million certification units remain unchanged. See [capacity-policy.md](capacity-policy.md) for the estimate, integer arithmetic proof and API compatibility.

All required local gates pass against public Gale `b56a9dd0b8ad479621a3594880f90c9add8c2824`, without a local override:

- Compiler/kernel tests: 29 JVM and 29 JS.
- Unchanged compact cohort: 3 tests on each platform; admitted 24/24 JVM and 23/24 JS.
- Complete original LWU accuracy assertions: 1 test on each platform.
- Whole-project JVM and JS compilation: exit 0, zero warning lines.
- Explicit server shutdown: exit 0.

The verification-only inherited selector asserts exactly one original LWU test and preserves its 100-voxel cells, seeds and timeout. LWU admission remains below the reported 95% milestone: local JVM 91%/81%, local JS 91%/83% at SNR 1/0.5. Its original accuracy assertions pass. This does not qualify clean LWU admission. Metrics retain stdout rounding. Native Linux results are independently retained in `../native`.

`source-manifest.json` binds all four capacity sources, adopted fit sources, controls, unchanged cohort and build pin. The source-only patch contains only the four capacity/fixture paths. Raw logs and receipts retain the initial compiler field-placement error and corrected final gates. No optional tests followed the required gates.
