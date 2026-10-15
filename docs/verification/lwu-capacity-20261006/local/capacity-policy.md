# Explicit LWU compiler capacity

The frozen LWU specification retains 14×9×7 shape nodes, a 0.1-second fine step, derivative training, tolerance 1e-3, maximum rank 48, 300 held-out points and the original seed. Default capacities remain 2,000,000 cells per array and 100,000,000,000 spectral work units. This caller explicitly requests 3,000,000 and 128,000,000,000. Total storage (16,000,000), training work (20,000,000) and certification work (100,000,000) remain fixed.

| Estimate | Count |
|---|---:|
| Fine samples | 321 |
| Grid points | 882 |
| Training columns | 8,820 |
| Training cells | 2,831,220 |
| Total storage cells | 9,024,270 |
| Training work | 8,498,985 |
| Spectral work | 120,096,758,484 |
| Certification work | 49,833,900 |

The smart constructor permits positive array capacity up to the fixed 16-million-cell storage envelope and positive spectral capacity up to 2^53−1. The estimate records the typed policy. Nondefault provenance records both capacities, including work-only overrides. Default canonical provenance retains the legacy golden exactly.

## Integer arithmetic

Every spectral operand is an integer; terms are nonnegative. Fixed storage bounds the matrix and k² by 16 million, so k ≤ 4,000. Existing bounds n ≤ 100,000 and columns ≤ 1,000,000 give:

`128 × 16,000,000 × 4,000 + 4 × 1,100,000 × 16,000,000 = 78,592,000,000,000 < 2^53`.

Each intermediate is bounded by the total. A separate computed-count guard refuses counts beyond 2^53−1 before conversion to Long, preserving the contract if common envelopes change.

## Qualification and remaining admission milestone

Kernel gates pass 29 tests on each platform. The unchanged compact cohort passes three tests on each platform, admitting 24/24 JVM and 23/24 JS. The complete original LWU accuracy assertion body passes once on each platform. Selected rank is 23 and compact dimension is 69. Whole-project JVM and JS compilation succeeds with no warnings; explicit server shutdown exits 0.

LWU admission remains below the separately reported 95% milestone: local JVM 91%/81% and local JS 91%/83% at SNR 1/0.5. Printed p95 latency is 0.0000/0.0100 seconds and FWHM 0.0100/0.0100 seconds on both platforms. Printed relative amplitude errors are 2.91e-4/4.50e-4 JVM and 3.19e-4/4.50e-4 JS. These metrics are rounded stdout values; 0.0000 does not claim exact zero. Accuracy assertions pass while the admission milestone remains unmet. Native Linux evidence is retained separately in `../native`.

## API and source compatibility

`KernelBasisCapacity` is an immutable validated policy with value equality, defaults and typed construction failures. Capacity is appended to `KernelBasisSpec`, `KernelBasisProvenance` and `KernelBasisEstimate` with defaults, so existing constructor calls that omit it continue to compile. Record arity changes affect positional pattern matches and compiled binary consumers; they require adjustment or recompilation. No binary compatibility claim is made. Default canonical provenance remains byte-identical. Numerical inputs, rank selection and scientific assertions are unchanged.
