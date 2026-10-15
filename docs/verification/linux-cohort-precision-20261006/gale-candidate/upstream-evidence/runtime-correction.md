# Runtime correction and additive Node24.21 qualification

The first candidate evidence README claimed Node24.1. That text was carried from an older environment assumption, not measured in the prototype gate. The full786JVM/774JS runs remain recorded, but their Node version was not captured. The original sealed receipt and118,128-byte archive are preserved as historical snapshots; their old Node wording is superseded by this correction.

The23 new comparison tests were subsequently run with an explicit Node24.21.0 binary directory first on PATH. Its version was checked before invocation; the recorded binary hash and raw success log are attached. All23 tests passed without compiler warnings, including both measured uphill controls and the false-positive previous-beta control. The numeric/test/public README source-only patch remains SHA256a18e2746e43e0414436d69c23534df74c3220c42d8ab5dc350fd032adc49736c.

The additive recorder failed after the test gate on an unavailable hashlib.file_digest attribute. Portable SHA256 metadata repairs that instrumentation failure. The raw sbt task log confirms23passes and24second task time; process exit code and whole-process elapsed time were not written, so the supplement does not invent them. No passed test was rerun.
