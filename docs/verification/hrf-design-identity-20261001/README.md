# HRF and design structural identity v2

Issue: `bd-01M3SC5BYS29CR1TVS2JCVZ3Y8`. The isolated source starts from committed main `debdf34b`; shared dirty HRF/design work is outside this change.

The reproduction at `13a6d35c79c17f2fb685c61dfc90b8ceacc1159b` demonstrates different descriptor, FIR-role, column and fingerprint strings on JVM and Scala.js. Integral doubles and exponent capitalization were platform-dependent, and role indices retained a literal `%02d`. `baseline.json` binds the observations to raw logs and actual exit metadata. Its Scala.js HRF test intentionally fails the JVM golden.

The new descriptor format begins `hrf-descriptor/v2`. All HRF parameter, derivative and integration alternatives use explicit tags, length-framed string/sequence fields and signed decimal `Double.doubleToLongBits` values. Rendering a Long is exact on both platforms. Finite values and signed zero retain every bit; all NaNs use Java's canonical NaN. Lag, block and normalization constructors retain their declared policy in a typed `HrfDerivation` and the source descriptor as a child. Blocking records width, precision, half-life, summation, normalization and integration policy. Renaming a generated kernel retains this policy.

Basis role indices have a minimum width of two decimal digits, and FIR intervals retain exact bits. Display column labels retain their existing naming policy.

Design fingerprints begin `design-schema/v2`. Rank tolerance, diagonal-R values, condition estimate, centering means, orthogonalization norms/tolerance, imputation constants and DCT receipt coordinates use bit encodings. The matrix and acquisition-time encodings already used bits and retain that convention. When a semantic basis-element ID exists, the canonical basis reference uses it rather than the display basis name. HRF assignment and construction receipts likewise use descriptor/element identity. Legacy references without an element ID retain their basis-name identity. User-supplied semantic strings remain literal identity input.

## Compatibility and artifact audit

This is a breaking identity change. Existing artifacts retain their original strings and model revisions; readers must never rewrite them by substituting a new prefix. Rebuild a design and any bindings/readouts from its original model specification, inputs and numerical policy to obtain v2 identities. Reuse with a v2 fit requires explicit requalification. Matching a display name is insufficient.

The audit of committed main found no persisted resource goldens containing `design-schema/v1:` or the old descriptor IDs. `ResultManifestWriterSuite` asserts the sidecar prefix and is updated to v2. `ResultManifestWriter` writes the complete fingerprint and structural columns; it does not parse or migrate the prefix. Estimate metadata codecs retain opaque `ColumnId` strings. `DesignSchema.validate` rejects an old fingerprint against the realized v2 matrix/schema, and `CoefficientAxis.structurallyCompatible` requires equality of the fingerprint and ordered column IDs. The shared regression suite exercises the old-fingerprint refusal.

DCT policy receipts now use `tr_bits` and `acquisition_start_bits`; `cutoff_bits` remains, while the redundant `cutoff_seconds` field is removed. Decode bit fields with `Double.longBitsToDouble` when displaying values. Imputation receipts use `impute-constant:<signed-bits>`; human-readable policy labels remain available separately.

The parked first-level candidate `aef31a29c238079de9050fb5fb68643291c3af15` has platform-specific design/readout goldens in `ResponseSourceBinderSuite`, with an upstream-convergence tripwire. Its owner will rebuild and review those goldens when the identity repair lands. This change does not qualify or land that candidate or its integration prerequisites.

## Evidence boundaries

`numeric_oracle.py` uses Python's independent standard-library binary packing to produce the literal finite-bit and SPMG descriptor goldens in `numeric-oracle.json`. Shared tests cover every HRF parameter/integration alternative, exact SPMG descriptor and element identity, role indices, signed zero, nested components and audit fields. Single-run, two-run and FIR design goldens run unchanged on JVM and Scala.js.

Identity parity applies when numerical contents and all other identity inputs are equal. The baseline 16-scan single-run SPMG model has two one-ULP differences in its matrix across platforms. The two-run and FIR baseline matrices are bit-identical. The 16-scan case remains a structural-identity golden and its computed values are checked against literal baseline contents within one ULP. The exact same literal numerical snapshot has a shared fingerprint golden on both platforms. This repair neither rounds numerical values nor asserts universal bit equality of platform transcendental arithmetic. A one-ULP matrix change must retain a distinct fingerprint.

Final qualification results are recorded separately after the bounded JVM, Scala.js and compile gates complete.
