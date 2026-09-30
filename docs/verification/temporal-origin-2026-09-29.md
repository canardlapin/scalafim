# Temporal sampling candidate, 29 September 2026

## Contract

The image4s provider owns NIfTI-1 `pixdim[4]` (Float32 byte 92), temporal
`xyzt_units` (byte 123), and `toffset` (Float32 byte 136). Its typed
`NiftiTemporalOrigin` accepts finite values that remain finite in Float32,
including negative origins. The header retains the stored value in the header's
declared unit. The fourth-axis time coordinate is `origin + index * step`,
where both stored terms have Float32 precision. This is a description of stored
volume coordinates; an effective first-level fit reference is a separate
scientific declaration.

Unknown temporal units keep the stored origin in the header. The default
provider policy yields an ordinal fourth axis; an explicit `AssumeSeconds`,
`AssumeMilliseconds`, or `AssumeMicroseconds` policy interprets both origin and
step in that assumed unit. Frequency units keep the origin in the header but
the frequency axis starts at zero, matching the existing frequency-axis policy;
the file's `toffset` is not converted into a frequency coordinate. A 3D image
has no fourth-axis sampling. Nonfinite `toffset` is malformed header data even
when the file has no time axis.

ScalaFIM's `writeSeries` and incremental scalar writer derive the physical
sampling when their fourth axis is a regular Time axis and the write options
have unknown temporal unit, no declared nonspatial pixel dimensions and zero
origin. This value-based rule also applies after changing only the datatype or
I/O limits. Declared options must agree with the axis's unit, step and origin
at stored Float32 precision; disagreement refuses before file creation. Regular
Time axes with unsupported units and explicit irregular numeric coordinates
refuse. A plain ordinal Time axis has no known physical sampling to compare;
the default writer records unknown temporal unit, zero origin and unit index
spacing. A caller may declare sampling explicitly for an ordinal axis; those
declared options are preserved as the caller's interpretation. Non-Time axes
also preserve caller options. The provider retains an unknown-unit fourth axis
as a generic ordinal axis. ScalaFIM's `readSeries` reidentifies that axis as
ordinal Time because the caller requested a series, without assigning physical
seconds or copying the data. Callers may use
`NiftiReadOptions` with `AssumeSeconds`, `AssumeMilliseconds`, or
`AssumeMicroseconds` to interpret its stored step and origin, or `Reject` to
refuse the file.

The estimate-set NIfTI fourth axis identifies estimands or covariance pairs,
not acquisition time. The estimate reader requires an unknown temporal unit,
zero origin and unit pixel dimension for that axis.

## Evidence and limits

Provider source base: `26a74ad99b9ee49a9555344e19b82d69a2ba50e4`.
Local candidate: `e0d720f4b968f7d6deeac6978aa986e75dc2f721`
(implementation `b999436`, added coverage `818ba35`, malformed-step validation
`e0d720f`). The provider run passed 58 JVM and 39 Scala.js tests, including
independent byte inspection of single, gzip and pair headers, malformed
origin, unknown-unit assumptions, millisecond/microsecond coordinates,
Float32 rounding, and incremental writer option retention. Its raw output
and exit receipt are `io-provider-final-r3.log` and `.meta.json` under
`/private/tmp/scalafim-execution-20260929/logs/`.

ScalaFIM `imageJVM/test` passed 379/379 and `imageJS/test` passed 351/351
against this provider source; both ran in `io-image-policy-r4.log` with exit 0
and no compiler warnings. `scalafimCompileAll` passed with exit 0 and no
warnings in `io-compile-all-r1.log`; that broad compile preceded the final
ordinal-series reidentification, which was then compiled and exercised by the
JVM and JS image suites. Neither a provider pin nor upstream publication is
implied by these local candidate checks.
