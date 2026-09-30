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
The first local candidate was `e0d720f4b968f7d6deeac6978aa986e75dc2f721`
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
JVM and JS image suites. These are historical local-override checks, not
evidence for the later default pin.

The reviewed provider follow-up is
`2695f891cbec31a7f565a9b39e2554fe3b6d4b40`. It additionally refuses a
raw zero `pixdim[4]` with typed `PixelDimension(4)` when an unknown temporal
unit is explicitly interpreted as seconds, milliseconds or microseconds.
Default ordinal interpretation and positive-step assumptions retain their
separate behaviors. A pre-fix standalone provider clone reproduced the new
regression failure; the corrected provider passed 59/59 JVM and 40/40 Scala.js
NIfTI tests (`io-provider-zero-final-r2.log` and `.meta.json`). The ScalaFIM
consumer candidate `52b26c524a73db672d49029ed1f6ea62ec18de28` passed
`imageJVM/test` 380/380 and `imageJS/test` 351/351 against that local provider
source (`io-image-zero-final-r2.log` and `.meta.json`).

The provider revision is hosted at the head of
`review/nifti-temporal-origin-20260929` and draft image4s PR #13. `git
ls-remote` resolved both that branch and `refs/pull/13/head` to the exact
revision above on 29 September 2026. This is review-branch publication, not a
merge into image4s `main`. ScalaFIM's `build.sbt` now names this exact revision;
the default-pin tests below qualify the source dependency fetched without
`scalafim.image4s.build`, not a ScalaFIM push or release.

The default source dependency was loaded at
`~/.sbt/1.0/staging/cd3a5082f5d2990355dd/image4s`, whose Git `HEAD` is
`2695f891cbec31a7f565a9b39e2554fe3b6d4b40`. The image JVM and JS build
logs show direct image4s sources compiling from that checkout. Sbt also loaded
an older image4s staging checkout through a transitive build definition; its
`HEAD` was `26a74ad99b9ee49a9555344e19b82d69a2ba50e4`. This evidence
qualifies ScalaFIM's direct default provider pin, not the removal of every
older transitive project reference.

The following commands used `run-sbt.py` in the ScalaFIM `execution/io`
checkout, without `-Dscalafim.image4s.build`. Each successful run has a
matching `.meta.json` exit receipt under
`/private/tmp/scalafim-execution-20260929/logs/`. Hashes are SHA-256 of the
complete raw `.log` files. All five runs exited 0 with no `[warn]` or
`[error]` lines.

| Command | Result | Raw log | SHA-256 |
| --- | --- | --- | --- |
| `imageJVM/test` | 380/380 | `io-default-pin-image-jvm-r2.log` | `b4aedcb6066c0f201e968429c447f91150912dad63997e8bdc7e3b30b19faa28` |
| `imageJS/test` | 351/351 | `io-default-pin-image-js-r1.log` | `8ffd88515058fc2a137b27d33fb0f89e6da4e43cbb9623347467485555e10707` |
| `estimatesIoJVM/test` | 20/20 | `io-default-pin-estimates-io-jvm-r2.log` | `bee9fc6745990981b1eb992a76f6ef84a7a1b27f546bdaf6205ec92cf0202717` |
| `estimatesIoJS/test` | 4/4 | `io-default-pin-estimates-io-js-r1.log` | `38347868a57d0e1498a54ad4ddef3e3e54540a3e650644f3af070f4750726620` |
| `scalafimCompileAll` | 89 successful alias steps | `io-default-pin-compile-all-r1.log` | `1deffc33362c7e338cc9e7dd4fe74f0acea5d4fdddbd0ea6a7b6642d1d8baff5` |

The first sandboxed JVM attempt stopped before sbt project loading because it
could not write `~/.sbt/boot/sbt.boot.lock`. A subsequent estimates IO attempt
used the nonexistent `estimatesIOJVM` target and stopped at sbt command
parsing; the successful `estimatesIoJVM` run above is the actual test evidence.
These local results qualify the pinned ScalaFIM candidate only. The stable
estimate interchange, HDF5, producer/workflow adoption and downstream release
gates remain separate open work.
