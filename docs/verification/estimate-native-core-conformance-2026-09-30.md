# Native Core-NIfTI conformance controls — 2026-09-30

This control adds a JVM-only, independently constructed NIfTI-1 fixture for
Core-NIfTI reader conformance. It does not change the format, producer, store,
or build dependency graph.

`NativeCoreConformanceSuite` writes a small single-file NIfTI-1 byte stream
field by field, without calling `NiftiEstimateSink`, a producer, or a NIfTI
writer. Its selected transform is a scanner `sform` with a negative x axis,
obliquity, and shear. Its coded `qform` deliberately identifies a distinct
`aligned-anatomical` alternative. The test checks hand-derived voxel-world corners from the loaded selected
transform and confirms that a digest-pinned replacement with a changed scanner
binding is refused by actual local-store open before it returns a consumer
source handle.

The fixture holds two observations, the ordered `task`/`drift` catalog, a
2 x 3 x 4 spatial domain, and 24 x-fastest sample identities. Its payload
value is independently defined as `1000 * observationOrdinal + 100 *
estimandOrdinal + sample`. Samples 7 and 19 are outside the declared support;
the manually encoded validity payload records that fact. Reordered access-map,
three-plane anchor, voxel-profile, ROI, and cohort-slab reads use only the
advertised `EstimateSelection` API and compare against this formula and the
independent expected validity code for every returned cell. The voxel profile
includes an outside-support sample. The suite also exercises capacity
rejection, duplicate-axis construction refusal, and cancellation with an
untouched destination.

A separate 352-byte NIfTI-1 header encodes dimension 32768 as the signed
16-bit overflow that the format's header field exposes. image4s retains that
signed header value; Core-NIfTI's typed geometry validation rejects its
incompatible logical dimensions before payload allocation or local-store
publication. Separately, a legitimate logical 32768 x 1 x 1 domain with one
supported sample is passed through the actual local sink opening path. The
NIfTI-1 writer refuses it, publishes no immutable unit payload, and removes
all owned staging files. This is a bounded format-limit control, not a
large-domain or resource-performance claim.

## Scope limits

The controls establish bounded synthetic Core-NIfTI reader behavior on the
JVM. They do not use a real or de-identified acquisition, establish renderer or
reader-UI behavior, qualify FIR or other scientific interpretation, test every
NIfTI fault mode, establish crash/power-loss behavior, qualify W5 consumer
reuse, or make a performance claim. Physical NIfTI I/O is JVM-specific; shared
estimate contract checks remain separately cross-platform.
