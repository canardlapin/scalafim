# Unified MVPA M1.01 identified-evidence evidence

Recorded: 2026-09-13

Packet: M1.01 (`bd-01M2BNEPV2TD9CXT3HDQG6DTKY`)

Base revision: `79a8b7d1fbe07c383bb8929a69973729c22ce64b`

## Result

The production `mvpa` module now contains one identified-evidence layer over
the admitted Multivar, ScalaFIM response, and ScalaFIM locus boundaries.
Legacy `SampleAxis`, `Response`, `PatternMatrix`, `PatternOperator`, and
`PatternSource` remain unchanged for their scheduled M1/M2 cutovers.

The new layer provides:

- `AxisRef[K]`, separating semantic keys, implementation ordinals, a compact
  SHA-256 axis signature, the full decoded axis record, Multivar nominal space,
  response `DomainId`, and the canonical locus owner;
- `Column[S, A]`, binding categorical, scalar, or metadata values to the exact
  sample space without extending the legacy `Response` ADT;
- `Observations[S, N]` over Multivar `Table[S, N]`, with dense, Gale CSR, and
  matrix-free construction routes sharing one scientific identity;
- `MultiResponse[S, F]`, retaining both the identified sample axis and an
  identified target-feature axis; and
- typed `Supervised` and `MultiResponseSupervised` pairings whose foreign row
  spaces fail compilation.

`mvpa` now depends directly on the existing `response` and `locus-data`
modules, plus the pinned Multivar core on JVM and Scala.js. No new registry,
response hierarchy, matrix hierarchy, fold engine, or resampling machinery was
introduced.

## Identity and boundary laws

1. Axis identity includes ordered stable keys, namespace, Multivar role, basis,
   units, scale, and source lineage. Reordering or changing any one of these
   fields changes the signature and Multivar descriptor.
2. The signature is a compact content address, not the sole compatibility
   proof. Public decode paths recompute it from the complete `AxisRecord`,
   compare the full stable-key record to the expected nominal `AxisRef`, then
   invoke Multivar's checked decoder before exposing numerical access.
3. The portable SHA-256 encoding is length-framed and has a fixed independent
   test vector. Axes are no longer restricted to the M0 prototype's 64
   coordinates; the cross-platform court includes 4096 coordinates.
4. `AxisIndex[K]` keeps ordinals as an implementation capability. Exported
   columns and tables carry ordered stable keys, not inferred row positions.
   The lawful parent/child `ReindexingLeg` and repeated-occurrence identity
   remain owned by M1.02.
5. Dense, sparse, and matrix-free representations retain distinct execution
   descriptors while sharing `EvidenceIdentity` when axes, response
   provenance, source, and Multivar `ValueIdentity` are equal. Sparse and
   matrix-free adapters do not materialize at construction.
6. ScalaFIM response `SourceId` and `Provenance`, Multivar `ValueIdentity`, and
   the canonical locus `DomainFactory` are composed directly. Provider failures
   remain typed cases of `EvidenceError`; they are not reduced to strings.

## Type-discipline review

The focused review found no casts, `Any` erasure, `null`, wildcard
exhaustivity suppression, weakened compiler policy, broad implicit conversion,
or hidden materialization. One initially partial locus lookup was changed to a
typed `Either`, provider error values were retained in the error ADT, and the
sparse convenience route was kept as an exact expansion of the same checked
table binder.

Compile-negative tests require exactly one diagnostic and a boundary-specific
message for:

- a locus point owned by a different runtime axis;
- a column paired with observations from a foreign sample space; and
- a multiresponse target paired with observations from a foreign sample
  space.

## Verification

Executed in the isolated implementation clone with Scala 3.7.4, sbt 1.11.7,
Java 25.0.1, and Node 26.7.0:

```text
sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaJVM/test
  132 passed, 0 failed, 0 errors

sbt -J-Xmx4G -Dsbt.task.cpus=2 -Dsbt.supershell=false mvpaJS/test
  132 passed, 0 failed, 0 errors

sbt -J-Xmx6G -Dsbt.task.cpus=2 -Dsbt.supershell=false scalafimCompileAll
  passed
```

The six owned Scala files were formatted through targeted `scalafmtOnly`.
The module-wide `mvpaJVM/Compile/scalafmtCheck` remains red on 22 unchanged
legacy MVPA sources, matching the repository's pre-existing formatting
baseline; it did not report any owned file.

## Scope boundary

This packet establishes production axes, columns, observations, and
multiresponse targets. It does not claim lawful restriction, draw occurrence
identity, resample4s binding, measurement frames, dataset streaming, predictive
replacement, inference, scientific calibration, or the frozen JDK 21/Node 22
resource profile. Those remain assigned to later packets, beginning with
M1.02.
