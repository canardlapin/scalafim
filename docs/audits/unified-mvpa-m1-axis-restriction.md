# Unified MVPA M1.02 lawful axis-restriction evidence

Recorded: 2026-09-14

Packet: M1.02 (`bd-01M2BNERS3BQVEZZNNJ6JTJWW3`)

Base revision: `c390b08ebb63523702d869043768d2ec871d3253`

## Result

The production `mvpa` module now binds Resample4s `Selection`, `Injection`,
`Draw`, and `Permutation` values to the exact identified parent axis created by
M1.01. Every binding produces a fresh nominal child space, a response-compatible
ordinal selection, and one matrix-free Gale gather/scatter operator wrapped by
Multivar `Lin`.

The new layer provides:

- `ReindexingLeg[P, K, R]`, whose path-dependent `Child` type prevents a leg
  from being applied to evidence from another sample space;
- `AxisMember[K]`, retaining the typed root key, its stable key, and a bounded
  nested `OccurrenceAddress` for repeated draws;
- associative staged composition that retains exact mapping steps and the
  final child type without casts or erased payloads;
- ordinary restriction methods for `Column`, `Observations`, and
  `MultiResponse`; and
- distinct `ValidationDesign`, `CrossFitDesign`, `BootstrapDesign`, and
  `RandomizationDesign` bindings over the admitted Resample4s plans and
  receipts.

The implementation reuses Resample4s ordinal algebra and coverage witnesses,
Gale's public `LinearOperator.fromFunctions`, Multivar semantic composition,
and the existing ScalaFIM response identity. It does not add another fold
engine, random generator, numerical matrix family, response hierarchy, or
compatibility wrapper.

## Identity and composition laws

1. Child identity includes the parent axis, exact ordinal mapping, operation
   kind, and complete staged lineage. Selection, injection, draw, and
   permutation therefore remain distinct even when they select the same root
   keys.
2. Nested restriction and a composed leg produce the same values, exact root
   keys, and key order as an independently calculated direct ordinal oracle.
   Left- and right-associated three-leg compositions have identical provider
   mappings, child records, and `ReindexingIdentity` values.
3. Draws append the position of each occurrence. Repeated source rows remain
   separate `AxisMember` values such as `[2, 0]` and `[2, 1]`; no `Map` or
   source-key grouping collapses them.
4. The forward numerical leg is an allocation-disciplined gather. Its adjoint
   is a scatter-add, so duplicate draws accumulate rather than overwrite. Both
   directions agree exactly with independently constructed dense matrices.
5. Binding and composing operator-backed observations do not read the source.
   The owned test uses a counted matrix-free operator and observes zero reads
   until explicit evaluation.

## Coverage and deterministic-stream laws

`ValidationDesign` accepts provider-certified `Coverage.Exact`, including
repeated exact plans. `CrossFitDesign` requires the stronger
`Coverage.ExactOnce`. `BootstrapDesign` retains `Draw` analysis rows and only
ordinary `Coverage`. Public provider `Plan.map` deliberately forgets its
stronger coverage witness, and compile-negative tests prove it cannot be
reintroduced by assignment.

The exact-once court independently counts every stable sample key once and
checks analysis/assessment disjointness in every unit. The repeated-exact court
counts every key twice without calling that plan exact-once.

`ScientificSeed` is the only public randomness input. Provider seeds are
derived internally from a purpose-specific stream domain and every character
of the complete sample-axis signature. Equal scientific inputs reproduce the
same receipt; reordered axes and different design purposes produce different
seeds and receipts. Accessing plan units forward or backward produces the same
axis-bound assignments, so worker identity and traversal order are absent from
the derivation.

## Type-discipline review

The focused review found no casts, `Any`/`Matchable` erasure, `null`, exposed
worker- or traversal-based seed coordinate, private numerical decomposition,
or duplicate resampling machinery. The public reindexing kind witness exists
only for the four concrete Resample4s operations, with an actionable compiler
diagnostic for an abstract `Reindexing`. Provider design, digest, fingerprint,
composition, response-index, and unknown-unit failures remain typed cases of
`EvidenceError`.

Compile-negative tests require exactly one boundary-specific diagnostic for:

- applying a leg from a foreign nominal row space;
- binding an abstract, kind-erased `Reindexing`;
- passing repeated `Coverage.Exact` as exact-once cross-fit evidence;
- treating bootstrap coverage as exact;
- retaining exact-once coverage after arbitrary `Plan.map`; and
- interchanging validation and bootstrap design families.

## Verification

Executed in the isolated implementation clone with Scala 3.7.4, sbt 1.11.7,
Java 25.0.1, and Node 26.7.0:

```text
sbt -Dsbt.supershell=false mvpaJVM/test
  143 passed, 0 failed, 0 errors

sbt -Dsbt.supershell=false mvpaJS/test
  143 passed, 0 failed, 0 errors

sbt -Dsbt.supershell=false scalafimCompileAll
  passed
```

The seven owned Scala files were formatted through targeted `scalafmtOnly`.
The first formatter command incorrectly supplied repository-relative paths to
the JVM subproject and stopped without finding a file; the corrected
subproject-relative invocation passed. `build.sbt` was kept as a narrow,
locally styled dependency edit rather than rewriting its pre-existing
whole-file formatting baseline.

## Scope boundary

This packet establishes lawful identified restriction, repeated-occurrence
identity, coverage-preserving design bindings, and scientific-coordinate
random streams. It does not yet migrate the legacy `FoldPlan`, implement
pairing/exchangeability policy, claim target-free fitted preparation, add
measurement frames, or qualify a scientific estimator. Those remain assigned
to M1.E1, M1.03, M1.06, and later predictive/inference packets. No push,
release publication, or foreign-store mutation is part of this candidate.
