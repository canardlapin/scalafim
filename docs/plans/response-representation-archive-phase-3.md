# Response/Representation/Archive Phase 3 Record

Status: complete and repository-wide verified on 2026-07-26  
Issue: `bd-01KYAA8MQNZ97F44JFPN6MQD39`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Track A integration record](response-representation-archive-track-a.md)

Phase 3 proves the representation boundary with one temporal-DCT vertical
slice. A representation compiles an output-domain selection into a pure,
typed, inspectable `DecodePlan[ResponseBlock]`. An in-memory interpreter
supplies three specialized logical payload values and the plan reconstructs
the selected response block without a general tensor abstraction, untyped
batch, cast, or string-result lookup.

## 1. Typed logical payload boundary

The temporal-DCT representation declares three distinct slots:

- `TemporalBasisSlot` produces `TemporalBasisValues`;
- `SpatialLoadingSlot` produces `SpatialLoadingValues`;
- `SampleOffsetSlot` produces `SampleOffsetValues`.

Each carrier owns a primitive `Array[Double]` with representation-specific
shape and access rules. The matrix-shaped carriers are not a shared matrix or
tensor supertype. Their slot/result relationship is carried by
`LogicalSlot[A]` and the `LogicalPayloadRead[A]` GADT. Logical identifiers and
roles are checked opaque strings used for identity and manifests; they are not
used to recover result types.

Public payload constructors copy caller-owned buffers and validate dimensions,
value counts, overflow, and finite values. `TemporalDctRepresentation.materialize`
is the checked assembly boundary that Phase 4 archive bindings can call after
recovering logical payloads.

## 2. Project-owned decode plan

`DecodePlan[A]` is a small project-owned algebra:

```text
Pure[A]
Request[A](LogicalPayloadRead[A])
Map[A, B](DecodePlan[A], A => B)
Zip[A, B](DecodePlan[A], DecodePlan[B])
Dependent[A, B](DecodePlan[A], A => DecodePlan[B])
```

`Pure`, `Request`, `Map`, and `Zip` form the normal applicative path.
`inspect` collects independent typed request leaves without executing them.
`runApplicative` interprets those leaves through a natural transformation to
an effect and refuses `Dependent` with a typed
`DecodePlanError.DependentReadBarrier`.

`Dependent` is an explicit sequential escape hatch. Inspection counts it as a
barrier, and only `runSequential` accepts it. It cannot silently make an
ordinary representation plan data-dependent.

## 3. Temporal-DCT compilation and execution

`TemporalDctRepresentation` owns:

- its output `ResponseSchema`;
- checked `DctSpec`, centering choice, and ridge penalty;
- typed basis, loading, and optional offset slots;
- its scientific `ReconstructionContract`;
- its independent `DecodeConsistency`.

Compilation first validates that the selection belongs to the model's schema
and that its output shape fits the primitive response carrier. It then emits
independent selected-row reads for the temporal basis, spatial loadings, and
optional sample offsets. The pure reconstruction kernel accumulates
basis/loading products in component order into a freshly allocated,
time-by-sample row-major `ResponseBlock`.

The in-memory interpreter is intentionally representation-specific. It
matches the GADT request, verifies the expected typed slot, selects only the
requested logical rows or entries, and returns the statically determined
payload type. There is no heterogeneous `Map[String, Any]`, public cast, or
closed central dispatch over every representation family.

## 4. Two independent numerical relations

Scientific reconstruction and execution-path consistency have different
jobs:

- `ReconstructionContract.Exact`,
  `DeterministicBounded(ReconstructionErrorBounds)`, or
  `ValidatedScientific` governs encode/decode fidelity relative to the source
  response.
- the representation's `DecodeConsistency` governs selected-versus-whole,
  partitioned, ordered, in-memory, and later archive-backed executions of the
  same represented value.

The full-rank centered DCT fixture declares bounded scientific reconstruction
because cosine projection incurs floating-point error. It declares
`ExactBits` path consistency because selected and whole reconstruction use the
same stored terms in the same component order. A separate rank-one fixture
proves that encoding rejects a representation whose declared exact
reconstruction contract is false.

## 5. Executable laws

The shared temporal-DCT suite proves on both the JVM and Scala.js:

- full-rank encode/decode satisfies the declared reconstruction bounds;
- plan inspection exposes the three typed independent reads and no barrier;
- selected decode agrees with selecting a whole decode under the
  representation's path-consistency policy;
- partitioned decode preserves requested order and agrees with one combined
  decode;
- output shape follows the resolved selection;
- same-shaped foreign-domain selections fail before plan construction;
- a basis request cannot typecheck as a loading result;
- a data-dependent read is visible and rejected by applicative execution;
- an incorrect exact reconstruction contract is enforced.

The focused source state passed:

```text
responseJVM/ResponseKernelSuite                 13 passed
responseJS/ResponseKernelSuite                  13 passed
latentJVM/TemporalDctRepresentationSuite         8 passed
latentJS/TemporalDctRepresentationSuite          8 passed
```

The complete response and latent module gates also passed:

```text
responseJVM/test   16 passed
responseJS/test    15 passed
latentJVM/test     93 passed
latentJS/test      93 passed
```

The repository-wide `compileAll testAll` gate passed end to end. The frozen
Phase 0 corpus receipt and recorded JVM/Scala.js digests remained valid without
regeneration. A final source scan found no cast, untyped result map, general
tensor, warning-suppression escape hatch, partial `.get`, or null in the new
production surface; `git diff --check` also passed.

## 6. Boundary for Phase 4

Phase 3 does not parse an LNA descriptor, choose physical payload ids, execute
an archive plan, or publish an archive. Phase 4 must bind each typed logical
read to the existing LNA physical vocabulary and compare the archive-backed
result with this in-memory interpreter under the same declared
`DecodeConsistency`.
