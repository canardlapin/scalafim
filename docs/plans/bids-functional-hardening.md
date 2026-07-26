# BIDS Validation and Effect-Boundary Hardening

ScalaFIM's BIDS module already has the right large-scale shape: a pure,
cross-compiled domain core with JVM filesystem adapters. This plan strengthens
that shape rather than replacing it with a functional-programming framework.

The governing rule is:

> Scala 3 types own BIDS meaning; Cats may accumulate independent validation
> results, and Cats Effect may control adapter-side effects, only where each
> dependency demonstrates a concrete improvement.

The work is tracked by Mote epic `bd-01KY5VHC78YM8R7T3RRY9BS08B`. It is a
follow-up to the completed initial BIDS epic
`bd-01KWVTT6RKEBYXTHPMZD23Q49V` and is non-blockingly related to the
BIDS-to-group workflow epic `bd-01KX6G842E07NN3XZSQF2Z5HH3`.

## Baseline

The accepted starting point on 2026-07-22 is:

- `bidsJVM/test`: 52 tests passed.
- `bidsJS/test`: 45 tests passed.
- `bids` has no internal ScalaFIM dependencies and no external library
  dependency of its own.
- The repository already pins Cats Core 2.12.0 and Cats Effect 3.5.4 in other
  cross-projects. This plan does not authorize a version change.
- `BidsManifest.fromRelativePaths` preserves unparsed files as
  `BidsFile(parsed = None)` but discards the reason returned by
  `BidsRegistry.parsePath`.
- `BidsProjectLoader` is a synchronous JVM adapter over blocking `java.nio`
  calls. Its directory streams are closed manually and sidecars are read
  sequentially.
- `BidsStudyCompiler` already accumulates `CatalogIssue` values, showing that
  downstream workflows need more than a single fail-fast ingest error.

Green tests establish the current baseline. They do not establish the new
diagnostic, resource-safety, cancellation, or bounded-concurrency contracts;
those require new focused evidence.

## Architectural Boundary

The shared BIDS core remains pure and cross-platform. The following values must
not acquire `IO`, `F[_]`, JVM paths, open handles, fibers, or runtime state:

- `BidsName`, `BidsEntities`, and registry/specification values;
- `BidsManifest`, `BidsFile`, and `BidsProject`;
- `BidsQuery` and entity/query-pattern values;
- `BidsTable`, `EventsTable`, metadata records, and confound results.

Pure parsing and validation return domain results. Filesystem traversal,
reading, cancellation, and bounded concurrency live in JVM or store
interpreters. No library method may call an `unsafeRun*` operation.

The current one-way internal dependency boundary remains authoritative:
`bids` may be consumed by `dataset`, `motion`, `fmri-workflow`, and
`dataset-zarr`; it must not depend back on them.

## Operation Taxonomy

| Operation | Semantics | Result discipline | Home |
| --- | --- | --- | --- |
| Parse one name, URI, JSON value, or table cell | Later work depends on the parsed prefix/value | Fail fast with `Either` | shared |
| Validate independent fields, entities, columns, or rows | All independent defects are useful | Accumulate ordered `BidsIssue` values | shared |
| Construct an always-valid domain value | Invalid states should not be representable | Private constructor plus total smart constructor | shared |
| Scan an inventory of relative paths | Valid files and diagnostics may coexist | `BidsValidationReport[BidsManifest]` | shared |
| Open/walk/read a local project | Root access may fail fatally; content defects may accumulate | Effect/fatal channel plus validation report | JVM adapter |
| Compile a study catalog | BIDS diagnostics inform workflow policy | Explicit mapping to `CatalogIssue` | `fmri-workflow` |

Sequential parsing must not be rewritten as applicative validation merely to
use Cats. Conversely, independent dataset defects must not be hidden behind a
single arbitrary first error.

## Diagnostic Contract

The shared diagnostic vocabulary will use domain types, not public Cats data
types:

```scala
enum BidsIssueSeverity:
  case Warning, Error

enum BidsIssueCode:
  case InvalidPath
  case InvalidName
  case InvalidEntity
  case InvalidTable
  case InvalidSidecar
  case MissingRequiredField
  case InconsistentMetadata

final case class BidsIssue(
    code: BidsIssueCode,
    severity: BidsIssueSeverity,
    path: Option[BidsPath],
    field: Option[String],
    message: String
)

final case class BidsValidationReport[+A](
    value: A,
    issues: Vector[BidsIssue]
)
```

The exact issue-code set may grow during F1, but each code must represent a
stable machine-actionable category. Messages explain an occurrence; callers
must not parse messages to recover category, severity, path, or field.

Ordering is deterministic:

1. normalized relative path;
2. issue-code declaration order;
3. entity/specification or column order;
4. stable field/message tie-breaker.

The manifest keeps its existing valid-file order for compatibility. Diagnostic
ordering does not depend on input iteration order or hash-map order.

### Strict and collecting policies

Collecting mode returns a usable value plus every recoverable issue. Strict
mode rejects when the report contains any `Error`, while preserving the entire
issue collection. Fatal failures such as an inaccessible project root remain
separate from content diagnostics.

Unrelated files such as editor metadata must remain distinguishable from files
that look like intended BIDS data but violate BIDS naming or entity rules. F1
must define this classification explicitly and test it; it must not label every
unknown file as invalid BIDS.

## Public API Compatibility

| Current API | Hardening path | Compatibility decision |
| --- | --- | --- |
| `BidsManifest.fromRelativePaths` | Add `fromRelativePathsChecked` returning a domain report | Keep the current permissive facade during this epic |
| `BidsProjectLoader.load` | Add checked synchronous loading, then an opt-in effectful loader | Keep the current return type and valid-project behavior |
| `BidsQuery(...)` with raw strings | Move callers to total typed factories; prevent unchecked execution | Migrate repository callers before narrowing the constructor |
| `EntityFilter` with `Vector[String]` | Introduce validated/non-empty typed predicates where semantically required | Preserve single-value convenience construction |
| `ConfoundStrategy(npcs, percentVariance)` | Make `Option[PcaRetention]` the core state | Retain a validating compatibility factory for old arguments |
| Generic event table loaders | Add a path-aware validated events-file result | Keep explicitly named generic TSV access separate |
| Single `BidsError` results | Derive deterministic primary errors from complete reports where compatibility requires one | Do not expose `ValidatedNec` in compatibility APIs |

Compatibility is not permission to preserve invalid core states indefinitely.
Where a public case-class constructor prevents the stated invariant, repository
callers will be migrated in F3 and the constructor narrowed deliberately.

## Dependency Admission

### Cats Core

Cats Core is admitted provisionally in F2 only for an implementation pilot.
The pilot must show all of the following:

- independent defects are accumulated with deterministic order;
- bespoke mutable/error-accumulation code is materially reduced;
- sequential parse logic remains direct and readable;
- public BIDS APIs remain domain-first unless a public Cats type has a written
  compatibility and extraction justification;
- JVM and Scala.js tests remain warning-clean.

`Traverse` syntax replacing the 16-line `BidsEither` helper is not, by itself,
sufficient justification. F2 records an explicit keep or remove decision.

### Cats Effect

Cats Effect is admitted provisionally in F4 at the JVM adapter boundary. A kept
pilot must demonstrate:

- blocking NIO is suspended through the appropriate effect capability;
- `Files.walk`, `Files.list`, and other closeables are resource-managed across
  success, error, and cancellation;
- independent reads use an explicit concurrency bound;
- result and diagnostic ordering remain deterministic;
- expected failures retain operation/path context;
- no runtime execution is hidden inside the library.

The initial effect dependency should be JVM-scoped unless a shared store
contract proves that a cross-platform dependency is necessary. F4 records an
explicit keep or remove decision.

### Portable store seam

F5 does not presume that `BidsStore[F]` should exist. It compares a direct
effectful JVM loader with the smallest plausible store algebra. The abstraction
is kept only if it measurably improves failure-injection tests, removes real IO
duplication, or unlocks an already-planned consumer. A rejected experiment must
leave no dead types or dependency residue.

Remote HTTP/S3 stores, browser filesystem APIs, caching, retry, and streaming
are explicitly outside F5.

## Consumer Inventory

The live repository consumers that constrain migration are:

- `fmri-workflow`: constructs manifests and queries in shared tests and loads
  projects in JVM ingest tests; `BidsStudyCompiler` already owns workflow-level
  issue policy.
- `motion`: consumes `BidsProject`, metadata records, and JVM paths; BIDS
  diagnostics must map into `MotionIoError` without collapsing useful context.
- `dataset`: reuses BIDS JSON and table parsing in the LNA JVM adapter.
- `dataset-zarr`: uses BIDS name parsing and the synchronous loader in its live
  NIfTI/BIDS bridge; this workstream is independently reserved and must be
  coordinated rather than edited opportunistically.
- BIDS module tests and README examples: use direct query construction,
  permissive manifests, synchronous loading, event tables, and confound
  strategies.

No downstream module is allowed to become the source of truth for BIDS issue
codes or parsing behavior.

## Execution Plan

1. **F0 -- contract freeze** (`bd-01KY5VKAWWK3811WVFCNZKRSA7`): publish this
   plan, inventory compatibility, and add no implementation dependency.
2. **F1 -- diagnostic foundation** (`bd-01KY5VKEWPTEBFNFA6093E6NTE`): add the
   domain issue/report model and checked manifest construction.
3. **F2 -- Cats validation pilot** (`bd-01KY5VKH7TBPCHCBHRTDWD1NT0`): use
   Cats Core internally for the proven accumulation seams and record keep/remove.
4. **F3 -- invalid-state hardening** (`bd-01KY5VKKV9MGJ9VBYQK45RQ5G8`): make
   query, confound-retention, event, and genuinely non-empty states precise.
   F2 and F3 may proceed independently after F1.
5. **F4 -- Cats Effect loader pilot** (`bd-01KY5VKQ9HQXDX77W974B452P3`): add
   resource-safe JVM loading with bounded concurrency and parity evidence.
6. **F5 -- store-seam decision** (`bd-01KY5VKSJ4SM93VQCM3TNDN80B`): keep,
   narrow, or remove the portable store experiment after F4 evidence exists.
7. **F6 -- integration/release gate** (`bd-01KY5VKVX9DMPEGPNQWPBZGYGZ`):
   migrate consumers, update durable docs, and run the full cross-platform gate.

The epic is blocked by F6. F6 is blocked by F2, F3, and F5; F5 is blocked by
F4; F2, F3, and F4 are blocked by F1; F1 is blocked by F0.

## Final Architecture Decisions

The implementation evidence narrows the provisional admissions as follows:

- **Keep Cats Core, internally.** `ValidatedNec` is used only behind the
  domain-first validation API for genuinely independent checks in datatype,
  query, and table construction. Sequential project traversal continues to use
  `Either`; no public compatibility API exposes Cats data types.
- **Keep Cats Effect on the JVM adapter boundary.** `BidsProjectLoaderF`
  suspends blocking NIO, manages closeables with `Resource`, bounds parallel
  sidecar reads, and preserves deterministic results. It does not select or run
  an effect runtime.
- **Keep a narrowed `BidsStore[F]`.** The algebra contains only deterministic
  recursive file entries and UTF-8 reads. The shared in-memory interpreter
  enables portable failure injection; the JVM `Path` interpreter owns NIO,
  `Throwable`, containment checks, and resource safety.
- **Keep the synchronous loader as a compatibility facade.** `loadChecked`
  returns operational failure separately from `BidsValidationReport` and
  accumulates independent filename, entity, participant-table, sidecar, and
  resolved-metadata defects; `load` retains its legacy fail-fast behavior;
  `loadStrict` preserves the complete report when content errors prevent
  execution.
- **Preserve diagnostics through workflow compilation.** `CatalogIssue` carries
  the original BIDS issue code, severity, path, and field, while successful
  compilation may retain warnings in `CatalogCompilation`. Motion and other
  JVM consumers that require valid identities enter through `loadStrict`.

Rejected additions remain rejected: public `Validated` APIs, effectful shared
domain values, hidden `unsafeRun*`, unbounded parallelism, generalized
filesystem operations, remote/browser stores, writes, caches, retry, and FS2
streaming. They require separate measured use cases rather than being implied
by the retained abstractions.

## Verification

Each shared-code slice must run both platforms:

```sh
sbt bidsJVM/test bidsJS/test
```

F4 adds JVM effect/resource tests. F6 additionally runs affected consumers and
the aggregate gates:

```sh
sbt motionJVM/test
sbt fmriWorkflowJVM/test fmriWorkflowJS/test
sbt compileAll testAll
```

Release evidence must include:

- a multi-defect fixture proving complete deterministic diagnostics;
- a valid fixture proving compatibility with the legacy manifest/loader result;
- strict-versus-collecting policy tests;
- resource finalization on success, failure, and cancellation;
- bounded-concurrency instrumentation;
- synchronous/effectful parity for kept loader behavior;
- recorded Cats Core, Cats Effect, and store-seam keep/remove decisions.

The F6 completion gate passed with 77 BIDS JVM tests, 59 BIDS JS tests, 21
workflow JVM tests, 18 workflow JS tests, 90 motion JVM tests, 77 motion JS
tests, and 9 dataset-zarr JVM tests. The final `compileAll testAll` repository
gate also passed on the same working tree on 2026-07-22.

No child or epic is complete merely because the dependency was added or the
existing tests remain green.

## Explicit Exclusions

- Full official BIDS schema-validator coverage.
- A repository-wide functional-programming migration.
- Replacing the JSON or TSV parser solely for ecosystem consistency.
- FS2 streaming without measured pressure from the current eager table API.
- Remote-store implementations, retries, caches, or durable workflow state.
- Treating `Resource`, fibers, or in-memory queues as durable scientific or
  workflow authority.
