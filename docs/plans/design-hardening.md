# design module hardening

Status: **landed** — phases 0–4 complete; phase 5 deliberately not scheduled.
Date: 2026-08-01.

A review of `modules/design` in the spirit of [`hrf-hardening.md`](hrf-hardening.md).
Everything numeric below was measured against the working tree, not inferred.
The probes are retained as characterization suites, so each phase lands as a
flipped assertion rather than a silent behaviour change.

---

## 1. What the module already gets right

This bounds how much should change. `design` is in considerably better shape
than `hrf` was at the equivalent point, and the `hrf` diagnosis does not
transfer.

- **No untyped escape hatches at all.** Across 10 347 lines of main source:
  zero `Any`, zero `Map[String, Any]`, zero `asInstanceOf`, zero `isInstanceOf`,
  zero `sys.error`. The `hrf` review opened on `Hrf.params: Map[String, Any]`;
  there is no equivalent here.
- **A real typed vocabulary exists**: 14 opaque types, 32 `enum`s, error ADTs
  with `def message`.
- **Closed sets are already parsed into enums at the boundary.** `BaselineModel`
  turns `"constant" | "poly" | "bs" | "ns"` into `BaselineBasis`, `"warn" |
  "error" | "drop" | "none"` into `NuisanceCheck`. That is parse-don't-validate
  done correctly and should be left alone.
- **`buildEither` is genuinely total** — it does not escape through an exception.
- **The formula already has a typed core.** `ModelFormula` is a public,
  directly constructible AST and `EventModelBuilder.buildEither` accepts it
  without any string. `FormulaParser` is one front-end over it, not the only
  door.

So the work is not "introduce types". It is that in four places the types that
exist are **not load-bearing** — constructed then bypassed, duplicated, or
discarded before they can do any work.

## 2. Blast radius (measured)

This is what makes the sequencing cheap.

| Surface | Consumers outside its own file | Cost to change |
|---|---|---|
| `columnById` and its 5 siblings | **1**, and it is a test (`TypedCoreSuite.scala:77`) | Free |
| Throwing `DataTable` accessors (`doubles`/`ints`/…) | **3 call sites in `design` main**, all in `hrf/HrfGenerators.scala` | Cheap |
| Formula AST (`ModelFormula`/`ArgValue`/`HrfCall`) | **1 downstream main file** — `fmri-workflow/AnalysisSpec.scala`, 4 sites | Cheap |
| String-literal formulas in non-test code | **0** | n/a |
| `DesignError` | referenced by `model/ModelError.scala` | Additive changes only |

Nothing here is load-bearing across the repo. The `.doubles(...)` calls in
`motion` and `model` are on unrelated fixture objects, not on `DataTable`.

## 3. Findings

### 3.1 A validated `ColumnId` can fail to name its own column (live bug)

`ColumnId.apply` routes through `validateDesignId`, which returns
`Names.sanitize(trimmed, allowDot)`. `sanitize` exists to make *generated
output* names R-safe (a `make.names` equivalent). Applying it to a *lookup key*
rewrites the key:

```
ColumnId("my col").value             == "my.col"     // not the input
table.contains("my col")             == true
table.columnEither("my col")         == Right(...)   // untyped path finds it
table.columnById(ColumnId("my col")) == Left(...)    // typed path misses it
```

The typed, validated path fails exactly where the untyped one succeeds. And the
map is **not injective**:

```
ColumnId("a.b").value == ColumnId("a b").value == "a.b"
```

so two distinct columns collapse to one id — an identifier that cannot
distinguish the things it identifies. The same conflation applies to `EventId`,
`ConditionId`, `FactorId` and `TermId` (`TermId("a.b").value == "a_b"`).

**Diagnosis.** One function does two jobs: *parsing* an identifier (must be
total on valid input, injective, and rejecting) and *formatting* a name for
output (lossy by design). Parsing must preserve. Formatting belongs in `Names`,
where column names are generated.

Pinned by `IdRoundTripProbeSuite`.

### 3.2 Structured `DesignError` cases are discarded on the build path

`buildEither` attains totality by catching `NonFatal` and stringifying:
`catchBuild(DesignError.fromThrowable)` yields `BuildFailed(t.getMessage)`.
Where the failing step called a *throwing* accessor instead of its `*Either`
twin, the purpose-built case is thrown away.

Fourteen realistic user mistakes, by the case actually produced:

| mistake | case |
|---|---|
| unknown column | **`BuildFailed`** |
| unknown basis call | **`BuildFailed`** |
| list column used as factor | **`BuildFailed`** |
| HRF column used as factor | **`BuildFailed`** |
| unknown basis | `UnknownBasis` |
| unknown contrast set | `UnknownContrast` |
| bad subset type | `InvalidSubset` |
| 7 formula-shape mistakes | `FormulaParse` |

**4 of 14 collapse to the stringly catch-all.** The first is sharpest:
`DesignError.MissingColumn(name)` exists for precisely that mistake and is never
reached, because `DataTable.column(name)` throws and the throw is caught and
flattened.

`MissingColumn` and `InvalidColumnType` *are* covered by tests — but only by
calling `columnEither` directly in isolation, never through the entry point. The
tests pass while the real path degrades. Same shape of blind spot as the `hrf`
per-event dispatch test.

Pinned by `ErrorFidelityProbeSuite`.

### 3.3 Forty-four `*Either` twins

Every fallible operation is written twice — once returning
`Either[DesignError, A]`, once throwing. There are **44** distinct `*Either`
methods.

`DataTable` shows the cost most clearly: six column types × three accessors
(`doubles` / `doublesEither` / `doublesById`) = **18 methods, ~90 lines, for one
concept**. The `*ById` tier merely unwraps to the `String` tier; the throwing
tier is what production calls.

This is the two-tier constructor convention from `AGENTS.md` applied
mechanically to every *operation* rather than to *construction*. It is also the
mechanism behind §3.2: while a throwing accessor is in scope, a build path can
pick it up by accident and lose the structured error.

### 3.4 The formula AST is typed in shape but stringly in its references

`ModelFormula` is a real ADT, but its own fields keep column and term
references as bare `String`:

```scala
final case class ModelFormula(onset: String, terms: Vector[TermCall])
ArgValue.Ident(value: String)                  // a column reference
HrfCall(id: Option[String], prefix: Option[String], …)   // term identifiers
```

`ArgValue.Ident("cond")` is where a column name stays untyped from parse all the
way to the runtime lookup — and that is precisely the path that produces
`BuildFailed` instead of `MissingColumn`. §3.2 is not a separate defect so much
as the visible symptom of this one.

`basis: Option[String]` is *not* in scope here: it keys the user-extensible
`BasisRegistry`, so an open set is correct — as is `basisClass: String`.

### 3.5 Ten near-identical opaque id types

`EventId`, `ConditionId`, `FactorId`, `TermId`, `ColumnId` are five
character-for-character copies of the same `opaque type X = String` +
`apply`/`unsafe`/`value` block, differing only in a `kind` label and an
`allowDot` flag. `DesignColumnIndex`, `ScanIndex`, `RunIndex`, `BasisIndex`,
`TermIndex` are five copies of the same `opaque type X = Int` block.

That is ~130 of `DesignTypes.scala`'s 190 lines. The *distinctions* are worth
keeping; the *definitions* need not be repeated.

## 4. Plan

The phases follow a causal chain, not a priority list: each one removes the
precondition for the next. Every phase must compile and pass on **both** JVM and
JS before the next starts, per `AGENTS.md`.

### Phase 0 — Characterization *(done)*

`IdRoundTripProbeSuite` and `ErrorFidelityProbeSuite` pin current behaviour.
`design` is at 153 tests green on both platforms with these included.

### Phase 1 — Make identifiers identify *(fixes §3.1)* — **done**

Split parsing from formatting. `ColumnId.apply` validates — non-empty, rejects
what cannot be an identifier — and **preserves** its input. Name sanitization
moves to where output column names are generated, in `Names`.

Add a round-trip law: for every accepted `s`, `Id(s).map(_.value) == Right(s)`.

*Must come first.* Phase 3 types the formula's references as `ColumnId`/`TermId`;
doing that before this fix would push the silent-rename bug into the formula
path, where `TermId("a.b")` would rewrite a user's term name to `a_b`.

**Done when:** `IdRoundTripProbeSuite`'s three failing-behaviour assertions flip,
and the round-trip law holds for all five id types.

**Landed as:** `validateDesignId` validates and returns its input unchanged.
It rejects rather than rewrites — blank, embedded control characters, and
leading/trailing whitespace are `InvalidId`, because a parser that trims is not
injective either (`" x "` and `"x"` would share an id). `allowDot` is gone: it
was a formatting flag, not a parsing one.

Two call sites did depend on the old rewrite, and both now say so out loud.
`CellSelector.factor` and `ContrastExpr.oneway`/`interaction` match a
[[FactorId]] against `Event.factor`'s `varName`, which is `Names.sanitize`d;
they normalize into that vocabulary through one `designFactorId` helper. The
lossy step is visible at the call site that needs it instead of hidden inside
`FactorId.apply`.

### Phase 2 — One accessor per concept *(fixes §3.3)* — **done**

Replace `DataTable`'s 18 accessors with a total, typeclass-driven one:

```scala
trait ColumnType[A]:
  def typeName: String
  def extract(column: Column): Option[Vector[A]]

def get[A](name: ColumnId)(using ColumnType[A]): Either[DesignError, Vector[A]]
```

`table.get[Double](id)` subsumes `doubles`/`doublesEither`/`doublesById`, and the
`Ints`-widen-to-`Doubles` rule lives in one instance instead of two methods.

Deleting the throwing accessors is the point: it makes §3.2 *unrepeatable*
rather than fixed once. Only 3 main call sites move, all in `HrfGenerators`.

Keep a throwing facade at top-level entry points only — which is what
`AGENTS.md` actually asks for.

**Done when:** `DataTable` exposes one accessor per concept, no throwing column
accessor remains, and `designJVM`/`designJS` are green.

**Landed as:** `DataTable` went from 19 accessors to four — `names`, `contains`,
`column`, and `get[A]` — with `ColumnType` instances carrying the per-type
`typeName` and extraction, including the `Ints`-widen-to-`Doubles` rule. Lookups
take a `ColumnId`, so a caller has to have parsed a name before it can ask.

Five throwing twins that nothing called (`resolveSeconds`,
`resolveNumericVector`, `resolveHrf`, `resolveHrfBasis`,
`raiseStrictDiagnostics`, `TableEnv.resolve`) went with them. Across `design`
main, `*Either` methods dropped from 44 to 36; the remainder are on paths whose
throwing halves were already gone.

`CovariateSpec.construct` became total in the process, and `GroupDesign`'s
covariate reader now distinguishes a missing column from a non-numeric one with
one lookup instead of two passes.

### Phase 3 — Type the formula's references, recover error fidelity *(fixes §3.4, §3.2)* — **done**

Turn the AST's references into the ids they denote:

```scala
ModelFormula(onset: ColumnId, terms: Vector[TermCall])
ArgValue.Ident(value: ColumnId)
HrfCall(id: Option[TermId], prefix: Option[TermId], …)
```

The parser becomes the boundary that lifts text into ids, and the build path
then carries a `ColumnId` into `table.get[A]`, which returns `MissingColumn` or
`InvalidColumnType` structurally. `catchBuild` stays as a backstop for genuinely
unexpected failures, so `BuildFailed` becomes rare rather than routine.

Touches one downstream main file (`fmri-workflow/AnalysisSpec.scala`, 4 sites).

**Done when:** no row in `ErrorFidelityProbeSuite`'s table reports `BuildFailed`,
and that suite's expectations are rewritten to assert the structured cases.

**Landed as:** the AST carries ids (`ModelFormula(onset: ColumnId)`,
`ArgValue.Ident(ColumnId)`, `id`/`prefix`/`label` as `TermId`), and the
tokenizer's own identifier rules are what establish them — `isIdentStart` /
`isIdentPart` already guarantee everything `ColumnId` requires, so the lift is
`ColumnId.unsafe` at exactly one place in `parsePrimary`. A `TermId` from a
string literal *can* fail, so `TermArgs.termId` reports it as a `ParseError`
with the offset the rest of the parser uses.

`contrasts` was also retyped, from `Option[ArgValue]` to `Option[String]` — the
same reasoning §3.4 applies to `basis`: it keys a user-extensible registry. That
was the one identifier position where `ColumnId` would have been a lie, and it
deleted `contrastSetKeyEither` and an unreachable `FormulaBinding` branch.

The build path's interior is now total: `toEvent`, `evalBasisCall`,
`parseBlockIds`, `toFactorStrings`, `requireIdentArg`/`requireIntArg`,
`inferTermTag`, and the `hrf_fun` resolver all return `Either`. Two cases were
added to `DesignError` for mistakes that had no home: `UnknownBasisFunction`
(with the calls the grammar knows) and `DegenerateBasis`.

One throwing lookup survives on purpose. `evalSubset` is a recursive evaluator
with a single error protocol — throw, and let `resolveSubsetMaskEither` name the
failure `InvalidSubset` — so `subsetColumn` is a deliberate local convention
landing in a structured case, not an accidental twin.

`ModelRecipe`'s `require(formula.onset.trim.nonEmpty, …)` and its `make`
counterpart are gone: with `onset: ColumnId` that is a property of the type.

`ErrorFidelityProbeSuite` now asserts the exact case for each mistake and, at
the end, walks 17 realistic formulas asserting that none reports `BuildFailed`.

### Phase 4 — Collapse the id boilerplate *(fixes §3.5)* — **done**

Two tagged newtypes replace ten hand-written ones:

```scala
opaque type DesignId[Tag] = String
opaque type OneBasedIndex[Tag] = Int
```

`DesignId[Event]` and `DesignId[Term]` remain distinct types. Purely mechanical,
and safe only once Phase 1 has settled what construction means — doing it
earlier would entrench sanitize-on-parse in a shared definition.

**Landed as:** two opaque types, two `sealed abstract` companions
(`IdCompanion[Tag](kind)` / `IndexCompanion[Tag](kind)`), and phantom tags in
`IdTag` / `IndexTag`. Each concrete type is now two lines:

```scala
type ColumnId = DesignId[IdTag.Column]
object ColumnId extends IdCompanion[IdTag.Column]("column")
```

The estimate of ~30 lines was optimistic: the 129 lines of *repetition* became
20, but the shared definitions, tags, and docs they were replaced by are ~70
lines, so `DesignTypes.scala` went 190 → 174 overall while also gaining two
error cases and the parsing contract in prose. The property that mattered — ten
copies of one definition became one definition used ten times — holds.

### Phase 5 — Optional: `col"…"` / `term"…"` interpolators — *not scheduled*

A small inline/macro validation of a single identifier at compile time, so a
directly built formula reads well:

```scala
Formula(onset = col"onset") + Hrf(col"cond", basis = "spmg3").id(term"task")
```

It needs no parser at compile time, so it requires **no module split**, and it
composes with the direct-AST path that already exists. Strictly ergonomic —
schedule only if the typed builder gets real use.

Not scheduled. The condition has not changed: direct AST construction is still
confined to tests, which read fine with `ColumnId.unsafe` behind a local
one-line helper.

## 5. Decisions

### No formula macro

Considered and declined. The reasoning, since it is not obvious:

- **The typed path already exists.** `ModelFormula` is public and
  `buildEither` takes it directly. A macro would add a *third* front-end
  duplicating `FormulaParser`, not remove a string.
- **It would improve the errors that are already good.** All 7 formula-shape
  mistakes already return `FormulaParse(detail, pos)` with a character offset.
  Moving that to compile time is a real but incremental gain.
- **It cannot touch the errors that are bad.** The 4 `BuildFailed` cases are
  *semantic* — unknown column, wrong column shape — and depend on a `DataTable`
  that is not known at compile time.
- **There is nothing to check yet.** Zero string-literal formulas exist outside
  tests, so today the beneficiary would be the test suite.
- **Cost is non-trivial:** `FormulaParser` would have to move to a module
  compiled before its use site, plus a macro module, on a cross-compiled JVM/JS
  build that currently contains no macros.

Revisit if a user-facing scripting layer appears and literal formulas become
common; the parser is self-contained, so the split stays cheap.

### No schema-typed `DataTable`

Scala 3.7 named tuples would allow a compile-time schema, which *would* catch
the semantic errors a macro cannot. Declined because it collides with the data
source: `bids4s` is wired in, and a BIDS `events.tsv` is read at **runtime**.
A compile-time schema would need an unchecked escape hatch for the dominant
case, taxing every user for a guarantee the common path cannot have.

### Not proposed

- **Re-typing the formula input.** A formula is user-supplied text; `String` is
  the correct input type for the parsing front-end.
- **Replacing `basisClass` / `basis` strings.** They key a user-extensible
  registry, so an open set is right.
- **Touching `BaselineModel`'s string parsers.** They are the pattern the rest
  of the module should follow, not a defect.
- **A `design-laws` module.** `hrf` earned one because it has algebraic
  structure (causality, additivity, translation equivariance). `design` is
  compilation and bookkeeping; laws here would be ceremony. Revisit if contrast
  algebra grows.

---

## 6. Result

`design` is at **157 tests green on both JVM and JS** (from 153), `compileAll`
is warning-clean, and `testAll` passes across every module and both platforms.

What changed about the four findings:

| finding | before | after |
|---|---|---|
| §3.1 id round-trip | `ColumnId("my col").value == "my.col"` | ids return their input or reject it |
| §3.2 error fidelity | 4 of 14 mistakes → `BuildFailed` | 0 of 17 → `BuildFailed` |
| §3.3 `*Either` twins | 44, `DataTable` 19 accessors | 36, `DataTable` 4 accessors |
| §3.5 id boilerplate | 10 hand-written copies | 2 definitions, 10 two-line uses |
