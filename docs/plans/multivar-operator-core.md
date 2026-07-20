# Multivar operator core — single-layer target architecture

Status: **target** (not yet implemented). Supersedes the *dual-layer* arrangement
described in [`multivar-duality-constitution.md`](multivar-duality-constitution.md).
The constitution's twelve invariants remain binding; this document adds the
structural collapse that makes them hold in *one* layer instead of two, and
specifies the migration. When the migration completes, this document folds back
into the constitution as its implemented state.

This is the committed design. The purpose of writing it before touching code is
to stop the sequence of partial refactors: every change below is measured against
a fixed target and a fixed set of invariants, not re-litigated per step.

---

## 1. Why change now

The basis is correct and stays: a duality diagram is executable semantics, not
decoration, and methods are compilers over a typed operator algebra — not a
grab-bag of estimators. That decision was reached deliberately (the
collection-of-algorithms and covariance-library alternatives were rejected) and
is not reopened here.

What is *not* cohesive is the **layering**. `multivar` currently carries two
parallel representations of the same mathematics:

- the **semantic operator layer** — `Lin` / `Table` / `Coordinate` /
  `GeometricOperator` / `SemanticDualityDiagram` — the conceptual basis; and
- a **legacy numeric mirror** — `MvMetric` / `DualityDiagram` / `MvMap` /
  the raw `GenPca` engine — which is what actually computes.

`SemanticGenPca.fit` prepares the typed diagram and then drops to `legacyDiagram`
+ `GenPca.fit`. The typed layer is a façade over an engine that speaks a
different vocabulary, and on top of that seam sits a zoo of per-method estimator
types (six named GenPCA result records; `PairedGmd` as a private third engine).

None of that is a wrong idea — it is the residue of building the right basis over
an older one without being permitted to delete the older one. Because the library
is new and has no external consumers, we remove that constraint and take the one
move a behavior-preserving refactor structurally cannot: **collapse to a single
layer and delete the legacy compute path.** Cohesion comes from that deletion.

---

## 2. The target, in one paragraph

There is one operator type. Every metric, cometric, covariance, scatter, penalty,
kernel, row link, table, and latent frame is that operator refined by *role* and
*evidence*, backed by an `OperatorRepresentation`, and the numeric engine computes
directly on it. Second-order feature operators come into existence only through
`secondOrder` (the `Xˢ · L · X` pullback); component-space operators only through
`compress` (the `Wˢ · S · W` reduction). The latent parameter is one
`FunctionalFrame`; scores and axes are derived from it, never stored as
independent truth. The base objective is a closed `enum`. Methods (`gpca`, `lda`,
`cca`, `plsc`, `rrr`, `multiset`) are named constructors that assemble spaces, a
row link, a normalization, and an objective into a typed problem, and each flows
through `secondOrder → compress → solver`. Bells and whistles — sparsity,
smoothness, shrinkage, low rank, nonnegativity, cross-view agreement — are never
new methods: they are structural *terms* (a functional or a feasible set pulled
back through a typed target expression) and upstream *operator policies*, kept as
distinct first-class concepts rather than collapsed into one "transformation".

---

## 3. The one operator type

Collapse `MvMetric`, `GeometricOperator`/`Form`, and `MvMap`/`Lin` into a single
directed operator. It carries the numeric kernel *inside* it — there is no second
diagram the engine secretly runs on.

```scala
final class Op[From <: Coordinate, To <: Coordinate, R <: Role] private[multivar] (
    private[multivar] val kernel: OperatorKernel,   // the ONLY numeric substrate
    val domain:   CoordinateEvidence[From],
    val codomain: CoordinateEvidence[To],
    val role:     R,                                 // phantom tag + runtime value
    val evidence: Evidence,                          // runtime-authoritative certificate
    val valueIdentity: ValueIdentity,
    val provenance:    SemanticProvenance):
  def andThen[Next <: Coordinate](next: Op[To, Next, ?]): Op[From, Next, ComposedRole]
  def star: Op[DualOf[To], DualOf[From], AdjointRole[R]]
```

- `OperatorKernel` is today's `SemanticKernel`, tagged with
  `OperatorRepresentation` (`Dense | Sparse | Diagonal | Block | LowRank |
  Kronecker | LazyAffine | MatrixFree`). Composition preserves structure where it
  can (block stays block) and degrades to `MatrixFree` otherwise. **This is the
  substrate the engine computes on. `MvMetric`/`MvMap` are deleted.**
- **Role** is a phantom type tag *and* a runtime value: `Table`, `Metric`,
  `Cometric`, `Covariance`, `Scatter`, `Penalty`, `Kernel`, `RowLink`, `Frame`,
  `Cross`, `Component`. Role advertises meaning; it is static because it is known
  at construction.
- **Evidence is not a phantom claim.** A fact like "SPD, verified at residual
  1e-9, this backend" is established at runtime by a numerical test and cannot be
  honestly lifted into a type. We keep the balance the code already found: a
  type-level *evidence-lattice tag* (`CertifiedSpd <: CertifiedPsd <:
  CertifiedSymmetric`, plus `Assumed*`) where it helps inference, with the
  authoritative claim a `Certificate` value bound to `valueIdentity`. `Unsafe.*`
  (with a mandatory reason) remains the only way to assert evidence not
  established.

Role-oriented aliases (orientations follow the constitution's `C —R→ C* —X→ O`
spine):

```scala
type Table[Rows <: SemanticSpace, Cols <: SemanticSpace]  = Op[Dual[Cols], Primal[Rows], TableRole]        // X : C* → O
type Metric[S <: SemanticSpace]                           = Op[Primal[S], Dual[S], MetricRole]             // R : C  → C*   (SPD)
type Cometric[S <: SemanticSpace]                         = Op[Dual[S], Primal[S], CometricRole]           // Q : C* → C
type Covariance[S <: SemanticSpace]                       = Op[Dual[S], Primal[S], CovarianceRole]         // S : C* → C   (PSD)
type Scatter[S <: SemanticSpace]                          = Op[Dual[S], Primal[S], ScatterRole]            // between/within
type RowLink[Os <: SemanticSpace, Ot <: SemanticSpace]    = Op[Primal[Ot], Dual[Os], RowLinkRole]          // L : O_t → O_s*
type Frame[Feat <: SemanticSpace, Comp <: SemanticSpace]  = Op[Primal[Comp], Dual[Feat], FrameRole]        // W : K  → C*
```

Metric, covariance, scatter, and penalty share this representation and differ only
by role, orientation, and evidence — they are never distinguished by their backing
array (constitutional invariant 5), and they are never merged into one *validated*
form even when the arrays coincide.

---

## 4. The two primitives

The whole point is that these are **not new machinery** — they are named,
role-stamping compositions over the `andThen`/`star` algebra that `Lin` already
has. The novelty is naming them, computing them without dropping to a legacy
layer, and attaching the correct result role and evidence.

### 4.1 `secondOrder` — the pullback `S_st = Xˢ · L · Xt`

```scala
def secondOrder[Os, Ot, Cs, Ct](
    xs: Table[Os, Cs],       // Dual[Cs] → Primal[Os]
    l:  RowLink[Os, Ot],     // Primal[Ot] → Dual[Os]
    xt: Table[Ot, Ct],       // Dual[Ct] → Primal[Ot]
): Op[Dual[Ct], Primal[Cs], CrossRole] =        // S_st : C_t* → C_s
  xt.andThen(l).andThen(xs.star)
```

Type-checked: `xt.andThen(l) : Dual[Ct] → Dual[Os]`; `xs.star : Dual[Os] →
Primal[Cs]`; composed `Dual[Ct] → Primal[Cs]`. One operation generates covariance
(`s = t`, `L = A`), scatter, cross-covariance, class between/within scatter,
co-inertia, partially matched cross-view operators, and multiset blocks — the
difference is entirely in `L`, which is supplied by the existing
`RowRelationships` ADT (`TypedRowLink`, `Coupling`, `IncidenceMap`,
`PartialInjection`, `HubAlignment`). No method hand-rolls `Xˢ L X` again.

Partial matching, class incidence, and hub alignment are just constructions of
`L` (`L = Pˢ A_E P` for entity hub `E`; `L = A P_B`/`A P_W` for classes). Nothing
downstream knows or cares whether the match was complete.

### 4.2 `compress` — the reduction `G_st = Wˢ · S · Wt`

```scala
def compress[Cs, Ct, Ks, Kt](
    ws: Frame[Cs, Ks],                              // Primal[Ks] → Dual[Cs]
    s:  Op[Dual[Ct], Primal[Cs], CrossRole],
    wt: Frame[Ct, Kt],                              // Primal[Kt] → Dual[Ct]
): Op[Primal[Kt], Dual[Ks], ComponentRole] =        // G_st : K_t → K_s*
  wt.andThen(s).andThen(ws.star)
```

Every objective is a scalar functional of these small component-space operators.

---

## 5. `FunctionalFrame` — the sole latent parameter

The primary fitted object is a frame of feature functionals `W : K → C*`. Scores
and axes are **derived accessors**, not stored records. This deletes the six-name
GenPCA result zoo (`StandardRowScores`, `PrincipalRowScores`, `ColumnAxes`,
`ColumnMetricLoadings`, `RowMetricLoadings`, `RowDualPrincipalScores`) in favor of
one object with named views.

```scala
final case class FunctionalFrame[Feat <: SemanticSpace, Comp <: SemanticSpace] private (
    w:        Frame[Feat, Comp],            // W : K → C*   (the primary parameter)
    cometric: Option[Cometric[Feat]]):      // Q = R⁻¹, needed to derive feature axes
  def scores[O <: SemanticSpace](x: Table[O, Feat]): Op[Primal[Comp], Primal[O], ScoreRole] =
    w.andThen(x)                            // T = X W : K → O
  def axes: Option[Op[Primal[Comp], Primal[Feat], AxisRole]] =
    cometric.map(q => w.andThen(q))         // V = Q W : K → C
```

This resolves the weights/loadings/axes terminology drift: `W` is the scoring
functional, `V = QW` the feature axis, `T = XW` the score, all from one fit.

---

## 6. The internal program — variables, parameterization, objective, structural terms

Methods lower to a single internal problem value. The pipeline is:

```
estimated operators  →  operator policies  →  free variable z
   →  parameterization P  →  semantic frame θ = P(z)
   →  base objective f(θ)  +  Σ penalties  s.t.  constraints
   →  solver lowering  →  certified result
```

Five concepts stay **distinct and first-class** — this is the one place the
"everything is a Transformation" collapse is explicitly refused, because the
objects compose but do not share semantics. Covariance shrinkage changes a
*statistical estimate*; a parameterization changes the *optimization domain*; a
chart *exposes* structure; an ℓ₁ norm expresses a *preference*; a feasible set
defines *feasibility*; a prox/projection is a *solver operation*.

```scala
final case class OperatorProblem private (
    context:        SemanticDualityDiagram[?, ?, ?],  // or a direct-sum study
    policies:       Vector[OperatorPolicy],           // ⑤ upstream: shrink/repair/restrict/gauge
    freeVariable:   FrameVariable,                    // z — the actual optimization coordinate
    parameterization: Parameterization,               // P : z ↦ θ  (identity for plain methods)
    objective:      Objective,                         // ① CLOSED enum base objective f — see below
    normalization:  Vector[Normalization],             // W_s' N_s W_s = I
    penalties:      Vector[PenaltyTerm],               // ② λ φ(T(θ)) — functional on a typed target
    constraints:    Vector[ConstraintTerm],            // ③ T(θ) ∈ C  — feasible set on a typed target
    resultContract: ResultSemantics)                   // inferred from the whole program (§6.4)
```

### 6.1 The base objective is a closed `enum`

```scala
enum Objective:
  case MaximizeTrace(g: ComponentOp)                            // GPCA, association
  case MaximizeCrossTrace(g: CrossComponentOp)                  // CCA, PLS-SVD, PLSC
  case GeneralizedRayleigh(num: ComponentOp, den: ComponentOp) // Fisher LDA
  case TraceRatio(num: ComponentOp, den: ComponentOp)          // trace-ratio LDA
  case MinimizeDisagreement(g: ComponentOp)                    // pure multiset alignment
```

**There is no free-form `Expr[Scalar]` *objective* at any boundary.** A closed
enum keeps exhaustive matches, typed result semantics, and parity-anchorability;
an open objective DSL would re-introduce the incoherence we are removing. New base
objectives (GCCA, co-inertia, ratio-trace) are added as enum cases *only when they
compile to the core exactly*. Note the distinction from §6.3: the *objective* `f`
is closed; the *structural terms* bolted onto it are an open-by-composition but
still-typed layer. A "sparse smooth partially-aligned CCA" is not a new method —
it is the `MaximizeCrossTrace` objective plus a pile of penalty/constraint terms.

### 6.2 Parameterization is distinct from regularization

`z` is the free optimization variable; `θ = P(z)` is the semantic parameter (a
`FunctionalFrame`). For plain methods `P = identity`. Nontrivial `P` **reduces the
domain** and must carry metadata a plain map does not: injectivity, surjectivity
onto the intended image, redundant coordinates, gauge symmetry, differential, and
whether a solution lifts/inverts uniquely.

```scala
enum Parameterization:
  case Identity
  case KnownSupport(embed: LinearMap[?, ?])          // W = E_S z
  case FixedRank(u: LinearMap[?, ?])                 // B = U V*  (many-to-one; record the gauge)
  case BlockDiagonal(blocks: Vector[Parameterization])
  case NullSpace(basis: LinearMap[?, ?], rankTol: Tolerance)  // Cw = 0 ⇒ w = N z
```

Exact known structure is parameterized (domain-reducing), **not** approximated by
a large penalty — that is a different scientific claim and a different geometry.

### 6.3 Structural terms — the pullback of a functional or set through a typed target

The single reusable mechanism for every "bell and whistle" is: pick a **typed
target map** that says *where to inspect* the parameter, then attach a
**functional** (soft preference) or a **feasible set** (hard requirement) that
says *what property* to prefer or require there.

```scala
def penalize[Z](target: Expr[Z], functional: Functional[Z], weight: Weight): PenaltyTerm
def constrain[Z](target: Expr[Z], to: FeasibleSet[Z]): ConstraintTerm
```

Three sub-layers, each a closed catalog of *capability-advertising* primitives —
openness is compositional, never syntactic:

- **Typed maps** carry a capability tag so lowering is deterministic:
  `LinearMap` (has an algebraic dual `T★`), `AffineMap`, `SmoothMap` (has
  JVP/VJP), `GeneralMap` (evaluation only). Only linear targets admit the
  quadratic pullback and the null-space rewrites. Primitive maps — feature chart
  `J`, graph incidence `D`, difference/derivative operators, score map `X`,
  group-extraction `E_g`, direct-sum projections, and the row links from
  `RowRelationships` — compose into an `Expr` graph; multi-input terms (e.g.
  aligned-score differences `P_sX_sW_s − P_tX_tW_t`) use product objects.
- **Functionals** `φ : Z → ℝ̄`: squared-norm, ℓ₁, group ℓ₂,₁, elastic-net, Huber,
  total variation, nuclear, log-det, indicator. Each advertises convexity,
  smoothness, separability, spectral structure, **prox availability**, conic
  representability. *The prox belongs to the functional, not to the target map.*
- **Feasible sets** `C`: zero/affine subspace, nonnegative orthant, simplex, box,
  norm ball, PSD cone, Stiefel manifold, fixed-support, cardinality-/rank-bounded.
  Each advertises convexity, closedness, **projection availability**, conic/manifold
  structure. *Projection belongs to the set, not to the map.*

Two rules make this honest rather than merely elegant:

- **No coordinate property without a chart; no quadratic norm without geometry.**
  Sparsity/nonnegativity/monotonicity/groups are chart-dependent — they are
  properties of `JW`, never of the abstract `W`. A quadratic penalty needs an
  explicit form `G`: `½‖Tθ‖²_G`.
- **A simple `prox_φ` does NOT imply a simple `prox_{φ∘T}`.** For linear `T`, the
  quadratic case pulls back exactly (`½‖Tθ‖²_G = ½⟨θ, T★GT θ⟩`, a canonical
  rewrite that carries equivalence evidence and can absorb into an eigen/normalization
  operator). The nonsmooth case does **not**: the compiler introduces an auxiliary
  `z = Tθ` and splits (ADMM / primal-dual), rather than misapplying `prox_φ` to
  `Tθ`. This is the load-bearing correctness guarantee of the whole layer.

### 6.4 Invariance and result semantics are inferred from the whole program

Invariance is a property of complete terms `φ∘T` and of the whole program, never
asserted on a node. `‖JW‖₂,₁` is right-orthogonally invariant (preserves a
`SparseSubspace`); `‖JW‖₁` is not (selects a particular `SparseFrame`). The result
contract is the intersection of the symmetry groups of the base objective, the
normalization, every penalty, and every constraint — and it decides whether the
answer is a subspace, an unordered/ordered/oriented frame, a signed-permutation
class, a paired frame, or an affine map.

### 6.5 Operator policies are a separate upstream stage

An operator policy modifies an *estimated statistical operator before the program
is built* — it is not parameter ridge. It **subsumes and generalizes** the
diagram's existing support-restriction / ridge / PSD-repair machinery (do not
build a second policy layer beside it) and adds shrinkage.

```scala
trait OperatorPolicy:
  def apply(op: CertifiedOperator): Either[MultivarError, CertifiedOperator]
  // carries: target, strength, scale-matching, joint-vs-blockwise, preserved claims,
  //          gauge/normalization effect; downgrades evidence when a step cannot preserve it.
```

CCA shrinks the **joint** block covariance (preserving block-adjoint, shared
gauge, and joint-PSD evidence); LDA shrinks within-class scatter used as its
normalization. When the strength `α` is *chosen from the data*, the policy stops
being a pure map in the program and moves to `ModelSpec`, refit inside every
training fold (§8, deferred).

---

## 7. Methods are named constructors

```scala
object Gpca:     def problem(d: SemanticDualityDiagram[?,?,?], k: ComponentCount): Either[MultivarError, OperatorProblem]
object Lda:      def problem(x: Table[?,?], classes: ClassDesign, within: Shrinkage, k: ComponentCount): Either[MultivarError, OperatorProblem]
object Cca:      def problem(p: PairedDualityDiagram, reg: CcaRegularization, k: ComponentCount): Either[MultivarError, OperatorProblem]
object Plsc:     def problem(p: PairedDualityDiagram, k: ComponentCount): Either[MultivarError, OperatorProblem]
object Rrr:      def problem(p: PairedDualityDiagram, dir: RegressionDirection, reg: RegressionRegularization, k: ComponentCount): ...
object Multiset: def problem(study: DirectSumStudy, design: BlockDesign, k: ComponentCount): ...
```

Each builds frame variables, a `secondOrder` construction of its operators from an
`L`, a normalization, and an objective — then lowers to a `linalg` solver and
lifts to a typed `Fit` carrying the `FunctionalFrame`, derived views, result
semantics, and certificates. The public identity of a method is its constructor
and its typed `Fit`, not a `MultivarEstimator` string a solver switches on. GPCA
normalizes by observed covariance-or-metric; PLS normalizes by declared feature
geometry — that distinction lives in `normalization`, not in separate engines.

---

## 8. Kept, deleted, deferred

**Kept (assets — do not rewrite):**
- `Coordinate`/`Primal`/`Dual`/`DualOf`, the operator algebra (`andThen`/`star`),
  `ValueIdentity`, `SemanticProvenance`, `OperatorRepresentation`.
- The certificate / evidence-lattice hybrid and the `Unsafe` boundary.
- `SemanticDualityDiagram` with row measure, centering-as-evidenced-projection,
  and `SingularGeometryPolicy`.
- The `RowRelationships` ADT (the `L` vocabulary) — it *feeds* `secondOrder`.
- The `MultisetObjectives` algebra — promoted to the universal objective layer.
- `multivar-ir` wire format and its conformance corpus.
- R parity fixtures (`GenPcaRReferenceFixtures`, `PairedLatentRReferenceFixtures`).

**Deleted (the legacy mirror and the zoo):**
- `MvMetric` as a separate numeric form → folded into `Op` (MetricRole + evidence).
- `DualityDiagram(X,D,Q)` legacy triple → `SemanticDualityDiagram` is the only diagram.
- `MvMap`/`Decoder`/`BiProjection`/`CrossProjection` fitted-map layer → results are `Op`/`FunctionalFrame`.
- `GenPca.fit(DualityDiagram, …)` legacy engine path → GenPCA computes on `Op`.
- `PairedGmd` as a private third engine → paired methods use the same primitives.
- the six GenPCA result records → one `FunctionalFrame` + accessors.
- `Plans.MultivarEstimator` estimator enum → method constructors are the identity.

**Specified now (§6), built incrementally — not in the core phases:**
- the variational/structural-term layer (parameterization, target maps,
  functionals, feasible sets) and its solver lowering — the seam is fixed by §6 so
  the core is designed against it, but families land one at a time (§10, phase 5),
  each with a lowering and a parity fixture. Methods call `linalg` directly until a
  family needs more; the `OperatorRepresentation` vocabulary is ready for the
  eventual compiler/"mills".
- the `ModelSpec` fitting lifecycle (preprocessing / fold-safe alignment /
  data-driven shrinkage and chart/graph selection) — owns every data-dependent
  operation, refit inside training folds; build when learned alignment lands.

**Deferred outright:** Krein/indefinite decompositions, GCCA/co-inertia/ratio-trace
as new objectives, distributed execution, Python/R bindings over the IR.

---

## 9. New structural laws (in addition to the constitution's twelve)

13. **One representation.** Metric, cometric, covariance, scatter, penalty,
    kernel, row link, table, and frame are all `Op` refined by role and evidence,
    computed on one `OperatorKernel`. No second numeric mirror exists.
14. **Second-order only via `secondOrder`; component only via `compress`.** No
    method constructs `Xˢ L X` or `Wˢ S W` by hand.
15. **One latent parameter.** The fitted latent object is a `FunctionalFrame`;
    `T = XW` and `V = QW` are derived, never stored as independent truth.
16. **The base objective is a closed `enum`.** No free-form scalar-expression
    *objective* at any boundary. New objectives are enum cases that compile to the
    core exactly, or they are not added. (Structural *terms*, law 19, are a
    separate, open-by-composition layer.)
17. **Method identity is a typed constructor + `Fit`.** No method-name string or
    estimator enum that a solver dispatches on.
18. **Five distinct variational concepts, never collapsed.** Typed map,
    parameterization, operator policy, functional, and feasible set have different
    semantics and stay first-class. Pure total maps compose as a category; a
    fitting procedure (data-driven shrinkage, learned chart/alignment) is not a
    pure map and enters the graph only after it is fitted and frozen with
    provenance.
19. **Every structural preference is a functional on a typed target; every hard
    condition is set-membership of a typed target.** No `sparse = true` /
    `smooth = true` flags, no unstructured constraint callbacks. No coordinate
    property without a chart; no quadratic norm without an explicit geometry.
20. **Operator shrinkage ≠ parameter ridge.** One changes the estimated
    statistics upstream (an `OperatorPolicy`); the other adds a term to the
    objective. Data-driven selection of either is a `ModelSpec`/fold operation,
    never a pure map in the program.
21. **Composed nonsmooth terms are split, not falsely simplified.** A simple
    `prox_φ` does not imply a simple `prox_{φ∘T}`; the linear-quadratic pullback
    `‖Tθ‖²_G ↦ ⟨θ, T★GT θ⟩` is a canonical rewrite carrying equivalence evidence,
    but nonsmooth composed terms lower via an auxiliary `z = Tθ`. Penalize and
    constrain stay distinct at the public surface even though the solver may treat
    a constraint as an indicator. Invariance and result semantics are inferred
    from the whole program, never asserted on a node.
22. **No orphan capabilities; every family is parity-anchored.** A trait a
    functional or set advertises must have a solver path that consumes it — the
    catalog grows only alongside a lowering, or a beautiful type yields
    `Unresolved` at runtime. Every bells-and-whistles family lands with a fixture
    against its reference (PMA/`sparcl`, `glmnet`, RGCCA, fused-lasso, …), and the
    fit reports its achieved guarantee (globally-certified … stationary … heuristic).

---

## 10. Migration sequence

Invariants that must hold, green on **both** JVM and Scala.js, after *every*
phase: the GenPCA and PairedLatent R parity fixtures pass; the `multivar-ir`
conformance corpus passes; `sbt testAll` is clean; shared `multivar` imports no
Breeze / dataset / image / scheduler / binding runtime.

- **Phase 1 — collapse.** Define `Op` + `secondOrder` + `compress` +
  `FunctionalFrame`. Re-base GenPCA to compute *through* them and **delete** the
  `MvMetric`/`DualityDiagram`/`MvMap`/legacy-`GenPca` compute path. Extend the IR
  with the new operator/frame/objective nodes (additive). Make `FunctionalFrame`
  the *semantic* parameter `θ` so a `Parameterization` (identity for now, §6.2) can
  sit above it later without rework. Gate: GenPCA fixtures + IR corpus. *This is
  the invasive, worth-it step; everything else is downstream.*
- **Phase 2 — LDA (proof).** Add LDA as a new method via `secondOrder` (between /
  within scatter as `Xˢ L_B X` / `Xˢ L_W X`), with a fresh R parity fixture. This
  proves the pullback generalizes on new code before any shipping method is
  touched further.
- **Phase 3 — paired family.** Re-express PLSC / CCA / RRR through the primitives,
  delete `PairedGmd`-as-engine, unify on `FunctionalFrame` (already W-primary).
  Gate: PairedLatent fixtures.
- **Phase 4 — multiset.** Route direct-sum studies through `secondOrder` on block
  operators; the promoted objective layer subsumes `MultisetObjectives`. Gate:
  multiset suites.
- **Phase 5 — the variational layer, one family at a time.** With the core
  collapsed and the seam fixed by §6, add bells and whistles incrementally, each as
  a `PenaltyTerm`/`ConstraintTerm` over a typed target plus its solver lowering and
  a parity fixture against the reference implementation. A sensible order:
  quadratic pullbacks first (ridge, graph smoothness — exact rewrites, eigen/normalization
  absorption), then separable prox (ℓ₁, group ℓ₂,₁ on explicit coordinates), then
  parameterizations (known support, fixed rank, null-space), then split/ADMM
  families (fused lasso / TV, overlapping groups), then operator policies
  (Ledoit–Wolf, joint-block CCA shrinkage). `ModelSpec`, the general solver
  compiler/"mills", and learned alignment follow only when a family or a
  fold-safe selection actually demands them.

When Phase 4 lands, the dual-layer language leaves the constitution and this
document becomes its implemented "operator core" section; Phase 5 accretes under
it without reopening the core.
