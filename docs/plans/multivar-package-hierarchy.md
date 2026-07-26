# Multivar package hierarchy

## Decision

`multivar` is one module and one lifecycle, but it is not one namespace. Its
packages expose the order in which meaning is acquired:

```text
core
  -> contract
  -> optimization
  -> solver
  -> lifecycle
  -> capability
  -> family.*
  -> workflow
  -> validation
```

This is an ownership order, not a second runtime architecture. Values move
through it without being copied into parallel models: a family declares an
`OperatorProgram`, a solver executes that program, lifecycle binds the
declaration to its evidence and fitted payload, capabilities expose lawful
post-fit operations, and `ModelSpec` composes the same artifacts fold-safely.
`multivar-ir` serializes those identities and transitions; it does not invent a
second vocabulary.

## Package ownership

| Package | Owns | May depend on |
| --- | --- | --- |
| `core` | semantic spaces, primal/dual orientation, operators, matrix views, metrics, preprocessing, row geometry, primitive solver capabilities | Gale/linalg only |
| `contract` | model families, estimands, mathematical contracts, formula bindings, symmetry vocabulary | `core` |
| `optimization` | declarative programs, functionals, penalties, constraints, parameterizations, requested claims | `core`, `contract` |
| `solver` | executable lowerings, compiler decisions, numerical algorithms, traces, convergence and achieved-guarantee evidence | `core`, `contract`, `optimization` |
| `lifecycle` | family-indexed declaration, compilation, execution and fitted-result binding | `core`, `contract`, `optimization` |
| `capability` | family-neutral fitted transforms, projection, synthesis and supplementary-data operations | `core` |
| `family.<name>` | one statistical vertical: its domain types, programs, adapters, receipts and family-specific fitted capabilities | lower generic layers and explicitly named family substrates |
| `workflow` | `ModelSpec`, fold-safe fitting, executable plans and cross-family pipelines | all lower packages |
| `validation` | oracle matrices, recovery designs and claim admission | all executable packages |

`core.numerics` is a source-directory subdivision of package `core`. It keeps
Gale carrier adapters and portable solver capability values physically apart
from the semantic algebra without creating another public layer.

## Family dependency law

Family packages are verticals, not a peer-to-peer soup. `spectral` is the
shared exact-decomposition substrate. Paired, canonical, CPCA, sparse and
kernel families may build on it. Multiblock may build on both spectral and
GLRM. The reverse edges are forbidden.

Consequently:

- PLSC, CCA, reduced-rank regression and paired transfer live in
  `family.paired`, not in generic capability or spectral files.
- exact direct-sum execution lives in `family.multiblock` as
  `ExactMultiblockPrograms`; `family.spectral` knows nothing about direct sums.
- GLRM and structured multiblock receipts live beside their family programs,
  while the generic family-indexed lifecycle remains family-neutral.
- shared mathematical notions such as `Ridge`, `FrameSymmetry`, sparsity charts
  and `OperatorLinearMap` live at the lowest layer that can state them without
  depending upward.

Cross-family composition must name its owner. If a new method requires two
families, place the adapter in the more specific family or in `workflow`; do
not add reciprocal imports.

## Source layout law

Production and test paths mirror public packages beneath
`scalafim/multivar/`. No production source may return to the flat package root.
Large files may remain large only when they implement one closed algebra; a
new independently nameable concept, receipt, compiler, or family adapter earns
its own file at its semantic owner.

The compile-time `PackageHierarchySuite` protects representative ownership
boundaries and rejects the former flat namespace. JVM and Scala.js compilation
protect the same hierarchy because all packages remain in `shared`.

## Import and compatibility policy

Consumers import semantic owners explicitly, for example:

```scala
import scalafim.multivar.core.{MatrixView, ComponentCount}
import scalafim.multivar.family.paired.Cca
import scalafim.multivar.workflow.ModelSpec
```

Scala 3.4 cannot wildcard-export a package from a package object. Recreating
the old flat surface would therefore require a hand-maintained alias for every
public member, duplicating the very namespace this hierarchy removes. This
pre-release migration deliberately updates repository consumers instead. Add
a root compatibility alias only for a demonstrated external migration need,
and only when it does not obscure ownership.

## Extension test

Before adding a public multivar type, answer in order:

1. Is it representation-free semantic algebra? Put it in `core`.
2. Does it state truth without executing? Put it in `contract` or
   `optimization`.
3. Does it lower, iterate, stop, or certify a computation? Put it in `solver`.
4. Does it bind declared, compiled, executed and fitted stages? Put it in
   `lifecycle`.
5. Is it a family-neutral operation on a fitted artifact? Put it in
   `capability`.
6. Does it name a statistical estimand or family-specific fit? Put the whole
   vertical in `family.<name>`.
7. Does it coordinate folds, policies or several families? Put it in
   `workflow`.

If two answers seem equally plausible, the dependency direction decides: the
type belongs at the lowest truthful layer that does not import upward.
