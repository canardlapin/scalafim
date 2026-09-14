# Max-null cutoff decision repair

Date: 2026-09-13

Mote: `bd-01M23ZX1KQB38EPD7W3PM56SE9` (Unified MVPA prerequisite X-THRESH)

Source baseline: `e071e83b3a23bc6f304b1e72625d6281d9cfc9ba`

Independent review baseline: `925da6da3ce88394145c012b2a8c68e2e5cca66d`

Status: deterministic cutoff/p-value disagreement repaired and qualified on
JVM and Scala.js. Broader maxT, Westfall-Young, and HierScan statistical
admission remains open and is not implied by this receipt.

## Defect and decision rule

For `B` max-null draws, the public adjusted p-value is

```text
p(t) = (1 + count(M_b >= t)) / (B + 1).
```

The old cutoff selected the `k`th descending null draw and represented it as an
inclusive boundary. For null draws `1, ..., 19` and alpha `0.05`, it returned
`Inclusive(19)`, even though `p(19) = 2/20 = 0.10`. Only a score strictly above
19 has `p = 1/20 = 0.05`.

The repaired rule counts attainable plus-one p-value ranks using the same
floating-point division as `pValues`, then returns an explicit
`ThresholdCutoff.Exclusive(kthNull)`. It does not rearrange the boundary test
as `floor(alpha * (B + 1))`, avoiding a second source of disagreement at
representable alpha boundaries.

`ThresholdCutoff` now supplies a typed `rejects(score)` decision:

- `Inclusive(c)` rejects finite `score >= c`.
- `Exclusive(c)` rejects finite `score > c`.
- `NoRejections` rejects no finite score.
- non-finite scores are typed `NonFiniteData("threshold score")` failures.

## Legacy-double policy

The legacy numeric convention is explicitly `score >= threshold`.

- `Inclusive(c).toLegacyDouble == c`.
- `Exclusive(c).toLegacyDouble == Math.nextUp(c)`.
- `NoRejections.toLegacyDouble == +Infinity`.

There is no finite `Double` between `c` and `nextUp(c)`, so the inclusive
legacy comparison is exactly equivalent to `score > c` for every finite
score. At `c == Double.MaxValue`, `nextUp(c)` is `+Infinity`, correctly
representing that no finite score can exceed the boundary.

Importing a finite legacy double is necessarily interpreted as
`Inclusive(value)` because a bare number cannot reconstruct an erased strict
comparison. Importing `+Infinity` retains `NoRejections`; other non-finite
values are rejected.

## Independent oracle court

`MaxNullDecisionOracleSuite` does not call the cutoff implementation to derive
its expected answer. For every distribution of lengths 1 through 5 over
`{-1, 0, 1, 2}` (1,364 distributions), it independently counts
`null >= observed`, forms the plus-one p-value, and compares `p <= alpha`.

The court crosses those distributions with:

- fixed alpha values;
- every attainable rank alpha and its adjacent representable values;
- every null value, `nextDown` and `nextUp` around it;
- interior probes and both finite `Double` extrema.

More than 200,000 decisions agree exactly among the independent count oracle,
the public `MaxNull.pValues`, the typed cutoff decision, and the legacy
`score >= toLegacyDouble` decision. Separate regressions retain the original
19-draw defect, duplicate ties, smallest draw count, finite extrema,
non-finite refusal, and finite legacy import semantics.

The existing hand-computed maxT fixture now expects `Exclusive(4)` at alpha
0.4 for max-null draws `[3, 4, 3, 5]`; equality at 4 is not rejected and
`nextUp(4)` is rejected.

## Verification

These bounded module gates passed without compiler warnings:

```sh
sbt -Dsbt.supershell=false \
  "thresholdJVM/testOnly scalafim.fmri.threshold.MaxNullDecisionOracleSuite scalafim.fmri.threshold.ThresholdCoreSuite"
sbt -Dsbt.supershell=false \
  "thresholdJS/testOnly scalafim.fmri.threshold.MaxNullDecisionOracleSuite scalafim.fmri.threshold.ThresholdCoreSuite"
sbt -Dsbt.supershell=false "thresholdJVM/test"
sbt -Dsbt.supershell=false "thresholdJS/test"
```

- Targeted court: 16/16 on JVM and 16/16 on Scala.js.
- Full threshold module: 24/24 on JVM and 24/24 on Scala.js.

The current run used sbt 1.11.7 with the Homebrew Java 25.0.1 runtime reported
by its launcher and Node 26.7.0 for Scala.js.

An independent review on 2026-09-14 repeated both targeted commands and both
full-module commands against the review baseline above. The targeted courts
again passed 16/16 on each platform and the full module again passed 24/24 on
each platform, without compiler warnings. Review of every repository call site
found no incomplete matches on `ThresholdCutoff`; existing HierScan construction
retains its intentionally inclusive semantics. The public-type review found no
erased comparison policy or untyped reachable failure in this bounded change.

## Deliberate non-claims

This repair establishes agreement between one max-null cutoff representation
and its already-public plus-one p-value rule. It does not establish:

- validity of a null-generation or exchangeability design;
- one- or two-sided orientation of an upstream statistic/null family;
- strong-control conditions for step-down procedures;
- spatial FWER or FDR calibration;
- validity of adaptive HierScan regions or priors;
- implementation of enum-only TFCE, cluster-FDR, or RFT methods;
- correction for invalid first-level standard errors or group modeling.

Those scientific and integration criteria remain open on X-THRESH. No default
significance policy, publication, commit, or push is authorized by this
receipt.
