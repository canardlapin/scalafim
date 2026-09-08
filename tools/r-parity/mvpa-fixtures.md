# MVPA numerical oracle fixtures

ScalaFIM keeps each retained external MVPA oracle beside the typed estimand it
protects. These are scientific reference values, not snapshots of a Scala
implementation. Every generator uses dense base-R equations and emits a
version-neutral Scala source that executes on both the JVM and Scala.js.

The versioned receipt
[`mvpa-oracle-manifest-v1.json`](mvpa-oracle-manifest-v1.json) is the exhaustive
inventory. Its schema is
[`mvpa-oracle-manifest-v1.schema.json`](mvpa-oracle-manifest-v1.schema.json).
For every family it records:

- the estimand, normalization, fold reduction, and family-specific policy;
- every declared axis and its exact ordered keys;
- the base-R equation provenance and claimed regeneration command;
- R, Scala, sbt, and Gale versions;
- SHA-256 digests for both generator and generated Scala output; and
- the shared Scala court that must run independently on JVM and Scala.js.

The retained families and their generators are:

| Estimand family | Generator | Shared JVM/JS court |
| --- | --- | --- |
| Crossvalidated RDM, including signed bias and pair order | `generate_crossvalidated_rdm_fixtures.R` | `CrossvalidatedRdmParitySuite` |
| Cross-domain correlation-centroid classification decision scores | `generate_cross_domain_classification_fixtures.R` | `predictive.CrossDomainClassificationSuite` |
| Held-out canonical-effect root | `generate_canonical_effect_fixtures.R` | `CanonicalEffectMvpaSuite` |
| Held-out MANOVA root spectrum | `generate_manova_fixtures.R` | `ManovaMvpaSuite` |
| Nonnegative canonical-effect root | `generate_constrained_canonical_fixtures.R` | `ConstrainedCanonicalMvpaSuite` |
| Signed cross-run Rayleigh statistic | `generate_signed_cross_run_rayleigh_fixtures.R` | `SignedCrossRunRayleighMvpaSuite` |

## Freshness check

The local and CI freshness gate needs only Python's standard library. It does
not install or invoke R:

```sh
python3 -m unittest tools/r-parity/test_mvpa_oracle_manifest.py
python3 tools/r-parity/mvpa_oracle_manifest.py
```

The first command includes hostile output and generator tampering, missing
estimand/axis metadata, false source invocation, and embedded runtime-version
cases. The second checks the committed receipt, build versions,
retained-family inventory, normalized repository paths, and all artifact
digests. The focused GitHub Actions gate runs these same commands.

## Regeneration

Regeneration is intentionally separate and requires `Rscript`:

```sh
tools/r-parity/regenerate_mvpa_oracles.sh
```

The driver reads each command from the manifest, requires it to be exactly
`Rscript --vanilla <recorded-generator>`, invokes that claimed source with a
deterministic locale and timezone, replaces the corresponding Scala output,
records the observed R version, refreshes both digests, and reruns the
R-independent checker. A fixture cannot be refreshed through an unrecorded
source.

Every numerical comparison must still name its convention. Crossvalidated
distances retain signed null estimates and the canonical upper-triangle effect
pair order. Classification compares declared-class decision scores and does not
invent a probability surface. Canonical and MANOVA courts retain their exact
run pairing, tested effect or hypothesis, regularization, direction convention,
and fold reduction.
