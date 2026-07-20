# Inference fixture contract v1

These files are deterministic outputs of `../generate.R` using the reference
checkout and runtime recorded in `provenance.tsv`. They are committed evidence,
not runtime dependencies; reviewing or testing ScalaFIM does not require R.

## Files

- `fixed_monte_carlo.tsv`: Phipson-Smyth fixed-draw p-values for greater,
  less, and two-sided alternatives. Null draws are explicit.
- `sequential_monte_carlo.tsv`: one Besag-Clifford early non-rejection and one
  full-budget rejection trajectory, including every consumed draw and batch.
- `unit_formation.tsv`: singleton and opt-in near-tie policies, including a
  partially selected subspace and a consecutive tie chain.
- `alignment.tsv`: a unique permutation/sign case and a deliberately ambiguous
  assignment, with diagnostic score and margin.
- `principal_angles.tsv`: analytic 30-degree, orthogonal, and partly tilted
  subspaces. Angles are in radians.
- `pca_ladder_input.tsv` and `plsc_ladder_input.tsv`: complete small matrices.
- `ladder_roots.tsv`, `ladder_steps.tsv`, and `ladder_units.tsv`: complete
  PCA/PLSC reference results, including null trajectories and stopping state.
- `phase6_families.tsv`: independent base-R fixtures for ridge-CCA canonical
  correlations, generalized eigen roots, held-out one-response RRR/OLS gain,
  constrained-PCA roots, multiblock consensus roots, and feature-level
  Bonferroni-family adjustments. Regenerate this file and its Scala mirror with
  `../generate_phase6.R`.

Matrix rows and columns and component members in TSV files are one-based to
match the R source. Matrix payloads in the generated Scala mirror are row-major;
fields that retain R indexing say `OneBased` explicitly.

## Semantics

The PCA ladder tests the Vitale P3 tail ratio on squared variance roots. Its
null independently permutes the rows of each residual column. The PLSC ladder
tests the squared leading singular value of the deflated cross-product. Its null
permutes Y rows relative to X. Both exact rank-two fixtures use `B = 39`, global
budget `117`, batch size `5`, alpha `0.1`, and stop at the first non-rejection.

The future Scala engine may use a different deterministic random generator. It
must match the frozen accumulator and ladder results when fed these null
trajectories; separate Scala tests will prove that its own seed splitting is
stable across execution order.
