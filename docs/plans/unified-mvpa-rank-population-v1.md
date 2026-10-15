# Rank nuisance population specification

This specification resolves the three-column Gaussian generator in M0.05
before any new nuisance pilot or confirmation observations. It does not change
the frozen protocol's criteria or declare scientific admission. Primary metric
binding is a separate pending decision.

For each independent row draw two independent standard Gaussian covariates
`z1,z2`. The nuisance matrix is `[1,z1,z2]`. Draw independent standardized
residual vectors `u` and `e`. For matched score coordinate `j`, let
`v_j = rho_j u_j + sqrt(1-rho_j²) e_j`; unmatched coordinates remain independent.
Use the protocol's four roots, dimensions and row counts without truncation.

Construct every observed score as
`x_j = .4 z1 + .4 z2 + sqrt(.68) u_j` and
`y_j = .4 z1 + .4 z2 + sqrt(.68) v_j`.
Each observed coordinate has variance one and population correlation `.4`
with each nuisance covariate. Covariates are independent of residuals. The
conditional score covariance is `.68` times the specified residual covariance,
so partial canonical roots remain exactly `rho`. The induced marginal
score correlations are retained; they are not the tested partial roots.

The independent R generator returns the full score/covariate covariance for
the prescribed simulator moment checks. Algebraic tests check its Schur
complement, positive definiteness, and residualized construction. These tests
do not substitute for the required 10,000-dataset QA per cell.

Intercept-only construction keeps its previous draws and draw order exactly.
The three-column generator draws covariates after those residual arrays. Root
and named noise seed assignments keep the frozen protocol namespace. Source
locks for future runs must include this document, `rank_population.R`,
`generate_known_truth.R`, and the coordinator. Historical source receipts and
the 600 earlier intercept pilot outcomes remain unchanged.

This document specifies a population; it does not freeze an incomplete campaign.
Simulator QA, pilot feasibility, independent numerical oracles, metric binding,
complete inventory, source locks and resource admission still precede confirmation.
