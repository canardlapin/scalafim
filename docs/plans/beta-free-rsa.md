# Beta-free RSA estimand

The one-shot RSA path treats a selected `PatternOperator` as the sufficient
interface. For fold (r), let (B) denote its implicit sample-by-feature map
and let (A_r) average the held-out samples into the common condition order.
The implementation obtains

\[
  M_r^\top = B^\top A_r^\top
\]

with one adjoint operator application, then immediately reduces the fold into
the accumulated condition geometry

\[
  G_{cv} =
  \frac{\sum_{r \ne s} M_r M_s^\top}{R(R-1)}
  =
  \frac{(\sum_r M_r)(\sum_r M_r)^\top -
         \sum_r M_r M_r^\top}{R(R-1)}.
\]

Crossnobis distances are the contrast quadratic forms

\[
  d_{ij} = (e_i-e_j)^\top G_{cv}(e_i-e_j),
\]

optionally divided by the selected feature count. They remain signed: negative
values are valid null estimates and are never truncated.

## Separation of nuisance domains

For `OneShotDataset`, temporal nuisance has already been encoded in each
response-independent `TrialReadout`. The RSA reduction sees only the resulting
linear operator. Trial/model-level nuisance is a different estimand and is
handled after the labeled crossvalidated RDM is constructed, through the
existing aligned `RdmScorer.PartialPearson` control models. The two projections
are neither conflated nor assumed to remove the same signal.

## Execution contract

`OperatorCrossvalidatedGeometry` visits one fold at a time. Its persistent
state is a feature-by-condition sum and a condition Gram; its largest explicit
scratch values are a sample-by-condition averaging map and one
feature-by-condition fold summary. The owned working-set bound is therefore

\[
  SC + 2FC + 2C^2
\]

doubles, independent of the number of folds. The avoided trial-pattern table
would contain (SF) doubles. `OperatorRdmReceipt` records both quantities and
fixes the trial-by-feature materialization count at zero. This bound excludes
the supplied operator, its source data, and Gale's primitive application
scratch.

Both `OperatorCrossnobisAnalysis` and `OperatorCrossnobisRsaAnalysis` are
ordinary `FoldRequiredOperatorRoiAnalysis` values. They therefore use the same
`MvpaTask`, `MvpaStream`, `FeatureSetPlan`, `RoiOutcome`, and labeled RDM/RSA
payloads as every other MVPA analysis. The explicit-pattern implementation is
retained as an oracle and parity adapter, not as a required stage.
