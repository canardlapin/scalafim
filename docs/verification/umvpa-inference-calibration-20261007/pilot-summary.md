# Sealed R0/R1/R2 pilot evidence

The three JVM pilots ran the committed source
`3b8d35e6aa989e2c6a5dadebabb2db608fc478cd`: exactly 200 datasets per cell,
B=199 per rank stage, and 477,600 completed compact fits altogether. All 600
records are evaluated, with no failed, refused, incomplete, missing, or duplicate
datasets. Independent inspection verified all dataset roots, stage-103 child
seeds, four-member order, population null vectors, 600 input file hashes, and
16 committed source locks. Source labels and the initial failed cost invocation
remain preserved.

`pilot-independent-analysis.py` and `pilot-descriptive-cp.R` independently count
the sealed decisions and use R `stats::qbeta` for exact dataset-level
Clopper–Pearson intervals. They perform no fitting or simulation. The existing
confirmation adjudicator retains its 10,000/5,000 requirements; these separate
200-dataset descriptions do not produce confirmation pass/fail decisions.

| Cell | Closed decision described | Rejections / datasets | Rate | Descriptive CP90 interval |
| --- | --- | --- | --- | --- |
| R0 | Any true-null rejection in the closed sequence | 8 / 200 | .040 | [.02006, .07101] |
| R1 | H2, after the earlier nonzero root | 6 / 200 | .030 | [.01314, .05835] |
| R2 | H3, after two earlier nonzero roots | 2 / 200 | .010 | [.001780, .03114] |

The R2/H3 closed interval lies below the frozen near-nominal lower bound .035.
This is a material pilot warning and is retained. It is not a completed
confirmation decision. R2/H2 detection is 49/200 (.245), so suppression by the
closed sequence is a plausible explanation; raw stage p-values were not recorded,
and this packet does not establish that explanation. No result is relabelled as
raw pointwise calibration. See `preconfirmation-metrics.md`.

Descriptive detections are R1/H1 161/200, R2/H1 182/200, and R2/H2 49/200.
Their one-sided 95% lower bounds are .75313, .86946, and .19557 respectively.
These are not the protocol's untested standard newly introduced root=.20
confirmation cells and confer no power qualification.

## Measured resources and runtime illustration

| Cell | Kernel wall total / mean per dataset | Process wall including startup | Sampled peak RSS |
| --- | --- | --- | --- |
| R0 | 2.577824 s / .0128891 s | 49.7117 s | 2,765,750,272 bytes |
| R1 | 2.544505 s / .0127225 s | 61.3059 s | 2,595,504,128 bytes |
| R2 | 2.384014 s / .0119201 s | 60.0242 s | 2,356,936,704 bytes |

Each worker had a 2 GiB JVM heap, four configured processors, one worker, a
3 GiB RSS cap, and a 900-second wall cap. CPU time was not measured; kernel and
process timings above are wall times. RSS values are monitored sample maxima.

The single B=1999 fixture measured .5180005 seconds for 7,996 compact fits, with
42.2857 seconds including cold startup. Its constant-rate projection across the
32 defined intercept-rank cells and 240,000 confirmation datasets is 34.533
kernel hours. Applying a linear B=199→1999 scale to the combined pilot mean
instead gives 8.378 kernel hours. Their difference exposes warm-up effects.
Neither is a verified lower bound or resource admission: n=160, reversed P/Q,
native JS, I/O, generation, bootstrap, and longer-run memory behavior are
unmeasured. Dataset and draw counts remain unchanged.

The independent R description emitted a startup LC_CTYPE warning and used the C
locale. Inputs are ASCII count tables, and the R session fingerprint is retained
in `pilot-analysis-R-session.txt`; no RNG is used by the interval calculator.

## Next native handoff and blockers

1. **M4.07:** retain both raw stage and closed outputs in future records, freeze
   their metric roles before confirmation, and qualify all required rank cells
   against unchanged criteria. Thirty-two defined intercept cells are only part
   of the required inventory. Three pilots do not establish criterion 2 or admit
   detectable rank. Nuisance definitions and repeated-block actions remain open.
2. **M4.08:** independently calibrate the available voxel omnibus factory,
   including partial-null spatial fields, nonfinite/member-failure completeness,
   and source replay/resource controls. The original mixed component/prediction
   family, component-slice partial-null actions, and FDR remain unavailable.
   Omnibus engineering tests do not release strong FWER.
3. **M4.09:** freeze remaining population/temporal/estimand definitions, complete
   the inventory and portable native kernel/assignment checks, and approve actual
   confirmation resources. Preserve population/conditional prediction-null
   contradictions and all refusals. Native JVM/JS fixture qualification does not
   replace the full 10,000-null / 5,000-alternative streams at B=1999.

No confirmation was invoked, no scientific method or threshold changed, and no
final calibration audit archive or released inference claim was created.
