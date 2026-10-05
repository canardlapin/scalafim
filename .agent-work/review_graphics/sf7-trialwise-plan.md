SF7 implementation draft (no production source edited while Gate15 is running)

TrialwiseCall additions: onsets, subset, phase, id. Existing default-only calls retain
schedule default onsets/durations and global sequential trial levels.

Parser/printer: accept and emit named onsets, subset, phase+parent, id. Phase and parent
must occur together as for hrf(). `id` is a column reference, not a term label; label
remains the term label.

Builder: resolve term timing/subset prior to EventTerm construction. For an explicit id
column, validate non-missing scalar identifiers are unique inside each block, then use
run-qualified factor levels so separately numbered runs do not share trial columns.
For phase provenance, qualify parent trial ids with block identity before EventPhase
construction because EventPhase requires globally unique parents; validate the caller's
unqualified parent ids per block. Subsetting selects each aligned axis before provenance
is materialized.

Tests: parsing/rendering roundtrip; build term-specific onsets+subset; phase provenance;
two unequal runs with repeated local ids; duplicate local id typed error. No LSS formula
syntax is proposed: existing FitStrategy.LeastSquaresSeparate remains the execution
mapping, but the builder currently exposes no target-specific LSS-1 design constructor.
