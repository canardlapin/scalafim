# MVPA external conformance court

This directory answers a narrow empirical question: when ScalaFIM and an
established MVPA/RSA toolkit state the same estimand, conventions, and
measurement support, do they return the same answer?

It is not a compatibility layer. The external programs generate versioned
test evidence; no production ScalaFIM code imports them, decodes their object
models, or preserves their APIs.

## Three distinct references

- `rsatoolbox_matlab` is the original MATLAB RSA Toolbox at the revision in
  `reference-lock.json`. Its unmodified `distanceLDC`, rank transform, and OLS
  routines run through Octave for the portable reference court.
- `rsatoolbox_python` is the newer Python replacement. It supplies the
  executable RDM, crossnobis, and RDM-comparison oracle.
- `pymvpa` supplies independent `PDist`, `NFoldPartitioner`,
  `CrossValidation`, LDA, and fixed-support searchlight scenarios.

The names are never used interchangeably in receipts or findings.

## Evidence flow

```text
fixture.py
  |-- rsatoolbox_reference.py ---------+
  |-- rsatoolbox_matlab_reference.m ---+--> finalize_reference.py
  `-- pymvpa_reference.py -------------+          |
                                                   +--> reference-results-v1.json
                                                   `--> generated Scala fixture
```

`reference-results-v1.json` is a reduced receipt, not a raw output dump. The
finalizer first proves cross-reference identities such as Python versus
MATLAB crossnobis, leave-one-out versus uniform all-pairs reduction, and
PyMVPA versus rsatoolbox observation distances. It refuses any gap above the
declared `1e-10` tolerance.

## Rebuild the reference environments

Python rsatoolbox:

```sh
uv venv -p 3.12 /tmp/scalafim-rsatoolbox-reference
uv pip install \
  --python /tmp/scalafim-rsatoolbox-reference/bin/python \
  -r tools/reference/mvpa-conformance/requirements-rsatoolbox.txt
```

PyMVPA is pinned to source because the revision is not a modern Python wheel:

```sh
git clone https://github.com/PyMVPA/PyMVPA.git /tmp/scalafim-pymvpa-reference
git -C /tmp/scalafim-pymvpa-reference checkout \
  f699189b5b7e7a1bcaaf6f0a19aa077d8879b422
uv venv -p 3.11 /tmp/scalafim-pymvpa-reference/.venv
uv pip install \
  --python /tmp/scalafim-pymvpa-reference/.venv/bin/python \
  -r tools/reference/mvpa-conformance/requirements-pymvpa.txt
(cd /tmp/scalafim-pymvpa-reference && .venv/bin/python setup.py build --no-libsvm)
```

Original MATLAB toolbox:

```sh
git clone https://github.com/rsagroup/rsatoolbox_matlab.git \
  /tmp/scalafim-rsatoolbox-matlab
git -C /tmp/scalafim-rsatoolbox-matlab checkout \
  91ad43359c8a6355de1dd726e69038b7e798720a
```

The PyMVPA runner contains two visible, behavior-neutral runtime shims needed
by this pinned source: discard SciPy's removed `extradoc` keyword and use the
last NumPy line that still provides `np.float`. The Octave wrapper supplies a
no-op MATLAB `import` and exposes the toolbox utility package. These are
recorded as ergonomics findings, not hidden setup.

## Regenerate and check

Run each external arm into temporary files, then reduce them:

```sh
MPLCONFIGDIR=/tmp/scalafim-mpl-cache \
  /tmp/scalafim-rsatoolbox-reference/bin/python \
  tools/reference/mvpa-conformance/rsatoolbox_reference.py \
  >/tmp/scalafim-rsatoolbox-python.json

PYTHONPATH=/tmp/scalafim-pymvpa-reference/build/py3k \
  /tmp/scalafim-pymvpa-reference/.venv/bin/python \
  tools/reference/mvpa-conformance/pymvpa_reference.py \
  >/tmp/scalafim-pymvpa.json

RSA_TOOLBOX_MATLAB_ROOT=/tmp/scalafim-rsatoolbox-matlab \
  tools/reference/mvpa-conformance/run_rsatoolbox_matlab_octave.sh \
  >/tmp/scalafim-rsatoolbox-matlab.txt

python3 tools/reference/mvpa-conformance/finalize_reference.py \
  --rsatoolbox-python /tmp/scalafim-rsatoolbox-python.json \
  --pymvpa /tmp/scalafim-pymvpa.json \
  --rsatoolbox-matlab /tmp/scalafim-rsatoolbox-matlab.txt \
  --check tools/reference/mvpa-conformance/reference-results-v1.json
```

Ordinary sbt tests do not start Python, Octave, MATLAB, or an external package.
They consume one generated shared Scala fixture and therefore run on both the
JVM and Scala.js. Check fixture freshness with:

```sh
python3 tools/reference/mvpa-conformance/generate_scala_fixture.py \
  --check modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/scenarios/MvpaExternalReferenceFixture.scala
```

## Ledger semantics

`docs/audits/mvpa-conformance/coverage.csv` assigns each investigated
capability one of these dispositions:

- `exact_parity`: same estimand and conventions, directly compared;
- `convention_equivalent`: an explicit reversible convention map is required;
- `conditional_prediction_parity`: labels/metrics agree but raw score
  definitions differ;
- `numeric_parity_receipt_gap`: the numerical result agrees, but the current
  ScalaFIM scientific identity cannot truthfully name a required boundary;
- `reproducible_downstream`: derivable from a typed ScalaFIM result, but not a
  first-class ScalaFIM estimand;
- `intentional_divergence`: ScalaFIM deliberately states a different policy;
- `absent_no_claim`: unsupported and not claimed;
- `pending`: intended overlap whose court has not run yet.

`findings.jsonl` is append-only. Every entry names its evidence, architectural
consequence, and Mote follow-up when one exists. Timing and allocation results
are kept separately because they are host/runtime receipts, not mathematical
truth.

The ordinary release gate validates the committed study with only the Python
standard library:

```sh
python3 tools/reference/mvpa-conformance/validate_study.py
```

That check covers reference-lock consistency, agreement tolerances, disposition
and ledger schemas, cross-platform assurance status, performance receipt/CSV
agreement, current benchmark-harness hashes, and required report scope. It does
not pretend to regenerate external evidence; the pinned environments and
commands above remain the separate live-reference court.

## Reproduce the ergonomics ledger

The source files mark the exact conformance-court bodies used for LOC
measurement. Rebuild and check the ledger with:

```sh
python3 tools/reference/mvpa-conformance/measure_ergonomics.py \
  --check docs/audits/mvpa-conformance/ergonomics.csv
```

These are physical court LOC, not package size, statement count, or a claim
about the shortest possible tutorial. `direct_*` counts the marked scientific
scenario. `support_*` counts the explicitly named reusable construction and
helper regions needed by it. Nonblank, noncomment lines are called executable
LOC. The qualitative columns remain separate: identity visibility, leakage
resistance, error locality, output labeling, extension shape, setup friction,
pain points, and elegance are not collapsed into a synthetic score.

## Reproduce the matched performance court

Performance is a separate host-local receipt. Every arm prepares axes, data,
designs, frames, and callable objects outside timing, then times one
steady-state public call at one fixed shape. Each setup validates the same
numeric checksum before a duration is admitted.

Generate the Python and Octave arms with the pinned environments above:

```sh
MPLCONFIGDIR=/tmp/scalafim-mpl-cache \
  /tmp/scalafim-rsatoolbox-reference/bin/python \
  tools/reference/mvpa-conformance/benchmark_reference.py \
  --tool rsatoolbox --repeats 9 \
  >/tmp/scalafim-mvpa-rsatoolbox-performance.json

PYTHONPATH=/tmp/scalafim-pymvpa-reference/build/py3k \
  /tmp/scalafim-pymvpa-reference/.venv/bin/python \
  tools/reference/mvpa-conformance/benchmark_reference.py \
  --tool pymvpa --repeats 9 \
  >/tmp/scalafim-mvpa-pymvpa-performance.json

RSA_TOOLBOX_MATLAB_ROOT=/tmp/scalafim-rsatoolbox-matlab \
  tools/reference/mvpa-conformance/run_benchmark_matlab_octave.sh \
  >/tmp/scalafim-mvpa-rsatoolbox-matlab-performance.txt
```

Run the matching ScalaFIM JMH court as documented in
`benchmarks/mvpa-jvm/README.md`, then validate and reduce all four raw inputs:

```sh
python3 tools/reference/mvpa-conformance/summarize_performance.py \
  --jmh /tmp/scalafim-mvpa-conformance-jmh.json \
  --rsatoolbox /tmp/scalafim-mvpa-rsatoolbox-performance.json \
  --pymvpa /tmp/scalafim-mvpa-pymvpa-performance.json \
  --matlab /tmp/scalafim-mvpa-rsatoolbox-matlab-performance.txt \
  --check-receipt docs/audits/mvpa-conformance/performance-receipt-20260826.json \
  --check-ledger docs/audits/mvpa-conformance/performance.csv
```

The receipt retains per-repeat summaries, checksums, runtimes, harness hashes,
and raw-input hashes. Raw benchmark dumps remain temporary. Python's
`tracemalloc` omits most NumPy/SciPy native allocation and is not comparable to
JMH bytes/op. Octave timing is not MATLAB timing. Neither time nor allocation
is a portable pass/fail threshold.
