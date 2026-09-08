#!/usr/bin/env python3
"""Reproduce the MVPA conformance-court LOC and ergonomics ledger.

The counts describe executable court scenarios, not package size and not a
minimal tutorial.  ``direct_*`` covers the code between a named scenario's
markers.  ``support_*`` is the union of the explicitly named construction and
helper regions needed by that scenario.  Executable LOC means nonblank,
noncomment physical source lines; it is deliberately not a statement count.
"""

from __future__ import annotations

import argparse
import ast
import csv
import io
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


ROOT = Path(__file__).resolve().parents[3]


@dataclass(frozen=True)
class Region:
    path: str
    kind: str
    start: str
    end: str | None = None


@dataclass(frozen=True)
class Court:
    scenario_id: str
    implementation: str
    direct: Region
    support: tuple[Region, ...]
    support_reuse: str
    required_concepts: str
    identity_visibility: str
    leakage_resistance: str
    error_locality: str
    output_labeling: str
    extension_shape: str
    setup_friction: str
    pain_points: str
    elegance_notes: str


RSA_PY = "tools/reference/mvpa-conformance/rsatoolbox_reference.py"
PYMVPA = "tools/reference/mvpa-conformance/pymvpa_reference.py"
MATLAB = "tools/reference/mvpa-conformance/rsatoolbox_matlab_reference.m"
SCALA_OBS = (
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/scenarios/"
    "RsaToolboxObservationConformanceSuite.scala"
)
SCALA_REL = (
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/scenarios/"
    "RsaToolboxRelationalConformanceSuite.scala"
)
SCALA_PRED = (
    "modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/scenarios/"
    "PyMvpaPredictiveConformanceSuite.scala"
)


def marker(path: str, name: str) -> Region:
    return Region(path, "marker", name)


def symbol(path: str, name: str) -> Region:
    return Region(path, "python-symbol", name)


def span(path: str, start: str, end: str) -> Region:
    return Region(path, "span", start, end)


COURTS = (
    Court(
        "RDM-OBS",
        "rsatoolbox-python-0.3.2",
        marker(RSA_PY, "rsatoolbox-python-observation-rdm"),
        (symbol(RSA_PY, "vector"),),
        "vector conversion is shared by all Python-rsatoolbox courts",
        "Dataset; observation and channel descriptors; calc_rdm; method string",
        "sample and feature descriptors are optional metadata; basis, units, scale, and provenance are not required",
        "not applicable to a fixed observation RDM",
        "runtime exceptions or numeric NaN; no per-measurement typed failure",
        "RDMs records a dissimilarity-measure string and pattern descriptors",
        "central method dispatch plus the extensible RDMs container",
        "ordinary modern Python environment, but importing the package initializes plotting/cache dependencies",
        "the method label euclidean means squared distance divided by channel count",
        "very concise for the standard case, with scientific conventions carried partly by documentation",
    ),
    Court(
        "RDM-OBS",
        "pymvpa-2.6.5.dev1",
        marker(PYMVPA, "pymvpa-observation-rdm"),
        (symbol(PYMVPA, "as_floats"),),
        "array conversion is shared by the PyMVPA courts",
        "Dataset; sample attributes; PDist; PDistTargetSimilarity; metric strings",
        "sample attributes are optional and the condensed output is primarily positional",
        "not applicable to a fixed observation RDM",
        "runtime exception or untyped numeric result",
        "Dataset result plus call-site metric; pair identity must be reconstructed from input order",
        "measure and mapper subclass protocols",
        "source build, Python-2-to-3 translation, old NumPy pin, and a visible SciPy documentation shim",
        "legacy environment dominates setup; stable pair keys are not intrinsic to the result",
        "compact scientific call once the legacy runtime has been made executable",
    ),
    Court(
        "RDM-OBS",
        "scalafim-public",
        marker(SCALA_OBS, "observation-rdm"),
        (span(SCALA_OBS, "  private def right[A]", "  // BEGIN SCENARIO observation-rdm"),),
        "identified axes, evidence, frame, and strategy are shared by three observation courts",
        "AxisRef; EvidenceTable; Observations; fit design; MeasurementFrame; distance; execution strategy",
        "ordered sample and neural identities, basis, units, scale, and provenance are mandatory and retained",
        "not applicable to a fixed observation RDM",
        "bind errors are typed; a degenerate row is a keyed local MeasurementOutcome.Failed",
        "typed pair domain, measurement identity, scientific-plan identity, and execution receipt",
        "open typed estimand compiler without editing a central payload enum",
        "normal sbt dependency graph; JVM and Scala.js compile the same court",
        "substantial construction ceremony; target similarity is not a first-class observation estimand",
        "the run call is coherent, but current low-level public construction is too verbose for routine use",
    ),
    Court(
        "RDM-XCV",
        "rsatoolbox-python-0.3.2",
        marker(RSA_PY, "rsatoolbox-python-relational-crossnobis"),
        (symbol(RSA_PY, "vector"),),
        "one function supplies whole and fixed-support measurements",
        "Dataset; condition and run descriptors; precision matrix; calc_rdm_crossnobis",
        "condition, run, and feature descriptors are visible, but complete coordinate identity is optional",
        "run descriptor states crossvalidation; independence is inferred rather than carried as a checked declaration",
        "runtime validation; failures are not isolated by measurement in this direct call",
        "RDMs labels crossnobis and preserves pattern descriptors",
        "method dispatch and RDM model classes",
        "modern pip environment with compiled scientific-Python dependencies",
        "fixed noise orientation and per-channel normalization must be learned from API conventions",
        "excellent brevity for canonical balanced designs",
    ),
    Court(
        "RDM-XCV",
        "rsatoolbox-matlab-91ad433",
        marker(MATLAB, "rsatoolbox-matlab-relational-crossnobis"),
        (span(MATLAB, "toolboxRoot =", "fixture = jsondecode"),),
        "toolbox path and fixture loading are shared with the RSA comparison court",
        "beta matrix; integer partition and condition vectors; Cholesky whitening; distanceLDC",
        "partition and condition identity is positional; feature identity and provenance are absent",
        "partition vector requests crossvalidation but does not encode an independence certificate",
        "MATLAB errors abort the call; no local measurement outcome",
        "numeric condensed distance vector; labeling is supplied by surrounding workflow code",
        "toolbox functions and MATLAB structs",
        "pinned source plus MATLAB; Octave needs a no-op import shim and explicit package utility path",
        "prewhitening orientation, positional labels, and toolbox path behavior are implicit",
        "short transparent numerical route, but little self-describing scientific identity",
    ),
    Court(
        "RDM-XCV",
        "scalafim-public",
        marker(SCALA_REL, "relational-crossnobis"),
        (span(SCALA_REL, "  private def right[A]", "  // BEGIN SCENARIO relational-crossnobis"),),
        "one relation source, certified precision, pairing, and frame support crossnobis and RSA",
        "effect, neural, partition, and training axes; capabilities; independence declaration; pairing; frame; strategy",
        "all axes and every measurement support are identified; receipts retain source, fit, and pairing identity",
        "ordered independent edges are declared and checked instead of inferred from unequal ordinals",
        "construction and bind failures are typed; each measurement can fail without discarding siblings",
        "keyed RDM domain, signed distances, per-measurement receipt, plan identity, and work accounting",
        "new relational estimands add a typed result and compiler instance, not a core enum case",
        "normal sbt build; identical shared test executes on JVM and Scala.js",
        "the honest source/capability/pairing construction is far too ceremonial at this level",
        "strongest scientific audit trail and weakest concise entry experience of the compared crossnobis calls",
    ),
    Court(
        "RSA-MODEL",
        "rsatoolbox-python-0.3.2",
        marker(RSA_PY, "rsatoolbox-python-relational-rsa"),
        (
            marker(RSA_PY, "rsatoolbox-python-relational-crossnobis"),
            symbol(RSA_PY, "vector"),
            symbol(RSA_PY, "rdm"),
        ),
        "reuses the fitted crossnobis values; model wrapping and comparison are small",
        "RDMs; compare method; explicit regression design; NumPy least squares",
        "condition descriptors label RDMs; regression term identity is maintained by surrounding code",
        "not applicable after the crossvalidated RDM is fixed",
        "runtime shape/numeric errors",
        "comparison matrix for correlations; regression labels are user-managed",
        "rich model-RDM hierarchy and comparison-method dispatch",
        "same modern Python-rsatoolbox environment",
        "weighted-model fitting uses objectives distinct from ordinary intercepted OLS",
        "concise model comparison; explicit NumPy is needed for the exact OLS estimand used here",
    ),
    Court(
        "RSA-MODEL",
        "rsatoolbox-matlab-91ad433",
        marker(MATLAB, "rsatoolbox-matlab-relational-rsa"),
        (
            span(MATLAB, "toolboxRoot =", "fixture = jsondecode"),
            marker(MATLAB, "rsatoolbox-matlab-relational-crossnobis"),
        ),
        "reuses distanceLDC output and toolbox utilities",
        "model vectors; fitModelOLS; corrcoef; rankTransform_equalsStayEqual",
        "model columns and coefficient terms are positional unless labeled externally",
        "not applicable after the crossvalidated RDM is fixed",
        "runtime errors abort the script",
        "numeric scalars/vectors; the wrapper adds explicit term labels to its receipt",
        "MATLAB function conventions and struct workflow",
        "same pinned MATLAB/Octave court",
        "term and pair labeling are external; Octave cannot run the full MATLAB workflow unchanged",
        "the direct statistics are readable and short",
    ),
    Court(
        "RSA-MODEL",
        "scalafim-public",
        marker(SCALA_REL, "relational-rsa"),
        (span(SCALA_REL, "  private def right[A]", "  // BEGIN SCENARIO relational-crossnobis"),),
        "reuses the relation source, pairing, precision, and measurement setup",
        "typed fit query; pair domain; named second-order models; Pearson, rank, and regression queries",
        "effect keys, model names, fit identity, and coefficient names survive the query",
        "not applicable after the independently paired fit is fixed",
        "typed query errors and explicit model-domain compatibility checks",
        "typed correlation/regression results tied to one relational fit",
        "open query functions over the relational fit rather than a universal scorer payload",
        "same JVM and Scala.js build",
        "model construction and repeated right-unwrapping obscure the concise scientific intent",
        "clear type relationships and reusable fit; ergonomic façade still needs compression",
    ),
    Court(
        "PRED-NFOLD-SL",
        "pymvpa-2.6.5.dev1",
        marker(PYMVPA, "pymvpa-centroid-searchlights"),
        (
            symbol(PYMVPA, "as_strings"),
            symbol(PYMVPA, "FormulaMatchedCentroid"),
            symbol(PYMVPA, "predictive_dataset"),
            symbol(PYMVPA, "folds"),
            symbol(PYMVPA, "run_classifier"),
        ),
        "dataset, partitioner, and classifier helpers are reused by LDA and every fixed support",
        "Dataset sample/feature attributes; Classifier lifecycle; NFoldPartitioner; CrossValidation; slicing",
        "sample IDs can be attached, but CrossValidation output does not retain stable source identity by default",
        "the classifier train/predict lifecycle is fold scoped; own-target-freedom is a convention rather than a type",
        "runtime failure generally aborts the measure",
        "predictions, truth, cvfolds, and stats; assessment sample IDs must be reconstructed",
        "classifier, mapper, partitioner, and measure subclass protocols",
        "source translation plus strict legacy NumPy/SciPy compatibility",
        "environment fragility, output rekeying, and manual fixed-support orchestration",
        "high-level CV composition remains pleasantly compact once the environment and custom learner exist",
    ),
    Court(
        "PRED-NFOLD-SL",
        "scalafim-public",
        marker(SCALA_PRED, "pymvpa-centroid-searchlights"),
        (span(SCALA_PRED, "  private def right[A]", "  // BEGIN SCENARIO pymvpa-centroid-searchlights"),),
        "axes, source, validation, frame, strategy, and classifier config are reused by all predictive courts",
        "sample/neural/class axes; typed target; bound resample4s schedule; frame; learner config; strategy",
        "OOF predictions are SampleId-keyed, but run grouping cannot yet be bound as a truthful source axis",
        "fold-local standardization is enforced by the predictive lifecycle and adversarially tested",
        "typed bind failure before execution; measurement failures remain local",
        "keyed OOF predictions, fold receipts, accuracy, plan identity, and execution work",
        "typed learner definitions compile through the predictive kernel",
        "normal cross-platform sbt build",
        "very high construction ceremony and a critical run-generalization receipt gap despite numerical parity",
        "auditable outputs and strong lifecycle; not yet an elegant routine-analysis façade",
    ),
)


def source_lines(path: str) -> list[str]:
    return (ROOT / path).read_text(encoding="utf-8").splitlines()


def region_lines(region: Region) -> set[tuple[str, int]]:
    lines = source_lines(region.path)
    if region.kind == "marker":
        begin = f"BEGIN SCENARIO {region.start}"
        end = f"END SCENARIO {region.start}"
        starts = [index for index, line in enumerate(lines) if begin in line]
        ends = [index for index, line in enumerate(lines) if end in line]
        if len(starts) != 1 or len(ends) != 1 or starts[0] >= ends[0]:
            raise ValueError(f"invalid markers for {region.start} in {region.path}")
        positions = range(starts[0] + 1, ends[0])
    elif region.kind == "span":
        starts = [index for index, line in enumerate(lines) if region.start in line]
        ends = [index for index, line in enumerate(lines) if region.end in line]
        if len(starts) != 1 or len(ends) != 1 or starts[0] >= ends[0]:
            raise ValueError(f"invalid span {region.start!r}..{region.end!r} in {region.path}")
        positions = range(starts[0], ends[0] + (0 if "BEGIN SCENARIO" in region.end else 1))
    elif region.kind == "python-symbol":
        tree = ast.parse("\n".join(lines) + "\n", filename=region.path)
        nodes = [
            node
            for node in tree.body
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef))
            and node.name == region.start
        ]
        if len(nodes) != 1 or nodes[0].end_lineno is None:
            raise ValueError(f"invalid Python symbol {region.start!r} in {region.path}")
        positions = range(nodes[0].lineno - 1, nodes[0].end_lineno)
    else:
        raise ValueError(f"unknown region kind: {region.kind}")
    return {(region.path, position) for position in positions}


def is_executable(path: str, line: str) -> bool:
    stripped = line.strip()
    if not stripped:
        return False
    suffix = Path(path).suffix
    if suffix == ".py":
        return not stripped.startswith("#")
    if suffix == ".m":
        return not stripped.startswith("%")
    return not stripped.startswith("//")


def counts(regions: Iterable[Region]) -> tuple[int, int]:
    locations: set[tuple[str, int]] = set()
    for region in regions:
        locations.update(region_lines(region))
    raw = len(locations)
    executable = sum(
        is_executable(path, source_lines(path)[position])
        for path, position in locations
    )
    return raw, executable


FIELDS = (
    "scenario_id",
    "implementation",
    "direct_raw_loc",
    "direct_executable_loc",
    "support_raw_loc",
    "support_executable_loc",
    "support_reuse",
    "required_concepts",
    "identity_visibility",
    "leakage_resistance",
    "error_locality",
    "output_labeling",
    "extension_shape",
    "setup_friction",
    "pain_points",
    "elegance_notes",
    "source",
)


def render() -> str:
    stream = io.StringIO(newline="")
    writer = csv.DictWriter(stream, fieldnames=FIELDS, lineterminator="\n")
    writer.writeheader()
    for court in COURTS:
        direct_raw, direct_executable = counts((court.direct,))
        support_raw, support_executable = counts(court.support)
        writer.writerow(
            {
                "scenario_id": court.scenario_id,
                "implementation": court.implementation,
                "direct_raw_loc": direct_raw,
                "direct_executable_loc": direct_executable,
                "support_raw_loc": support_raw,
                "support_executable_loc": support_executable,
                "support_reuse": court.support_reuse,
                "required_concepts": court.required_concepts,
                "identity_visibility": court.identity_visibility,
                "leakage_resistance": court.leakage_resistance,
                "error_locality": court.error_locality,
                "output_labeling": court.output_labeling,
                "extension_shape": court.extension_shape,
                "setup_friction": court.setup_friction,
                "pain_points": court.pain_points,
                "elegance_notes": court.elegance_notes,
                "source": court.direct.path,
            }
        )
    return stream.getvalue()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", type=Path)
    args = parser.parse_args()
    rendered = render()
    if args.check is not None:
        expected = args.check.read_text(encoding="utf-8")
        if expected != rendered:
            raise SystemExit(f"ergonomics ledger drift: {args.check}")
        print(f"checked {args.check}")
    else:
        print(rendered, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
