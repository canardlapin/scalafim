#!/usr/bin/env python3
"""Check Root source ownership and actual provider Compile classpath closures.

The sbt task supplies resolved paths, including transitive source projects and
binary artifacts. This is an ownership tripwire, not a Scala parser or a claim
that arbitrary renamed implementations can be recognized automatically.
"""
import argparse
import json
from pathlib import Path
import re
import sys


RETIRED_MODULES = {
    "locus-data", "mvpa-foundation-spike", "image", "transform", "atlas",
    "surface", "surface-view", "surface-view-raster", "surface-view-javafx",
    "surface-view-three", "image-view", "image-view-canvas", "image-view-java2d",
    "image-view-javafx", "mvpa",
}
RETIRED_TYPES = {
    "AxisRef", "ReindexingLeg", "FiniteDomain", "DomainRegistry", "DomainFactory",
    "Parcellation", "Searchlight", "CenteredSearchlight", "TriangleMesh",
    "WorldSpace", "FrameCatalog", "NativeContext", "AtlasRealization",
    "SampleSpace", "NeuroVolume", "NeuroSeries", "SourceProvenance",
    "ProvenanceId", "EvidenceRef", "VoxelRegion",
}
FAMILIES = ("neuroim4s", "neurosurf4s", "neuroatlas4s", "mvpa4s", "image4s",
            "mesh4s", "spatial4s", "reframe4s", "intaglio")
NO_IMAGING = {name: set() for name in FAMILIES if name != "mvpa4s"}
POLICIES = {
    "mvpa-predictive": {**NO_IMAGING, "mvpa4s": {"core", "predictive"}},
    "mvpa-core": {**NO_IMAGING, "mvpa4s": {"core"}},
    "mvpa-locus": {**NO_IMAGING, "mvpa4s": {"core", "locus"}},
    "mvpa-atlas": {**NO_IMAGING, "mvpa4s": {"core", "locus", "atlas"}, "neuroatlas4s": {"core"}},
    "mvpa-surface": {**NO_IMAGING, "mvpa4s": {"core", "locus", "surface"}, "neurosurf4s": {"core"},
                     "neuroim4s": {"spaces"}, "image4s": {"geometry"},
                     "mesh4s": {"core", "geometry", "graph4s"},
                     "spatial4s": {"core"}},
    "atlas-core": {"neuroatlas4s": {"core"}, "neuroim4s": {"spaces"},
                   "neurosurf4s": set(), "image4s": set(), "mesh4s": set(),
                   "reframe4s": set(), "intaglio": set(), "mvpa4s": set()},
    "surface-core": {"neurosurf4s": {"core"}, "neuroim4s": {"spaces"},
                     "neuroatlas4s": set(), "image4s": {"geometry"},
                     "reframe4s": set(), "intaglio": set(), "mvpa4s": set()},
    "volume-core": {"neuroim4s": {"core", "spaces"}, "neurosurf4s": set(),
                    "neuroatlas4s": set(), "intaglio": set(), "mvpa4s": set()},
    "spaces": {**NO_IMAGING, "neuroim4s": {"spaces"}, "spatial4s": {"core"},
               "mvpa4s": set()},
    "locus-core": {**NO_IMAGING, "mvpa4s": set()},
    "locus-data": {**NO_IMAGING, "mvpa4s": set()},
    "locus-provenance": {**NO_IMAGING, "mvpa4s": set()},
}
EXPECTED_GATES = {f"{name}-{platform}" for name in POLICIES for platform in ("JVM", "JS")}


def code_only(source: str) -> str:
    # Keep newlines for locations, mask comments (including nested blocks) and
    # quoted literals. Backtick identifiers remain visible to the name check.
    out = list(source)
    index = 0
    while index < len(source):
        end = index
        if source.startswith("//", index):
            end = source.find("\n", index)
            if end < 0:
                end = len(source)
        elif source.startswith("/*", index):
            depth, end = 1, index + 2
            while end < len(source) and depth:
                if source.startswith("/*", end):
                    depth += 1
                    end += 2
                elif source.startswith("*/", end):
                    depth -= 1
                    end += 2
                else:
                    end += 1
        elif source.startswith('"""', index):
            found = source.find('"""', index + 3)
            end = len(source) if found < 0 else found + 3
        elif source[index] == '"':
            end = index + 1
            while end < len(source):
                if source[end] == "\\":
                    end += 2
                elif source[end] == '"':
                    end += 1
                    break
                else:
                    end += 1
        if end > index:
            for pos in range(index, end):
                if out[pos] != "\n":
                    out[pos] = " "
            index = end
        else:
            index += 1
    return "".join(out)


def inspect_source(root: Path) -> tuple[list[str], list[str]]:
    files, errors = [], []
    for source in sorted((root / "modules").glob("*/**/src/main/scala/**/*.scala")):
        relative = source.relative_to(root)
        if "target" in relative.parts:
            continue
        files.append(str(relative))
        if relative.parts[1] in RETIRED_MODULES:
            errors.append(f"{relative}: retired implementation module")
        code = code_only(source.read_text())
        for match in re.finditer(r"\b(?:class|trait|enum|object|type)\s+`?(\w+)`?", code):
            if match[1] in RETIRED_TYPES:
                line = code.count("\n", 0, match.start()) + 1
                errors.append(f"{relative}:{line}: provider-owned declaration {match[1]}")
        if re.search(r"\bpackage\s+scalafim\.(?:image|surface|atlas|transform|locus)(?:\s|[.;:]|$)", code):
            errors.append(f"{relative}: retired provider namespace")
    if not files:
        errors.append("no production Scala sources inspected")
    return files, errors


def provider_module(path: str) -> tuple[str, str] | None:
    families = "|".join(FAMILIES)
    match = re.search(rf"/modules/({families})-([^/]+)/", path)
    if match:
        return match[1], match[2]
    match = re.search(r"/intaglio/(?:modules/)?([^/]+)/", path, re.IGNORECASE)
    if match:
        return "intaglio", match[1].removeprefix("intaglio-")
    # Published coordinates, including Scala.js cross-version suffixes.
    match = re.match(rf"({families})-([^_]+)_(?:sjs\d+_)?\d+.*\.jar$", Path(path).name)
    return (match[1], match[2]) if match else None


def resolved_owner(path: str, projects: list[dict], artifacts: dict) -> tuple[str, str] | None:
    resolved = Path(path).resolve()
    candidates = [project for project in projects
                  if resolved.is_relative_to(Path(project["base"]).resolve())]
    if candidates:
        project = max(candidates, key=lambda row: len(Path(row["base"]).parts))
        if project["owner"]:
            module = re.sub(r"(?:JVM|JS)$", "", project["id"])
            return project["owner"], module.removeprefix(project["owner"] + "-")
    artifact = artifacts.get(str(resolved))
    if artifact:
        name = artifact["name"]
        if artifact["organization"] == "scalafim" or name.startswith("scalafim-"):
            return "scalafim", name
        for family in FAMILIES:
            if name.startswith(family + "-"):
                return family, name.removeprefix(family + "-").split("_", 1)[0]
    if resolved.name.startswith("scalafim-") and resolved.suffix == ".jar":
        return "scalafim", resolved.name
    return provider_module(path)


def inspect_classpaths(root: Path, gates: dict[str, list[str]], builds: list[str],
                       projects: list[dict] | None = None, artifacts: dict | None = None) -> list[str]:
    errors = []
    if set(gates) != EXPECTED_GATES:
        errors.append(f"classpath gate set differs: missing={sorted(EXPECTED_GATES - set(gates))}, extra={sorted(set(gates) - EXPECTED_GATES)}")
    revisions: dict[str, set[str]] = {}
    for build in builds:
        match = re.fullmatch(r"https://github.com/canardlapin/([^/#]+)\.git#([0-9a-f]{40})", build)
        if match:
            revisions.setdefault(match[1], set()).add(match[2])
    if not builds:
        errors.append("no loaded build URIs supplied")
    for repo, pins in revisions.items():
        if len(pins) > 1:
            errors.append(f"multiple loaded revisions of {repo}: {sorted(pins)}")
    for gate, paths in gates.items():
        if not paths:
            errors.append(f"{gate}: empty Compile classpath")
        policy = POLICIES.get(gate.rsplit("-", 1)[0], {})
        for path in paths:
            resolved = Path(path).resolve()
            module = resolved_owner(path, projects or [], artifacts or {})
            if (module and module[0] == "scalafim") or resolved.is_relative_to(root.resolve()) or re.search(r"/scalafim(?:-[^/]*)?/modules/", str(resolved)):
                errors.append(f"{gate}: ScalaFIM provider backedge: {path}")
            if "test-classes" in resolved.parts or any("-testkit" in part for part in resolved.parts):
                errors.append(f"{gate}: Test dependency in Compile: {path}")
            if resolved.name.startswith(("javafx-", "scalafx_")):
                errors.append(f"{gate}: platform rendering dependency in Compile: {path}")
            if module and "testkit" in module[1] and not any("-testkit" in part for part in resolved.parts):
                errors.append(f"{gate}: Test dependency in Compile: {path}")
            if module and module[0] in policy and module[1] not in policy[module[0]]:
                errors.append(f"{gate}: forbidden {module[0]}-{module[1]}: {path}")
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--classpath-manifest", type=Path)
    parser.add_argument("--receipt", type=Path)
    args = parser.parse_args()
    files, errors = inspect_source(args.root)
    gates: dict[str, list[str]] = {}
    builds = []
    projects = []
    artifacts = {}
    if args.classpath_manifest:
        for line in args.classpath_manifest.read_text().splitlines():
            key, value = line.split("\t", 1)
            if key == "@project":
                owner, project, base = value.split("\t")
                projects.append({"owner": owner, "id": project, "base": base})
            elif key == "@artifact":
                path, organization, name = value.split("\t")
                artifacts[path] = {"organization": organization, "name": name}
            elif key == "@build":
                builds.append(value)
            else:
                gates.setdefault(key, []).append(value)
        if not projects or not artifacts:
            errors.append("resolved project/artifact ownership metadata is missing")
        errors += inspect_classpaths(args.root, gates, builds, projects, artifacts)
    receipt = {"schema": "scalafim.ecosystem-boundaries.v1", "source_files": files,
               "loaded_builds": builds, "resolved_projects": projects, "resolved_artifacts": artifacts,
               "compile_classpaths": gates, "errors": errors}
    if args.receipt:
        args.receipt.parent.mkdir(parents=True, exist_ok=True)
        args.receipt.write_text(json.dumps(receipt, indent=2) + "\n")
    for error in errors:
        print(error, file=sys.stderr)
    print(f"ecosystem boundaries: {len(files)} production files, {len(gates)} Compile closures, {len(errors)} errors")
    return int(bool(errors))


if __name__ == "__main__":
    raise SystemExit(main())
