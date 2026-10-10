"""Clean-checkout build preparation for the custodian (design 4.2, F10; review F12).

``git clone --no-hardlinks`` the repository, ``git checkout --detach <full reviewed SHA>``, then
refuse unless HEAD equals the SHA, the tree is clean, no submodule (160000) exists, every tracked
symlink (120000) is an in-tree alias of a tracked regular file (``check_tracked_symlink``), and no
scalafim override reaches sbt through the environment, the tree's opts files, or the user's global
sbt configuration.  The stamp records the SHA, the
build.sbt hash, toolchain identities and the hashes of the custody tool and its lockfile.  Builds
run under ``env -i`` plus an allowlist (``run_build``).  This module never runs sbt itself.
"""
from __future__ import annotations

import hashlib
import json
import os
import posixpath
import re
import shutil
import subprocess
from pathlib import Path

OVERRIDE_PATTERNS = (
    "scalafim.gale.build",
    "-Dscalafim.",
    "galeRevision=",
    "-Dsbt.global.base",
    "-Dsbt.boot.directory",
    "-Dsbt.ivy.home",
)
ENV_VARS = (
    "SBT_OPTS",
    "JAVA_OPTS",
    "JAVA_TOOL_OPTIONS",
    "JDK_JAVA_OPTIONS",
    "_JAVA_OPTIONS",
    "SBT_ARGS",
    "JVM_OPTS",
    "SBT_OPTS_FILE",
)
OPTS_FILES = (".jvmopts", ".sbtopts", ".sbt/jvmopts")
GLOBAL_OPTS = (".sbtopts", ".jvmopts")
NOHOOKS = ("-c", "core.hooksPath=/dev/null")
# accepted tracked symlinks are materialised as real symlinks, never as plain files holding the target
SYMLINKS = ("-c", "core.symlinks=true")
REGULAR_MODES = ("100644", "100755")
ENV_ALLOWLIST = (
    "PATH",
    "HOME",
    "JAVA_HOME",
    "LANG",
    "LC_ALL",
    "TERM",
    "TMPDIR",
    "USER",
)
TOOL_DIR = Path(__file__).resolve().parent
LOCKFILE = TOOL_DIR.parent / "requirements.lock"


class CheckoutError(Exception):
    pass


def _run(args, cwd=None, check=True) -> str:
    r = subprocess.run(args, cwd=cwd, capture_output=True, text=True)
    if check and r.returncode:
        raise CheckoutError(f"{' '.join(args[:3])}: {r.stderr.strip()}")
    return (r.stdout or r.stderr).strip()


def _has_override(text: str) -> bool:
    return any(p in text for p in OVERRIDE_PATTERNS)


def check_no_override(tree: Path, env=None, home: Path | None = None) -> None:
    """Environment, the tree's opts files, and the global sbt config under ``home`` (default $HOME)."""
    env = os.environ if env is None else env
    for v in ENV_VARS:
        if _has_override(env.get(v, "")):
            raise CheckoutError(f"scalafim override in environment variable {v}")
    for f in OPTS_FILES:
        p = Path(tree) / f
        if p.is_file() and _has_override(p.read_text()):
            raise CheckoutError(f"scalafim override in {f}")
    home = Path(env.get("HOME", Path.home())) if home is None else Path(home)
    for f in GLOBAL_OPTS:
        p = home / f
        if p.is_file() and _has_override(p.read_text()):
            raise CheckoutError(f"scalafim override in ~/{f}")
    sbt = home / ".sbt"
    if sbt.is_dir():
        for p in (
            list(sbt.glob("*/*.sbt"))
            + list(sbt.glob("*/plugins/*.sbt"))
            + list(sbt.glob("*/plugins/*.scala"))
            + list(sbt.glob("*.sbt"))
        ):
            try:
                t = p.read_text()
            except OSError:
                continue
            if _has_override(t) or re.search(r"\bgale\b", t, re.I):
                raise CheckoutError(
                    f"gale/scalafim override in global sbt config ~/.sbt/{p.relative_to(sbt)}"
                )


def check_clean(tree: Path) -> None:
    out = _run(["git", "status", "--porcelain", "--ignored=no"], cwd=tree)
    if out:
        raise CheckoutError("working tree is not clean:\n" + out)


def _tree_entries(tree: Path) -> dict[str, tuple[str, str]]:
    """Every entry of HEAD's tree, directories included: ``{path: (mode, object id)}``."""
    r = subprocess.run(
        ["git", "ls-tree", "-r", "-t", "-z", "--full-tree", "HEAD"],
        cwd=tree,
        capture_output=True,
    )
    if r.returncode:
        raise CheckoutError(f"git ls-tree: {r.stderr.decode(errors='replace').strip()}")
    entries = {}
    for rec in r.stdout.split(b"\0"):
        if not rec:
            continue
        meta, _, path = rec.partition(b"\t")
        mode, _kind, oid = meta.decode().split()
        entries[path.decode("utf-8", "surrogateescape")] = (mode, oid)
    return entries


def check_tracked_symlink(link: str, target: bytes, entries: dict[str, tuple[str, str]]) -> str:
    """Return the in-tree path a tracked symlink resolves to, or raise ``CheckoutError``.

    The rule is deliberately strict: the target must be a canonical relative path (no absolute
    path, no ``..``, ``.`` or empty component, no backslash or NUL), resolved against the link's
    own directory inside the commit's tree, and it must name a tracked regular file (100644 or
    100755).  A target that is another symlink is refused (no chains, not even in-tree ones), as is
    a dangling target, a directory, or a submodule."""
    try:
        text = target.decode("utf-8")
    except UnicodeDecodeError:
        raise CheckoutError(f"tracked symlink {link}: target is not UTF-8") from None
    if not text or "\0" in text or "\\" in text or "\n" in text:
        raise CheckoutError(f"tracked symlink {link}: malformed target {text!r}")
    if text.startswith("/"):
        raise CheckoutError(f"tracked symlink {link}: absolute target {text!r} refused")
    parts = text.split("/")
    if ".." in parts:
        raise CheckoutError(f"tracked symlink {link}: '..' in target {text!r} refused (escape)")
    if any(c in ("", ".") for c in parts):
        raise CheckoutError(f"tracked symlink {link}: non-canonical target {text!r} refused")
    resolved = posixpath.join(posixpath.dirname(link), text)
    entry = entries.get(resolved)
    if entry is None:
        raise CheckoutError(f"tracked symlink {link}: dangling target {resolved!r} is not tracked")
    mode = entry[0]
    if mode == "120000":
        raise CheckoutError(f"tracked symlink {link}: target {resolved!r} is a symlink (chain refused)")
    if mode == "040000":
        raise CheckoutError(f"tracked symlink {link}: target {resolved!r} is a directory")
    if mode == "160000":
        raise CheckoutError(f"tracked symlink {link}: target {resolved!r} is a submodule")
    if mode not in REGULAR_MODES:
        raise CheckoutError(f"tracked symlink {link}: target {resolved!r} has mode {mode}")
    return resolved


def check_no_links_or_submodules(tree: Path) -> dict[str, str]:
    """Refuse submodules and unsafe tracked symlinks in HEAD; return the accepted symlinks as
    ``{link path: target text}``.  Each accepted link must also be materialised on disk as a real
    symlink whose target text matches the commit and resolves to the tracked file inside ``tree``."""
    tree = Path(tree)
    entries = _tree_entries(tree)
    if any(mode == "160000" for mode, _ in entries.values()):
        raise CheckoutError("submodule refused")
    if (tree / ".gitmodules").exists() or ".gitmodules" in entries:
        raise CheckoutError("submodule configuration refused")
    accepted = {}
    root = tree.resolve()
    for link, (mode, oid) in sorted(entries.items()):
        if mode != "120000":
            continue
        r = subprocess.run(["git", "cat-file", "blob", oid], cwd=tree, capture_output=True)
        if r.returncode:
            raise CheckoutError(f"tracked symlink {link}: cannot read target")
        resolved = check_tracked_symlink(link, r.stdout, entries)
        text = r.stdout.decode("utf-8")
        p = tree / link
        if not p.is_symlink() or os.readlink(p) != text:
            raise CheckoutError(f"tracked symlink {link}: not materialised as the committed symlink")
        real = p.resolve(strict=True)
        if real != root / resolved or real.is_symlink() or not real.is_file():
            raise CheckoutError(f"tracked symlink {link}: does not resolve to {resolved} in the tree")
        accepted[link] = text
    return accepted


def tool_hashes() -> dict:
    h = hashlib.sha256()
    for p in sorted(TOOL_DIR.glob("*.py")):
        h.update(p.name.encode() + b"\0" + p.read_bytes() + b"\0")
    return {
        "custody_tool_sha256": h.hexdigest(),
        "requirements_lock_sha256": hashlib.sha256(LOCKFILE.read_bytes()).hexdigest()
        if LOCKFILE.is_file()
        else None,
    }


def toolchain() -> dict:
    def v(args):
        try:
            return _run(args, check=False).splitlines()[0]
        except Exception:
            return "unavailable"

    return {
        "git": v(["git", "--version"]),
        "java": v(["java", "-version"]),
        "sbt_script": v(["sbt", "--version"]),
        "node": v(["node", "--version"]),
        "python": v(["python3", "--version"]),
    }


def sbt_state(home: Path) -> dict:
    """~/.sbt/repositories hash and the SHAs of the staged source-dependency clones."""
    sbt = Path(home) / ".sbt"
    rep = sbt / "repositories"
    staging = {}
    for d in sorted(sbt.glob("*/staging/*/*")):
        if (d / ".git").exists():
            r = subprocess.run(
                ["git", "-C", str(d), "rev-parse", "HEAD"],
                capture_output=True,
                text=True,
            )
            staging[d.relative_to(sbt).as_posix()] = (
                r.stdout.strip() if r.returncode == 0 else "unavailable"
            )
    return {
        "sbt_repositories_sha256": hashlib.sha256(rep.read_bytes()).hexdigest()
        if rep.is_file()
        else None,
        "sbt_staging_clones": staging,
    }


def stamp_for(tree: Path, sha: str, *, env=None, home: Path | None = None) -> dict:
    tree = Path(tree)
    head = _run(["git", "rev-parse", "HEAD"], cwd=tree)
    if head != sha:
        raise CheckoutError(f"HEAD {head} is not the reviewed SHA {sha}")
    check_clean(tree)
    links = check_no_links_or_submodules(tree)
    check_no_override(tree, env, home)
    bs = tree / "build.sbt"
    text = bs.read_text() if bs.is_file() else ""
    props = tree / "project" / "build.properties"
    gal = [ln.strip() for ln in text.splitlines() if "galeRevision" in ln][:5]
    running = tool_hashes()
    in_tree = tree / "tools" / "phrf-comparison" / "custody" / "phrf_custody"
    in_tree_hash = None
    if in_tree.is_dir():
        h = hashlib.sha256()
        for p in sorted(in_tree.glob("*.py")):
            h.update(p.name.encode() + b"\0" + p.read_bytes() + b"\0")
        in_tree_hash = h.hexdigest()
        if in_tree_hash != running["custody_tool_sha256"]:
            raise CheckoutError(
                "the custody tool in use differs from the one at the reviewed SHA"
            )
    return {
        "reviewed_sha": sha,
        "build_sbt_sha256": hashlib.sha256(text.encode()).hexdigest()
        if bs.is_file()
        else None,
        "galeRevision_lines": gal,
        "build_properties": props.read_text().strip() if props.is_file() else None,
        "toolchain": toolchain(),
        **running,
        **sbt_state(
            Path(env.get("HOME", Path.home()))
            if home is None and env
            else (home or Path.home())
        ),
        "custody_tool_sha256_in_checkout": in_tree_hash,
        "tracked_symlinks": links,
        "clean": True,
        "scalafim_override": False,
    }


def prepare(
    repo: Path,
    sha: str,
    checkout_dir: Path,
    stamp_path: Path,
    *,
    env=None,
    home: Path | None = None,
) -> dict:
    """Fresh ``git clone --no-hardlinks`` at ``sha``; verify; write the stamp.  The clone is
    removed on refusal."""
    if not re.fullmatch(r"[0-9a-f]{40}", sha):
        raise CheckoutError("reviewed SHA must be the full 40-hex commit id")
    check_no_override(Path(repo), env, home)
    if _run(["git", "rev-parse", "--verify", f"{sha}^{{commit}}"], cwd=repo) != sha:
        raise CheckoutError("SHA does not resolve to itself")
    if Path(checkout_dir).exists():
        raise CheckoutError("checkout directory exists")
    # no global hooks or templates may run or be copied during clone and checkout
    _run(
        [
            "git",
            *NOHOOKS,
            *SYMLINKS,
            "clone",
            "-q",
            "--no-hardlinks",
            "--no-checkout",
            "--template=",
            str(repo),
            str(checkout_dir),
        ]
    )
    try:
        _run(
            ["git", *NOHOOKS, *SYMLINKS, "checkout", "-q", "--detach", sha],
            cwd=checkout_dir,
        )
        stamp = stamp_for(Path(checkout_dir), sha, env=env, home=home)
    except CheckoutError:
        shutil.rmtree(checkout_dir, ignore_errors=True)
        raise
    Path(stamp_path).write_text(json.dumps(stamp, indent=1, sort_keys=True) + "\n")
    return stamp


def clean_env(env=None) -> dict:
    env = os.environ if env is None else env
    return {k: env[k] for k in ENV_ALLOWLIST if k in env}


def run_build(tree: Path, argv: list[str], env=None, home: Path | None = None) -> int:
    """Run ``argv`` in ``tree`` as ``env -i KEY=VAL... argv`` with the allowlisted variables only."""
    ce = clean_env(env)
    check_no_override(tree, ce, home)
    return subprocess.run(
        ["/usr/bin/env", "-i", *[f"{k}={v}" for k, v in ce.items()], *argv], cwd=tree
    ).returncode
