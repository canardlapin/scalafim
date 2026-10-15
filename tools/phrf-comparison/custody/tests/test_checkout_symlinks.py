"""Tracked-symlink rule of the clean checkout (S9 reconciliation finding, 2026-10-10).

A tracked symlink is accepted only as an in-tree alias of a tracked regular file: canonical relative
target, resolved against the link's directory in the commit's tree, no '..', no chains.  It is
materialised as a real symlink, so the checkout stays byte-identical to the commit (git status clean).
"""
import os
import subprocess
from pathlib import Path

import pytest

from phrf_custody import checkout as C


def git(repo, *a):
    return subprocess.run(["git", *a], cwd=repo, check=True, capture_output=True, text=True).stdout.strip()


@pytest.fixture
def repo(tmp_path):
    r = tmp_path / "repo"; r.mkdir()
    git(r, "init", "-q"); git(r, "config", "user.email", "t@example.org"); git(r, "config", "user.name", "t")
    (r / "build.sbt").write_text('val galeRevision = "abc"\n')
    (r / "AGENTS.md").write_text("# agents\n")
    (r / "run.sh").write_text("#!/bin/sh\n"); (r / "run.sh").chmod(0o755)
    (r / "docs").mkdir(); (r / "docs" / "guide.md").write_text("guide\n")
    git(r, "add", "-A"); git(r, "commit", "-qm", "init")
    return r


@pytest.fixture
def home(tmp_path):
    h = tmp_path / "home"; h.mkdir(); return h


def add_link(repo, path, target):
    p = repo / path
    p.parent.mkdir(parents=True, exist_ok=True)
    os.symlink(target, p)
    git(repo, "add", path); git(repo, "commit", "-qm", f"link {path}")


def prep(repo, tmp_path, home, name="wt"):
    return C.prepare(repo, git(repo, "rev-parse", "HEAD"), tmp_path / name, tmp_path / "stamp.json",
                     env={}, home=home)


# accepted -------------------------------------------------------------------------------------

@pytest.mark.parametrize("link,target", [
    ("CLAUDE.md", "AGENTS.md"),          # the case on main
    ("docs/alias.md", "guide.md"),       # resolved against the link's own directory
    ("x/y.md", "z/w.md"),                # target in a subdirectory of the link's directory
])
def test_in_tree_alias_of_regular_file_accepted(repo, tmp_path, home, link, target):
    if link == "x/y.md":
        (repo / "x" / "z").mkdir(parents=True); (repo / "x" / "z" / "w.md").write_text("w\n")
        git(repo, "add", "-A"); git(repo, "commit", "-qm", "w")
    add_link(repo, link, target)
    st = prep(repo, tmp_path, home)
    assert st["tracked_symlinks"] == {link: target}
    wt = tmp_path / "wt"
    p = wt / link
    assert p.is_symlink() and os.readlink(p) == target           # materialised as a symlink
    assert p.resolve().parent.is_relative_to(wt.resolve())
    assert git(wt, "status", "--porcelain") == ""                 # byte-identical to the commit


def test_executable_target_accepted_and_no_links_stamps_empty(repo, tmp_path, home):
    st = prep(repo, tmp_path, home, "wt0")
    assert st["tracked_symlinks"] == {}
    add_link(repo, "go", "run.sh")
    assert prep(repo, tmp_path, home, "wt1")["tracked_symlinks"] == {"go": "run.sh"}


# refused --------------------------------------------------------------------------------------

@pytest.mark.parametrize("link,target,why", [
    ("esc", "../outside", "'..'"),
    ("docs/esc", "../AGENTS.md", "'..'"),             # stays in-tree but '..' is refused outright
    ("inner", "docs/../AGENTS.md", "'..'"),
    ("abs", "/etc/passwd", "absolute"),
    ("dot", "./AGENTS.md", "non-canonical"),
    ("dbl", "docs//guide.md", "non-canonical"),
    ("trail", "docs/", "non-canonical"),
    ("dang", "missing.md", "dangling"),
    ("case", "agents.md", "dangling"),                 # no case-insensitive resolution
    ("dir", "docs", "directory"),
])
def test_unsafe_symlink_refused_and_clone_removed(repo, tmp_path, home, link, target, why):
    add_link(repo, link, target)
    with pytest.raises(C.CheckoutError, match=f"tracked symlink {link}: .*{why}"):
        prep(repo, tmp_path, home)
    assert not (tmp_path / "wt").exists()


def test_symlink_chain_refused_even_in_tree(repo, tmp_path, home):
    add_link(repo, "CLAUDE.md", "AGENTS.md")
    add_link(repo, "chain.md", "CLAUDE.md")
    with pytest.raises(C.CheckoutError, match="chain.md: .*chain refused"):
        prep(repo, tmp_path, home)


def test_symlink_to_submodule_refused():
    entries = {"sub": ("160000", "1" * 40), "AGENTS.md": ("100644", "2" * 40)}
    with pytest.raises(C.CheckoutError, match="submodule"):
        C.check_tracked_symlink("l", b"sub", entries)
    assert C.check_tracked_symlink("l", b"AGENTS.md", entries) == "AGENTS.md"
    with pytest.raises(C.CheckoutError, match="mode"):
        C.check_tracked_symlink("l", b"odd", {"odd": ("100664", "3" * 40)})


@pytest.mark.parametrize("raw,why", [(b"", "malformed"), (b"a\nb", "malformed"), (b"a\\b", "malformed"),
                                     (b"\xff", "UTF-8")])
def test_malformed_target_refused(raw, why):
    with pytest.raises(C.CheckoutError, match=why):
        C.check_tracked_symlink("l", raw, {"a": ("100644", "0" * 40)})


def test_link_not_materialised_as_symlink_refused(repo, tmp_path, home):
    add_link(repo, "CLAUDE.md", "AGENTS.md")
    prep(repo, tmp_path, home)
    wt = tmp_path / "wt"
    (wt / "CLAUDE.md").unlink(); (wt / "CLAUDE.md").write_text("AGENTS.md")  # as with core.symlinks=false
    with pytest.raises(C.CheckoutError, match="not materialised"):
        C.check_no_links_or_submodules(wt)


def test_retargeted_link_on_disk_refused(repo, tmp_path, home):
    add_link(repo, "CLAUDE.md", "AGENTS.md")
    prep(repo, tmp_path, home)
    wt = tmp_path / "wt"
    (wt / "CLAUDE.md").unlink(); os.symlink("build.sbt", wt / "CLAUDE.md")
    with pytest.raises(C.CheckoutError, match="not materialised"):
        C.check_no_links_or_submodules(wt)


def test_this_repository_tree_passes():
    """The enclosing scalafim checkout (current main tracks CLAUDE.md -> AGENTS.md) passes the rule."""
    root = Path(__file__).resolve().parents[4]
    assert (root / "build.sbt").is_file() and (root / "tools" / "phrf-comparison" / "custody").is_dir()
    links = C.check_no_links_or_submodules(root)
    assert links.get("CLAUDE.md") == "AGENTS.md"
    entries = C._tree_entries(root)
    assert set(links) == {p for p, (m, _) in entries.items() if m == "120000"}
