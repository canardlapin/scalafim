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
    (r / "project").mkdir(); (r / "project" / "build.properties").write_text("sbt.version=1.11.7\n")
    git(r, "add", "-A"); git(r, "commit", "-qm", "init")
    return r


@pytest.fixture
def home(tmp_path):
    h = tmp_path / "home"; h.mkdir(); return h


def prep(repo, tmp_path, home, name="wt", env=None, sha=None):
    return C.prepare(repo, sha or git(repo, "rev-parse", "HEAD"), tmp_path / name, tmp_path / "stamp.json",
                     env={} if env is None else env, home=home)


def test_happy_path_is_a_fresh_clone_and_stamps_everything(repo, tmp_path, home):
    sha = git(repo, "rev-parse", "HEAD")
    st = prep(repo, tmp_path, home)
    assert st["reviewed_sha"] == sha and st["clean"] and not st["scalafim_override"]
    assert st["galeRevision_lines"] == ['val galeRevision = "abc"']
    wt = tmp_path / "wt"
    assert git(wt, "rev-parse", "HEAD") == sha and (wt / ".git").is_dir()  # a clone, not a worktree
    assert not (wt / ".git").is_file()
    assert len(st["custody_tool_sha256"]) == 64 and len(st["requirements_lock_sha256"]) == 64
    assert sha in (tmp_path / "stamp.json").read_text()


def test_no_hardlinks_to_source_objects(repo, tmp_path, home):
    prep(repo, tmp_path, home)
    src = {p.stat().st_ino for p in (repo / ".git" / "objects").rglob("*") if p.is_file()}
    dst = [p.stat().st_ino for p in (tmp_path / "wt" / ".git" / "objects").rglob("*") if p.is_file()]
    assert dst and not (src & set(dst))


def test_dirty_tree_refused(repo, tmp_path, home):
    sha = git(repo, "rev-parse", "HEAD"); prep(repo, tmp_path, home)
    wt = tmp_path / "wt"
    (wt / "stray.txt").write_text("x")
    with pytest.raises(C.CheckoutError, match="not clean"):
        C.stamp_for(wt, sha, env={}, home=home)
    (wt / "stray.txt").unlink(); (wt / "build.sbt").write_text("changed\n")
    with pytest.raises(C.CheckoutError, match="not clean"):
        C.stamp_for(wt, sha, env={}, home=home)


def test_wrong_head_refused(repo, tmp_path, home):
    sha = git(repo, "rev-parse", "HEAD"); prep(repo, tmp_path, home)
    (repo / "f").write_text("1"); git(repo, "add", "f"); git(repo, "commit", "-qm", "2")
    with pytest.raises(C.CheckoutError, match="not the reviewed SHA"):
        C.stamp_for(tmp_path / "wt", git(repo, "rev-parse", "HEAD"), env={}, home=home)


def test_tracked_symlink_and_submodule_refused_and_clone_removed(repo, tmp_path, home):
    # an in-tree alias of a tracked file is accepted (test_checkout_symlinks.py); an escaping one is not
    os.symlink("../outside", repo / "link"); git(repo, "add", "link"); git(repo, "commit", "-qm", "ln")
    with pytest.raises(C.CheckoutError, match="symlink link: '..'"):
        prep(repo, tmp_path, home, "wt1")
    assert not (tmp_path / "wt1").exists()
    git(repo, "rm", "-q", "link"); git(repo, "commit", "-qm", "rm")
    git(repo, "update-index", "--add", "--cacheinfo", "160000", "0" * 39 + "1", "sub")
    git(repo, "commit", "-qm", "sub")
    with pytest.raises(C.CheckoutError, match="submodule"):
        prep(repo, tmp_path, home, "wt2")


@pytest.mark.parametrize("var", ["SBT_OPTS", "JAVA_TOOL_OPTIONS", "JAVA_OPTS"])
def test_override_in_environment_refused(repo, tmp_path, home, var):
    with pytest.raises(C.CheckoutError, match=var):
        prep(repo, tmp_path, home, env={var: "-Xmx1g -Dscalafim.gale.build=../gale"})
    with pytest.raises(C.CheckoutError, match=var):
        prep(repo, tmp_path, home, env={var: "-Dscalafim.anything=1"})
    assert not (tmp_path / "wt").exists()


def test_override_in_committed_opts_file_refused(repo, tmp_path, home):
    (repo / ".jvmopts").write_text("-Xmx6g -Dscalafim.gale.build=../gale\n")
    git(repo, "add", ".jvmopts"); git(repo, "commit", "-qm", "bad")
    with pytest.raises(C.CheckoutError, match=".jvmopts"):
        prep(repo, tmp_path, home)
    assert not (tmp_path / "wt").exists()


def test_global_sbt_config_and_home_opts_refused(repo, tmp_path, home):
    (home / ".sbt" / "1.0" / "plugins").mkdir(parents=True)
    (home / ".sbt" / "1.0" / "global.sbt").write_text('libraryDependencies += "x" % "gale" % "1"\n')
    with pytest.raises(C.CheckoutError, match="global sbt"):
        prep(repo, tmp_path, home)
    (home / ".sbt" / "1.0" / "global.sbt").unlink()
    (home / ".sbt" / "1.0" / "plugins" / "p.sbt").write_text("// -Dscalafim.gale.build=x\n")
    with pytest.raises(C.CheckoutError, match="global sbt"):
        prep(repo, tmp_path, home)
    (home / ".sbt" / "1.0" / "plugins" / "p.sbt").unlink()
    (home / ".sbtopts").write_text("-Dscalafim.gale.build=../gale\n")
    with pytest.raises(C.CheckoutError, match="~/.sbtopts"):
        prep(repo, tmp_path, home)
    (home / ".sbtopts").unlink()
    prep(repo, tmp_path, home)  # all clean now


def test_abbreviated_sha_and_branch_refused(repo, tmp_path, home):
    sha = git(repo, "rev-parse", "HEAD")
    for bad in (sha[:10], "HEAD", "master", "main"):
        with pytest.raises(C.CheckoutError, match="40-hex"):
            prep(repo, tmp_path, home, sha=bad)


def test_tool_hash_mismatch_with_checkout_refused(repo, tmp_path, home):
    d = repo / "tools" / "phrf-comparison" / "custody" / "phrf_custody"; d.mkdir(parents=True)
    (d / "x.py").write_text("print(1)\n"); git(repo, "add", "-A"); git(repo, "commit", "-qm", "tool")
    with pytest.raises(C.CheckoutError, match="custody tool"):
        prep(repo, tmp_path, home)


def test_build_runs_under_env_i_with_allowlist(repo, tmp_path, home, capfd):
    prep(repo, tmp_path, home)
    env = {"PATH": os.environ["PATH"], "HOME": str(home), "SECRET_TOKEN": "leak", "SBT_OPTS": "-Xmx1g"}
    assert C.clean_env(env) == {k: env[k] for k in ("PATH", "HOME")}
    rc = C.run_build(tmp_path / "wt", ["/usr/bin/env"], env=env, home=home)
    out = capfd.readouterr().out
    assert rc == 0 and "SECRET_TOKEN" not in out and "SBT_OPTS" not in out and "HOME=" in out
    with pytest.raises(C.CheckoutError):  # the global-config guard also runs at build time
        (home / ".sbtopts").write_text("-Dscalafim.x=1\n")
        C.run_build(tmp_path / "wt", ["/usr/bin/env"], env=env, home=home)
