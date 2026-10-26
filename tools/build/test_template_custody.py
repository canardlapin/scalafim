"""Focused safety tests for template custody and template-only GC.

Run with: python3 -m unittest tools/build/test_template_custody.py
"""

import contextlib
import fcntl
import importlib.machinery
import importlib.util
import io
import os
import plistlib
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from types import SimpleNamespace
from pathlib import Path
from unittest import mock


HERE = Path(__file__).resolve().parent


def load(name, path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class CustodySuite(unittest.TestCase):
    def setUp(self):
        self.tc = load("template_custody_test", HERE / "template_custody.py")
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.cache = self.root / "cache"
        self.source = self.cache / "templates" / ("a" * 16)
        self.source.mkdir(parents=True)
        (self.source / ".git").mkdir()
        (self.source / ".git" / "index").write_text("tracked metadata")
        (self.source / ".ignored").write_text("untracked ignored payload")
        executable = self.source / "run"
        executable.write_text("#!/bin/sh\n")
        executable.chmod(0o755)
        os.symlink("run", self.source / "run-link")
        self.destination = self.root / "external" / self.source.name
        self.destination.parent.mkdir()

    def custody(self):
        with mock.patch.object(self.tc, "volume_uuid", return_value="VOL"):
            return self.tc.create_custody(self.source, self.destination, self.cache, "VOL")

    def test_full_mirror_preserves_git_ignored_symlink_and_mode(self):
        receipt = self.custody()
        self.assertEqual(receipt["source"], str(self.source.resolve()))
        self.assertTrue((self.destination / ".git" / "index").is_file())
        self.assertEqual((self.destination / ".ignored").read_text(), "untracked ignored payload")
        self.assertTrue((self.destination / "run-link").is_symlink())
        self.assertEqual(os.readlink(self.destination / "run-link"), "run")
        self.assertEqual((self.destination / "run").stat().st_mode & 0o777, 0o755)
        with mock.patch.object(self.tc, "volume_uuid", return_value="VOL"):
            self.assertEqual(self.tc.verify_custody(self.source, self.destination, self.cache, "VOL"), (True, "verified"))

    def test_changed_or_missing_custody_is_not_verified(self):
        self.custody()
        (self.destination / ".ignored").write_text("changed")
        with mock.patch.object(self.tc, "volume_uuid", return_value="VOL"):
            self.assertFalse(self.tc.verify_custody(self.source, self.destination, self.cache, "VOL")[0])
        missing = self.root / "external" / ("b" * 16)
        with mock.patch.object(self.tc, "volume_uuid", return_value="VOL"):
            self.assertFalse(self.tc.verify_custody(self.source, missing, self.cache, "VOL")[0])

    def test_volume_uuid_queries_mount_root_for_nested_destination(self):
        mount = (self.root / "mounted-volume").resolve()
        nested = mount / "a" / "deep" / "destination"
        nested.mkdir(parents=True)
        result = SimpleNamespace(returncode=0, stdout=plistlib.dumps({"VolumeUUID": "VOL"}))
        with mock.patch.object(self.tc.sys, "platform", "darwin"), mock.patch.object(
            self.tc.os.path, "ismount", side_effect=lambda path: Path(path) == mount
        ), mock.patch.object(self.tc.subprocess, "run", return_value=result) as run:
            self.assertEqual(self.tc.volume_uuid(nested), "VOL")
        self.assertEqual(run.call_args.args[0], ["diskutil", "info", "-plist", str(mount)])


class TemplateGcSuite(unittest.TestCase):
    def setUp(self):
        self.sw = load("sbt_warm_template_gc_test", HERE / "sbt-warm")
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name) / "worktree"
        (self.root / "project").mkdir(parents=True)
        (self.root / "build.sbt").write_text("")
        self.home = Path(self.tmp.name) / "home"
        self.sw.HOME = self.home
        (self.home / "templates").mkdir(parents=True)

    def template(self, key, marker=".pin-key-v2", marker_value=None):
        path = self.home / "templates" / key
        path.mkdir()
        if marker:
            (path / marker).write_text(marker_value or key)
        return path

    def gc(self, apply=False):
        out = io.StringIO()
        with contextlib.redirect_stdout(out), mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "template_is_active", return_value=(False, "inactive")
        ):
            self.assertEqual(self.sw.gc_templates(apply), 0)
        return out.getvalue()

    def test_dry_run_protects_orphan_legacy_reference_and_unsafe_candidates(self):
        protected = "a" * 16
        candidate = "b" * 16
        unsafe = "c" * 16
        self.template(protected)
        self.template(candidate)
        self.template(unsafe, marker=None)
        (self.home / "templates" / unsafe / ".pin-key-v2").mkdir()
        orphan = self.home / "bases" / "orphan"
        orphan.mkdir(parents=True)
        (orphan / ".pin-key").write_text(protected)
        out = self.gc()
        self.assertIn(f"keep    {protected}", out)
        self.assertIn(f"remove  {candidate}", out)
        self.assertIn(f"keep    {unsafe}", out)
        self.assertTrue((self.home / "templates" / candidate).exists())

    def test_malformed_second_marker_rejects_even_after_a_matching_first_marker(self):
        key = "2" * 16
        template = self.template(key)
        (template / ".pin-key").mkdir()
        out = self.gc()
        self.assertIn(f"keep    {key}", out)
        self.assertNotIn(f"remove  {key}", out)

    def test_apply_rejects_missing_or_invalid_custody_without_deleting(self):
        key = "d" * 16
        template = self.template(key)
        with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "template_is_active", return_value=(False, "inactive")
        ), mock.patch.object(self.sw, "lsof_apply_positive_control", return_value=(True, "observed")), mock.patch.object(
            self.sw, "custody_module"
        ) as custody:
            custody.return_value.verify_custody.return_value = (False, "missing receipt")
            self.assertEqual(self.sw.gc_templates(True, self.root / "external", "VOL"), 0)
        self.assertTrue(template.exists())

    def test_apply_aborts_when_lsof_positive_control_silently_reports_inactive(self):
        key = "3" * 16
        template = self.template(key)
        with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "lsof_apply_positive_control", return_value=(False, "positive control was not observed (inactive)")
        ), mock.patch.object(self.sw, "custody_module") as custody:
            self.assertEqual(self.sw.gc_templates(True, self.root / "external", "VOL"), 1)
        custody.assert_not_called()
        self.assertTrue(template.exists())

    def test_positive_control_rejects_a_silent_lsof_false_negative(self):
        with mock.patch.object(self.sw, "template_is_active", return_value=(False, "inactive")):
            observed, reason = self.sw.lsof_apply_positive_control(self.home / "templates")
        self.assertFalse(observed)
        self.assertIn("not observed", reason)

    def test_positive_control_accepts_production_probe_observation_of_child(self):
        with mock.patch.object(
            self.sw, "template_is_active", return_value=(True, "open file reported by lsof")
        ) as probe:
            observed, reason = self.sw.lsof_apply_positive_control(self.home / "templates")
        self.assertTrue(observed)
        self.assertIn("child open descendant", reason)
        self.assertEqual(probe.call_count, 1)

    def test_external_symlink_base_protects_current_and_legacy_keys(self):
        current_key = "4" * 16
        legacy_key = "5" * 16
        self.template(current_key)
        self.template(legacy_key, marker=".pin-key", marker_value=legacy_key)
        (self.home / "bases").mkdir()
        external = self.root / "external-retained-base"
        external.mkdir()
        (external / ".pin-key-v2").write_text(current_key)
        (external / ".pin-key").write_text(legacy_key)
        os.symlink(external, self.home / "bases" / "retained")
        with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "template_is_active", return_value=(False, "inactive")
        ), contextlib.redirect_stdout(io.StringIO()) as output:
            self.assertEqual(self.sw.gc_templates(False), 0)
        self.assertIn(f"keep    {current_key}", output.getvalue())
        self.assertIn(f"keep    {legacy_key}", output.getvalue())

    def test_broken_symlink_base_fails_closed_before_template_collection(self):
        key = "6" * 16
        template = self.template(key)
        (self.home / "bases").mkdir()
        os.symlink(self.root / "missing-retained-base", self.home / "bases" / "broken")
        with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "template_is_active", return_value=(False, "inactive")
        ):
            self.assertEqual(self.sw.gc_templates(False), 1)
        self.assertTrue(template.exists())

    def test_parse_keeps_template_gc_separate_from_base_gc(self):
        self.assertEqual(self.sw.parse(["--gc"]), ("gc", False))
        self.assertEqual(self.sw.parse(["--gc-templates"]), ("gc-templates", (False, None, None)))
        self.assertEqual(
            self.sw.parse(["--gc-templates", "--apply", "--custody-root", "/volume/c", "--volume-uuid", "VOL"]),
            ("gc-templates", (True, Path("/volume/c"), "VOL")),
        )
        self.assertEqual(
            self.sw.parse(["first/test", "--", "second/test"]),
            ("run", ["first/test", "second/test"]),
        )

    def test_activity_probe_retains_stderr_or_timeout_even_with_no_open_files(self):
        template = self.template("f" * 16)
        with mock.patch.object(
            self.sw.subprocess, "run", return_value=SimpleNamespace(returncode=1, stderr="walk failed")
        ):
            self.assertEqual(self.sw.template_is_active(template)[0], True)
        with mock.patch.object(
            self.sw.subprocess, "run", side_effect=subprocess.TimeoutExpired("lsof", 10)
        ):
            self.assertEqual(self.sw.template_is_active(template)[0], True)
        with mock.patch.object(
            self.sw.subprocess,
            "run",
            return_value=SimpleNamespace(returncode=1, stdout="p123\n", stderr=""),
        ):
            active, reason = self.sw.template_is_active(template)
        self.assertTrue(active)
        self.assertEqual(reason, "open file reported by lsof")

    def test_activity_probe_detects_real_open_descendant_file(self):
        template = self.template("1" * 16)
        child = template / "open-child"
        child.write_text("in use")
        code = (
            "import sys; "
            "handle = open(sys.argv[1]); "
            "print('ready', flush=True); "
            "sys.stdin.readline()"
        )
        process = subprocess.Popen(
            [sys.executable, "-c", code, str(child)],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        try:
            self.assertEqual(process.stdout.readline().strip(), "ready")
            active, reason = self.sw.template_is_active(template)
            if not active:
                self.skipTest(f"lsof cannot observe this child handle here: {reason}")
            self.assertTrue(active, reason)
            self.assertIn("open file", reason)
        finally:
            if process.stdin is not None:
                process.stdin.write("\n")
                process.stdin.flush()
                process.stdin.close()
            process.wait(timeout=5)
            if process.stdout is not None:
                process.stdout.close()
            if process.stderr is not None:
                process.stderr.close()

    def test_apply_waits_for_existing_bases_lock_before_final_removal(self):
        key = "e" * 16
        template = self.template(key)
        self.home.mkdir(exist_ok=True)
        held = self.sw.open_home_lock()
        fcntl.flock(held, fcntl.LOCK_EX)
        result = {}
        removed = threading.Event()

        def body():
            with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
                self.sw, "template_is_active", return_value=(False, "inactive")
            ), mock.patch.object(self.sw, "lsof_apply_positive_control", return_value=(True, "observed")), mock.patch.object(
                self.sw, "custody_module"
            ) as custody, mock.patch.object(
                self.sw.shutil, "rmtree", side_effect=lambda path, *args, **kwargs: removed.set()
            ):
                custody.return_value.verify_custody.return_value = (True, "verified")
                result["code"] = self.sw.gc_templates(True, self.root / "external", "VOL")

        thread = threading.Thread(target=body)
        thread.start()
        time.sleep(0.05)
        self.assertFalse(removed.is_set())
        fcntl.flock(held, fcntl.LOCK_UN)
        held.close()
        thread.join(5)
        self.assertFalse(thread.is_alive())
        self.assertEqual(result["code"], 0)
        self.assertTrue(removed.is_set())
        self.assertTrue(template.exists())  # rmtree was mocked: no test cache deletion.
