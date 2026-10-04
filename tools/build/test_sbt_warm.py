"""Tests for tools/build/sbt-warm (stdlib only).

Run with: python3 -m unittest tools/build/test_sbt_warm.py
"""

import contextlib
import hashlib
import importlib.machinery
import importlib.util
import io
import shutil
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).resolve().parent / "sbt-warm"
_REAL_RUN = subprocess.run


def load_module():
    loader = importlib.machinery.SourceFileLoader("sbt_warm", str(SCRIPT))
    spec = importlib.util.spec_from_loader("sbt_warm", loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


SHA_A = "a" * 40
SHA_B = "b" * 40
SHA_C = "c" * 40

BUILD = f"""
// Gale is pinned. Revision bumps are coordinated upstream.
lazy val galeRevision = "{SHA_A}"
lazy val galeBuild = uri(s"https://github.com/canardlapin/gale.git#$galeRevision")
lazy val resample4sRevision =
  "{SHA_B}"
lazy val resample4sBuild =
  uri(
    s"https://github.com/canardlapin/resample4s.git#$resample4sRevision"
  )
"""


class WarmCase(unittest.TestCase):
    def setUp(self):
        self.sw = load_module()
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        tmp = Path(self.tmp.name)
        self.home = tmp / "home"
        self.sw.HOME = self.home
        self.sw.STOP_SECONDS = 0
        self.root = tmp / "worktree"
        (self.root / "project").mkdir(parents=True)
        (self.root / "build.sbt").write_text(BUILD)
        (self.root / "project" / "build.properties").write_text("sbt.version=1.11.7\n")
        (self.root / "project" / "plugins.sbt").write_text(
            'addSbtPlugin("x" % "y" % "1")\n'
        )
        # Any real subprocess in a test is a bug; fail loudly instead.
        for name in ("call", "run"):
            patcher = mock.patch.object(
                self.sw.subprocess,
                name,
                side_effect=AssertionError(f"unexpected subprocess.{name}"),
            )
            patcher.start()
            self.addCleanup(patcher.stop)

    def key(self):
        return self.sw.pin_key(self.root)

    def edit_build(self, old, new):
        path = self.root / "build.sbt"
        text = path.read_text()
        self.assertIn(old, text)
        path.write_text(text.replace(old, new))

    def make_base(self):
        base = self.sw.base_for(self.root)
        (base / "staging" / "dep").mkdir(parents=True)
        (base / "staging" / "dep" / "classes").write_text("compiled")
        (base / ".pin-key").write_text(self.key())
        return base


class PinKeySuite(WarmCase):
    def test_split_line_revision_bump_changes_key(self):
        before = self.key()
        self.edit_build(SHA_B, SHA_C)
        self.assertNotEqual(before, self.key())

    def test_single_line_revision_bump_changes_key(self):
        before = self.key()
        self.edit_build(SHA_A, SHA_C)
        self.assertNotEqual(before, self.key())

    def test_comment_mentioning_revision_does_not_change_key(self):
        before = self.key()
        self.edit_build(
            "Revision bumps are coordinated upstream.",
            "Revision bumps land upstream first (see README).",
        )
        (self.root / "build.sbt").write_text(
            (self.root / "build.sbt").read_text() + "\n// Revision notes: none\n"
        )
        self.assertEqual(before, self.key())

    def test_new_repository_changes_key(self):
        before = self.key()
        (self.root / "build.sbt").write_text(
            BUILD + 'uri("https://github.com/canardlapin/alder.git")\n'
        )
        self.assertNotEqual(before, self.key())

    def test_plugins_change_changes_key(self):
        before = self.key()
        (self.root / "project" / "plugins.sbt").write_text(
            'addSbtPlugin("x" % "y" % "2")\n'
        )
        self.assertNotEqual(before, self.key())


class PrepareBaseSuite(WarmCase):
    def test_refuses_to_discard_base_while_server_alive(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        with mock.patch.object(
            self.sw, "server_alive", return_value=True
        ), mock.patch.object(self.sw, "run_client", return_value=0) as client:
            with self.assertRaises(RuntimeError):
                self.sw.prepare_base(self.root)
        client.assert_called_once_with(self.root, base, "shutdown")
        self.assertEqual((base / "staging" / "dep" / "classes").read_text(), "compiled")

    def test_discards_stale_base_once_server_stopped(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        alive = iter([True, False])
        with mock.patch.object(
            self.sw, "server_alive", side_effect=lambda *_: next(alive)
        ), mock.patch.object(self.sw, "run_client", return_value=0):
            self.assertEqual(self.sw.prepare_base(self.root), base)
        self.assertFalse((base / "staging").exists())
        self.assertEqual((base / ".pin-key").read_text(), self.key())

    def test_same_pins_keep_base_and_record_worktree(self):
        base = self.make_base()
        with mock.patch.object(
            self.sw,
            "server_alive",
            side_effect=AssertionError("no liveness check needed"),
        ):
            self.sw.prepare_base(self.root)
        self.assertTrue((base / "staging" / "dep" / "classes").exists())
        self.assertEqual((base / ".worktree").read_text(), str(self.root))
        self.assertTrue((base / "global.sbt").exists())


class CliSuite(WarmCase):
    def run_main(self, *argv):
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            code = self.sw.main(list(argv))
        return code, out.getvalue(), err.getvalue()

    def test_help_is_not_destructive(self):
        base = self.make_base()
        self.edit_build(
            SHA_B, SHA_C
        )  # a pin change that would otherwise discard the base
        for flag in ("--help", "-h"):
            with mock.patch.object(
                self.sw,
                "prepare_base",
                side_effect=AssertionError("prepare_base called"),
            ):
                code, out, _ = self.run_main(flag)
            self.assertEqual(code, 0)
            self.assertIn("usage:", out)
        self.assertTrue((base / "staging" / "dep" / "classes").exists())

    def test_unknown_option_rejected_without_side_effects(self):
        with mock.patch.object(
            self.sw, "prepare_base", side_effect=AssertionError("prepare_base called")
        ):
            code, _, err = self.run_main("--verbose", "compile")
        self.assertEqual(code, 2)
        self.assertIn("unknown option", err)
        self.assertFalse(self.home.exists())

    def test_misused_flags_rejected(self):
        for argv in (
            ["--apply"],
            ["--status", "--shutdown"],
            ["--status", "compile"],
            [],
        ):
            code, _, _ = self.run_main(*argv)
            self.assertEqual(code, 2, argv)

    def test_dash_dash_passes_commands_through(self):
        self.assertEqual(
            self.sw.parse(["--", "-v", "compile"]), ("run", ["-v", "compile"])
        )
        self.assertEqual(
            self.sw.parse(["a/test", "b/test"]), ("run", ["a/test", "b/test"])
        )
        self.assertEqual(self.sw.parse(["--gc"]), ("gc", False))
        self.assertEqual(self.sw.parse(["--gc", "--apply"]), ("gc", True))

    def test_shutdown_does_not_touch_base(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        before = sorted(str(p) for p in base.rglob("*"))
        with mock.patch.object(
            self.sw, "worktree", return_value=self.root
        ), mock.patch.object(
            self.sw, "server_running", return_value=True
        ), mock.patch.object(
            self.sw, "prepare_base", side_effect=AssertionError("prepare_base called")
        ), mock.patch.object(self.sw, "run_client", return_value=0) as client:
            code, _, _ = self.run_main("--shutdown")
        self.assertEqual(code, 0)
        client.assert_called_once_with(self.root, base, "shutdown")
        self.assertEqual(before, sorted(str(p) for p in base.rglob("*")))

    def test_shutdown_without_server_is_a_noop(self):
        with mock.patch.object(
            self.sw, "worktree", return_value=self.root
        ), mock.patch.object(
            self.sw, "server_running", return_value=False
        ), mock.patch.object(
            self.sw, "run_client", side_effect=AssertionError("client started")
        ):
            code, _, _ = self.run_main("--shutdown")
        self.assertEqual(code, 0)


class ClientIoSuite(WarmCase):
    def test_non_tty_forces_dumb_terminal_and_devnull_stdin(self):
        env = {"TERM": "xterm-256color"}
        with mock.patch.object(self.sw.sys, "stdin", io.StringIO()):
            extra = self.sw.client_io(env)
        self.assertEqual(env["TERM"], "dumb")
        self.assertEqual(extra, {"stdin": subprocess.DEVNULL})

    def test_tty_keeps_terminal(self):
        env = {"TERM": "xterm-256color"}
        tty = mock.Mock()
        tty.isatty.return_value = True
        with mock.patch.object(self.sw.sys, "stdin", tty):
            extra = self.sw.client_io(env)
        self.assertEqual(env["TERM"], "xterm-256color")
        self.assertEqual(extra, {})


class CloneTreeSuite(WarmCase):
    def source(self):
        source = Path(self.tmp.name) / "source"
        (source / "server" / "abc").mkdir(parents=True)
        (source / "staging").mkdir()
        (source / "staging" / "f").write_text("x")
        (source / ".worktree").write_text("/elsewhere")
        return source

    def test_failed_copy_is_cleaned_up_and_not_renamed(self):
        source = self.source()
        target = Path(self.tmp.name) / "target"

        def failing(command, **_):
            Path(command[-1]).mkdir()
            (Path(command[-1]) / "half").write_text("partial")
            return subprocess.CompletedProcess(
                command, 1, "", "No space left on device"
            )

        with mock.patch.object(self.sw.subprocess, "run", side_effect=failing) as run:
            with self.assertRaises(RuntimeError):
                self.sw.clone_tree(source, target)
        self.assertGreaterEqual(run.call_count, 1)
        self.assertFalse(target.exists())
        self.assertFalse(target.with_name("target.partial").exists())

    def test_falls_back_when_clonefile_unsupported(self):
        source = self.source()
        target = Path(self.tmp.name) / "target"
        calls = []

        def fake(command, **_):
            calls.append(command)
            if "-c" in command:
                return subprocess.CompletedProcess(
                    command, 1, "", "cp: clonefile failed: Operation not supported"
                )
            shutil.copytree(command[-2], command[-1], symlinks=True)
            return subprocess.CompletedProcess(command, 0, "", "")

        with mock.patch.object(self.sw.sys, "platform", "darwin"), mock.patch.object(
            self.sw.subprocess, "run", side_effect=fake
        ):
            self.sw.clone_tree(source, target)
        self.assertEqual([c[:2] for c in calls], [["cp", "-c"], ["cp", "-p"]])
        self.assertEqual((target / "staging" / "f").read_text(), "x")
        self.assertFalse((target / "server").exists())
        self.assertFalse((target / ".worktree").exists())

    def test_real_copy_excludes_server_and_record(self):
        source = self.source()
        target = Path(self.tmp.name) / "target"
        with mock.patch.object(self.sw.subprocess, "run", wraps=_REAL_RUN):
            self.sw.clone_tree(source, target)
        self.assertEqual((target / "staging" / "f").read_text(), "x")
        self.assertFalse((target / "server").exists())
        self.assertFalse((target / ".worktree").exists())
        self.assertFalse(target.with_name("target.partial").exists())


class GcSuite(WarmCase):
    def add_base(self, worktree_path, record=True):
        name = hashlib.sha256(str(worktree_path).encode()).hexdigest()[:16]
        base = self.home / "bases" / name
        (base / "staging").mkdir(parents=True)
        if record:
            (base / ".worktree").write_text(str(worktree_path))
        return base

    def run_gc(self, apply, alive=()):
        out = io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(
            io.StringIO()
        ), mock.patch.object(
            self.sw, "base_server_alive", side_effect=lambda base: base in alive
        ):
            self.assertEqual(self.sw.gc(apply=apply), 0)
        return out.getvalue()

    def test_dry_run_vs_apply(self):
        gone = Path(self.tmp.name) / "deleted-worktree"
        gone_live = Path(self.tmp.name) / "deleted-but-serving"
        orphan = self.add_base(gone)
        serving = self.add_base(gone_live)
        live = self.add_base(self.root)
        legacy = self.add_base(Path(self.tmp.name) / "legacy", record=False)
        mismatched = self.home / "bases" / "0000000000000000"
        mismatched.mkdir()
        (mismatched / ".worktree").write_text(str(gone))

        report = self.run_gc(apply=False, alive={serving})
        self.assertIn(f"remove  {orphan.name}", report)
        for base in (orphan, serving, live, legacy, mismatched):
            self.assertTrue(base.exists(), base)

        report = self.run_gc(apply=True, alive={serving})
        self.assertIn(f"removed {orphan.name}", report)
        self.assertFalse(orphan.exists())
        for base in (serving, live, legacy, mismatched):
            self.assertTrue(base.exists(), base)


class RunSuite(WarmCase):
    def test_template_snapshot_after_failed_command(self):
        base = self.sw.base_for(self.root)  # fresh worktree: no base yet
        codes = iter([0, 1])

        def client(root, base, command):
            (base / "staging" / "dep").mkdir(parents=True, exist_ok=True)
            return next(codes)

        with mock.patch.object(
            self.sw, "run_client", side_effect=client
        ) as run_client, mock.patch.object(
            self.sw, "server_running", return_value=True
        ), mock.patch.object(
            self.sw, "acquire_slot", return_value=io.StringIO()
        ), mock.patch.object(self.sw, "clone_tree") as clone:
            code = self.sw.run(self.root, ["a/compile", "b/test", "c/test"])
        self.assertEqual(code, 1)
        self.assertEqual(run_client.call_count, 2)
        clone.assert_called_once_with(base, self.home / "templates" / self.key())

    def test_no_snapshot_when_build_never_loaded(self):
        with mock.patch.object(
            self.sw, "run_client", return_value=1
        ), mock.patch.object(
            self.sw, "server_running", return_value=False
        ), mock.patch.object(
            self.sw, "acquire_slot", return_value=io.StringIO()
        ), mock.patch.object(
            self.sw, "clone_tree", side_effect=AssertionError("snapshot taken")
        ):
            self.assertEqual(self.sw.run(self.root, ["compile"]), 1)

    def test_no_snapshot_when_template_exists(self):
        (self.home / "templates" / self.key()).mkdir(parents=True)
        self.make_base()
        with mock.patch.object(
            self.sw, "run_client", return_value=0
        ), mock.patch.object(
            self.sw, "server_running", return_value=True
        ), mock.patch.object(
            self.sw, "acquire_slot", return_value=io.StringIO()
        ), mock.patch.object(
            self.sw, "clone_tree", side_effect=AssertionError("snapshot taken")
        ):
            self.assertEqual(self.sw.run(self.root, ["compile"]), 0)


class LivenessSuite(WarmCase):
    def test_stale_active_json_is_not_running(self):
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text(
            '{"uri":"local://%s"}' % (Path(self.tmp.name) / "missing" / "sock")
        )
        self.assertFalse(self.sw.server_running(self.root))

    def test_listening_socket_is_alive(self):
        import socket

        short = tempfile.mkdtemp(prefix="sw")  # unix socket paths are length-limited
        self.addCleanup(shutil.rmtree, short, True)
        path = Path(short) / "sock"
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(str(path))
        server.listen(1)
        try:
            self.assertTrue(self.sw.socket_alive(path))
        finally:
            server.close()
        self.assertFalse(self.sw.socket_alive(path))


if __name__ == "__main__":
    unittest.main()
