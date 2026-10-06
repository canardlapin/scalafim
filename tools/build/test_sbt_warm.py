"""Tests for tools/build/sbt-warm (stdlib only).

Run with: python3 -m unittest tools/build/test_sbt_warm.py
"""

import contextlib
import fcntl
import hashlib
import importlib.machinery
import importlib.util
import io
import json
import shutil
import socket
import subprocess
import tempfile
import threading
import types
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).resolve().parent / "sbt-warm"
# The last main commit carrying the pre-2026-10-04 script (rmtree on a .pin-key mismatch).
LEGACY_COMMIT = "2223f13a"
_REAL_RUN = subprocess.run


def load_module(name="sbt_warm", path=SCRIPT):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def load_legacy_module():
    """main's pre-fix script, from git history; None when history is unavailable."""
    result = _REAL_RUN(
        [
            "git",
            "-C",
            str(SCRIPT.parent),
            "show",
            f"{LEGACY_COMMIT}:tools/build/sbt-warm",
        ],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    module = types.ModuleType("sbt_warm_legacy")
    exec(compile(result.stdout, "sbt-warm@" + LEGACY_COMMIT, "exec"), module.__dict__)
    return module


SHA_A = "a" * 40
SHA_B = "b" * 40
SHA_C = "c" * 40
SERVER_IDENTITY = {"uri": "local:///test/sock", "device": 1, "inode": 2, "ctime_ns": 3}

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
        """A base as this script leaves it after a run on the current pins."""
        base = self.sw.base_for(self.root)
        (base / "staging" / "dep").mkdir(parents=True)
        (base / "staging" / "dep" / "classes").write_text("compiled")
        (base / ".pin-key-v2").write_text(self.key())
        (base / ".pin-key").write_text(self.sw.legacy_pin_key(self.root))
        return base

    def run_main(self, *argv):
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            code = self.sw.main(list(argv))
        return code, out.getvalue(), err.getvalue()

    def short_socket_dir(self):
        short = tempfile.mkdtemp(prefix="sw")  # unix socket paths are length-limited
        self.addCleanup(shutil.rmtree, short, True)
        return Path(short)


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

    def test_build_properties_change_and_removal_change_key(self):
        before = self.key()
        (self.root / "project" / "build.properties").write_text("sbt.version=1.11.8\n")
        bumped = self.key()
        self.assertNotEqual(before, bumped)
        (self.root / "project" / "build.properties").unlink()
        self.assertNotIn(self.key(), (before, bumped))

    def test_legacy_key_misses_split_line_pin(self):
        # Documents why the v2 key exists: the legacy key cannot see this bump.
        before = self.sw.legacy_pin_key(self.root)
        self.edit_build(SHA_B, SHA_C)
        self.assertEqual(before, self.sw.legacy_pin_key(self.root))


class PrepareBaseSuite(WarmCase):
    def prepare(self):
        with mock.patch.object(
            self.sw,
            "server_alive",
            side_effect=AssertionError("prepare_base must not probe servers"),
        ), mock.patch.object(
            self.sw,
            "run_client",
            side_effect=AssertionError("prepare_base must not call the client"),
        ):
            return self.sw.prepare_base(self.root)

    def test_pin_change_keeps_base_and_rewrites_markers(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        self.assertEqual(self.prepare(), (base, self.key()))
        self.assertEqual((base / "staging" / "dep" / "classes").read_text(), "compiled")
        self.assertEqual((base / ".pin-key-v2").read_text(), self.key())
        self.assertEqual(
            (base / ".pin-key").read_text(), self.sw.legacy_pin_key(self.root)
        )

    def test_missing_markers_keep_base_and_are_written(self):
        base = self.make_base()
        (base / ".pin-key-v2").unlink()
        (base / ".pin-key").unlink()
        self.assertEqual(self.prepare(), (base, self.key()))
        self.assertTrue((base / "staging" / "dep" / "classes").exists())
        self.assertEqual((base / ".pin-key-v2").read_text(), self.key())
        self.assertEqual(
            (base / ".pin-key").read_text(), self.sw.legacy_pin_key(self.root)
        )

    def test_same_pins_keep_base_and_record_worktree(self):
        base = self.make_base()
        self.prepare()
        self.assertTrue((base / "staging" / "dep" / "classes").exists())
        self.assertEqual((base / ".worktree").read_text(), str(self.root))
        self.assertTrue((base / "global.sbt").exists())

    def seed_and_capture(self, *template_names):
        for name in template_names:
            (self.home / "templates" / name).mkdir(parents=True)
        with mock.patch.object(
            self.sw,
            "clone_tree",
            side_effect=lambda source, target: target.mkdir(parents=True),
        ) as clone:
            base, _ = self.prepare()
        return base, clone

    def test_new_base_seeds_from_v2_template_first(self):
        base, clone = self.seed_and_capture(
            self.key(), self.sw.legacy_pin_key(self.root)
        )
        clone.assert_called_once_with(self.home / "templates" / self.key(), base)

    def test_new_base_falls_back_to_legacy_template(self):
        base, clone = self.seed_and_capture(self.sw.legacy_pin_key(self.root))
        clone.assert_called_once_with(
            self.home / "templates" / self.sw.legacy_pin_key(self.root), base
        )

    def test_new_base_without_template_is_empty(self):
        base, clone = self.seed_and_capture()
        clone.assert_not_called()
        self.assertTrue(base.is_dir())


class LegacyScriptSafetySuite(WarmCase):
    """A base touched by this script must survive older copies of the script."""

    def touched_base(self):
        base = self.make_base()
        (base / ".pin-key").unlink()
        (base / ".pin-key-v2").unlink()
        self.edit_build(SHA_B, SHA_C)  # a split-line bump: the v2 key changes
        with mock.patch.object(self.sw, "server_alive", return_value=True):
            self.sw.prepare_base(self.root)
        return base

    def test_marker_matches_the_legacy_comparison(self):
        base = self.touched_base()
        # The legacy rmtree condition: `not marker.exists() or marker.read_text() != key`.
        marker = base / ".pin-key"
        self.assertTrue(marker.exists())
        self.assertEqual(marker.read_text(), self.sw.legacy_pin_key(self.root))

    def test_legacy_prepare_base_leaves_base_alone(self):
        legacy = load_legacy_module()
        if legacy is None:
            self.skipTest(f"git history for {LEGACY_COMMIT} is unavailable")
        base = self.touched_base()
        legacy.HOME = self.home
        legacy.prepare_base(self.root)
        self.assertEqual((base / "staging" / "dep" / "classes").read_text(), "compiled")
        # And the legacy code really is destructive on a mismatch, so the check above has teeth.
        (base / ".pin-key").write_text("stale")
        with contextlib.redirect_stderr(io.StringIO()):
            legacy.prepare_base(self.root)
        self.assertFalse((base / "staging").exists())


class CliSuite(WarmCase):
    def test_help_is_not_destructive(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        for flag in ("--help", "-h"):
            with mock.patch.object(
                self.sw,
                "prepare_base",
                side_effect=AssertionError("prepare_base called"),
            ), mock.patch.object(
                self.sw, "worktree", side_effect=AssertionError("git called")
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


class ShutdownSuite(WarmCase):
    def shutdown(self, alive_sequence, client=None):
        alive = iter(alive_sequence)
        target = self.root / "project" / "target"
        target.mkdir(exist_ok=True)
        (target / "active.json").write_text(json.dumps({"uri": "local:///test/sock"}))
        with mock.patch.object(
            self.sw, "worktree", return_value=self.root
        ), mock.patch.object(
            self.sw, "server_alive", side_effect=lambda *_: next(alive)
        ), mock.patch.object(
            self.sw, "server_running", return_value=True
        ), mock.patch.object(
            self.sw, "prepare_base", side_effect=AssertionError("prepare_base called")
        ), mock.patch.object(self.sw, "run_client", return_value=0) as run_client:
            code, _, _ = self.run_main("--shutdown")
        return code, run_client

    def test_shutdown_does_not_touch_base_and_has_a_timeout(self):
        base = self.make_base()
        self.edit_build(SHA_B, SHA_C)
        before = sorted(str(p) for p in base.rglob("*"))
        code, client = self.shutdown([True, True, False])
        self.assertEqual(code, 0)
        client.assert_called_once_with(
            self.root, base, "shutdown", timeout=self.sw.STOP_SECONDS
        )
        self.assertEqual(before, sorted(str(p) for p in base.rglob("*")))

    def test_shutdown_without_server_is_a_noop(self):
        code, client = self.shutdown([False])
        self.assertEqual(code, 0)
        client.assert_not_called()

    def test_shutdown_reports_a_server_that_stays_alive(self):
        code, _ = self.shutdown([True] * 10)
        self.assertEqual(code, 1)

    def test_server_alive_consults_base_sockets(self):
        # Without the base-socket half, a server with a stale active.json reads as dead.
        base = self.make_base()
        with mock.patch.object(
            self.sw, "server_running", return_value=False
        ), mock.patch.object(
            self.sw, "base_server_alive", side_effect=lambda b: b == base
        ):
            self.assertTrue(self.sw.server_alive(self.root, base))

    def test_shutdown_repoints_stale_active_json_at_live_base_socket(self):
        short = self.short_socket_dir()
        self.sw.HOME = short / "h"
        base = self.make_base()
        sock_dir = base / "server" / "x"
        sock_dir.mkdir(parents=True)
        listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        listener.bind(str(sock_dir / "sock"))
        listener.listen(16)  # probes are never accepted; keep the backlog open
        self.addCleanup(listener.close)
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text(
            json.dumps({"uri": "local:///nonexistent/sock"})
        )

        def client(root, base_, command, timeout=None):
            listener.close()
            (sock_dir / "sock").unlink()
            return 0

        with mock.patch.object(
            self.sw, "worktree", return_value=self.root
        ), mock.patch.object(self.sw, "run_client", side_effect=client) as run_client:
            code, _, _ = self.run_main("--shutdown")
        self.assertEqual(code, 0)
        run_client.assert_called_once()
        self.assertEqual(
            json.loads((target / "active.json").read_text())["uri"],
            f"local://{sock_dir / 'sock'}",
        )

    def test_shutdown_repairs_malformed_active_records_from_single_live_base_socket(self):
        self.sw.HOME = self.short_socket_dir() / "h"
        base = self.make_base()
        sock_dir = base / "server" / "x"
        sock_dir.mkdir(parents=True)
        target = self.root / "project" / "target"
        target.mkdir()
        for payload in ("[]", "null", "1", '{"uri":1}', "{broken"):
            with self.subTest(payload=payload):
                listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                listener.bind(str(sock_dir / "sock"))
                listener.listen(32)
                self.addCleanup(listener.close)
                (target / "active.json").write_text(payload)

                def client(root, base_, command, timeout=None):
                    listener.close()
                    (sock_dir / "sock").unlink()
                    return 0

                with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
                    self.sw, "run_client", side_effect=client
                ) as run_client:
                    code, _, _ = self.run_main("--shutdown")
                self.assertEqual(code, 0)
                run_client.assert_called_once_with(self.root, base, "shutdown", timeout=self.sw.STOP_SECONDS)
                self.assertEqual(self.sw.active_uri(self.root), f"local://{sock_dir / 'sock'}")

    def test_shutdown_does_not_launch_client_for_ambiguous_base_with_malformed_active(self):
        base = self.make_base()
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text("[]")
        with mock.patch.object(self.sw, "live_base_sockets", return_value=[Path("/a"), Path("/b")]), mock.patch.object(
            self.sw, "run_client", side_effect=AssertionError("replacement server started")
        ):
            self.assertFalse(self.sw.stop_server(self.root, base))
        self.assertEqual((target / "active.json").read_text(), "[]")


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

    def test_run_client_passes_devnull_and_dumb_term_without_tty(self):
        base = self.make_base()
        with mock.patch.object(self.sw.sys, "stdin", io.StringIO()), mock.patch.dict(
            self.sw.os.environ, {"TERM": "xterm"}
        ), mock.patch.object(self.sw.subprocess, "call", return_value=0) as call:
            self.assertEqual(self.sw.run_client(self.root, base, "compile"), 0)
        args, kwargs = call.call_args
        self.assertEqual(args[0], ["sbt", "--client", "compile"])
        self.assertIs(kwargs["stdin"], subprocess.DEVNULL)
        self.assertEqual(kwargs["env"]["TERM"], "dumb")
        self.assertIn(f"-Dsbt.global.base={base}", kwargs["env"]["SBT_OPTS"])

    def test_run_client_timeout_returns_124(self):
        with mock.patch.object(
            self.sw.subprocess, "call", side_effect=subprocess.TimeoutExpired("sbt", 1)
        ), contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(
                self.sw.run_client(self.root, self.make_base(), "shutdown", timeout=1),
                124,
            )


class CloneTreeSuite(WarmCase):
    def source(self):
        source = Path(self.tmp.name) / "source"
        (source / "server" / "abc").mkdir(parents=True)
        (source / "staging").mkdir()
        (source / "staging" / "f").write_text("x")
        (source / ".worktree").write_text("/elsewhere")
        (source / self.sw.STARTUP_RECORD).write_text("server metadata must not be cloned")
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

        with mock.patch.object(
            self.sw.subprocess, "run", side_effect=failing
        ) as run, contextlib.redirect_stderr(io.StringIO()):
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
        ), contextlib.redirect_stderr(io.StringIO()):
            self.sw.clone_tree(source, target)
        self.assertEqual([c[:2] for c in calls], [["cp", "-c"], ["cp", "-p"]])
        self.assertEqual((target / "staging" / "f").read_text(), "x")
        self.assertFalse((target / "server").exists())
        self.assertFalse((target / ".worktree").exists())
        self.assertFalse((target / self.sw.STARTUP_RECORD).exists())

    def test_real_copy_excludes_server_and_record(self):
        source = self.source()
        target = Path(self.tmp.name) / "target"
        with mock.patch.object(self.sw.subprocess, "run", wraps=_REAL_RUN):
            self.sw.clone_tree(source, target)
        self.assertEqual((target / "staging" / "f").read_text(), "x")
        self.assertFalse((target / "server").exists())
        self.assertFalse((target / ".worktree").exists())
        self.assertFalse((target / self.sw.STARTUP_RECORD).exists())
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
        orphan = self.add_base(gone)
        serving = self.add_base(Path(self.tmp.name) / "deleted-but-serving")
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


class ResourceConfigSuite(WarmCase):
    @contextlib.contextmanager
    def live(self, record=True, identity=SERVER_IDENTITY):
        base = self.make_base()
        (self.home / "templates" / self.key()).mkdir(parents=True)
        if record:
            self.sw.write_startup(base, self.sw.requested_config(), SERVER_IDENTITY)
        with mock.patch.object(self.sw, "worktree", return_value=self.root), mock.patch.object(
            self.sw, "server_alive", return_value=True
        ), mock.patch.object(self.sw, "server_identity", return_value=identity):
            yield base

    def test_matching_warm_server_runs_without_replacing_startup(self):
        with self.live() as base, mock.patch.object(
            self.sw, "run_client", return_value=0
        ) as client, mock.patch.object(
            self.sw, "acquire_slot", return_value=io.StringIO()
        ), mock.patch.object(self.sw, "write_startup", side_effect=AssertionError("warm write")):
            code, _, _ = self.run_main("compile")
        self.assertEqual(code, 0)
        client.assert_called_once_with(self.root, base, "compile")

    def assert_refused_before_mutation(self):
        with mock.patch.object(
            self.sw, "prepare_base", side_effect=AssertionError("base modified")
        ), mock.patch.object(
            self.sw, "run_client", side_effect=AssertionError("client called")
        ), mock.patch.object(self.sw, "acquire_slot", side_effect=AssertionError("slot acquired")):
            code, _, err = self.run_main("compile")
        self.assertEqual(code, 1)
        self.assertIn("--shutdown", err)
        return err

    def test_each_requested_launch_setting_mismatch_refuses_before_mutation(self):
        for setting, wanted in (("HEAP", "4g" if self.sw.HEAP != "4g" else "5g"), ("CPUS", "8" if self.sw.CPUS != "8" else "9"), ("IDLE_MINUTES", self.sw.IDLE_MINUTES + 1)):
            with self.subTest(setting=setting), self.live(), mock.patch.object(self.sw, setting, wanted):
                err = self.assert_refused_before_mutation()
                self.assertIn("differs", err)
                self.assertIn(str(wanted), err)
            shutil.rmtree(self.home)

    def test_missing_corrupt_and_unbound_record_refuse(self):
        for payload in (None, "{broken", "[]", '{"config": null}', json.dumps({
            "config": self.sw.requested_config(), "identity": {**SERVER_IDENTITY, "inode": 99}
        })):
            with self.subTest(payload=payload), self.live(record=False) as base:
                if payload is not None:
                    (base / self.sw.STARTUP_RECORD).write_text(payload)
                self.assertIn("unknown", self.assert_refused_before_mutation())
            shutil.rmtree(self.home)

    def test_unaddressable_live_server_refuses_even_with_record(self):
        with self.live(identity=None):
            self.assertIn("unknown", self.assert_refused_before_mutation())

    def test_malformed_active_records_are_unknown_for_run_status_and_shutdown(self):
        base = self.make_base()
        target = self.root / "project" / "target"
        target.mkdir()
        for payload in ("[]", "null", "1", '{"uri":1}', "{}", "{broken"):
            with self.subTest(payload=payload), mock.patch.object(self.sw, "worktree", return_value=self.root):
                (target / "active.json").write_text(payload)
                self.assertTrue(self.sw.server_running(self.root))
                self.assertIsNone(self.sw.server_identity(self.root))
                self.assertIn("unknown", self.assert_refused_before_mutation())
                code, out, _ = self.run_main("--status")
                self.assertEqual(code, 0)
                self.assertIn("config   unknown", out)
                with mock.patch.object(self.sw, "run_client", side_effect=AssertionError("replacement server started")):
                    self.assertEqual(self.run_main("--shutdown")[0], 1)
                self.assertEqual((target / "active.json").read_text(), payload)
        self.assertTrue(base.exists())

    def test_nonlocal_active_uri_is_kept_and_treated_as_unknown(self):
        base = self.make_base()
        target = self.root / "project" / "target"
        target.mkdir()
        payload = json.dumps({"uri": "tcp://127.0.0.1:12345"})
        (target / "active.json").write_text(payload)
        with mock.patch.object(self.sw, "worktree", return_value=self.root):
            self.assertTrue(self.sw.server_running(self.root))
            self.assertIsNone(self.sw.server_identity(self.root))
            self.assertIn("unknown", self.assert_refused_before_mutation())
            self.assertIn("config   unknown", self.run_main("--status")[1])
            with mock.patch.object(self.sw, "live_base_sockets", return_value=[Path("/a")]), mock.patch.object(
                self.sw, "run_client", side_effect=AssertionError("nonlocal client called")
            ):
                self.assertFalse(self.sw.stop_server(self.root, base))
            self.assertEqual((target / "active.json").read_text(), payload)

    def test_status_reports_requests_and_matching_or_unknown_startup(self):
        with self.live() as base:
            code, out, _ = self.run_main("--status")
            self.assertEqual(code, 0)
            self.assertIn("config   matches", out)
            self.assertIn("not verified effective JVM flags", out)
            with mock.patch.object(self.sw, "HEAP", "4g"):
                _, out, _ = self.run_main("--status")
            self.assertIn("config   differs", out)
            (base / self.sw.STARTUP_RECORD).unlink()
            _, out, _ = self.run_main("--status")
            self.assertIn("recorded startup config unknown", out)

    def test_shutdown_ignores_unknown_or_different_startup(self):
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text(json.dumps({"uri": "local:///test/sock"}))
        with self.live(record=False), mock.patch.object(self.sw, "stop_server", return_value=True) as stop:
            self.assertEqual(self.run_main("--shutdown")[0], 0)
            stop.assert_called_once()
        shutil.rmtree(self.home)
        with mock.patch.object(self.sw, "HEAP", "4g"), self.live(), mock.patch.object(
            self.sw, "CPUS", "8"
        ), mock.patch.object(self.sw, "stop_server", return_value=True) as stop:
            self.assertEqual(self.run_main("--shutdown")[0], 0)
            stop.assert_called_once()

    def test_dead_failed_launch_removes_stale_record_and_records_nothing(self):
        base = self.make_base()
        self.sw.write_startup(base, self.sw.requested_config(), SERVER_IDENTITY)
        with mock.patch.object(self.sw, "server_alive", return_value=False), mock.patch.object(
            self.sw, "server_identity", return_value=None
        ), mock.patch.object(self.sw, "server_running", return_value=False), mock.patch.object(
            self.sw, "run_client", return_value=1
        ), mock.patch.object(self.sw, "acquire_slot", return_value=io.StringIO()):
            self.assertEqual(self.sw.run(self.root, ["compile"]), 1)
        self.assertFalse((base / self.sw.STARTUP_RECORD).exists())

    def test_shutdown_between_commands_reenters_serialized_cold_startup(self):
        alive = {"value": False, "generation": 0}
        calls = []

        def identity(_):
            return {**SERVER_IDENTITY, "inode": alive["generation"]} if alive["value"] else None

        def client(root, base, command):
            calls.append(command)
            if command == "shutdown":
                alive["value"] = False
            else:
                alive["value"] = True
                alive["generation"] += 1
            return 0

        (self.home / "templates" / self.key()).mkdir(parents=True)
        self.make_base()
        with mock.patch.object(self.sw, "server_alive", side_effect=lambda *_: alive["value"]), mock.patch.object(
            self.sw, "server_identity", side_effect=identity
        ), mock.patch.object(self.sw, "run_client", side_effect=client), mock.patch.object(
            self.sw, "acquire_slot", return_value=io.StringIO()
        ), mock.patch.object(self.sw, "prepare_base", wraps=self.sw.prepare_base) as prepare:
            self.assertEqual(self.sw.run(self.root, ["compile", "shutdown", "test"]), 0)
        self.assertEqual(calls, ["compile", "shutdown", "test"])
        self.assertEqual(prepare.call_count, 2)
        self.assertEqual(self.sw.read_startup(self.sw.base_for(self.root))["identity"]["inode"], 2)

    def test_same_uri_socket_restart_invalidates_startup(self):
        sock = self.short_socket_dir() / "sock"
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text(json.dumps({"uri": f"local://{sock}"}))
        base = self.make_base()
        first = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        first.bind(str(sock))
        first.listen(32)
        try:
            original = self.sw.server_identity(self.root)
            self.assertIsNotNone(original)
            self.sw.write_startup(base, self.sw.requested_config(), original)
        finally:
            first.close()
        sock.unlink()
        second = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        second.bind(str(sock))
        second.listen(32)
        self.addCleanup(second.close)
        current = self.sw.server_identity(self.root)
        self.assertEqual(original["uri"], current["uri"])
        self.assertNotEqual(original, current)
        with self.assertRaisesRegex(RuntimeError, "unknown"):
            self.sw.require_matching_startup(self.root, base, self.sw.requested_config())


class WorktreeLockSuite(WarmCase):
    """Real fcntl locks: a run in one thread versus shutdown/gc in another."""

    def start_run(self, root):
        entered, release = threading.Event(), threading.Event()
        result = {}
        base = self.make_base()
        self.sw.write_startup(base, self.sw.requested_config(), SERVER_IDENTITY)

        def client(root_, base, command, timeout=None):
            entered.set()
            release.wait(10)
            return 0

        def body():
            result["code"] = self.sw.run(root, ["compile"])

        thread = threading.Thread(target=body)
        patches = [
            mock.patch.object(self.sw, "run_client", side_effect=client),
            mock.patch.object(self.sw, "acquire_slot", return_value=io.StringIO()),
            mock.patch.object(self.sw, "server_running", return_value=False),
            mock.patch.object(self.sw, "server_alive", return_value=True),
            mock.patch.object(self.sw, "server_identity", return_value=SERVER_IDENTITY),
        ]
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)
        with contextlib.redirect_stderr(io.StringIO()):
            thread.start()
            self.assertTrue(entered.wait(10))
        return thread, release, result

    def test_shutdown_refuses_while_a_run_holds_the_lock(self):
        thread, release, result = self.start_run(self.root)
        try:
            with mock.patch.object(
                self.sw, "worktree", return_value=self.root
            ), mock.patch.object(
                self.sw,
                "stop_server",
                side_effect=AssertionError("stopped a busy server"),
            ):
                code, _, err = self.run_main("--shutdown")
            self.assertEqual(code, 1)
            self.assertIn("commands are running", err)
        finally:
            release.set()
            thread.join(10)
        self.assertEqual(result["code"], 0)
        with mock.patch.object(
            self.sw, "worktree", return_value=self.root
        ), mock.patch.object(self.sw, "server_alive", return_value=False):
            code, _, _ = self.run_main("--shutdown")
        self.assertEqual(code, 0)

    def test_concurrent_runs_share_the_lock(self):
        base = self.sw.base_for(self.root)
        thread, release, _ = self.start_run(self.root)
        try:
            with self.sw.worktree_lock(base) as other, self.sw.worktree_lock(
                base
            ) as third:
                # A second shared holder gets in without blocking (raises otherwise)...
                fcntl.flock(other, fcntl.LOCK_SH | fcntl.LOCK_NB)
                # ...but an exclusive claim does not.
                self.assertFalse(self.sw.try_exclusive(third))
        finally:
            release.set()
            thread.join(10)

    def test_two_warm_runs_reach_clients_together(self):
        base = self.make_base()
        self.sw.write_startup(base, self.sw.requested_config(), SERVER_IDENTITY)
        entered = {name: threading.Event() for name in ("compile", "test")}
        release = threading.Event()
        results = {}

        def client(root, base_, command):
            entered[command].set()
            self.assertTrue(release.wait(10))
            return 0

        def body(command):
            try:
                results[command] = self.sw.run(self.root, [command])
            except BaseException as error:
                results[command] = error

        with mock.patch.object(self.sw, "server_alive", return_value=True), mock.patch.object(
            self.sw, "server_identity", return_value=SERVER_IDENTITY
        ), mock.patch.object(self.sw, "run_client", side_effect=client), mock.patch.object(
            self.sw, "acquire_slot", side_effect=io.StringIO
        ), mock.patch.object(self.sw, "server_running", return_value=False):
            threads = [threading.Thread(target=body, args=(command,)) for command in entered]
            try:
                for thread in threads:
                    thread.start()
                for event in entered.values():
                    self.assertTrue(event.wait(10))
                with self.sw.worktree_lock(base) as held:
                    self.assertFalse(self.sw.try_exclusive(held))
            finally:
                release.set()
                for thread in threads:
                    thread.join(10)
                    self.assertFalse(thread.is_alive())
        self.assertEqual(results, {"compile": 0, "test": 0})

    def test_different_cold_configs_cannot_both_start(self):
        other = load_module("sbt_warm_other")
        other.HOME = self.home
        other.HEAP = "4g" if self.sw.HEAP != "4g" else "5g"
        base = self.make_base()
        entered, release, contender = threading.Event(), threading.Event(), threading.Event()
        alive = {"value": False}
        results = {}

        def launch(root, base_, command):
            entered.set()
            self.assertTrue(release.wait(10))
            alive["value"] = True
            return 0

        def body(module, name):
            if name == "other":
                contender.set()
            try:
                results[name] = module.run(self.root, ["compile"])
            except BaseException as error:
                results[name] = error

        with contextlib.ExitStack() as stack:
            for module in (self.sw, other):
                stack.enter_context(mock.patch.object(module, "server_alive", side_effect=lambda *_: alive["value"]))
                stack.enter_context(mock.patch.object(module, "server_identity", side_effect=lambda _: SERVER_IDENTITY if alive["value"] else None))
                stack.enter_context(mock.patch.object(module, "server_running", return_value=False))
                stack.enter_context(mock.patch.object(module, "acquire_slot", side_effect=io.StringIO))
            stack.enter_context(mock.patch.object(self.sw, "run_client", side_effect=launch))
            unwanted = stack.enter_context(mock.patch.object(other, "run_client", side_effect=AssertionError("second cold launch")))
            threads = [threading.Thread(target=body, args=(module, name)) for module, name in ((self.sw, "first"), (other, "other"))]
            try:
                threads[0].start()
                self.assertTrue(entered.wait(10))
                # A real second descriptor cannot take even a shared startup lock.
                with self.sw.worktree_lock(base) as held:
                    with self.assertRaises(BlockingIOError):
                        fcntl.flock(held, fcntl.LOCK_SH | fcntl.LOCK_NB)
                threads[1].start()
                self.assertTrue(contender.wait(10))
            finally:
                release.set()
                for thread in threads:
                    if thread.ident is not None:
                        thread.join(10)
                        self.assertFalse(thread.is_alive())
            unwanted.assert_not_called()
        self.assertEqual(results["first"], 0)
        self.assertIsInstance(results["other"], RuntimeError)
        self.assertIn("differs", str(results["other"]))
        self.assertEqual(self.sw.read_startup(base)["config"]["heap"], self.sw.HEAP)

    def test_gc_apply_skips_a_base_whose_lock_is_held(self):
        gone = Path(self.tmp.name) / "removed-worktree"
        base = self.sw.base_for(gone)
        (base / "staging").mkdir(parents=True)
        (base / ".worktree").write_text(str(gone))
        holder = self.sw.worktree_lock(base)
        self.addCleanup(holder.close)
        fcntl.flock(holder, fcntl.LOCK_SH)
        out = io.StringIO()
        with contextlib.redirect_stdout(out), mock.patch.object(
            self.sw, "base_server_alive", return_value=False
        ):
            self.sw.gc(apply=True)
        self.assertTrue(base.exists())
        self.assertIn("holds its worktree lock", out.getvalue())
        holder.close()
        with contextlib.redirect_stdout(io.StringIO()), mock.patch.object(
            self.sw, "base_server_alive", return_value=False
        ):
            self.sw.gc(apply=True)
        self.assertFalse(base.exists())


class RunSuite(WarmCase):
    def run_with(self, codes, server_running=True, before_first=None):
        codes = iter(codes)
        state = {"alive": False}

        def client(root, base, command, timeout=None):
            (base / "staging" / "dep").mkdir(parents=True, exist_ok=True)
            if before_first is not None:
                before_first()
            state["alive"] = server_running
            return next(codes)

        @contextlib.contextmanager
        def running():
            with mock.patch.object(
                self.sw, "server_running", side_effect=lambda _: state["alive"]
            ), mock.patch.object(
                self.sw, "server_alive", side_effect=lambda *_: state["alive"]
            ), mock.patch.object(
                self.sw, "server_identity", side_effect=lambda _: SERVER_IDENTITY if state["alive"] else None
            ):
                yield

        return (
            mock.patch.object(self.sw, "run_client", side_effect=client),
            running(),
            mock.patch.object(self.sw, "acquire_slot", return_value=io.StringIO()),
        )

    def test_template_snapshot_after_failed_command(self):
        base = self.sw.base_for(self.root)  # fresh worktree: no base yet
        client, running, slot = self.run_with([0, 1])
        with client as run_client, running, slot, mock.patch.object(
            self.sw, "clone_tree"
        ) as clone, contextlib.redirect_stderr(io.StringIO()):
            code = self.sw.run(self.root, ["a/compile", "b/test", "c/test"])
        self.assertEqual(code, 1)
        self.assertEqual(run_client.call_count, 2)
        clone.assert_called_once_with(base, self.home / "templates" / self.key())

    def test_snapshot_uses_the_key_the_base_was_prepared_with(self):
        prepared = self.key()
        client, running, slot = self.run_with(
            [0], before_first=lambda: self.edit_build(SHA_B, SHA_C)
        )
        with client, running, slot, mock.patch.object(
            self.sw, "clone_tree"
        ) as clone, contextlib.redirect_stderr(io.StringIO()):
            self.sw.run(self.root, ["compile"])
        clone.assert_called_once_with(
            self.sw.base_for(self.root), self.home / "templates" / prepared
        )

    def test_no_snapshot_when_build_never_loaded(self):
        # staging exists, so only the server_running gate can prevent the snapshot.
        client, running, slot = self.run_with([1], server_running=False)
        with client, running, slot, mock.patch.object(
            self.sw, "clone_tree", side_effect=AssertionError("snapshot taken")
        ), contextlib.redirect_stderr(io.StringIO()):
            self.assertEqual(self.sw.run(self.root, ["compile"]), 1)
        self.assertTrue((self.sw.base_for(self.root) / "staging").exists())

    def test_no_snapshot_when_template_exists(self):
        (self.home / "templates" / self.key()).mkdir(parents=True)
        self.make_base()
        client, running, slot = self.run_with([0])
        with mock.patch.object(
            self.sw, "clone_tree"
        ) as clone, contextlib.redirect_stderr(io.StringIO()):
            # The base exists, so prepare_base does not seed; any clone is a snapshot.
            with client, running, slot:
                self.assertEqual(self.sw.run(self.root, ["compile"]), 0)
        clone.assert_not_called()


class LivenessSuite(WarmCase):
    def test_stale_active_json_is_not_running(self):
        target = self.root / "project" / "target"
        target.mkdir()
        (target / "active.json").write_text(
            '{"uri":"local://%s"}' % (Path(self.tmp.name) / "missing" / "sock")
        )
        self.assertFalse(self.sw.server_running(self.root))

    def test_listening_socket_is_alive(self):
        path = self.short_socket_dir() / "sock"
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(str(path))
        server.listen(16)  # probes are never accepted; keep the backlog open
        try:
            self.assertTrue(self.sw.socket_alive(path))
        finally:
            server.close()
        self.assertFalse(self.sw.socket_alive(path))

    def test_unprobeable_socket_names_the_base_to_delete(self):
        base = self.make_base()
        sock = base / "server" / "x" / "sock"
        sock.parent.mkdir(parents=True)
        sock.write_text("")
        err = io.StringIO()
        with mock.patch.object(
            self.sw.socket.socket,
            "connect",
            side_effect=OSError("AF_UNIX path too long"),
        ), contextlib.redirect_stderr(err):
            self.assertTrue(self.sw.socket_alive(sock))
        self.assertIn(str(base), err.getvalue())


if __name__ == "__main__":
    unittest.main()
