import hashlib
import importlib.util
import io
from pathlib import Path
import tarfile
import tempfile
import unittest

spec = importlib.util.spec_from_file_location("evidence_archive", Path(__file__).with_name("evidence_archive.py"))
archive_tool = importlib.util.module_from_spec(spec)
spec.loader.exec_module(archive_tool)


class EvidenceArchiveTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.archive = self.root / "evidence.tar"
        self.data = {"docs/verification/a.log": b"failed attempt\n", "docs/verification/b.gz": b"\x00\xffbytes"}
        self.pack()

    def pack(self, extra=None):
        with tarfile.open(self.archive, "w") as bundle:
            for name, data in self.data.items():
                member = tarfile.TarInfo(name)
                member.size = len(data)
                bundle.addfile(member, io.BytesIO(data))
            if extra:
                bundle.addfile(extra)
        self.manifest = {
            "archive": {"bytes": self.archive.stat().st_size, "sha256": hashlib.sha256(self.archive.read_bytes()).hexdigest()},
            "entries": [{"path": name, "bytes": len(data), "sha256": hashlib.sha256(data).hexdigest()} for name, data in self.data.items()],
        }

    def test_verify_restore_and_idempotent_restore(self):
        self.assertEqual(len(archive_tool.verify(self.manifest, self.archive)), 2)
        destination = self.root / "restored"
        self.assertEqual(archive_tool.restore(self.manifest, self.archive, destination), 2)
        for name, data in self.data.items():
            self.assertEqual((destination / name).read_bytes(), data)
        self.assertEqual(archive_tool.restore(self.manifest, self.archive, destination), 0)

    def test_corruption_is_refused_before_restore(self):
        self.archive.write_bytes(self.archive.read_bytes() + b"tampered")
        destination = self.root / "restored"
        with self.assertRaisesRegex(ValueError, "SHA-256"):
            archive_tool.restore(self.manifest, self.archive, destination)
        self.assertFalse(destination.exists())

    def test_per_file_digest_is_checked(self):
        self.manifest["entries"][0]["sha256"] = "0" * 64
        with self.assertRaisesRegex(ValueError, "member SHA-256"):
            archive_tool.verify(self.manifest, self.archive)

    def test_existing_different_file_is_not_overwritten(self):
        destination = self.root / "restored"
        conflict = destination / "docs/verification/b.gz"
        conflict.parent.mkdir(parents=True)
        conflict.write_bytes(b"keep my work")
        with self.assertRaisesRegex(ValueError, "overwrite"):
            archive_tool.restore(self.manifest, self.archive, destination)
        self.assertEqual(conflict.read_bytes(), b"keep my work")
        self.assertFalse((destination / "docs/verification/a.log").exists())

    def test_symlink_member_is_refused(self):
        member = tarfile.TarInfo("docs/verification/link")
        member.type = tarfile.SYMTYPE
        member.linkname = "../../outside"
        self.pack(member)
        with self.assertRaisesRegex(ValueError, "non-regular"):
            archive_tool.verify(self.manifest, self.archive)

    def test_traversal_and_duplicate_manifest_are_refused(self):
        self.manifest["entries"].append(dict(self.manifest["entries"][0]))
        with self.assertRaisesRegex(ValueError, "duplicate"):
            archive_tool.verify(self.manifest, self.archive)
        self.manifest["entries"].pop()
        self.manifest["entries"][0]["path"] = "../outside"
        with self.assertRaisesRegex(ValueError, "unsafe"):
            archive_tool.verify(self.manifest, self.archive)

    def test_symlink_destination_is_refused(self):
        destination = self.root / "restored"
        destination.mkdir()
        outside = self.root / "outside"
        outside.mkdir()
        (destination / "docs").symlink_to(outside, target_is_directory=True)
        with self.assertRaisesRegex(ValueError, "unsafe restore"):
            archive_tool.restore(self.manifest, self.archive, destination)
        self.assertEqual(list(outside.iterdir()), [])

    def test_manifest_must_cover_every_member(self):
        self.manifest["entries"].pop()
        with self.assertRaisesRegex(ValueError, "unexpected"):
            archive_tool.verify(self.manifest, self.archive)


if __name__ == "__main__":
    unittest.main()
