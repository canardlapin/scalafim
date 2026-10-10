"""The custody lock must be a full ``--require-hashes`` lock (S9 reconciliation, owner decision 2026-10-10)."""
import re
from pathlib import Path

LOCK = Path(__file__).resolve().parents[1] / "requirements.lock"
TOP_LEVEL = {"cryptography": "50.0.2", "numpy": "2.5.3", "py-ecc": "8.0.0", "pytest": "9.1.1"}
PIN = re.compile(r"^([A-Za-z0-9][A-Za-z0-9._-]*)==([^\s\;]+)")
HASH = re.compile(r"^--hash=sha256:[0-9a-f]{64}$")


def requirements(text):
    """Logical requirement lines (continuations joined, comments dropped) as (name, version, hashes)."""
    out, cur = [], ""
    for line in text.splitlines():
        s = line.split(" #", 1)[0].strip() if not line.lstrip().startswith("#") else ""
        if not s:
            continue
        if s.endswith("\\"):
            cur += s[:-1] + " "
            continue
        out.append((cur + s).split())
        cur = ""
    assert not cur, "dangling line continuation"
    reqs = []
    for toks in out:
        m = PIN.match(toks[0])
        assert m, f"not an exact == pin: {toks[0]!r}"
        hashes = toks[1:]
        assert all(HASH.match(h) for h in hashes), f"bad hash option in {toks[0]}"
        reqs.append((m.group(1).lower().replace("_", "-"), m.group(2), hashes))
    return reqs


def unhashed(text):
    return [n for n, _, h in requirements(text) if not h]


def test_every_pin_has_sha256_hashes():
    reqs = requirements(LOCK.read_text())
    assert reqs
    assert not unhashed(LOCK.read_text()), f"pins without --hash: {unhashed(LOCK.read_text())}"
    names = [n for n, _, _ in reqs]
    assert len(names) == len(set(names)), "duplicate pin"


def test_top_level_versions_unchanged():
    pins = {n: v for n, v, _ in requirements(LOCK.read_text())}
    for name, version in TOP_LEVEL.items():
        assert pins.get(name) == version


def test_install_instruction_requires_hashes():
    head = [ln for ln in LOCK.read_text().splitlines() if "pip install" in ln]
    assert head and all("--require-hashes" in ln for ln in head)


def test_checker_rejects_a_hashless_lock():
    import pytest
    assert unhashed("numpy==2.5.3\npytest==9.1.1 \\\n    --hash=sha256:" + "a" * 64 + "\n") == ["numpy"]
    with pytest.raises(AssertionError, match="bad hash"):
        requirements("numpy==2.5.3 \\\n    --hash=md5:abc\n")
    with pytest.raises(AssertionError, match="exact == pin"):
        requirements("numpy>=2\n")
