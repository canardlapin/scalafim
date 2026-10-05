import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
sys.path.insert(0, str(Path(__file__).resolve().parent))


@pytest.fixture(autouse=True)
def _tmp_cwd(tmp_path, monkeypatch):
    """Run every test in its own tmp dir so nothing (for example a default custody.log.jsonl) is
    ever written into the source tree."""
    monkeypatch.chdir(tmp_path)
