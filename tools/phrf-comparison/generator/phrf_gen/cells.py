"""Cell specifications (subset of the protocol section 5 cell list that this generator supports)."""
from __future__ import annotations

from dataclasses import asdict, dataclass, field


@dataclass(frozen=True)
class Cell:
    cell_id: str
    kind: str  # "condition" | "trial"
    truth: str  # TG | TL | TS | TX | NULL
    snr: float = 1.0
    spacing: str = "dense"  # condition: dense | sparse ; trial: slow | fast
    deviation: str = "persistent"  # trial: persistent | iid | heavy | none
    latency_jitter: bool = False  # trial: +-1 s onset jitter in the truth only
    ar: tuple = (0.3,)  # true AR coefficients
    duration: float = 0.0  # condition neural duration (s)
    n_voxels: int = 200
    n_pool: int = 0  # trial: extra pure-noise voxels (GLMsingle noise pool)
    tr: float = 1.0
    tr_aligned_onsets: bool = False  # trial: onsets (nominal) on the TR grid; for cells scored by GLMsingle (T-G)
    null_cal: bool = False  # noise from the 'nullcal' stream purpose
    note: str = ""

    def to_json(self) -> dict:
        d = asdict(self)
        d["ar"] = list(self.ar)
        return d


def _c(cid, truth, snr, **kw):
    return Cell(cid, "condition", truth, snr, **kw)


def _t(cid, truth, spacing, **kw):
    kw.setdefault("n_pool", 100)
    return Cell(cid, "trial", truth, 1.0, spacing, **kw)


CELLS = {c.cell_id: c for c in [
    # pilot cells (protocol section 8, step 3)
    _c("C-TX-.5", "TX", 0.5),
    _c("C-TS-1", "TS", 1.0),
    _c("C-TS-.5", "TS", 0.5),
    _c("C-TG-.5", "TG", 0.5),
    _t("T-TX-fast", "TX", "fast", tr_aligned_onsets=True),
    _t("T-TX-jit", "TX", "fast", deviation="heavy", latency_jitter=True, tr_aligned_onsets=True),
    _t("T-TS-fast", "TS", "fast"),
    # additional cells that need no new machinery
    _c("C-TG-1", "TG", 1.0),
    _c("C-TL-1", "TL", 1.0),
    _c("C-TX-1", "TX", 1.0),
    _c("C-TG-.25", "TG", 0.25),
    _c("C-NULL", "NULL", 0.0),
    _c("C-NULL-cal", "NULL", 0.0, null_cal=True),
    _c("C-TX-sparse", "TX", 1.0, spacing="sparse",
       note="ISI 10-14 s cannot hold 300 events in 600 s; as many events as fit are placed"),
    _c("C-TX-AR2", "TX", 1.0, ar=(0.4, 0.2), note="AR(2) truth; coefficients are a harness choice"),
    _c("C-TX-duration", "TX", 1.0, duration=2.0, note="neural duration fixed at 2 s (protocol: 0-4 s)"),
    _c("C-TX-TR2", "TX", 1.0, tr=2.0),
    _t("T-TX-slow", "TX", "slow", tr_aligned_onsets=True),
    _t("T-TS-slow", "TS", "slow"),
    _t("T-TG-slow", "TG", "slow"),
    _t("T-TG-fast", "TG", "fast"),
    _t("T-TX-iid", "TX", "fast", deviation="iid"),
]}
