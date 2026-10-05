"""drand beacon fetch and verification (League of Entropy default chain, pedersen-bls-chained).

Verification model
------------------
* Structural checks always run: chain hash, round, randomness == SHA-256(signature), and
  agreement of every relay on every field.
* BLS verification is REQUIRED by default (F6); callers must opt out explicitly with
  ``require_bls=False`` (CLI ``--allow-unverified``), and the record then says bls_verified=false.
* The BLS check (signature over SHA-256(previous_signature || round as 8-byte big-endian),
  public key on G1, signature on G2, ciphersuite BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_)
  runs when ``py_ecc`` is importable.  When it is not, ``verify_beacon`` returns
  ``bls_verified=False`` (only when opted out); the fallback is independent re-fetch from
  two relays (``fetch_round`` requires agreement of at least ``min_relays``).
* Future rounds are never fetched: ``fetch_round`` refuses a round whose time has not arrived.
"""
from __future__ import annotations

import hashlib
import json
import time
import urllib.request
from dataclasses import dataclass

CHAIN_HASH = "8990e7a9aaed2ffed73dbd7092123d6f289930540d7651336225dc172e51b2ce"
CHAIN_PUBLIC_KEY = (
    "868f005eb8e6e4ca0a47c8a77ceaa5309a47978a7c71bc5cce96366b5d7a569937c529eeda66c7293784a9402801af31"
)
GENESIS_TIME = 1595431050
PERIOD = 30
SCHEME = "pedersen-bls-chained"
RELAYS = ("https://api.drand.sh", "https://api2.drand.sh", "https://drand.cloudflare.com")


class BeaconError(Exception):
    pass


@dataclass(frozen=True)
class Beacon:
    round: int
    signature: str
    previous_signature: str
    randomness: str
    bls_verified: bool


def round_time(round_: int) -> int:
    return GENESIS_TIME + (round_ - 1) * PERIOD


def current_round(now: float | None = None) -> int:
    t = time.time() if now is None else now
    return int((t - GENESIS_TIME) // PERIOD) + 1


def _bls_available() -> bool:
    try:
        import py_ecc.bls  # noqa: F401
    except ImportError:
        return False
    return True


def verify_beacon(d: dict, *, require_bls: bool = True) -> Beacon:
    """Verify one beacon response dict.  Raises BeaconError on any failure."""
    try:
        round_ = int(d["round"])
        sig, prev, rnd = d["signature"], d["previous_signature"], d["randomness"]
        sig_b, prev_b = bytes.fromhex(sig), bytes.fromhex(prev)
    except (KeyError, ValueError, TypeError) as e:
        raise BeaconError(f"malformed beacon: {e!r}") from e
    if round_ < 1 or len(sig_b) != 96 or len(prev_b) != 96:
        raise BeaconError("bad round or signature length")
    if hashlib.sha256(sig_b).hexdigest() != rnd:
        raise BeaconError("randomness is not SHA-256(signature)")
    verified = False
    if _bls_available():
        from py_ecc.bls import G2Basic

        msg = hashlib.sha256(prev_b + round_.to_bytes(8, "big")).digest()
        try:
            ok = G2Basic.Verify(bytes.fromhex(CHAIN_PUBLIC_KEY), msg, sig_b)
        except Exception as e:  # malformed point etc.
            raise BeaconError(f"BLS verification error: {e!r}") from e
        if not ok:
            raise BeaconError("BLS signature invalid")
        verified = True
    elif require_bls:
        raise BeaconError("BLS verifier (py_ecc) not installed; cannot satisfy require_bls")
    return Beacon(round_, sig, prev, rnd, verified)


def _http_get(url: str, timeout: float = 20.0) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": "phrf-custody/1"})
    with urllib.request.urlopen(req, timeout=timeout) as r:  # noqa: S310 (https relays only)
        return r.read()


def round_exists(round_: int, *, relays=RELAYS, http_get=_http_get) -> bool | None:
    """Does the network already serve ``round_``?  True: yes.  False: a relay says it is not yet
    available (404/425).  None: no relay could be asked (network unavailable).  Used for the v0
    ordering check, so unlike ``fetch_round`` it does not refuse future rounds itself."""
    import urllib.error

    answered = False
    for relay in relays:
        try:
            raw = http_get(f"{relay}/{CHAIN_HASH}/public/{round_}")
            if int(json.loads(raw).get("round", -1)) == round_:
                return True
            answered = True
        except urllib.error.HTTPError as e:
            if e.code in (404, 425):
                answered = True
        except Exception:
            continue
    return False if answered else None


def fetch_round(
    round_: int,
    *,
    relays=RELAYS,
    min_relays: int = 2,
    require_bls: bool = True,
    now: float | None = None,
    http_get=_http_get,
) -> tuple[Beacon, list[dict]]:
    """Fetch ``round_`` from >= min_relays relays; return (beacon, raw log entries).

    The raw log entry per relay holds the relay, the SHA-256 of the raw response bytes and
    the response text, for the custody log.  All relays must agree on the parsed content.
    """
    if round_ > current_round(now):
        raise BeaconError(f"round {round_} is in the future (current {current_round(now)}); refusing to fetch")
    parsed, log = [], []
    for relay in relays:
        if not relay.startswith("https://"):
            raise BeaconError("relays must be https")
        try:
            raw = http_get(f"{relay}/{CHAIN_HASH}/public/{round_}")
            d = json.loads(raw)
        except Exception as e:
            log.append({"relay": relay, "error": repr(e)})
            continue
        try:
            b = verify_beacon(d, require_bls=require_bls)
        except BeaconError as e:
            raise BeaconError(f"{relay}: {e}") from e
        if b.round != round_:
            raise BeaconError(f"{relay}: returned round {b.round}, wanted {round_}")
        parsed.append(b)
        log.append({"relay": relay, "response_sha256": hashlib.sha256(raw).hexdigest(), "response": raw.decode()})
    if len(parsed) < min_relays:
        raise BeaconError(f"only {len(parsed)} relay(s) answered; need {min_relays}")
    if len({(b.signature, b.previous_signature, b.randomness) for b in parsed}) != 1:
        raise BeaconError("relays disagree")
    return parsed[0], log
