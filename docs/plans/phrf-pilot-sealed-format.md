# phrf-sealed/1: sealed blob format for the PHRF pilot

Status: normative for S7 (JVM runner) and S9 (Python custody tooling). Version `phrf-sealed/1`. Reference implementation: `tools/phrf-comparison/custody/phrf_custody/seal.py`. Golden vectors: `tools/phrf-comparison/custody/tests/fixtures/sealed_format_vectors.json` (regenerated and checked by `tests/vectors.py` and `tests/test_vectors.py`). Mote bd-01M3VZTP1V5ZP8WSJQM59QGN38.

Purpose: the runner encrypts every raw result to the owner's X25519 public key **at write time**, so plaintext raw results never reach disk. The private key is never on this machine. A format change requires a new version string (the `\x01` in the magic and `"phrf-sealed/1"` in HKDF info) and new vectors.

All integers are big-endian. `||` is concatenation.

## 1. Primitives

- X25519 (RFC 7748). AES-256-GCM (12-byte nonce, 16-byte tag, tag appended to the ciphertext). HKDF-SHA256 (RFC 5869) with **no salt** (the default of HashLen zero bytes), output length 32.
- `recipient_pub` = the owner's raw 32-byte X25519 public key. `eph_priv`/`eph_pub`: a **fresh** ephemeral X25519 key pair **per blob**, from a CSPRNG. Reuse of an ephemeral key across blobs is forbidden.
- `nonce`: 12 random bytes per blob (CSPRNG). Because the AES key is unique per blob the nonce could be fixed; random is mandated so that a key-reuse bug does not become a nonce-reuse break.

## 2. Key derivation

```
shared = X25519(eph_priv, recipient_pub)                                   # 32 bytes
info   = "phrf-sealed/1 blob" (ASCII, 18 bytes) || recipient_pub || eph_pub  # 18 + 32 + 32
key    = HKDF-SHA256(ikm = shared, salt = none, info = info, L = 32)
```

Reject an all-zero `shared` (low-order point) on the reading side; the owner key is fixed so this cannot occur for honest writers.

## 3. Blob file

```
offset  len  field
0       8    magic            50 48 52 46 53 42 01 00   ("PHRFSB", version 1, flags 0)
8       32   eph_pub
40      8    recipient_id     first 8 bytes of SHA-256(recipient_pub)
48      12   nonce
60      n    ciphertext || tag   (n = padded_envelope_len + 16)
```

- **AAD** = the 60 header bytes (offsets 0-59), verbatim.
- Total file length = 60 + padded_envelope_len + 16.
- **Blob file name** on disk: `blobs/<lowercase hex SHA-256 of the whole file>.enc`. Writers write to a temporary name in the same directory, fsync, then rename. Names carry no order, dataset or cell information.

## 4. Plaintext envelope and padding (F11)

```
envelope = kind (1 byte)                      0 = data blob, 1 = CLOSE record
        || name_len (2 bytes)
        || name (UTF-8, name_len bytes)       logical name, see below
        || data_len (8 bytes)
        || data (data_len bytes)
padded   = envelope || zero bytes, to padded_envelope_len = bucket(len(envelope))
```

`bucket(n)` (size buckets hide the exact length of the plaintext):

- n <= 4096: 4096;
- 4096 < n <= 2^30: the next power of two >= n;
- n > 2^30: the next multiple of 2^30.

Readers verify that `padded_envelope_len = bucket(end)` where `end` is the end of `data`, that all padding bytes are zero, and that `kind` is 0 or 1. A blob is encrypted in one shot (the reference implementation holds one blob in memory); keep individual blobs well under 1 GiB, and put one dataset-arm result in one blob.

**Logical names** are `/`-separated relative paths: non-empty, at most 1024 UTF-8 bytes, no NUL, no backslash, no leading `/`, and no empty, `.` or `..` component. Writers and readers must both reject anything else (path traversal). Names inside blobs are encrypted; the sealed directory shows no names.

## 5. Store layout and CLOSE record

```
<store>/blobs/<sha256-hex>.enc     one file per blob
<store>/SEALED                     JSON receipt, written at close (not part of the tree digest)
```

Writers are **stateless**: any number of processes may append concurrently, and a crashed run is resumed simply by appending more blobs (new ephemeral keys; no key material survives or is needed). **Writer read-back (MUST).** Before publishing a blob the writer decrypts it with its own `eph_priv` and the recipient **public** key (the shared secret is symmetric: `X25519(eph_priv, recipient_pub)`), parses the envelope, and compares kind, name and data with its inputs; after the durable write it re-reads the file and compares the bytes. Any mismatch aborts the write and nothing is published. Plaintext raw results are removed or released only after this check has passed.

**Temporary files.** A writer writes `blobs/<hash>.enc.tmp`, fsyncs, then renames to `<hash>.enc`. A crash can leave `*.tmp` files. Readers **ignore** any `blobs/*.tmp` file, the sealed-tree digest **excludes** them, and `close` **deletes** them before listing.

**Resume and duplicates (B2).** Logical names are encrypted, so a writer cannot see what is already sealed and no progress journal exists. Instead, the reader accepts a logical name sealed more than once **only if every copy has byte-identical plaintext**, and refuses (`logical name sealed twice with different plaintext`) if any copy differs. Consequences for the runner: a resumed run may simply recompute and re-seal a dataset it is unsure about; the plaintext of a `(cell, dataset, arm)` blob must therefore be a deterministic function of the root seed and inputs, and anything non-deterministic (wall-clock timings, thread ids, attempt counters) must go in blobs with attempt-unique names (for example `timing/<cell>/<dataset>/<attempt>`), never in a blob whose name can repeat. A differing duplicate means a determinism bug and the owner will see it at unseal. The S7 runner's concrete names, the "first completed attempt is scored" rule and the recompute-once design are in section 10. The CLOSE record counts every data-blob **file** (duplicates included), so deleting one copy is detected.

`close` (exactly once, by the custodian's final process) deletes `*.tmp`, lists `blobs/`, takes the sorted lowercase-hex blob hashes `h_1..h_n` (data blobs only, i.e. everything present before the CLOSE blob is written), and writes one blob with `kind = 1`, `name = "CLOSE"` and `data` equal to the JSON text (UTF-8)

```
{"blobs_digest": "<hex SHA-256 of h_1 || "\n" || ... || h_n>", "format": "phrf-sealed/1", "n_blobs": n}
```

(keys sorted, Python `json.dumps(..., sort_keys=True)` spacing; readers parse JSON, so spacing is not normative). `SEALED` holds `format`, `recipient_fp` and `sealed_tree_digest` = SHA-256 of the sorted `sha256sum`-style listing (`<hex>  <relative path>\n`, paths relative to the store root with `/`, sorted bytewise, excluding `SEALED` and `*.tmp`). A store without a CLOSE record is **partial** and is opened only with an explicit allow-partial decision (runbook: partial seals follow the uniform drop rule only).

## 6. Reader (owner side) checks, in order

0. If the owner supplies the **out-of-band anchor** (the sealed-tree digest recorded in manifest v1 and the custody log), the computed tree digest of the store must equal it (`unseal --expect-tree-digest`). Without the anchor, `SEALED` alone proves nothing, because it sits next to the data.
1. Directory contains only `blobs/` and optionally `SEALED`; `blobs/*.tmp` files are ignored.
2. Each file name equals the SHA-256 of its content.
3. Magic, `recipient_id` equals the owner's key, GCM authenticates (header as AAD), envelope and padding valid, name valid.
4. Duplicate logical names only with byte-identical plaintext; at most one CLOSE; the CLOSE record is a JSON object with integer `n_blobs` and string `blobs_digest` (anything else is an error); its count and digest match the data-blob file hashes (unless partial is allowed).
5. If `SEALED` exists, its `sealed_tree_digest` matches.

Only then are files written, each under a resolved path that must stay inside the output directory. Before writing, the reader also refuses logical names that collide under Unicode NFC normalisation plus case folding (so the result does not depend on the filesystem) and any name that is also a directory prefix of another name; a non-file entry inside `blobs/` is an error; low-order or zero X25519 points are reported as errors, not exceptions.

## 7. Golden vectors

Test-only keys: `recipient_priv = 01 02 03 ... 20` (bytes 1..32), `eph_priv = 65 66 ... 84` (bytes 101..132), `nonce = c9 ca ... d4` (bytes 201..212). Derived:

| quantity | value |
|---|---|
| recipient_pub | `07a37cbc142093c8b755dc1b10e86cb426374ad16aa853ed0bdfc0b2b86d1c7c` |
| eph_pub | `5714769d116bf76436ae74bc793d2c30ad1903c59ac5273805c7e2698b410c36` |
| shared | `c9ea6a3f79a000b60b076d4afc990b272f3f0b5aaa3f0b8713c209273e363863` |
| key (HKDF) | `665b9a44d9dc53403fd4b07f0337b4d362f2c985e7e770ee71d898cbe19d6816` |

Blobs (full bytes for `data-small` are in the JSON as `blob_hex`; the others are given by length and SHA-256 of the whole blob file, and are reproduced from their inputs):

| id | kind, name, data | padded envelope | blob length | SHA-256 of blob |
|---|---|---|---|---|
| data-small | 0, `dataset/C-TS-1/0007`, `"phrf golden vector plaintext" 00 01 02 ff` | 4096 | 4172 | `bd3a0f58042be4be51f02c63b55b88648d63e55f96ac3c7c315d68bcb21054e4` |
| close | 1, `CLOSE`, `{"blobs_digest": "00"*32, "format": "phrf-sealed/1", "n_blobs": 1}` | 4096 | 4172 | `5ea59e19b44cbd8c8a1dfb6df4aa3fc0ac2ee3ce64d03a22618df78a02a8675f` |
| pad-exact-4096 | 0, `x`, 4084 bytes `i mod 251` | 4096 (envelope exactly 4096) | 4172 | `3e7be729e8ca219e76963ef028414cf587dc6f476e4582c520bef4dcd82cdef1` |
| pad-next-8192 | 0, `x`, 4085 bytes `i mod 251` | 8192 (envelope 4097) | 8268 | `e4f58a7ec759a18a0bfe65c4f0710b5881cfeec578a6f2bb318bda8bbc9d172c` |
| empty | 0, `e`, 0 bytes | 4096 | 4172 | `5f584e0df71ab8a7fb1f064088a3931e8bc918e523a78105bc1ead207564128d` |

The JSON also gives, per case, `envelope_sha256` (of the padded envelope), `aad_hex` (the 60 header bytes) and the file name. A JVM implementation conforms when, with the injected `eph_priv` and `nonce`, it reproduces every `blob_sha256` byte for byte, and when it decrypts each vector with `recipient_priv`. The JVM test must also write one blob with random `eph_priv`/`nonce` and have `python -m phrf_custody` (`unseal`) read it.

**Published known-answer tests** (independent of the self-generated vectors above; `tests/test_kats.py`): the JVM implementation should also pass RFC 7748 section 6.1 (X25519: Alice priv `77076d0a...2c2a`, pub `8520f009...4e6a`; Bob priv `5dab087e...e0eb`, pub `de9edb7d...2b4f`; shared `4a5d9d5b...1742`) and RFC 5869 appendix A.1 and A.3 (HKDF-SHA256; A.3 has the empty salt and info this format uses for salt). The AES-GCM implementation should be the platform's standard one (JCA `AES/GCM/NoPadding`, 128-bit tag).

## 8. Runner contract for the root pipe and its acknowledgement

The custodian session passes root64 on an inherited pipe and requires proof that the runner received it. Two environment variables name inherited descriptors: `PHRF_ROOT_FD` (read end, carries one line: root64 in decimal followed by `\n`) and `PHRF_ROOT_ACK_FD` (write end of the acknowledgement pipe).

The runner must:

1. read the root line from `PHRF_ROOT_FD` once (a single read of up to 64 bytes returns the whole line);
2. **close `PHRF_ROOT_FD` and remove both variables from its environment immediately**, before starting any subprocess, executor or thread pool that forks, and never log, store or forward the value other than into the seed derivation;
3. write the **acknowledgement** immediately, **before doing anything that can block or take long** (the session's timeout runs from launch to this write), and **close the ack descriptor right after writing it**; the acknowledgement `hex(SHA-256(L))` to `PHRF_ROOT_ACK_FD`, where `L` is the exact bytes read in step 1 including the trailing `\n` (64 lowercase ASCII hex characters, nothing else), and close that descriptor.

The session creates all pipe ends non-inheritable and passes only the read end of the root pipe and the write end of the ack pipe to the runner, so grandchildren cannot inherit them unless the runner re-exports them. The runner is started in its own session and process group; on a missing, wrong or late acknowledgement the session SIGKILLs the whole group, reaps the leader, and confirms that the process-group members are gone (via `ps`, zombies counted as gone) before any restart, so a launcher script or sbt wrapper cannot leave the real runner alive; it also kills any leftover group members after every attempt, so a runner must not leave background processes. Descendants that leave the process group (`setsid`, double-fork daemonisation) escape this check, and the runner contract **forbids** them, with the one sanctioned exception below. The session counts the attempt as failed, whatever the exit status, if the acknowledgement is missing, wrong or late (a runner that has not acknowledged by the timeout is killed), or if the root pipe was broken. A conforming JVM runner reads the line, computes SHA-256 over those bytes, writes the 64 hex characters to the ack descriptor, and closes both.

**Sanctioned exception: the GLMsingle child group (S6 bridge).** The bridge must be able to SIGKILL the GLMsingle child and every descendant without killing the runner, so the child runs in a process group of its own. That group leaves the runner's group, so a SIGKILL of the runner's group does not reach it. It is allowed only under this parent-death watchdog (`GlmSingleBridge.WatchdogScript`, started as `/usr/bin/perl -e <script> <runner-pid> -- <command>`):

- The runner's direct child is the watchdog. The watchdog calls `setpgrp(0, 0)`, so its pid is the new group's id, and forks the command into that group. It is a member of the group it guards, so killing the group kills the watchdog too.
- The watchdog compares `getppid()` with the runner pid passed on its command line. It does so before forking, so a runner that died during launch never gets the command run, and then every 0.1 s while the command runs. When the parent differs (the runner died, including by a SIGKILL of the runner's group), it sends SIGKILL to its own group, itself included. SIGTERM, SIGINT and SIGHUP to the watchdog do the same.
- When the command exits, the watchdog SIGKILLs any other member of its group (background stragglers, found with `pgrep -g`) and exits with the command's status.
- **Guarantee and bound.** After the runner dies, by any signal including SIGKILL, every member of the child group still in that group is SIGKILLed within one poll interval (0.1 s) plus signal delivery. Tested: an intermediate parent standing in for the runner was SIGKILLed, and the launcher, the command and a background grandchild were all gone 0.04 s later (the test bound is 2 s). The guarantee does not cover a descendant that leaves the child group itself (`setsid`). That remains forbidden: the frozen `from_generator.py` and GLMsingle do not do it. It also does not cover a SIGKILL sent to the watchdog alone, which nothing in the pilot sends. The session's post-attempt `ps` check of the runner's group does not see the child group. The watchdog bound is what replaces that check for this group.
- The bridge's own timeout, interrupt and shutdown paths are unchanged: they SIGKILL the child group by its id (`kill -KILL -- -<pgid>`), which takes the watchdog with it, and confirm with `pgrep -g` that the group is empty.

Every tool the bridge starts outside that group (`hdiutil`, `newfs_hfs`, `mount`, `umount`, `ps`, `pgrep`, `kill`) has `PHRF_ROOT_FD` and `PHRF_ROOT_ACK_FD` removed from its environment (`ChildEnvironment.scrub`). The GLMsingle child gets a cleared environment.

## 9. Security notes

- The writer holds only the public key, so it cannot read what it wrote after the ephemeral key is discarded (it can verify each blob only during write, with the ephemeral key). Authors cannot decrypt even if they copy the store.
- The blob count and the padded sizes are visible; they are the metadata covered by the no-peeking ban (runbook section 3).
- Integrity of the set is by the CLOSE record and the tree digest; both are verified by the owner at unseal. A hostile writer could still withhold or add blobs before close; this format is not a defence against a hostile custodian (design 3.2).
- Plaintext name and data lengths are hidden up to the bucket.
- `sealed_store_path_sha256` in the plaintext `stamp.json` is an unsalted commitment to the store path, not a secret. Anyone who can guess the path can confirm it, so it hides the path only from casual reading. It binds the stamp to the store.

## 10. Runner layout (S7, JVM `PilotRunner`), normative for the owner's reader

This section fixes the logical names the S7 runner writes and how the owner interprets them.

- `<unit>` is `<cell>/dNNNN/<arm>`, with the dataset index zero-padded to 4 digits.
- `<runId>` is 16 lowercase hex characters. It is drawn once per runner **invocation**, inside `run()`. One `PilotRunner` instance is one invocation: a second `run()` on the same instance is refused (`RunnerReused`).
- `invocation` is the 1-based ordinal of that invocation on the output directory. The runner persists it in the plaintext `cost.json` and increments it at start, before it seals anything.

**Rule.** Only two names are deterministic: `meta/root-check` and `meta/stamp`.

- Both are re-sealed, as identical duplicates, by every invocation. A differing duplicate means a resume under another root or another stamp, and section 6 refuses the store.
- Every unit record and every aggregate carries `<runId>`. A resume therefore never seals a second copy of any unit's data.
- **Nondeterminism** is detected by the owner, not by the reader. The owner compares `payload_sha256` across the ledger records of one unit. A mismatch is recorded for that unit in the deviation report and never invalidates the store.

| Logical name | Plaintext |
|---|---|
| `meta/root-check` | `hex(SHA-256(root64 decimal + "\n"))`, the acknowledgement hash, never the root |
| `meta/stamp` | the sealed stamp: every `stamp.json` field plus `sealed_store_path` (the full absolute path, which the plaintext stamp holds only as `sealed_store_path_sha256`) |
| `data/<unit>/<runId>` | unit payload of a scheduled commit, any status (a status placeholder when the arm emitted nothing) |
| `ledger/<unit>/<runId>` | ledger record: JSON with keys exactly `cell dataset arm invocation run_id phase attempts status code payload payload_sha256 entries`; `phase` is `scheduled` here |
| `timing/<unit>/<runId>` | timing record: every attempt of this invocation (status `retried`/`done`/`refused`/`failed`, code, wall and child CPU seconds, named timings, result-adjacent notes) |
| `rerun/<runId>/{data,ledger,timing}/<unit>` | the same three records for a unit recomputed for the scorer, with `phase = rerun` |
| `aggregate/<runId>/diagnostics` | the S8 sealed diagnostics |
| `aggregate/<runId>/record` | JSON `run_id`, `datasets` (D), `whitelist_sha256`, `corpus_outcomes_sha256`, `corpus_timing_sha256`, `units` |

**Unit payload.** The encoding is big-endian Java `DataOutput`:

1. `writeUTF("phrf-cmp-s7-unit-payload-v1")`;
2. `writeUTF(status)`;
3. `int n`;
4. then `n` entries in name order, each `writeUTF(name)`, `long length`, and the bytes.

`writeUTF` is a 2-byte length followed by modified UTF-8; all names here are ASCII. The entries are the arm's raw blobs, for example `result`, `fit` or `tuning`. `payload_sha256` is the SHA-256 of these bytes.

**Fixed blob set.** Each unit commit seals exactly three blobs, the payload, the ledger record and the timing record, whatever its status and however many attempts it took. Retries seal nothing of their own.

**First completed attempt is scored.**

1. For each unit, the owner takes the ledger records with `phase = scheduled`.
2. The scored one has the smallest `invocation`. Ties cannot occur, because one invocation commits a unit at most once; if one did occur, it would be broken by the smaller `run_id` in bytewise order.
3. The record's `payload` names the payload blob, and its `payload_sha256` must match that blob.
4. A scheduled ledger record **counts even without its timing record**. The commit seals payload, ledger, then timing, and a crash, a hard stop, or another worker's crash between the last two leaves the timing record missing.
5. A payload without a ledger record is an orphan of an abandoned commit and is not scored.
6. A unit has several scheduled records only when a commit was not followed by its job's completion marker in the same invocation. This happens after:
   - a crash or power loss;
   - a hard stop that tripped while other units of the job were still running;
   - abandonment because another worker crashed (the crash flag).

   The later records are diagnostic only.
7. `rerun` records are never scored.

**Recompute-once (decision D1) and the scorer-input check (F4).**

*What the runner does.* The runner cannot read the store, and the whitelist is computed in the runner's process. The invocation that reaches an accepted decision therefore recomputes, once, every kept job that an earlier invocation completed. It seals the results under `rerun/<runId>/...`, and their CPU counts toward the guard, so a crash late in the pilot can force an approved ceiling raise. The scorer consumes, for every kept unit, the commit of the aggregating invocation: a `scheduled` commit for jobs first run in that invocation, a `rerun` commit for the others.

*What `units` holds.* The aggregate record lists that commit for every kept unit, as `{cell, dataset, arm, run_id, phase, payload_sha256}` sorted by (cell, dataset, arm).

*What the owner checks at unseal.*

- **Primary check, which needs only this spec.** For every listed unit, the owner compares `payload_sha256` with that of the first completed attempt, as defined above. This requires nothing beyond the sealed records and this section.
- **Secondary check.** The owner rebuilds the corpus from the first completed attempts and the generator truth, and recomputes `corpus_outcomes_sha256`. This depends on the S10 `ScoreFeed` mapping, which turns the payload arms and the generator truth into per-voxel outcomes. That mapping is not yet specified (design section 6, slice S10), so this check becomes available once S10 specifies it.

*Policy:*

- any mismatch, of a unit or of the outcome digest, **invalidates the whitelist released from that aggregate**;
- the owner records the exact list of mismatching units in the deviation report;
- recomputing the whitelist owner-side from the first attempts, after the confirmatory analysis is closed, is allowed only as a documented deviation.

The runner seals at most one aggregate per run id: a second `aggregateAndSeal` with the same run id is refused with `AlreadyAggregated`. Several aggregates may exist, for example a partial pilot and then the full one. The authoritative aggregate is the one whose `whitelist_sha256` is recorded in manifest v1.

*Timings.* The whitelist's timing block comes from the aggregating invocation's own measurements, recomputed units included. It is informative only. `corpus_timing_sha256` binds it, but timings differ between attempts by nature, so a timing difference never invalidates the whitelist.

**`CorpusDigest` byte layout** (SHA-256 over big-endian Java `DataOutput`):

- `writeUTF` is a 2-byte length followed by modified UTF-8;
- `int` is 4 bytes and `long` is 8;
- `double` is the 8-byte IEEE-754 bit pattern from `doubleToLongBits`, so every NaN is canonical;
- `boolean` is 1 byte, 0 or 1.

A per-voxel outcome is one tag byte, followed by the value only when estimated:

- `0` = estimated, then the value;
- `1` = refused;
- `2` = failed.

An arm map is written as `int k` (the number of methods present), then, for each present method in `Method.all` order (`PHRF CAN INF3 FIR LSA LSS rLSS GLMs-D`): `writeUTF(method code)`, `int voxels`, then each voxel's outcome.

`corpus_outcomes_sha256`:

1. `writeUTF("phrf-cmp-corpus-outcomes-v1")`, `int D`.
2. For each condition cell in the order `C-TX-.5 C-TS-1 C-TS-.5 C-TG-.5`: `writeUTF(cell id)`, `int n`, then each dataset in index order: `int dataset`, `double noiseVariance`, `double horizonSeconds`, then the arm map, whose estimated value is a `double`.
3. For each trial cell in the order `T-TX-fast T-TX-jit T-TS-fast`: `writeUTF(cell id)`, `int n`, then each dataset:
   - `int dataset`;
   - `int t`, then `t` ints for the condition of each trial;
   - `int v`, then for each voxel `int t` and `t` doubles of true amplitude;
   - the arm map, whose estimated value is `int t` plus `t` doubles.
4. `int c`, then each coverage dataset: `int dataset`, `int voxels`, then each voxel's outcome, whose estimated value is `double signedRelativePeakError`, `double tauError`, `boolean covered`.

`corpus_timing_sha256`: `writeUTF("phrf-cmp-corpus-timing-v1")`, then these vectors, each as `int n` and `n` doubles: trial ML seconds per voxel, trial preparation seconds per alpha, alpha preparation profiles (`int p`, then one vector per profile), GLMsingle seconds per dataset, cold condition preparation seconds. Then `double totalCpuSeconds`.

**Plaintext outside the store** (runner output directory):

- `stamp.json`: hashes and versions, `recipient_fp` (the owner key fingerprint, already public), and `sealed_store_path_sha256`, never the path itself;
- `cost.json`: `cpu_seconds_total` and `invocations`;
- `selection.json`: D, df and the UCL factor;
- `progress/<cell>/dNNNN.done` plus `.sha256` markers;
- optionally the S6 scratch custody log: a hash-chained JSONL of scratch lifecycle events, holding paths, device names and residue counts but no result.

The markers' contents are constant, but their modification times reveal when each job completed, and the custody log carries timestamps. The output directory therefore lives inside the custodian work directory, under the no-peeking rule (runbook section 3).
