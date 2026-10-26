# U3 topology identity baseline

Baseline: `dda3152a9` on local branch `simplify/phase-a-20261019`.
This change freezes compatibility before changing the topology implementation.
It does not adopt mesh4s or complete `eco-u3-topology`.

The fixtures use a consistently oriented open quad with faces `(0,1,2)` and
`(0,2,3)`, and two disconnected oriented triangles for the MVPA adapter. They
avoid the orientation-conflicted synthetic tetrahedra that U3 must correct.

- Surface topology retains hex key `fcf7e2e81f78e46f` and signed decimal
  `-218456570315873169`; reversing quad face-row order yields
  `6861754c1ed19bde` and `7521421822480456670`.
- Scene capture and JSON encoding preserve the hex spelling. Decoding restores
  the original model, admits changed coordinates with identical topology, and
  refuses reordered faces.
- MVPA's surface adapter retains decimal ID `1580453993426280883` for the two
  disconnected triangles, alongside its existing connectivity/weight checks.
- Atlas publication retains the left/right SHA-256 surface identities for
  `fsaverage6`, including exact face order. Existing checks establish coordinate
  independence and reject a reordered support identity.

[reference.py](reference.py) computes the constants independently with Python
integer operations, explicit little-endian preimage encoding, and stdlib
`hashlib`. It imports no project code. Run `python3 reference.py` and compare
with [reference.json](reference.json). Murmur keys are compatibility/cache
identifiers; full ordered-index comparison remains the compatibility proof.

Verification results and log hashes are recorded in `gates.json`. Full raw logs
are retained under the common Git directory's
`verification/module-simplification-20261019/`. The initial JVM attempt caught
an incorrect expected hemisphere spelling (`L` rather than `lh`); the assertion
was corrected to the existing protocol. No implementation changed.

Next: compose `TriangleMesh` from mesh4s topology and point fields with strict
admission, share the owner across alternate realizations, and make its vertex
domain authoritative. The recovered mesh4s pin is still unpublished; no
provider publication or topology cutover is included here.
