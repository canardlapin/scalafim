# Restored cross-provider scenario acceptance

Hosted CI run `40009019497`, focused job `120604258126`, executed the Python
preflight and rejected six stale scenario paths. Four transform workflow courts
were already retained in provider-spike. Two missing surface workflow courts are
restored there with their helpers and 37 exact historical fixture blobs.

Only test code, test dependencies, registry validation and documentation change.
Production sources and provider pins remain those qualified at `5c7936ef6`.

Local result: `providerSpikeJVM/test` 62 passed; `providerSpikeJS/test` 60 passed.
All six scenario suites and their mutation, frame-order and reference-hash
controls executed on both platforms. Original tolerances and caveat policies
remain binding. See `execution.json` and the complete compressed gate 68 log.

The unchanged warning gate rejected an initially deprecated array conversion
(gate 63); the exact historical supported conversion was restored and reviewed.

Final stdlib validation: 38 registered scenarios, 11 Python checks, 89 supported
test targets scheduled across 37 bounded batches. The previous combined 16-test
Python check also included pinned-dependency admission controls.

Independent source review approved the restoration and the one-line warning
repair. Fixture custody and the original failed hosted log are retained here.
Local execution is separate from hosted CI completion and native product
acceptance. Native59 remains an adverse 171 passed / 1 focus-failed / 29 blocked run.

The complete staged whitespace check flags historical whitespace in three exact,
hash-bound licence/notice files. Their bytes are preserved; the check passes for
every other staged path. See `retained-license-whitespace.json`.
