# Native colour publication review

Initial candidate: `229a65f3e403f043a363ead4625e98248d9dcda4`.
Corrected candidate: `140e4ab0a75e116d4172374318d158ec93e06c1a`.

Independent read-only review identified a material defect: changed colour buffers could retain mounted resource keys, causing a new picking plan to be published without uploading its pixels. Resource keys alone were insufficient evidence of clean native state.

The correction compares actual topology, geometry and colour buffers, plus layer metadata. Same-key changes now trigger the necessary native operation; equal-valued replacement colours remain clean. The actual Application-thread probe rejects buffers substituted after preparation without changing pixels, accepts the original prepared blue update, checks its displayed atlas pixel and verifies token expiry. The native suite passes 59 tests; the Application-thread probe passes. Raw evidence is in `checks/native-preparation-review-gate-02.txt`.

Independent re-review of the corrected exact SHA found the blocker resolved with no remaining material findings in this fix scope. The reviewer read code and the retained log and did not rerun builds. This review does not qualify GPU interpolation, performance, or rollback after arbitrary native exceptions. Affine atlas encodings remain refused by production capability validation.
