# Composition qualification checkpoint

Candidate: `e2df2a9e83b9792b06837881aeab8ccd4f263d47`. Local native rendering proof for the P2 composition
milestone. This does not qualify the full SurfViewJS C01–C12 matrix; annotation
rendering, mapping/visibility completion, flat-map hosts, and publication work
remain outstanding.

- Shared suite: 174 JVM / 174 JS passed at `eb306d9d`; the final guard corrections
  passed all 20 composition tests on each platform at the candidate above.
- Raster: 33 JVM / 33 JS. Three.js: 36 JS. JavaFX: 51 JVM.
- JavaFX native probe: 3,161 original-identity interior picks; shared crossing
  depth independent of submission order; four panels; camera isolation; resize;
  display translations; perspective projection; rejected-resource image unchanged.
- Three.js native browser: nine required checks passed on installed Playwright
  Chromium with ANGLE Metal. Includes distinct panel pixels and actual native pick
  ownership, one-mesh reuse, zero camera-only uploads, depth winner in both orders,
  original face-1 barycentrics, injected constructor failure and recovery.
- Browser contexts and server closed; final ownership audit found no automated
  top-level browser process. Browser version/module SHA and GPU are in provenance.
- Independent source review accepted this candidate's corrections: topology and
  geometry revisions stay distinct, face provenance is bounded, finite frusta are
  required, saved per-view framing is validated. Review performed no executions.

[Open the paired-image gallery](index.html). JavaFX/raster pairs share inputs;
Three before/after images prove camera isolation. These images are not presented
as completed SurfViewJS appearance parity. The earlier recovery gallery retains
the actual SurfViewJS color mismatches.

Raw logs retain platform runtime warnings and original bytes. Java 25 emits
native-access/Unsafe warnings; Scala compilation produced no source warnings.
