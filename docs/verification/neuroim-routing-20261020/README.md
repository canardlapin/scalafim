# Anatomical routing: publication and qualification

The approved graph4s and neuroim4s commits are published on `main`, with exact
checkout/tracking/live-remote SHA parity recorded in
[publication.json](publication.json). The fresh default-pin check exposed a
stale transitive graph4s pin in mesh4s. A one-line correction is qualified
locally and awaits separate publication authorization. Routing remains in Mote
review until that correction is published and default pins pass without overrides.

| Repository | Exact source | State |
| --- | --- | --- |
| graph4s | `856e6fb770cf5f4c5512b0607d8c3733e75e7460` | Published on main |
| neuroim4s | `7ed49f12b1a90e18ea5ed67d48343e98a7b9547b` | Published on main |
| mesh4s | `53973bf8f764ed280b639fa36df469eb1891c642` | Local candidate; published parent `54b7278880e6db38c543a4a565355afd864e576a` |
| ScalaFIM | `d3e7d6067c64c527b04c2ea9afd01ef1ef4f7bbb` | Local source cutover and pin guard |

Gale remains `779ed3d8169cdd9f4eb9ab10b1ca9defadc27715`. All other pins are
unchanged. The accepted `eco/u3-topology-20261019` branch remains at
`e98cc987041c3ea013c3637ba3f48cb7e24877a3`; the root source checkout is untouched.
The mesh candidate changes one dependency revision line, with no API, numerical,
resource, geometry or wire-format changes. Its verified recovery bundle is
retained with the raw logs.

## Coherence correction

The initial local check used a graph4s source override that was forwarded into
mesh4s. This masked mesh4s's default `6a72b58` pin. The first published-source
build loaded both `6a72b58` and `856e6fb`: its independent producer tests passed,
but the combined graph failed the one-revision acceptance rule. That adverse
loaded-build snapshot is retained in the publication receipt.

With the corrected mesh candidate, a loaded-build audit verifies one revision
per repository across 19 loaded source builds. The current consumer gates use
**only** a mesh4s checkout override; graph4s and neuroim4s resolve from their
published immutable Git pins. Full no-override acceptance is still pending.

The existing `tools/prepare-pinned-dependencies.sh` guard now checks the mesh4s
and neuroim4s graph4s pins before bootstrapping dependencies. Eight offline tests
pass, including mismatched/missing pins and multiline declarations. The new
regression cases failed against the old guard; their expected failures are
retained rather than overwritten.

## Verification

**1,532 tests passed**, with two existing optional JS tests skipped:

| Gate | Passed |
| --- | ---: |
| Published graph4s algorithms and neuroim4s routing/reader consumers | 68 |
| mesh4sCheckAll, including JVM/JS laws and allocation courts | 164 |
| ScalaFIM surface JVM / JS | 322 / 279 |
| ScalaFIM spatial, atlas, MVPA-spatial and examples JVM | 384 |
| ScalaFIM spatial, atlas, MVPA-spatial and workflow examples JS | 307 |
| Dependency pin guard | 8 |

All ScalaFIM modules and examples compile on JVM and JS. The existing isolated
native atlas oracle and atlas-to-MVPA workflow pass. The skipped JS checks are
the optional real-field pointwise placement and same-size hemisphere timing
courts. Optional timing budgets were not enabled. No all-repository test run,
hosted CI verdict, independent review or new native visual qualification is
claimed.

[publication.json](publication.json) records exact commands, code revisions,
exit statuses, test summaries, loaded source revisions and SHA-256 hashes.
[qualification.json](qualification.json) retains the earlier 759-test local
candidate evidence. Full raw logs and verified thin provider bundles live under
the shared Git directory's `verification/neuroim-routing-20261020/`.

## Ownership and behavior

Volume-only admission, registration routing, sampling and scoped reads live in
optional neuroim4s modules; generic directed weighted routes live in graph4s.
ScalaFIM retains mixed surface/hybrid and functional field integration, with
replaced volume and NIfTI implementations deleted. The remaining symbols have
explicit [destinations](ownership.md).

The checks preserve actual provider grids/frame owners, forward-first inverse
routing, parallel-edge order, inverse penalties and minimum route quality;
coordinate composition order, single-pass value sampling and boundary coverage;
sparse support, target/observation order, lazy preparation, cache freshness,
endian/scaling and closed read resources. Missing maps remain non-executable
even for an empty target demand. Existing recipe/fingerprint prefixes and
geometry text are retained. The NIfTI field adapter now refuses non-volume
geometry at preparation, before constructing the volume-only reader.

## Reproduce

From this exact ScalaFIM source, use the sole temporary override:

```text
-Dscalafim.mesh4s.build=/path/to/mesh4s-at-53973bf
```

Run bounded sbt batches with a 4 GiB heap:

```text
surfaceJVM/test surfaceJS/test
spatialJVM/test atlasJVM/test mvpaSpatialJVM/test atlasExamplesJVM/test workflowExamplesJVM/test
spatialJS/test atlasJS/test mvpaSpatialJS/test workflowExamplesJS/test
scalafimCompileAll examplesCompile
python3 -m unittest discover -s tools/ci -p test_pinned_dependencies.py
```

Run `mesh4sCheckAll` in the exact mesh candidate. Published producer gates use
`neuroimRoutingTestAll` plus graph4s algorithms on both platforms. The receipt
contains the exact source-qualified project syntax and the existing owned sbt
global base used to avoid the known fresh-template Zinc path issue.

Production Scala deltas remain ScalaFIM **-662**, neuroim4s **+1,194**, graph4s
**+94**: ecosystem **+626**. The unused ScalaFIM meta-build helper deletion removes
another **274** build Scala lines. No ScalaFIM module is added; neuroim4s gains
two optional artifacts. This intermediate slice does not establish an
ecosystem-wide LOC or module-count reduction. U6 must assess the final counts.
