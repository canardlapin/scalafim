# image4s incremental output: ScalaFIM adoption checkpoint

> Subsequent correction (2026-09-09): the canonical image migration already exists on ScalaFIM main. The failures below describe the older active branch and an abandoned compatibility experiment, not a missing canonical implementation. The canonical provider/consumer bridge now passes both image platforms and direct dataset/fit suites. See [the canonical qualification](canonical-incremental-output-2026-09-09.md) for the exact four-path candidate, tests and remaining branch-integration requirements.

9 September 2026. Output adoption `bd-01M21H6P1908Q5PGSZYWQZHWRN` remains open.
Required native migration: `bd-01M23Q9Q92SBD5PHZZJYVY9EC3`.

## Result

| Image module | Current pinned baseline | Migrated candidate |
| --- | --- | --- |
| JVM | 247 passed, 0 failed | 212 passed, 35 failed |
| Scala.js/Node | 234 passed, 0 failed | 201 passed, 33 failed |

The unmodified ScalaFIM source first produces 84 compiler errors against the
current image4s candidate. A seven-file isolated migration clears compilation:
`NeuroSpace`, `Image4sInterop`, `MorphismFields`, `NeuroSlice`, `NeuroVol`,
`NeuroVec`, plus the convention assertion in `TypedImageCoreSuite`.
It preserves generic compatibility values under a separate `LegacyImageValue`
semantic tag, rather than granting continuous/categorical/mask capabilities.
These source edits are **experimental and not installed in native ScalaFIM**.

The remaining failures expose the identity contract: legacy `NeuroSpace` and
`VolumeSpace` are opaque aliases and `GridCompatibility` relies on equality;
current image4s `SampleSpace` equality means runtime identity. Derived spatial
spaces retain grid/frame ownership but need not be the same sample-space object.
Independently constructed legacy spaces have different ephemeral frame owners.
Affected tests include sparse masks/series, ROI selection, resampling, roundtrip
geometry and compatibility. They must not be silenced by comparing affines alone,
merging all frames, skipping tests or downgrading scientific ownership checks.

## Completed prerequisite

Restored checked D2/D3 refinement in native image4s issue
`bd-01M23PZVZKGA7CY5A2T8F17WSN`. It preserves exact owners and hidden frame types,
rejects wrong dimensions, and introduces no consumer-local cast.
Core suites: 89 JVM / 85 JS; NIfTI suites: 54 JVM / 36 JS, all passing.
The original writer's 114 recorded inputs matched the native checkout before
this follow-on; the new provider bundle records the changed core and added test.

## Next implementation boundary

1. Separate legacy numeric-grid compatibility from identified physical-frame
   alignment; preserve frame ownership through derived spaces and define explicit
   admission of separately loaded or persisted geometry.
2. Audit equality, selection, sparse support, resampling and downstream
   dataset/fit/application consumers, with unit/convention/non-spatial-axis refusal
   tests and passing baseline/candidate gates on both platforms.
3. Adopt the exact provider closure only after that migration. Then implement the
   scientific selected-map binding and native sink/catalog; application W5 remains
   downstream. The withdrawn ScalaFIM codec draft must not be reused.

## Reproduction and limits

The [compressed bundle](image4s-output-adoption-2026-09-09.json.gz) embeds baseline
and candidate sources, native and focused build definitions, provider inputs,
and full logs. SHA-256: `fc562252aa63e5c05568e78d3bd456c41d59e5069d89438d05d2cef3dc7084b1`.
Provider evidence SHA-256: `c81a7b34c4e9a6b2a5960d802c4d84676e84e51d65ab3b409715a1abd50e81cf`.

Candidates: `/private/tmp/scalafim-output-legacy-baseline-1`,
`/private/tmp/scalafim-output-resume-1`, and
`/private/tmp/image4s-output-refinement-1`. Run the focused `imageJVM/test` and
`imageJS/test` tasks. The migration uses the explicit property
`-Dscalafim.image4s.build=/private/tmp/image4s-output-refinement-1`;
the baseline uses the unchanged immutable pin.

The full native build cannot load the pinned Alder composite's implicit
`../gale` dependency in this isolated layout. The focused harness extracts the
unchanged image/locus-data definitions and dependencies, not a replacement test
implementation. This is module-level evidence, not full-repository acceptance.
The workspace impact tool also rejects uncatalogued `.backups` and `plsneuro`;
no workspace declarations were changed. Local Java25/Node26 evidence does not
prove hosted JDK17/21. Native source/build pins, commits and publication are unchanged.
