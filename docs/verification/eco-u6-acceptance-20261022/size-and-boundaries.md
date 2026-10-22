# Size and ownership comparison

Git object counts compare plan baseline `86b3acf1e64d762be99b52ae146fa115b9050356` with frozen Root `92154d81c39b6a384be195dde0e933ed0dff29ca` and the exact final provider pins in report47. They count tracked production Scala under `modules/<name>/{shared,jvm,js}/src/main/scala`; physical lines include comments and blanks. Tests, examples, assets, generated sources and modules without in-scope sources are excluded. A module root is a source directory, not a count of sbt platform projects.

| Repository | Source-bearing module roots | Production physical lines |
| --- | ---: | ---: |
| ScalaFIM before | 49 | 214,313 |
| ScalaFIM after | 34 | 132,082 |
| neuroim4s final | 11 | 18,853 |
| neuroatlas4s final | 8 | 8,179 |
| mvpa4s final | 6 | 20,690 |
| neurosurf4s final | 16 | 35,692 |

ScalaFIM has 15 fewer source-bearing module roots and 82,231 fewer production lines (38.4%). The four standalone libraries have optional modules that make their dependency boundaries explicit. These figures demonstrate redistribution and a smaller functional-analysis repository; they do not establish net ecosystem shrink or identical functionality per line.

The Root baseline does not pin these four repositories, so repository-specific before counts cannot be inferred or set to zero. Report48 instead defines explicit former ownership areas:

| Former Root ownership area -> final library | Before roots / lines | After roots / lines |
| --- | ---: | ---: |
| image, image-view variants, transform -> neuroim4s | 6 / 19,192 | 11 / 18,853 |
| atlas -> neuroatlas4s | 1 / 7,594 | 8 / 8,179 |
| mvpa -> mvpa4s | 1 / 15,816 | 6 / 20,690 |
| surface, surface-view variants -> neurosurf4s | 5 / 29,007 | 16 / 35,692 |

These are ownership-area comparisons rather than equivalent implementation inventories. Mixed `spatial`/`mvpa-spatial` and functional integrations are excluded; the old image/surface areas include generic machinery whose proper owner is elsewhere. Final providers contain new and optional work. The scripts list exact paths, per-module figures and exclusions.

Dependency counts are deliberately labeled by their actual measurement. For whole Root `build.sbt`, lexical `.dependsOn` call sites change127->119, `ProjectRef` call sites59->108, and `libraryDependencies` mentions29->15. Final whole-provider figures are:

| Provider | .dependsOn sites | ProjectRef sites | libraryDependencies mentions |
| --- | ---: | ---: | ---: |
| neuroim4s | 26 | 27 | 6 |
| neuroatlas4s | 17 | 15 | 2 |
| mvpa4s | 17 | 32 | 2 |
| neurosurf4s | 35 | 37 | 6 |

These are declaration syntax counts, not distinct resolved edges: helpers, macros, platform projects, test mappings and transitive dependencies affect the graph. Report48 includes old project-definition syntax counts, with a different scope; those must not be compared as resolved before/after dependency totals. Boundary acceptance instead uses actual Root Compile closures and independent minimal-core classpaths, with explicit optional-dependency exclusions.

Reproducible inputs and counts: `scalafim-eco-size-report-47.json`/`.py`, `scalafim-eco-ownership-slices-48.json`/`.py`. The scripts read Git objects without checking out or changing source.
