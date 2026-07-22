# Graphics extraction and standalone publication

Status: extraction-ready contract. Moving the modules to another repository or
publishing release artifacts is a separate delivery step.

## Frozen boundary

The lift-and-shift unit is `graphics` plus its four renderer artifacts:

| Artifact | JVM | Scala.js | Dependency |
|---|---:|---:|---|
| `scalafim-graphics` | yes | yes | none |
| `scalafim-graphics-svg` | yes | yes | `scalafim-graphics` |
| `scalafim-graphics-canvas` | no | yes | `scalafim-graphics` |
| `scalafim-graphics-java2d` | yes | no | `scalafim-graphics` |
| `scalafim-graphics-javafx` | yes | no | `scalafim-graphics`; OpenJFX is provided |

The core owns plotting semantics, scenes, device resolution, layout, and the
renderer conformance contract. Backends remain thin interpreters. Image,
design, and other ScalaFIM domains are downstream consumers and are not part of
the extraction.

`GraphicsExtractionGuardSuite` runs under `graphicsJVM/test` and therefore
`testAll`. It rejects production imports from any other `scalafim` domain,
requires every production package to remain under `scalafim.graphics`, checks
that core has no internal build dependency, and checks that every backend
depends only on core with its declared platform matrix.

## Namespace decision

The initial standalone release keeps `scalafim.graphics` and its backend
subpackages. Retaining the namespace lets current consumers change artifact
coordinates without rewriting source and makes extraction a repository/build
operation rather than a semantic refactor. A neutral namespace or project name
can be considered only as a later, separately versioned migration; it is not a
prerequisite for publication.

## Versioning and publication

The extracted repository owns one version line for the graphics family,
independent of ScalaFIM. All artifacts in a release use the same version, Scala
version, and Scala.js binary suffix. The first release may begin at `0.1.0`;
subsequent compatibility policy and release notes belong to that repository.
Core, SVG, Canvas, Java2D, and JavaFX remain separately selectable artifacts so
portable consumers never acquire a platform renderer or toolkit transitively.

Publication needs the ordinary standalone metadata that the monorepo currently
inherits: organization, licenses, SCM, developers, versioning, signing, and
repository credentials. OpenJFX stays `Provided`. The release gate publishes
both core platforms, both SVG platforms, Canvas for Scala.js, and Java2D/JavaFX
for the JVM from one tagged source revision.

## Lift-and-shift procedure

1. Copy the five `modules/graphics*` directories without changing source files
   or package declarations.
2. Copy only the shared Scala 3, MUnit, cross-project, and Scala.js build
   settings they use; declare the artifact matrix above.
3. Run `graphicsJVM/test`, `graphicsJS/test`, and all backend suites. The shared
   conformance cases remain the cross-backend behavioral oracle.
4. Publish a candidate family version and compile a small JVM and Scala.js
   consumer against artifacts rather than source directories.
5. Change ScalaFIM's graphics dependencies to the published coordinates and
   rerun `compileAll` and `testAll` before deleting the in-tree modules.

No plotting, layout, unit, guide, or renderer behavior changes during these
steps. Any such change is normal library development and must land before or
after extraction with its own tests and version decision.

## Current verification

From the ScalaFIM root:

```sh
sbt graphicsJVM/test graphicsJS/test \
  graphicsSvgJVM/test graphicsSvgJS/test graphicsCanvasJS/test \
  graphicsJava2dJVM/test graphicsJavafxJVM/test
```

The final monorepo handoff additionally requires `sbt compileAll testAll`.
