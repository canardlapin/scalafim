# Gale compatibility policy

Scalafim consumes Gale as its generic numerical substrate. The supported build
line is deliberately narrower than either project in isolation so that a single
artifact set can serve the JVM and Scala.js modules.

| Component | Scalafim policy | Verified migration baseline |
| --- | --- | --- |
| Scala | Scala 3 binary line; Scalafim compiles with 3.4.2 | Scalafim 3.4.2 consuming Gale built with 3.3.8 |
| sbt | version pinned in `project/build.properties` | 1.10.5 |
| Scala.js | exact plugin version shared with Gale | 1.22.0 |
| JVM | JDK 21 or newer for portable Gale core | JDK 22 and JDK 25 |
| Node | Node 22 or newer for Scala.js tests | Node 24.1.0 |

The Gale dependency must be an immutable released or source commit pin. A
developer-local `publishLocal` artifact, an unversioned sibling checkout, or a
floating branch is not an acceptable build input. The pin must identify both
the Gale revision and its artifact provenance, and its distribution must carry
the owner-selected Gale license.

Before changing the pin or any toolchain row, run a consumer gate that constructs
and multiplies a `gale.linalg.DMat`, invokes a representative spectral operation,
executes on the JVM, and completes a full-optimized Scala.js link. Then run the
full Scalafim `compileAll` and `testAll` aliases. Platform success is conjunctive:
passing only the JVM or only fast-linked JavaScript is not sufficient.

As of 2026-07-19, Gale commit
`d510ed72e88457eeb87f2ba16fc470b2ec646eed` contains the required migration APIs
and passes its JVM, Scala.js, full-link, parity, and backend gates. It is not yet
a consumable Scalafim pin: Gale intentionally has no owner-selected license,
canonical SCM remote, or publishing destination, so no release tag or artifact
is claimed here.
