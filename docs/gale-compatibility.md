# Gale compatibility policy

Scalafim consumes Gale as its generic numerical substrate. The supported build
line is deliberately narrower than either project in isolation so that a single
artifact set can serve the JVM and Scala.js modules.

| Component | Scalafim policy | Verified migration baseline |
| --- | --- | --- |
| Scala | Scala 3 binary line; Scalafim compiles with 3.7.4 | Scalafim 3.7.4 consuming Gale built with 3.3.8 |
| sbt | version pinned in `project/build.properties` | 1.11.7 |
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

The initial immutable pin is Gale commit
`ef540198b0cfd5678e14f85cdc7ea904f87812ba`. It contains the required migration
APIs, passes Gale's JVM, Scala.js, full-link, parity, and backend gates, and adds
the owner-selected Apache-2.0 license plus canonical Git and POM provenance.
Scalafim consumes the named `coreJVM` and `coreJS` projects directly from that
Git commit; advancing the pin requires the consumer gate above.

An earlier immutable pin was Gale commit
`2d9c8542607e1cdb78ce8bc1fd4d8ae5b96c8100`. It retains the migration baseline
and adds the portable projected generalized-Rayleigh kernel, typed termination,
and KKT/feasibility/normalization certificates required by constrained
canonical models. The Gale core suites at this revision pass 506 JVM and 496
Scala.js tests; ScalaFIM's consumer and full repository gates remain mandatory
for every subsequent pin advance.

The current immutable pin is Gale commit
`4485cc775ae8233789b019d24a920f86391e9523`. It adds `gale.linalg.DctBasis`,
the explicit DCT-II basis column factory that the `design` cutoff-period drift
basis and the `latent` temporal basis both construct their columns with. Gale's
`formatCheck` and `testAll` pass at this revision: 638 JVM and 625 Scala.js core
tests and 44 laws tests on each platform, with zero failures.

Advancing this pin does not by itself make the full repository green. Sibling
libraries carry their own Gale pins, and `image4s` at ScalaFIM's current pin
still selects Gale `d55fe2f97196a76ab7879e1a12f1e92403aeba06`, whose
`DMatBuilder` exposes `updateRowMajor` rather than `writeLinear`. Where both
Gale builds reach one classpath the older class shadows the newer, which fails
`latent` at compile time and the first-level-to-group scenario at run time with
`NoSuchMethodError`. Closing that gap requires advancing the `image4s` pin, not
a further Gale change.
