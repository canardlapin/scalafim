# Stock JavaFX Metal capability checkpoint

The capability seam passes on this macOS/aarch64 machine for stock JavaFX
25.0.4 and 26.0.2. This checkpoint does not admit either renderer as the product
backend. It establishes a fail-closed way to prove which Prism pipeline started
and which JavaFX artifacts supplied it before the larger correctness, visual,
interaction, resource, packaging, and consumer gates run.

The forked probe captures Prism's supported verbose startup output on both
standard streams, parses the exact initialized pipeline class, and then records
the JavaFX runtime version, JDK and OS identity, requested pipeline order,
fallback policy, relevant JVM arguments, public JavaFX class origins, and
SHA-256 of the loaded base and graphics JARs. The implementation does not link
to, copy, or reflect over private Prism classes. Admission refuses a missing or
ambiguous initialization line, a pipeline other than the exact known
`com.sun.prism.mtl.MTLPipeline`, fallback, version drift, absent or unexpected
artifacts, artifact hash drift, and `--patch-module`, `--add-exports`, or
`--add-opens` arguments. Toolkit exit is in an outer `finally` block so a failed
startup or assertion does not deliberately leave JavaFX running.

Both successful lanes used exact ScalaFIM source revision
`c97f7af91b2c21fb99e332e27db1116dcf5cf4a8` on source base
`535977a86270bd3cc5c033eda165a0370870dd22`, plus the declared build-only patch
`bab79f11e8cf21bd7a48ca8da953d6972bba014429e5ea14fa99d5c5706ca033`,
and Intaglio `8bef37e36e91dc390833c1ee1fb841dad1db4b17` plus the already declared
consumer patch `0c70dce77637ebbb8485b189e2a4480ad27adfe22244323b196bc62037127655`.
The machine reported OpenJDK 25.0.1, macOS 14.3, and aarch64.

The JavaFX 25 lane selected Maven artifacts 25.0.4 and reported runtime
`25.0.4+2`. The base JAR hash was
`bb679b745988aa4658bb2d605c5fa5b213fcd29b441b8f111e93c58b4d870ec2` and
the graphics JAR hash was
`906fec9ec70c32066e99052b1b82262051a2dc287f9f94d83beec59730e1b0fd`.
The final log is
`/private/tmp/scalafim-metal-admission-native-c97f7af-20260912.log`, SHA-256
`93e829777b2306cc2e0a52ef9acbd2f040f0bf1385b17e3a59402d638053ffe8`.

The JavaFX 26 control selected Maven artifacts 26.0.2 and reported runtime
`26.0.2+3`. The base JAR hash was
`5a3ecdaafec0f6539e70137c354b4ca3f8472e023eaf5a5778f6739025d330a2` and
the graphics JAR hash was
`4529f5aee7adb2ae6b1be185418b48e90912c92cd2bb75b9bf90041e1f747ea6`.
The final log is
`/private/tmp/scalafim-metal-admission-native-jfx26-c97f7af-20260912.log`, SHA-256
`b486a800f9446e705e7ad26c7b2829be67334e4de55410d768487330b606dd0b`.

`JavaFxSurfaceProbeSuite` passed 19 tests after the review corrections. Three
real negative runs demonstrated that the native probe fails closed: it refused
a run with no captured initialized-pipeline line, refused `25.0.4` when the
runtime was exactly `25.0.4+2`, and refused an incorrectly transcribed JavaFX 26
base hash while printing the observed receipt and mismatch.

The full commands select `-Dprism.order=mtl`,
`-Dprism.noFallback=true`, `-Dprism.verbose=true`, and
`--enable-native-access=ALL-UNNAMED` through the forked test JVM's
`javaOptions`, then run `JavaFxRuntimeCapabilityProbe` with the exact runtime
version and both expected artifact hashes. The successful receipt records no
module mutation and no fallback in either lane.

The current run is classpath based and JavaFX reports its standard warning that
classes were loaded from an unnamed module. A clean packaged module-path launch
therefore remains required. The checkpoint also leaves open the matched
real-cortex beta/FIR visual matrix, independent scalar and pick oracles, at
least 20 alternating painted-frame updates, latency and memory/resource
measurements, other-platform outcomes, and exact PLS Neuro consumer admission.
Those gates decide whether stock JavaFX 25 Metal becomes the supported backend;
this capability checkpoint alone does not.
