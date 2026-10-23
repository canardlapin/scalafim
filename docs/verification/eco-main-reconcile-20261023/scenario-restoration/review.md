# Bounded independent source review

Reviewed the restored surface courts and CI registration delta against the original
contracts at `2010ebc77^`, with scientific registry contracts at `5c7936ef6`.
Verdict: APPROVE bounded source repair, pending JVM and Scala.js execution.

The existing provider-spike court is reused. Four transform courts were already
retained. Two missing surface courts are restored with 37 exact historical fixture
blobs. Numerical thresholds, native references, mutation controls, compile-time
frame refusals and caveat policies remain unchanged. Only public API ownership,
frame-error wrapping, immutable coordinate reads and the JS resource locator adapt.

SurfaceChainScenarioSuite.scala SHA-256:
`174e86b330ab8599807f50fbb089fabb071b43388889087cc99a3946dbf7a361`

SurfaceRealChainScenarioSuite.scala SHA-256:
`986cf7b86791ca8fba09461e651dad5526a10cdd79829dced18dc40cb109d25c`

Two test-only provider dependencies are added; production code and source pins
are unchanged. All six manifest contract hashes agree with the historical
registration, and both integration targets remain scheduled once in full CI.

The review does not establish numerical execution, hosted CI completion or native
product qualification. Those require their own completed receipts.

Follow-up review approved restoration of the historical supported `IArray.genericWrapArray` conversion after the warning gate rejected deprecated `IArray.toArray`. No tolerances or values changed.
