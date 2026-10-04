# UMVPA M2.08 independent relational audit

Source base: `b1c5b0ae205c13e97a1218ab6852a29783872f6a`; isolated branch `work/umvpa-finish-20261001`. Pinned providers: Gale `18d24dbb5056122032b0278f8bad557a9bb1cf23`, Multivar `f74d631720d65147c51496dcbdd37c01912de1cb`, Alder `e555bad92307af1c2cbc104aef398cb9d9de88f0`, resample4s `6bc4172a966c92f1b06811eac64ac2bada9fef9b`.

Mote packet `bd-01M2BNFYS71SHY4RFPJP1DB5MY`. Independent read-only review found and repaired real defects beyond M2.07: conditional-error cache identity omitted exact endpoint sources/access; residual metric provenance used ambiguous case-class rendering; ordinary scalar traces lost small terms through cancellation; cache rejection reasons did not identify changed dependencies. A second review caught explanations blaming unchanged slots. All are repaired at their production source, with regressions.

Independent oracle constants: identity and residual metrics on the identical single ordered pairing yield 3 and 23/17, normalized by two features to 3/2 and 23/34. The separately named identity baseline retains its all-distinct unit-weight mean admission; the matched single-edge test uses an explicit identity closure through the general query. With H=1, K=[[2,1],[-3,-1]] and beta rows (1,2),(3,-1),(-2,4), contributions are -11 and -2; edge weights 2 and -1/2 yield sum -21 and mean -14. Reversing every edge and transposing K preserves both; failing to transpose yields mean 55/(3/2). Dense/operator/low-rank traces preserve diag(1e16,1,-1e16)'s exact trace 1 and refuse nonfinite results.

Identity mutation tests change only endpoint readout revision while preserving acquisition support and conditional reasoning: fresh assessment becomes descriptive and stale cached geometry refuses reuse. Residual source fields that collide under display rendering now give distinct metric identities. Actual LeaveOneGroupOut designs with changed grouping/membership and plan assignments invalidate affected products. Restricted beta rows retain the original shared acquisition support and remain descriptive; feature restriction does not create independent acquisition errors. Model-only changes name only that model parameter and leave geometry reusable.

Independent final review accepts the production changes and identified one invalid test admission, which was corrected without weakening the baseline. Focused gate48 passed 59 tests per platform before later regressions; gate54-rerun2 passed 29 tests per platform covering actual grouping and restricted beta support. Final owning gate61 is the authoritative latest-source gate (results below).

No origin declaration authenticates an external operator payload or supplies missing conditional error assumptions. Arbitrary streaming providers and universal unbiasedness are unqualified. These checks establish the declared source/provider laws, cancellation behavior and scoped invalidation paths.

Source SHA-256 at gate61:

```
5685a05a29467d26bd2547a93182114647b8e159c7c9b1a5b9260b2ff6be56ee  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/Queries.scala
d3fe6de6a87d8f90aaf8bb7c8496d01d3d87dc58f8fccc393bf03a1ab714a7e8  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/QueryReuse.scala
cf095eecfedda3d08ed11f92ea79cf423491dec161cdacb46120c2997fbca553  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/RelationConsumers.scala
6632134f421095ab3c78a9627d101d8e19ee97d4dd166e2c25bede15ee709a59  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/ResidualMetric.scala
ad1416a07fb9e428c4ea97cc1fea1b4de591d5d9d6b022ecbdec636205407b6e  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/PairingSuite.scala
f5c535a8730855aa32ed458dfa8eb6344a1fdfb738bd312165fe3e6d3e66bbb2  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/QueryOracleSuite.scala
0f47b05414035f34d4957f2a231b814dfe2a8ad4fdd9dd9e3045acd988c73154  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/QueryReuseSuite.scala
fb0656724d96a255964416ba364260163665d9853910797dfc7c40db4f8ca729  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/ResidualMetricSuite.scala
```

Final combined owning gate61 exited 0: mvpa 354 JVM/354 JS, mvpa-fit 49 JVM/49 JS, and mvpa-dataset 91 JVM/91 JS: **988 tests**. `scalafimCompileAll` passed both platforms in 260.6 seconds. No warning/error diagnostics occurred in the raw final gate. The frozen source manifest was rechecked unchanged.

Raw log SHA-256:

```
769b0e637ea40f32109a41fcc4383eabd0d618299f379ee97c151a9d60016666  gate61-owning-pattern-relational.log
54e4f8f45147da1946d8cd40e2e74900e4c1bd7a3f2c605e9b2397472d398efb  gate48-relational-audit-repair-rerun3.log
94431475e64a3993780b5766a3963e286c7a7f0715014fff7b8200b79559d1ed  gate54-m208-last-regressions-rerun2.log
```
