# UMVPA M2.07 relational consumer verification

Source base: `7bbae8bfff2b982537102eee3e8db4ddd376fe1f`; isolated branch `work/umvpa-finish-20261001`. Mote packet `bd-01M2BNFWEJFFZYMGJB3Z41RGKP`.

One admitted relation set now supports signed crossvalidated RDM, owned retained geometry, RSA comparison, first-order contrasts and typed rectangular products. Geometry and comparison receipts form separate dependency nodes. Preparation/noise mismatches explicitly require refitting. Missing effects and numerical failures are method-owned outcomes; they are not dropped or converted into universal payload cases.

Pearson and average-tie Spearman use a stable shared numerical kernel. Partial Pearson normalizes centered controls and uses Gale QR with explicit rank and residual tolerances, an intercept, interleaved two-column RHS and typed singular/zero-residual results. Independent expected partial correlation is 1/sqrt(3), including opposite extreme scales and large offsets. Empty controls explicitly mean intercept-only Pearson.

Rectangular endpoints carry exact nominal axes. Tests include 2x3 orientation/reversal, compile-time foreign-axis refusal, missing effects before placeholder reads and framed provenance whose case-class display strings collide. Ordinary samplewise geometry is separately admitted and preserves repeated-item cross-block exclusion, exact item membership, reordering and undefined rows. Feature-model regression is explicitly dispositioned to the already independently qualified AlderFeatureModel predictive adapter; legacy workflow removal belongs to M1.12/M2.09.

Immediate native scalar/RDM/RSA work requires explicit stable scoped replay. The provider declaration alone is insufficient: acquisition mints a private capability for the exact guarded readout; every forward/adjoint application still checks closure. Expiration precedes resource cleanup, including returned/thrown failures. Retained consumers require owned replay and refuse scoped sources before callbacks. Public Relation constructors reject borrowed scoped witnesses; provider-private construction and restrictions preserve the legitimate original lifetime. Witnesses retain only declaration strings and expiration state, with immutable dependency equality/hash after expiration.

The matrix-backed native TrialReadout oracle has run means [1,10], [2,20], [4,40] and their negatives. Its signed feature-normalized RDM is (-50.5,-454.5,-202); detached Pearson against (1,2,3) is -3sqrt(3)/14. It remains inspectable after closure; escaped direct/restricted forward and adjoint applications refuse. Other cases cover nested/partial acquisition, independent same-revision lifetimes and read-free witness-transplant refusal. Arbitrary streaming providers are not admitted merely because they offer a callable operator.

Independent read-only review found and repaired three earlier consumer defects (large-offset centering, missing rectangular effects, display-string receipt collision), followed by the scoped-witness transplant defect. Their regression cases are included. Identity/provenance declarations do not authenticate an external operator payload or unrecorded analyst access; numerical adapter budgets exclude resident operators, provider scratch and object overhead. Signed geometry does not acquire an unbiasedness claim from resource ownership.

Final owning gate44 exited 0: mvpa 343 JVM/343 JS and mvpa-fit 49 JVM/49 JS, 784 tests total. `scalafimCompileAll` passed on both platforms in 236.8 seconds. No Scala compiler warning/error diagnostics occurred; the sbt GC advisory and shared Ivy lock waits were environmental diagnostics, not source warnings.

Evidence directory: `/private/tmp/scalafim-umvpa-finish-evidence-20261001`. Focused gate40 passed 48 core tests on JVM and JS; gate42 passed 11 native and 7 admission tests on each platform. Gate43 passed the provider-private attachment regression, 11 native and 22 consumer/admission tests per platform. The final owning gate includes the subsequent scoped cleanup tests: rejected nested acquisition preserves the outer capability, partial acquisition expires the first scope, and returned/thrown close failures observe expiration before each callback. M2.08 independent qualification and M2.09 cutover/deletion remain separate open packets.

Source SHA-256 at final gate:

```
6ad533a52d5337e6c86ccc5ef598dce0dca46f913fe1e2cf1947eb8a3c63ea17  modules/mvpa-fit/shared/src/main/scala/scalafim/fmri/mvpa/fit/IdentifiedReadoutRelations.scala
7d3bc55fca2f208deeeb9ea78572809eedb538a73bde13beadc540c287e7dbfb  modules/mvpa-fit/shared/src/test/scala/scalafim/fmri/mvpa/fit/IdentifiedReadoutRelationsSuite.scala
334b147dce2d1e954f94b490fa412bdab2c4df68b9dadbf39bc76f4e2bd315b0  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Rsa.scala
cf542a4c484ee975e6715ee3cb617dfe54d0c593acd54a67c56a110dd9defb59  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/RsaScoreKernels.scala
5fafe87b6a7d0865d468c1484f8648b109f853a1ff29253f550d40dfaea5fc39  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/Queries.scala
656f51aae93a83aa420a82e4d571ff787a7b46f789020b23ae0d056ac02a4eee  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/QueryReuse.scala
6b47f38992922609d55188d4bfc5e5b22f57a38ccfcbc2beefceb68e709f5c85  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/RelationConsumers.scala
5ddc450d9dff6cd19798271ee803ab4ef9e56999177fb07261ace403582d1b52  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/Relations.scala
71edea7fa35a8c75a496ecfcb75dca0db8433e82a6631eabbef301327509a330  modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/relation/ScopeReplay.scala
26317352cdd3a44d52df80a198c08a745f07fe2a895f86c71e9bb1afb19481b7  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/RsaScoreKernelsSuite.scala
5a148fda1058c7fdb6871456d7c4c8b31f2eba2d56297677e4ad9d6ba3f5de7c  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/RelationConsumerAdmissionSuite.scala
c3e412f2fbf871166bd7bd842d10e5f414f5d33e25f517520baac5dced7d6ae2  modules/mvpa/shared/src/test/scala/scalafim/fmri/mvpa/relation/RelationConsumersSuite.scala
```

Raw log SHA-256:

```
0f64b453536f50478104e2f1928dcc53a4a64e540186d1ddc43ac621b4a71131  gate40-core-scoped.log
2fb9cb9f1d528d2620a4380e7bcb5bf7dfdcec70337e38453592c1ec2019f8cb  gate42-native-dense.log
d0c8f121b05040f9f458e79a10eae1771a1e4898b188813f660601ab72d2a4df  gate43-scoped-attachment.log
c59c55170819ec66dd8c0b3cb8f7ad3d117f9d449620e4759d894789ac371397  gate44-relational-owning.log
```
