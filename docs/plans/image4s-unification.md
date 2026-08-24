# image4s and Ravel unification

This document records the completed first integration of immutable image4s
`Sampled` values and Ravel storage into ScalaFIM. Its former compatibility
rules are no longer the forward design contract.

The greenfield architecture is now specified by
[`../decisions/native-image-data-model.md`](../decisions/native-image-data-model.md),
and its repository-wide execution ledger is
[`native-image-migration.md`](native-image-migration.md).

In particular, the following transitional behavior is superseded:

- first-axis-fastest ScalaFIM array ingress;
- compatibility `linear(i)` and legacy-order export;
- the universal `ScalaFimValues` semantic tag;
- compact series shaped `(time, support-position)`; and
- duplicate ScalaFIM voxel-domain and ROI ownership.

The retained foundation is one immutable image4s `Sampled` value backed by one
Ravel array, with no parallel dense buffer. Current immutable dependency pins
are Ravel `f804ba51242aae3a1442b3855a20bd896ffa8b64`, image4s
`31bc8f87d8349fd3296496979c95eeb3ec11ae21`, and locus4s
`58c9739be51345ad9adc4bc9c9e7335023254ec9`. Local sibling overrides remain
diagnostic only; release evidence uses those immutable Git coordinates.
