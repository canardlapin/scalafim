# Upstream request draft: HalfFlow geometry ownership

Request prepared for the reframe4s owner under STP-P8.04. It has not been
submitted to an upstream tracker or published; the packet forbids foreign-store
mutation and publication.

Pinned ScalaFIM provider: `9a4508351d74567147b8ea3221d82db89e5892b0`.
Development inverse provider inspected: `842ec9a752d76793f936d7024681d50de89117db`.

HalfFlow retains private geometry implementations under
`modules/reframe4s-halfflow/shared/src/main/scala/reframe4s/halfflow/internal/`,
including `Affine.scala` and
`DenseFieldMorphism.scala`. Please replace those geometry owners with image4s
capabilities and retain only HalfFlow scientific policy and execution adapters.
A migration should bind its numerical and ownership checks to the exact provider
candidate. No consumer pin should claim this cleanup until that evidence exists.

This request is bounded to the existing housekeeping criterion. It creates no
new ScalaFIM abstraction, dependency or ticket.
