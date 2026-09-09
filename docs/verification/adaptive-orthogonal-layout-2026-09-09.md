# Adaptive orthogonal layout and focused planes

The renderer-neutral viewer can choose its arrangement from the physical slice
proportions and current viewport, with stable logical-pixel margins and gaps.
SinglePlane presents one anatomical plane without changing the shared cursor,
image geometry, convention, display settings or stored per-plane navigation.
Hidden planes are not sampled, drawn or picked. The [viewer guide](../image-viewer.md)
describes the public API and visible receipt contract.

The [source-bound receipt](adaptive-orthogonal-layout-2026-09-09/receipt.json) records
35 JVM and 35 JavaScript image-view tests, plus JavaFX 2, Java2D 2 and Canvas 5
consumer tests, all passing. Five new shared tests check an independent rectangular
packing oracle on six anisotropic viewport sizes, fit/aspect/inset/gap bounds,
portrait and wide improvements, every focused plane, visible-only sampling and
pointer targeting, state preservation after resize and return to All, and tiny
viewport behavior. The [complete log](adaptive-orthogonal-layout-2026-09-09/test.log)
retains one pre-existing linop4s build-definition warning; no changed-source
compiler warning was emitted.

This is a local, uncommitted provider candidate. It maximizes fitted image area
among the existing L, row and column arrangements. It does not solve arbitrary
rectangle packing, and these toolkit-free checks do not establish native visual
quality, application selector/persistence, or end-to-end interaction latency.
PLS Neuro integration is tracked separately by bd-01M23RG0ED4BXVPT9KT18R135Y in
its own store; this native implementation is bd-01M23XCRSS05C30WNWZKCM74DM.
