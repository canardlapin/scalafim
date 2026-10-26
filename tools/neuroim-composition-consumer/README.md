# neuroim-composition-consumer

Standalone acceptance consumer for immutable neuroim4s `4f75c2bf141449d032f9fe3c558ef13a22a4d59f`.

Run `sbt "runMain consumer.NeuroimCompositionConsumer"` in this directory. The consumer writes and reads synthetic noncubic NIfTI volume and 4D series fixtures with typed world evidence, selects a series volume, loads and interprets an ITK text transform, renders a transformed slice with Java2D, and checks the actual pick transition, literal value and pixel oracles, and foreign-frame refusal. It rejects unrelated runtime artifacts and removes its temporary files. The single terminal scenario line reports named steps and PASS; an error reports FAIL and throws.
