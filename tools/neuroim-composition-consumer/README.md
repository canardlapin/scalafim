# neuroim-composition-consumer

Standalone acceptance consumer for immutable neuroim4s `21df6416e4e28e97cfbe39e20d7303349be7e4cf`.

`runMain consumer.NeuroimCompositionConsumer` writes and reads a noncubic NIfTI volume with typed world evidence, maps a query point through a public `WorldTransform.Linear`, renders the resulting slice through the Java2D host, picks the rendered panel, and checks the analytic voxel value and foreign-frame refusal. It rejects a loaded ScalaFIM or surface artifact on its runtime classpath.
