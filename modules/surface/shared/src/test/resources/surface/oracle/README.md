# DCBC fs-LR 32k GIFTI oracle

`fs_LR.32k.L.midthickness.surf.gii.gz` is a deterministic outer-gzip copy of
the left fs-LR 32k midthickness surface published by Diedrichsen Lab's DCBC
repository. The source repository applies the MIT license reproduced in
`LICENSE.DCBC.txt`.

The checked-in file is test corpus, not a ScalaFIM mesh interchange format.
Platform ingestion still owns conversion from raw GIFTI bytes to the local
typed `SurfaceGeometry` rendition.

`manifest.json` pins the upstream commit, Git blob, source and container
checksums, and an independent NiBabel 5.3.2 oracle. Ordinary JVM and Scala.js
tests consume only this local, checksum-verified corpus and never use network
state.
