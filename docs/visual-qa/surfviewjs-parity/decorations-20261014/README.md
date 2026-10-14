# Annotation and network display evidence

Source candidate: `68dc4732fa30a26130d64a210da5c272800738af`. This gallery qualifies the bounded synthetic annotation implementation, not full SurfViewJS parity. C05/C06/C09 cortex/reference matrix rows remain unqualified.

The live Three.js backend and actual JavaFX SubScenes render disks, spheres, crosshairs, contours with halos, transparent labels, and weighted network lines/tubes. Glyph hits retain decoration IDs and view/instance origins; labels pass through to scientific picks. Source coordinates and uploaded scientific resources stay unchanged.

Validation: shared surface-view 193 tests on each JVM/JS; raster 41 on each; JavaFX 51; Three.js 41. Native JavaFX probes pass 3,824 typed/pass-through picks at 320x240 and 640x480. Label mask IoU is 0.9722 and all other primitive mask IoUs meet the unchanged 0.85 gate. Browser proof passes all 29 checks on its recorded Metal environment. Independent RGBA and mip fixtures qualify this device's texture behavior; automatic mip reduction is not assumed portable.

The parent visually inspected same-scale JavaFX/raster plates and browser texture captures. Filled blue geometry intentionally occludes the right half of depth-tested glyphs. Crosshairs use explicit overlay depth. World-size glyphs follow the declared projection; pixel-size glyphs retain screen size. These are controlled geometry specimens, not anatomical presentation examples.

Two adverse results remain here. Before premultiplied texture filtering, the browser alpha-edge red channel was 50 instead of 67. Before label ordering, a near translucent label hid the farther label entirely: RGB 128,0,127 instead of 128,64,63. Both now pass independent oracle probes in both submission orders. Native JavaFX restricts depth-tested texture geometry to camera-facing planes so object sorting is valid.

Opaque solid decorations and pass-through textured labels are the admitted P3 contract. Scientific surface translucency and clipping still belong to P4. Exact source binding, raw logs, browser lifecycle audits, and artifact hashes are included. Local landing, remote push, and final parity acceptance are separate events.
