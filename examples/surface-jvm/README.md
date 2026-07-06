# surface-jvm examples

Runnable JVM examples for `scalafim-surface`.

These examples cover:

- reading a tiny FreeSurfer/SUMA ASCII surface;
- reading a tiny ASCII GIFTI surface with a POINTSET transform;
- deriving mesh topology from loaded geometry;
- building a `LabeledSurface`;
- extracting parcel units, representative vertices, and boundary contacts.

Run the smoke tests:

```sh
sbt surfaceExamplesJVM/test
```

Inspect bundled FreeSurfer and GIFTI geometry:

```sh
sbt "surfaceExamplesJVM/runMain scalafim.examples.surface.inspectExampleSurfaces"
```

Run the labeled-surface parcel workflow:

```sh
sbt "surfaceExamplesJVM/runMain scalafim.examples.surface.summarizeSurfaceParcels"
```

The example project bundles tiny text fixtures under `src/main/resources` so the
commands run without external data. The same methods also accept explicit
`Path` values when you want to inspect real local surface files.
