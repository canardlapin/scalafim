# scalafim examples

Runnable examples live outside `modules/` so library modules stay focused on
public APIs and tests. Each examples project is non-published and has smoke
tests so snippets do not drift away from the code.

Current examples:

- `surface-jvm`: FreeSurfer/GIFTI surface IO and labeled-surface parcel
  workflows.
- `atlas-jvm`: standard atlas descriptors, explicit JVM loaders, coordinate
  lookup, parcel reduction, and atlas-to-MVPA feature plans.
- `workflows`: one cross project for portable workflow examples, with JVM-only
  FreeSurfer and platform bindings kept in their platform source roots.

Run all examples smoke tests with:

```sh
sbt examplesTest
```

Add future examples with the same shape:

```text
examples/<name>/
  README.md
  src/main/scala/scalafim/examples/<name>/
  src/test/scala/scalafim/examples/<name>/
```

Keep real-data examples explicit about paths and cache policy. Examples should
not perform hidden downloads.
