# Multivariate inference moved to Multivar

ScalaFIM's former `modules/inference` prototype is now the standalone
`multivar-inference` artifact:

```scala
libraryDependencies +=
  "io.github.canardlapin" %% "multivar-inference" % multivarVersion
```

Source imports move from:

```scala
import scalafim.inference.*
```

to:

```scala
import multivar.inference.*
```

Resampling actions and deterministic seeds are supplied by Resample4s. The
scientific design, frozen R fixtures, typed compiler, ordered ladders,
structured actions, stability summaries, and family protocols moved together.
Historical fixture identifiers beginning with `scalafim-inference-` remain
unchanged compatibility data.

ScalaFIM does not re-export the new package. Future ScalaFIM integration should
be a narrow downstream adapter over fitted or domain-specific data, not a
second inference implementation.
