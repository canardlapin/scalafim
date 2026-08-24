addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.4.4")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")

// 0.1.1 labels a Java-23 classfile as a Java-22 multi-release entry.  Pin the
// corrected artifact so the formatter gate runs on the project's JDK 22.
dependencyOverrides += "io.github.alexarchambault" % "is-terminal" % "0.1.2"
