package scalafim.graphics

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class GraphicsExtractionGuardSuite extends munit.FunSuite:
  private val graphicsModules =
    Vector("graphics", "graphics-svg", "graphics-canvas", "graphics-java2d", "graphics-javafx")

  private lazy val root: Path =
    var candidate = Path.of(sys.props("user.dir")).toAbsolutePath.normalize
    while candidate != null && !Files.isRegularFile(candidate.resolve("build.sbt")) do
      candidate = candidate.getParent
    if candidate == null then fail("could not locate the repository root from user.dir")
    candidate

  test("graphics production sources import no ScalaFIM domain outside graphics") {
    val forbidden = """(?m)^\s*(?:import|export)\s+scalafim\.(?!graphics(?:\.|\s|\{|\*|$))([^\s;]+)""".r
    val violations = productionSources.flatMap { path =>
      forbidden.findAllMatchIn(Files.readString(path)).map(found => s"${root.relativize(path)}: ${found.matched.trim}")
    }

    assertEquals(violations, Vector.empty)
  }

  test("graphics production packages remain under the extraction namespace") {
    val violations = productionSources.flatMap { path =>
      val declaration = Files
        .readAllLines(path)
        .asScala
        .iterator
        .map(_.trim)
        .find(_.startsWith("package "))
      declaration match
        case Some(value) if value == "package scalafim.graphics" || value.startsWith("package scalafim.graphics.") =>
          None
        case other => Some(s"${root.relativize(path)}: ${other.getOrElse("missing package declaration")}")
    }

    assertEquals(violations, Vector.empty)
  }

  test("build keeps core dependency-free and every backend dependent only on core") {
    val build = Files.readString(root.resolve("build.sbt"))
    val core = projectBlock(build, "graphics", "graphicsJS")
    assertEquals(dependencies(core), Vector.empty)
    assert(core.contains("crossProject(JSPlatform, JVMPlatform)"))

    val backends = Vector(
      ("graphicsSvg", "graphicsSvgJS", "crossProject(JSPlatform, JVMPlatform)"),
      ("graphicsCanvas", "graphicsCanvasJS", "crossProject(JSPlatform)"),
      ("graphicsJava2d", "graphicsJava2dJVM", "crossProject(JVMPlatform)"),
      ("graphicsJavafx", "graphicsJavafxJVM", "crossProject(JVMPlatform)")
    )
    backends.foreach { case (name, end, platform) =>
      val block = projectBlock(build, name, end)
      assertEquals(dependencies(block), Vector("graphics"), clues(name))
      assert(block.contains(platform), clues(name, platform))
    }
  }

  private def productionSources: Vector[Path] =
    graphicsModules.flatMap { module =>
      val moduleRoot = root.resolve("modules").resolve(module)
      if !Files.isDirectory(moduleRoot) then Vector.empty
      else
        val stream = Files.walk(moduleRoot)
        try
          stream.iterator.asScala
            .filter(path => Files.isRegularFile(path))
            .filter(_.toString.endsWith(".scala"))
            .filter(_.iterator.asScala.exists(_.toString == "main"))
            .toVector
        finally stream.close()
    }

  private def projectBlock(build: String, start: String, end: String): String =
    val from = build.indexOf(s"lazy val $start =")
    val until = build.indexOf(s"lazy val $end", from + 1)
    if from < 0 || until < 0 then fail(s"could not locate build block $start .. $end")
    build.substring(from, until)

  private def dependencies(block: String): Vector[String] =
    raw"\.dependsOn\(([^)]*)\)".r
      .findAllMatchIn(block)
      .flatMap(_.group(1).split(',').iterator.map(_.trim).filter(_.nonEmpty))
      .toVector
