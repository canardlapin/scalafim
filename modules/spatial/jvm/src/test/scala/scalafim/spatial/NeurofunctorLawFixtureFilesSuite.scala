package scalafim.spatial

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

import scalafim.spatial.fixtures.NeurofunctorLawFixtures

/** The embedded neurofunctor law fixtures must be the checked-in TSV/JSON files, line for line, so the shared suite
  * replays exactly what the R generator wrote.
  */
class NeurofunctorLawFixtureFilesSuite extends munit.FunSuite:

  private def lines(name: String): Vector[String] =
    val url = Option(getClass.getResource(s"/scalafim/spatial/neurofunctor-laws/$name")).getOrElse(fail(s"missing fixture $name"))
    Files.readAllLines(Path.of(url.toURI), StandardCharsets.UTF_8).asScala.toVector

  Vector(
    "manifest.json" -> NeurofunctorLawFixtures.manifest,
    "domains.tsv" -> NeurofunctorLawFixtures.domains,
    "edges.tsv" -> NeurofunctorLawFixtures.edges,
    "triplets.tsv" -> NeurofunctorLawFixtures.triplets
  ).foreach { (name, embedded) =>
    test(s"$name matches its embedded copy"):
      assertEquals(embedded, lines(name))
  }
