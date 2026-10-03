package scalafim.fmri.mvpa.spatial

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import locus4s.{Relation, Region, Selection}
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.DigestAlgorithm
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.dataset.predictive.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.image.ExactVolumeSearchlight
import scalafim.locus.{CenteredSearchlight, Searchlight}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class SpatialPredictiveAcceptanceSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def valueId(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private val input = Vector(
    Vector(2.0, 1.0, 0.0, -1.0, 3.0), Vector(-2.0, -1.0, 1.0, 1.0, -3.0),
    Vector(-1.0, -2.0, 2.0, 2.0, -2.0), Vector(1.0, 2.0, -2.0, -2.0, 2.0),
    Vector(3.0, 1.0, -1.0, -3.0, 1.0), Vector(-3.0, -1.0, 3.0, 3.0, -1.0)
  )
  private val labels = Vector(0, 1, 1, 0, 0, 1)
  private val groups = Vector(0, 0, 1, 1, 2, 2)
  private val declaration = MeasurementFrameDeclaration("predictive-acceptance", "v1", Vector.empty)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "a", 1.0 -> "b")))

  /** Slow scalar oracle independent of all production standardization,
    * centroid, scoring and probability helpers. */
  private def oracle(columns: Vector[Int]): Vector[Vector[Double]] =
    input.indices.toVector.map: testRow =>
      val train = input.indices.filter(i => groups(i) != groups(testRow)).toVector
      val means = columns.map(c => train.map(i => input(i)(c)).sum / train.size)
      val scales = columns.zip(means).map((c, mean) => math.sqrt(train.map(i => math.pow(input(i)(c) - mean, 2)).sum / (train.size - 1)))
      val normalized = input.map(row => columns.indices.map(j => (row(columns(j)) - means(j)) / scales(j)).toVector)
      val scores = Vector(0, 1).map: label =>
        val members = train.filter(i => labels(i) == label)
        val centroid = columns.indices.map(j => members.map(i => normalized(i)(j)).sum / members.size).toVector
        normalized(testRow).zip(centroid).map(_ * _).sum - 0.5 * centroid.map(x => x * x).sum + math.log(members.size.toDouble / train.size)
      val exp = scores.map(score => math.exp(score - scores.max))
      exp.map(_ / exp.sum)

  private final class Fixture:
    val samples = right(AxisRef.fromStableKeys("spatial-samples", SpaceRole.Samples, input.indices.map(i => s"trial-$i").toVector, "trial", "none", "one"))
    val neural = right(AxisRef.fromStableKeys("spatial-neural", SpaceRole.Observed, Vector.tabulate(5)(i => s"v$i"), "voxel", "psc", "raw"))
    val response = right(AxisRef.fromStableKeys("spatial-target", SpaceRole.Observed, Vector("class"), "target", "none", "one"))
    val sourceId = SourceId.unsafe("spatial-fixture")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("spatial-root"), sourceId)))
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(6, 5, input.flatten), valueId("spatial-patterns"), source))
    val targets = right(MultiResponse.fromDense(samples, response, DMat.dense(6, 1, labels.map(_.toDouble)), valueId("spatial-targets"), source))
    val groupColumn = right(Column.fromValues(samples, groups, valueId("spatial-groups")))
    val validation = right(LeaveOneGroupOutDesign.bind(samples, groupColumn, ScientificSeed.fromLong(23))(_.toString)).validation
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector.tabulate(6)(_.toLong + 100), DataFingerprint.external("spatial-v1")))
    var closed = 0

    def run(frame: MeasurementFrame[neural.Id, String, SpatialMeasurementRendition[neural.Locus]], expected: Map[String, Vector[Int]]): Vector[SpatialLocalOutcome[neural.Locus, Double]] =
      val visitor = new MeasurementVisitor[samples.Id, neural.Id, String, SpatialMeasurementRendition[neural.Locus], Double]:
        def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id, String, SpatialMeasurementRendition[neural.Locus]] { type Local = L }, measured: MeasuredObservations[samples.Id, L]): Either[MeasurementFailure, Double] =
          val name = entry.rendition.label.getOrElse(fail("missing fixture label"))
          val columns = expected(name)
          val rows = right(AlderPredictiveAdmission.nativeMeasurement(observations, entry.measurement, targets, samples.toRecord.stableKeys,
            DataFingerprint.external("spatial-metadata"), mapping, right(NativeReadPolicy(columns.size, right(MaterializationBudget(10000))))))
          val actualInput = right(rows.root.training(mapping.nativeIds)).data.foldRows(Vector.empty[Vector[Double]])((out, _, example) => out :+ example.input.toVector)
          assertEquals(actualInput, input.map(row => columns.map(row)))
          val chosenCoding = if name == "failure" then right(SwiftTargetCoding(Vector(0.0 -> "a", 1.0 -> "b", 2.0 -> "missing"))) else coding
          AlderSwiftCentroid.crossValidate(rows, validation, chosenCoding) match
            case Left(error) => Left(MeasurementFailure.Task(error.toString))
            case Right(result) =>
              val probabilities = oracle(columns)
              probabilities.indices.foreach: row =>
                probabilities(row).indices.foreach: column =>
                  assertEqualsDouble(result.probabilities(row, column), probabilities(row)(column), 1e-12)
              assertEquals(result.rows.map(_.stableKey), samples.toRecord.stableKeys)
              assertEquals(result.assessment.samples, 6L)
              Right(result.assessment.accuracy)
      val result = frame.traverse(1)(Right(MeasurementResource(observations) { closed += 1 }))(visitor)
      assertEquals(result.error, None)
      assertEquals(closed, 1)
      result.value.map: outcome =>
        val label = outcome.rendition.label.get
        val weight = label match
          case "first" => 1.0
          case "last" => 3.0
          case "failure" => 100.0
          case _ => 1.0
        SpatialLocalOutcome(outcome.descriptor.id, outcome.rendition.support, weight, outcome.value.left.map(_.toString))

    def assertScatter(outcomes: Vector[SpatialLocalOutcome[neural.Locus, Double]]): Unit =
      assertEquals(outcomes.count(_.value.isLeft), 1)
      val scattered = right(SpatialMeasurementScatter.scatter(neural.locus, outcomes, SpatialScatterAlgebra.weightedMeanDouble))
      val reversed = right(SpatialMeasurementScatter.scatter(neural.locus, outcomes.reverse, SpatialScatterAlgebra.weightedMeanDouble))
      assertEquals(scattered.toVector, reversed.toVector)
      scattered(neural.locus.indexAtValidatedOrdinal(1)) match
        case SpatialScatterCell.Aggregated(value, contributors, denominator, failures) =>
          val a = oracle(Vector(0, 1)).zip(labels).count((p, y) => p.indices.maxBy(p) == y).toDouble / 6
          val b = oracle(Vector(1, 2)).zip(labels).count((p, y) => p.indices.maxBy(p) == y).toDouble / 6
          assertEqualsDouble(value, (a + 3 * b) / 4, 1e-12)
          assertEqualsDouble(denominator, 4.0, 0.0)
          assertEquals(contributors.map(_.weight), Vector(1.0, 3.0))
          assertEquals(failures.length, 1)
        case other => fail(other.toString)
      assertEquals(scattered(neural.locus.indexAtValidatedOrdinal(4)), SpatialScatterCell.Unvisited)

  test("actual selected [3,1] and regional [1,3] values survive native prediction in deterministic site order"):
    val fixture = new Fixture
    val selection = right(Selection.fromOrdinals(fixture.neural.locus, Vector(3, 1)))
    val region = right(Region.fromOrdinals(fixture.neural.locus, Vector(3, 1)))
    val sites = Vector(
      SpatialMeasurementSite(MeasurementId.unsafe("selection"), SpatialMeasurementSupport.Selected(selection), label = Some("selection")),
      SpatialMeasurementSite(MeasurementId.unsafe("region"), SpatialMeasurementSupport.Regional(region), label = Some("region"))
    )
    val frame = right(SpatialMeasurementFrames.regions(fixture.neural, declaration, sites))
    val reversed = right(SpatialMeasurementFrames.regions(fixture.neural, declaration, sites.reverse))
    assertEquals(frame.identity, reversed.identity)
    val result = fixture.run(frame, Map("selection" -> Vector(3, 1), "region" -> Vector(1, 3)))
    assertEquals(result.map(_.measurement.value), Vector("region", "selection"))
    assert(result.forall(_.value.isRight))

  test("volume neighborhoods run native prediction and scatter around a real local fit failure"):
    val fixture = new Fixture
    val centers = right(Region.fromOrdinals(fixture.neural.locus, Vector(2, 1, 0)))
    val relation = right(Relation.fromOrdinalRows(fixture.neural.locus, fixture.neural.locus,
      Vector(Vector(0, 1), Vector(1), Vector(1, 2), Vector.empty, Vector.empty).iterator.map(_.iterator)))
    val neighborhoods = right(ExactVolumeSearchlight.fromRelation(centers, relation))
    val names = Vector("first", "failure", "last")
    val frame = right(SpatialMeasurementFrames.volumeNeighborhoods(fixture.neural, declaration, neighborhoods, index => Some(names(index.ordinal))))
    fixture.assertScatter(fixture.run(frame, Map("first" -> Vector(0, 1), "failure" -> Vector(1), "last" -> Vector(1, 2))))

  test("nonempty centered surface patches run native prediction and preserve successful overlap denominators"):
    val fixture = new Fixture
    val centers = right(Region.fromOrdinals(fixture.neural.locus, Vector(0, 1, 2)))
    val relation = right(Relation.fromOrdinalRows(fixture.neural.locus, fixture.neural.locus,
      Vector(Vector(0, 1), Vector(1), Vector(1, 2), Vector.empty, Vector.empty).iterator.map(_.iterator)))
    val patches = right(CenteredSearchlight.validate(right(Searchlight.make(centers, relation))))
    val names = Vector("first", "failure", "last")
    val frame = right(SpatialMeasurementFrames.surfacePatches(fixture.neural, declaration, patches, index => Some(names(index.ordinal))))
    fixture.assertScatter(fixture.run(frame, Map("first" -> Vector(0, 1), "failure" -> Vector(1), "last" -> Vector(1, 2))))
