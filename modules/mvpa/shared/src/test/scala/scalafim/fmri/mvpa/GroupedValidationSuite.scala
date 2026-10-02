package scalafim.fmri.mvpa

import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{Coverage, DigestAlgorithm, Plan, Selection, Split, UnitKey}
import scala.compiletime.testing.typeCheckErrors

class GroupedValidationSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private final case class Run(display: String, stable: String)

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(keys: String*): AxisRef[String] =
    right(
      AxisRef.fromStableKeys(
        "trials",
        SpaceRole.Samples,
        keys.toVector,
        "acquisition-order",
        "trial",
        "unscaled",
        Vector("source:run-groups")
      )
    )

  private def valueId(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def runColumn(
      samples: AxisRef[String],
      runs: Vector[Run],
      identity: String
  ): Column[samples.Id, Run] =
    right(Column.fromValues(samples, runs, valueId(identity)))

  private def schedule[S <: SemanticSpace, K, G](
      design: LeaveOneGroupOutDesign[S, K, G]
  ): Map[String, (Vector[String], Vector[String])] =
    design.keys
      .map: key =>
        val unit = right(design.at(key))
        unit.heldOut.stableKey -> (
          unit.analysis.members.map(_.parentStableKey),
          unit.assessment.members.map(_.parentStableKey)
        )
      .toMap

  test("identified leave-one-group-out retains complete membership and predictor separation"):
    val samples = axis("s5", "s1", "s9", "s2", "s8", "s4")
    val runs = Vector(
      Run("Run B", "run-b"),
      Run("Run B", "run-b"),
      Run("Run A", "run-a"),
      Run("Run A", "run-a"),
      Run("Run C", "run-c"),
      Run("Run C", "run-c")
    )
    val column = runColumn(samples, runs, "runs-v1")
    val grouping = right(GroupingColumn.bind(samples, column)(_.stable))
    val design = right(
      LeaveOneGroupOutDesign.fromGrouping(grouping, ScientificSeed.fromLong(42L))
    )
    val exact: Plan[Split[Selection], Coverage.ExactOnce] = design.plan
    val targets = right(Column.fromValues(samples, Vector(5, 1, 9, 2, 8, 4), valueId("targets-v1")))

    assertEquals(exact.shape.repeats, 1)
    assertEquals(exact.shape.foldsPerRepeat, 3)
    assertEquals(grouping.identity.column, column.identity)
    assertEquals(grouping.identity.stableGroupKeys, Vector("run-b", "run-a", "run-c"))
    assertEquals(
      grouping.memberships.map(membership => (membership.sampleStableKey, membership.group.stableKey)),
      Vector(
        "s5" -> "run-b",
        "s1" -> "run-b",
        "s9" -> "run-a",
        "s2" -> "run-a",
        "s8" -> "run-c",
        "s4" -> "run-c"
      )
    )
    assertEquals(design.receipt.samples, samples.descriptor)
    assertEquals(design.receipt.grouping, grouping.identity)

    val units = design.keys.map(key => right(design.at(key)))
    units.foreach: unit =>
      val analysis = targets.reindex(unit.analysis)
      val assessment = targets.reindex(unit.assessment)
      val analysisRootKeys = unit.analysis.members.map(_.parentStableKey)
      val assessmentRootKeys = unit.assessment.members.map(_.parentStableKey)
      assertEquals(analysisRootKeys.toSet.intersect(assessmentRootKeys.toSet), Set.empty[String])
      assertEquals(
        (analysisRootKeys ++ assessmentRootKeys).sorted,
        samples.toRecord.stableKeys.sorted
      )
      assertEquals(analysis.values, unit.analysis.ordinals.toVector.map(targets.values))
      assertEquals(assessment.values, unit.assessment.ordinals.toVector.map(targets.values))
      assertEquals(
        unit.assessment.ordinals.toVector.map(runs(_).stable).distinct,
        Vector(unit.heldOut.stableKey)
      )

    val assessed = units.flatMap(_.assessment.ordinals.toVector)
    assertEquals(assessed.sorted, Vector.range(0, samples.size))
    val reverse = design.keys.reverse.map(key => right(design.at(key)).identity).reverse
    assertEquals(reverse, units.map(_.identity))

  test("display-label changes preserve stable grouping semantics but remain visible in source provenance"):
    val samples = axis("s1", "s2", "s3", "s4")
    val firstRuns = Vector(
      Run("Run one", "run-1"),
      Run("Run one", "run-1"),
      Run("Run two", "run-2"),
      Run("Run two", "run-2")
    )
    val renamedRuns = Vector(
      Run("Session alpha", "run-1"),
      Run("Session alpha", "run-1"),
      Run("Session beta", "run-2"),
      Run("Session beta", "run-2")
    )
    val firstGrouping = right(GroupingColumn.bind(samples, runColumn(samples, firstRuns, "runs-v1"))(_.stable))
    val renamedGrouping =
      right(GroupingColumn.bind(samples, runColumn(samples, renamedRuns, "runs-renamed-v1"))(_.stable))
    val seed = ScientificSeed.fromLong(90210L)
    val first = right(LeaveOneGroupOutDesign.fromGrouping(firstGrouping, seed))
    val repeated = right(LeaveOneGroupOutDesign.fromGrouping(firstGrouping, seed))
    val renamed = right(LeaveOneGroupOutDesign.fromGrouping(renamedGrouping, seed))

    assertEquals(firstGrouping.identity.membershipSignature, renamedGrouping.identity.membershipSignature)
    assert(firstGrouping.identity != renamedGrouping.identity)
    assertEquals(first.receipt, repeated.receipt)
    assertEquals(first.receipt.plan, renamed.receipt.plan)
    assert(first.receipt != renamed.receipt)
    assertEquals(schedule(first), schedule(renamed))
    assertEquals(
      first.keys.map(key => right(first.at(key)).heldOut.value.display),
      Vector("Run one", "Run two")
    )
    assertEquals(
      renamed.keys.map(key => right(renamed.at(key)).heldOut.value.display),
      Vector("Session alpha", "Session beta")
    )

  test("row reordering requires a matching axis while preserving group-keyed scientific membership"):
    val samples = axis("a", "b", "c", "d", "e", "f")
    val reorderedSamples = axis("e", "c", "a", "f", "d", "b")
    val runs = Vector(
      Run("A", "run-a"),
      Run("A", "run-a"),
      Run("B", "run-b"),
      Run("B", "run-b"),
      Run("C", "run-c"),
      Run("C", "run-c")
    )
    val reorderedRuns = Vector(runs(4), runs(2), runs(0), runs(5), runs(3), runs(1))
    val original = right(
      LeaveOneGroupOutDesign.bind(
        samples,
        runColumn(samples, runs, "runs-original"),
        ScientificSeed.fromLong(1L)
      )(_.stable)
    )
    val reordered = right(
      LeaveOneGroupOutDesign.bind(
        reorderedSamples,
        runColumn(reorderedSamples, reorderedRuns, "runs-reordered"),
        ScientificSeed.fromLong(1L)
      )(_.stable)
    )
    val originalAssessment = schedule(original).view.mapValues(_._2.sorted).toMap
    val reorderedAssessment = schedule(reordered).view.mapValues(_._2.sorted).toMap

    assertEquals(originalAssessment, reorderedAssessment)
    assert(original.receipt.plan != reordered.receipt.plan)
    assert(original.grouping.identity.membershipSignature != reordered.grouping.identity.membershipSignature)
    assert(
      Column
        .bind(samples, reorderedSamples.toRecord, reordered.grouping.column.values, valueId("forged"))
        .isLeft
    )

    val errors = typeCheckErrors("""import scalafim.fmri.mvpa.*
import multivar.core.*
import resample4s.core.*
def invalid[S <: SemanticSpace,T <: SemanticSpace,K,G](
  samples: AxisRef[K] { type Id = S },
  foreignGroups: Column[T,G],
  seed: ScientificSeed,
  stable: G => String
)(using DigestAlgorithm) = LeaveOneGroupOutDesign.bind(samples, foreignGroups, seed)(stable)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("foreignGroups"))

  test("identified schedule agrees with the independent literal leave-one-block-out schedule"):
    val samples = axis("a", "b", "c", "d", "e", "f", "g")
    val blocks = Vector(20, 20, 10, 10, 30, 30, 30)
    val column = right(Column.fromValues(samples, blocks, valueId("literal-blocks-v1")))
    val identified = right(
      LeaveOneGroupOutDesign.bind(samples, column, ScientificSeed.fromLong(123L))(_.toString)
    )
    val identifiedSchedule = identified.keys
      .map: key =>
        val unit = right(identified.at(key))
        unit.heldOut.stableKey -> (
          unit.analysis.ordinals.toVector,
          unit.assessment.ordinals.toVector
        )
      .toMap
    val expected = Map(
      "20" -> (Vector(2, 3, 4, 5, 6), Vector(0, 1)),
      "10" -> (Vector(0, 1, 4, 5, 6), Vector(2, 3)),
      "30" -> (Vector(0, 1, 2, 3), Vector(4, 5, 6))
    )
    assertEquals(identifiedSchedule, expected)

  test("invalid stable keys and single-group validation fail through typed boundaries"):
    val samples = axis("a", "b", "c")
    val runs = Vector(Run("A", "run-a"), Run("A", "run-a"), Run("B", "run-b"))
    val column = runColumn(samples, runs, "runs-v1")

    GroupingColumn.bind(samples, column)(run => s" ${run.stable}") match
      case Left(EvidenceError.GroupingFailure(GroupingError.InvalidStableGroupKey(0, detail))) =>
        assertEquals(detail, "must not have surrounding whitespace")
      case other => fail(s"expected invalid stable group key, obtained $other")

    val single = right(
      GroupingColumn.bind(
        samples,
        runColumn(samples, Vector.fill(3)(Run("Only", "only")), "single-run")
      )(_.stable)
    )
    LeaveOneGroupOutDesign.fromGrouping(single, ScientificSeed.fromLong(0L)) match
      case Left(EvidenceError.GroupingFailure(GroupingError.TooFewGroups(1, 2))) => ()
      case other => fail(s"expected typed too-few-groups error, obtained $other")

    val valid = right(LeaveOneGroupOutDesign.bind(samples, column, ScientificSeed.fromLong(0L))(_.stable))
    valid.at(UnitKey(0, valid.grouping.groupCount)) match
      case Left(EvidenceError.UnknownResampleUnit(error)) =>
        assertEquals(error.key, UnitKey(0, valid.grouping.groupCount))
      case other => fail(s"expected typed unknown-unit error, obtained $other")
