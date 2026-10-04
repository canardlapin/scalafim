package scalafim.fmri.mvpa.fit

import gale.linalg.DMat
import multivar.core.{SemanticSpace, SpaceRole, ValueId, ValueIdentity}
import scalafim.dataset.RunId
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, Observations}
import scalafim.fmri.mvpa.analysis.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Test-only native run-set construction. It deliberately binds each run to a
  * real time axis and content identity rather than synthesizing feature plans. */
object NativeCanonicalFixtures:
  trait Packed[G]:
    type P <: SemanticSpace
    type N <: SemanticSpace
    val source: CanonicalRunSet[P, N, G]

  type PackedContrast = Packed[CanonicalGeometrySchedule]
  type PackedManova = Packed[ManovaGeometrySchedule]

  val budget: ResourceBudget =
    ResourceBudget(ResourceLimit.OwnedNumeric(100000000L), MaterializationPolicy.AllowSourceCopy(100000000L))

  def right[A](value: Either[?, A]): A = value.fold(error => throw AssertionError(error.toString), identity)

  def contrast(
      responses: Vector[DMat], schedules: Vector[CanonicalGeometrySchedule],
      runIds: Vector[RunId] = Vector.empty, featureKeys: Vector[String] = Vector.empty
  ): PackedContrast =
    pack(responses, schedules, runIds, featureKeys, "contrast")

  def manova(
      responses: Vector[DMat], schedules: Vector[ManovaGeometrySchedule],
      runIds: Vector[RunId] = Vector.empty, featureKeys: Vector[String] = Vector.empty
  ): PackedManova =
    pack(responses, schedules, runIds, featureKeys, "manova")

  private def pack[G](
      responses: Vector[DMat], schedules: Vector[G], suppliedIds: Vector[RunId], suppliedFeatures: Vector[String], label: String
  ): Packed[G] =
    require(responses.nonEmpty && responses.length == schedules.length)
    val ids = if suppliedIds.nonEmpty then suppliedIds else responses.indices.map(index => RunId(s"run-$index")).toVector
    require(ids.length == responses.length)
    val keys = if suppliedFeatures.nonEmpty then suppliedFeatures else Vector.tabulate(responses.head.cols)(index => s"neural-$index")
    val neural = right(AxisRef.fromStableKeys(s"native-$label-neural", SpaceRole.Observed, keys, "fixture", "psc", "raw"))
    val partitions = right(AxisRef.fromStableKeys(s"native-$label-runs", SpaceRole.Samples, ids.map(_.value), "fixture", "run", "raw"))
    val runs: Vector[CanonicalRunEvidence[neural.Id, G]] = responses.zip(schedules).zipWithIndex.map:
      case ((response, schedule), index) =>
        val time = right(AxisRef.fromStableKeys(s"native-$label-time-$index", SpaceRole.Samples,
          Vector.tabulate(response.rows)(row => s"t-$index-$row"), "fixture", "time", "raw"))
        val sourceId = SourceId.unsafe(s"native-$label-source-$index")
        val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe(s"native-$label-root-$index"), sourceId)))
        val observations = right(Observations.fromDense(time, neural, response,
          ValueIdentity.source(ValueId.unsafe(s"native-$label-content-$index-${CanonicalGlobal.momentIdentity(response)}")), source))
        right(CanonicalRunEvidence.fromObservations(ids(index), time, neural, observations, schedule,
          ObservationReplay.Scoped(s"native-$label", s"run-$index"), new ObservationProductResource:
            def acquire() = Right(())
            def close() = Right(()),
          ObservationProviderCosts()))
    val runSet = right(CanonicalRunSet.make(partitions)(runs)(key => RunId(key)))
    new Packed[G]:
      type P = partitions.Id
      type N = neural.Id
      val source: CanonicalRunSet[P, N, G] = runSet
