package scalafim.spatial

import scala.collection.mutable

class SpatialLazyContractSuite extends munit.FunSuite with SpatialLazyContractLaws:

  override protected def contractAdapter(): SpatialLazyContractAdapter =
    ReferenceSpatialLazyAdapter()

  registerSpatialLazyContractLaws()

private final case class ReferenceRoot(
  id: String,
  domain: String,
  data: Vector[Double],
  available: Boolean
)

private final case class ReferenceView(
  root: ReferenceRoot,
  domain: String,
  requestedSteps: Vector[String],
  demand: Option[Vector[Int]],
  sampling: String,
  inversePolicy: String,
  lineage: Vector[LazyContractProvenance]
)

private final class ReferenceSpatialLazyAdapter private () extends SpatialLazyContractAdapter:
  override type View = ReferenceView

  private var sourceReadCount = 0
  private var compilationCount = 0
  private var resamplingCount = 0
  private var cacheWriteCount = 0
  private val resultCache = mutable.LinkedHashMap.empty[String, LazyContractEvaluation]

  override val root: ReferenceView =
    ReferenceView(
      root = ReferenceRoot("root-data", "root", Vector(0.0, 8.0, 0.0, 4.0), available = true),
      domain = "root",
      requestedSteps = Vector.empty,
      demand = None,
      sampling = "linear",
      inversePolicy = "exact-only",
      lineage = Vector.empty
    )

  override val unavailableRoot: ReferenceView =
    ReferenceView(
      root = ReferenceRoot("external-root", "root", Vector.empty, available = false),
      domain = "root",
      requestedSteps = Vector.empty,
      demand = None,
      sampling = "linear",
      inversePolicy = "exact-only",
      lineage = Vector.empty
    )

  override def rootId(view: ReferenceView): String =
    view.root.id

  override def domain(view: ReferenceView): String =
    view.domain

  override def requestedSteps(view: ReferenceView): Vector[String] =
    view.requestedSteps

  override def demand(view: ReferenceView): Option[Vector[Int]] =
    view.demand

  override def to(
    view: ReferenceView,
    target: String
  ): Either[LazyContractError, ReferenceView] =
    route(view.root.domain, target).map { _ =>
      view.copy(
        domain = target,
        requestedSteps = view.requestedSteps :+ s"to:$target"
      )
    }

  override def rows(
    view: ReferenceView,
    indices: Vector[Int]
  ): Either[LazyContractError, ReferenceView] =
    if indices.exists(index => index < 0 || index >= view.root.data.length) then
      Left(LazyContractError.InvalidDemand(indices))
    else
      Right(
        view.copy(
          requestedSteps = view.requestedSteps :+ s"rows:${indices.mkString(",")}",
          demand = Some(indices)
        )
      )

  override def withSampling(view: ReferenceView, sampling: String): ReferenceView =
    view.copy(sampling = sampling)

  override def withInversePolicy(view: ReferenceView, policy: String): ReferenceView =
    view.copy(inversePolicy = policy)

  override def executionKey(view: ReferenceView): Either[LazyContractError, String] =
    route(view.root.domain, view.domain).map { selectedRoute =>
      Vector(
        view.root.id,
        view.root.domain,
        view.domain,
        selectedRoute.mkString(">"),
        view.demand.fold("all")(_.mkString(",")),
        view.sampling,
        view.inversePolicy,
        "reference-pullback-v1"
      ).mkString("|")
    }

  override def value(
    view: ReferenceView
  ): Either[LazyContractError, LazyContractEvaluation] =
    if !view.root.available then Left(LazyContractError.DataUnavailable(view.root.id))
    else if view.domain == "approx-target" && view.inversePolicy == "exact-only" then
      Left(LazyContractError.InverseQualityRejected(view.domain))
    else
      for
        selectedRoute <- route(view.root.domain, view.domain)
        key <- executionKey(view)
      yield
        resultCache.getOrElseUpdate(
          key,
          evaluateAndRecord(view, selectedRoute, key)
        )

  override def materialize(
    view: ReferenceView
  ): Either[LazyContractError, ReferenceView] =
    value(view).map { evaluation =>
      val materializedId = s"materialized:${evaluation.cacheKey}"
      ReferenceView(
        root = ReferenceRoot(materializedId, view.domain, evaluation.values, available = true),
        domain = view.domain,
        requestedSteps = Vector.empty,
        demand = None,
        sampling = view.sampling,
        inversePolicy = view.inversePolicy,
        lineage = view.lineage :+ evaluation.provenance
      )
    }

  override def metrics: LazyContractMetrics =
    LazyContractMetrics(
      sourceReads = sourceReadCount,
      compilations = compilationCount,
      spatialResamplings = resamplingCount,
      resultCacheWrites = cacheWriteCount
    )

  private def evaluateAndRecord(
    view: ReferenceView,
    selectedRoute: Vector[String],
    key: String
  ): LazyContractEvaluation =
    compilationCount += 1
    sourceReadCount += 1
    if selectedRoute.nonEmpty then resamplingCount += 1

    val fullValues = sampleRoot(view.root.data, shiftFor(view.domain))
    val requestedRows = view.demand.getOrElse(fullValues.indices.toVector)
    val values = requestedRows.map(fullValues)
    val support = sourceSupport(view.root.data.length, requestedRows, shiftFor(view.domain))
    val provenance =
      LazyContractProvenance(
        root = view.root.id,
        requestedSteps = view.requestedSteps,
        route = selectedRoute,
        demand = view.demand,
        compiler = "reference-pullback-v1",
        fusedSpatially = selectedRoute.lengthCompare(1) > 0
      )
    val evaluation = LazyContractEvaluation(values, support, key, provenance)
    cacheWriteCount += 1
    evaluation

  private def route(source: String, target: String): Either[LazyContractError, Vector[String]] =
    (source, target) match
      case (sameSource, sameTarget) if sameSource == sameTarget => Right(Vector.empty)
      case ("root", "mid") => Right(Vector("root-to-mid"))
      case ("root", "target") => Right(Vector("root-to-mid", "mid-to-target"))
      case ("root", "approx-target") => Right(Vector("root-to-approx-target"))
      case ("target", "mid") => Right(Vector("target-to-mid"))
      case _ => Left(LazyContractError.UnsupportedRoute(source, target))

  private def shiftFor(target: String): Double =
    target match
      case "mid" => 0.5
      case "target" => 1.0
      case "approx-target" => 0.25
      case _ => 0.0

  private def sampleRoot(data: Vector[Double], shift: Double): Vector[Double] =
    data.indices.toVector.map { targetRow =>
      val sourcePosition = targetRow.toDouble + shift
      val lower = math.floor(sourcePosition).toInt
      val upper = lower + 1
      val fraction = sourcePosition - lower.toDouble
      sample(data, lower) * (1.0 - fraction) + sample(data, upper) * fraction
    }

  private def sourceSupport(
    sourceSize: Int,
    targetRows: Vector[Int],
    shift: Double
  ): Vector[Int] =
    targetRows
      .flatMap { targetRow =>
        val sourcePosition = targetRow.toDouble + shift
        val lower = math.floor(sourcePosition).toInt
        val upper = lower + 1
        val fraction = sourcePosition - lower.toDouble
        val support = if fraction == 0.0 then Vector(lower) else Vector(lower, upper)
        support.filter(index => index >= 0 && index < sourceSize)
      }
      .distinct
      .sorted

  private def sample(data: Vector[Double], index: Int): Double =
    if index < 0 || index >= data.length then 0.0 else data(index)

private object ReferenceSpatialLazyAdapter:
  def apply(): ReferenceSpatialLazyAdapter =
    new ReferenceSpatialLazyAdapter()
