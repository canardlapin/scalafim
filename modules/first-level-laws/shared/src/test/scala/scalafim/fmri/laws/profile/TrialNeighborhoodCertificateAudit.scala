package scalafim.fmri.laws.profile

import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.NormalizationRule

/** Dense, charged feasibility certificate. It proves original-observation
  * coefficient bounds at points and inverse conditioning over boxes, but has
  * no production fast-path or adaptive/scientific admission claim.
  */
object TrialNeighborhoodCertificateAudit:
  final case class PointBound(horizon: Double, node: Int, radius: Double,
      preparedRelativeError: Double, originalRelativeError: Double,
      originalQueryError: Double, etaUpper: Option[Double], relativeBound: Option[Double],
      queryBound: Option[Double], refusal: Option[String],
      retainedScalars: Long, normalProductTerms: Long, seconds: Double)
  final case class BoxBound(horizon: Double, node: Int, radius: Double,
      etaUpper: Option[Double], refusal: Option[String], seconds: Double)
  final case class Result(points: Vector[PointBound], boxes: Vector[BoxBound])

  def run(horizon: Double = 48.0, nodes: Vector[Int] = Vector(0, 7),
      radii: Vector[Double] = Vector(0.0, 0.001, 0.01, 0.1),
      boxRadii: Vector[Double] = Vector(1e-6, 1e-5, 1e-4, 1e-3),
      completed: String => Unit = _ => ()): Result =
    val f = TrialNeighborhoodAudit.fixture(30, horizon)
    val outputs = f.prepare.flatMap(_.trialOutputs).fold(e => throw new IllegalArgumentException(e.message), identity)
    val axis = outputs.axis
    val grid = TrialNeighborhoodAudit.grid(f, "corners")
    val bank = axis.preparation.objective(grid).fold(e => throw new IllegalArgumentException(e.message), identity)
    val points = Vector.newBuilder[PointBound]
    val boxes = Vector.newBuilder[BoxBound]
    nodes.foreach: node =>
      val reference = grid.point(node).coordinates
      val inverse = TrialObservationEnclosure.referenceInverse(f, reference)
      val unitReference = Vector.tabulate(3)(i => if (node & (1 << i)) == 0 then 0.0 else 1.0)
      def coordinates(radius: Double) = TrialNeighborhoodAudit.coordinates(f,
        unitReference.map(x => x + (if x == 0.0 then radius else -radius)))
      radii.foreach: radius =>
        val actual = coordinates(radius)
        val direct = TrialNeighborhoodAudit.originalDesign(f, actual)
        val raw = TrialNeighborhoodAudit.response(f, direct)
        val original = f.copy(rawBlock = raw).oracleDesign(0, direct)
        val input = ProfileTrialResponse.make(axis, axis.selectedResponseRows, ProfileTrialResponseDomain.Original, raw)
          .fold(e => throw new IllegalArgumentException(e.message), identity)
        def readout(mode: ProfileTrialReadoutMode, request: OutputRequest) =
          ProfileTrialReadout.freeze(bank, axis, actual, node, NormalizationRule.Unnormalised, mode)
            .flatMap(_.newWorker().evaluate(input, request)).fold(e => throw new IllegalArgumentException(e.message), identity)
        val corrected = readout(ProfileTrialReadoutMode.CorrectedReference, DecodedTrialCheckpoint.request)
        val candidate = corrected.trialAmplitudes.get ++ corrected.nuisanceCoefficients
        val exact = readout(ProfileTrialReadoutMode.ExactShape, DecodedTrialCheckpoint.request).trialAmplitudes.get
        val weights = Vector.tabulate(f.trials)(j => if j % 2 == 0 then 1.0 else -1.0)
        val query = ProfileTrialSignedQuery.make("signed", axis, weights, 1e-6).fold(e => throw new IllegalArgumentException(e.message), identity)
        val queried = readout(ProfileTrialReadoutMode.CorrectedReference,
          OutputRequest.TrialQueries(Vector(query), NormalizationRule.Unnormalised)).queries.head.value
        val enclosure = TrialObservationEnclosure.prepare(f, actual, actual, raw)
        val bound = enclosure.certify(candidate, inverse, Some(weights ++ Vector.fill(f.nuisance)(0.0)))
        def relative(a: Vector[Double], b: Vector[Double]): Double =
          math.sqrt(a.take(f.trials).zip(b).map((x, y) => (x - y) * (x - y)).sum / b.take(f.trials).map(x => x * x).sum)
        points += PointBound(horizon, node, radius, relative(candidate, exact), relative(candidate, original),
          math.abs(queried - weights.zip(original).map(_ * _).sum), bound.toOption.map(_.contractionInfinityUpper),
          bound.toOption.map(b => TrialObservationEnclosure.trialRelativeErrorUpper(candidate, f.trials, b)),
          bound.toOption.map(b => TrialObservationEnclosure.queryErrorUpper(candidate, queried, weights, b)),
          bound.left.toOption.map(_.toString), enclosure.retainedScalarValues, enclosure.normalProductTerms, enclosure.buildSeconds)
        completed(s"certificate horizon=$horizon node=$node radius=$radius: ${bound.map(_.contractionInfinityUpper)}")
      boxRadii.foreach: radius =>
        val opposite = coordinates(radius)
        val lower = reference.zip(opposite).map((a, b) => math.min(a, b))
        val upper = reference.zip(opposite).map((a, b) => math.max(a, b))
        val enclosure = TrialObservationEnclosure.prepare(f, lower, upper, new Array[Double](f.rows))
        val bound = enclosure.certify(Vector.fill(f.trials + f.nuisance)(0.0), inverse)
        boxes += BoxBound(horizon, node, radius, bound.toOption.map(_.contractionInfinityUpper),
          bound.left.toOption.map(_.toString), enclosure.buildSeconds)
        completed(s"box horizon=$horizon node=$node radius=$radius: ${bound.map(_.contractionInfinityUpper)}")
    Result(points.result(), boxes.result())
