package scalafim.atlas.workflows

import gale.linalg.Matrix
import locus4s.data.Field
import scalafim.atlas.*
import scalafim.connectivity.*

enum ParcelWorkflowError:
  case WrongParcelOwner
  case InvalidBatchName(name: String)
  case DuplicateBatchName(name: String)
  case Reduction(name: String, cause: AtlasReductionError)
  case Connectivity(cause: ConnectivityError)

  def message: String = this match
    case WrongParcelOwner => "parcel values require the realization's exact live parcel owner"
    case InvalidBatchName(name) => s"batch name must be non-empty: '$name'"
    case DuplicateBatchName(name) => s"duplicate batch name '$name'"
    case Reduction(name, cause) => s"batch '$name': ${cause.message}"
    case Connectivity(cause) => cause.message

/** The immutable bridge records the scientific basis independently of any
  * later estimator or scheduling attempt. Rows are times, columns canonical
  * parcels, and node IDs encode the full persistent parcel key losslessly.
  */
final case class AtlasConnectivityInput private[workflows] (
    series: ParcelTimeSeries,
    source: AtlasRealizationIdentity,
    parcelKeys: Vector[String]
):
  def correlation: Either[ConnectivityError, StaticConnectivity] =
    ConnectivityEstimators.weightedCorrelation(series)

object ParcelWorkflows:
  def connectivity(realization: AtlasRealization)(
      samples: Vector[Field[realization.P, Double]],
      timing: TimeAxis
  ): Either[ParcelWorkflowError, AtlasConnectivityInput] =
    val parcels = realization.parcelDomain
    if samples.exists(field => !parcels.sameRuntimeOwnerAs(field.space)) then
      Left(ParcelWorkflowError.WrongParcelOwner)
    else if samples.length != timing.sampleCount then
      Left(ParcelWorkflowError.Connectivity(ConnectivityError.MatrixShapeMismatch(
        s"parcel samples expected ${timing.sampleCount} timepoints, got ${samples.length}")))
    else
      val points = parcels.indices.toVector
      val keys = points.map(realization.parcelKeys.apply)
      val nodes = points.zip(keys).map: (point, key) =>
        NodeSpec(NodeId.unsafe(encodedKey(key)), realization.metadata(point).label.value)
      val basis = realization.parcelDomainRecord.identity
      for
        provenance <- NodeAxisProvenance.declared(s"atlas-${basis.structuralFingerprint}", Some(basis.descriptorVersion))
          .left.map(ParcelWorkflowError.Connectivity.apply)
        axis <- NodeAxis.from(nodes, provenance).left.map(ParcelWorkflowError.Connectivity.apply)
        values =
          val builder = Matrix.newBuilder(samples.length, points.length)
          var time = 0
          while time < samples.length do
            var parcel = 0
            while parcel < points.length do
              builder(time, parcel) = samples(time)(points(parcel))
              parcel += 1
            time += 1
          builder.result()
        series <- ParcelTimeSeries.from(values, axis, timing).left.map(ParcelWorkflowError.Connectivity.apply)
      yield AtlasConnectivityInput(series, realization.identity, keys)

  /** Sequential named extraction delegates the same reduction and missing/
    * empty policies as a single extraction. No scheduler is introduced.
    */
  def batch(realization: AtlasRealization)(
      inputs: Vector[(String, Field[realization.X, Double])],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[Field[realization.X, Boolean]] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[ParcelWorkflowError, Vector[(String, Field[realization.P, Double])]] =
    val names = inputs.map(_._1)
    names.find(_.trim.isEmpty) match
      case Some(name) => Left(ParcelWorkflowError.InvalidBatchName(name))
      case None =>
        names.groupBy(identity).collectFirst { case (name, occurrences) if occurrences.length > 1 => name } match
          case Some(name) => Left(ParcelWorkflowError.DuplicateBatchName(name))
          case None =>
            inputs.foldLeft[Either[ParcelWorkflowError, Vector[(String, Field[realization.P, Double])]]](Right(Vector.empty)):
              (result, input) => result.flatMap: outputs =>
                AtlasReduce.reduceField(realization)(input._2, reducer, mask, policy)
                  .left.map(ParcelWorkflowError.Reduction(input._1, _))
                  .map(values => outputs :+ (input._1 -> values))

  private def encodedKey(key: String): String =
    "parcel-" + key.iterator.map(character => f"${character.toInt}%04x").mkString
