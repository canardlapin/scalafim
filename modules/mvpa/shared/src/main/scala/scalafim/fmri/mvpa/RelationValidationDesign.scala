package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.IndexSpace
import resample4s.core.Selection

/** One exact training/assessment assignment over a partition axis.
  *
  * The semantic keys are retained alongside the Resample4s selections so numerical code never has to reinterpret a
  * local ordinal as a partition identity.
  */
final class RelationValidationFold private[mvpa] (
    val heldOut: PartitionId,
    val training: Vector[PartitionId],
    val analysis: Selection,
    val assessment: Selection
)

/** Fit/evaluation roles for relation-valued evidence. This is intentionally distinct from both predictive sample
  * validation and independent relation pairing: a canonical direction is fitted from training relations and then
  * evaluated once on an identified held-out relation.
  */
final class RelationValidationDesign[P <: SemanticSpace] private (
    val partitions: PartitionAxis[P],
    val folds: Vector[RelationValidationFold],
    val generalizesOver: GeneralizationAxis,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign:
  def fold(heldOut: PartitionId): Option[RelationValidationFold] =
    partitions.axis.positionOf(heldOut).map(folds)

object RelationValidationDesign:
  private val Protocol = "scalafim-mvpa-relation-validation/v1"

  def leaveOnePartitionOut[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      generalizesOver: GeneralizationAxis
  ): Either[RelationValidationDesignError, RelationValidationDesign[P]] =
    if partitions.axis.size < 2 then Left(RelationValidationDesignError.InsufficientPartitions(partitions.axis.size))
    else
      for
        indexSpace <- IndexSpace
          .of(partitions.axis.size)
          .left
          .map(error => RelationValidationDesignError.Resampling(error.message))
        folds <- buildFolds(partitions, indexSpace)
        references <- referencesOf(partitions, generalizesOver)
        identity <- DesignIdentity(
          DesignKind.unsafe("relation-validation"),
          Vector(
            "assignment" -> assignmentDigest(folds),
            "generalization-axis" -> generalizesOver.name.value,
            "generalization-space" -> generalizesOver.identity.fingerprint.value,
            "partition-axis" -> partitions.name.value,
            "partition-space" -> partitions.axis.identity.fingerprint.value,
            "protocol" -> Protocol,
            "role" -> "fit-training-relations-evaluate-held-out-relation"
          )
        ).left.map(RelationValidationDesignError.Identity.apply)
      yield new RelationValidationDesign(
        partitions,
        folds,
        generalizesOver,
        identity,
        references
      )

  private def buildFolds[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      indexSpace: IndexSpace
  ): Either[RelationValidationDesignError, Vector[RelationValidationFold]] =
    val output = Vector.newBuilder[RelationValidationFold]
    var heldOutPosition = 0
    while heldOutPosition < partitions.axis.size do
      val trainingPositions =
        IArray.unsafeFromArray(
          Array.tabulate(partitions.axis.size - 1): position =>
            if position < heldOutPosition then position else position + 1
        )
      val assessmentPositions = IArray(heldOutPosition)
      val analysis = Selection.from(trainingPositions, indexSpace) match
        case Left(error) =>
          return Left(RelationValidationDesignError.Resampling(error.message))
        case Right(value) => value
      val assessment = Selection.from(assessmentPositions, indexSpace) match
        case Left(error) =>
          return Left(RelationValidationDesignError.Resampling(error.message))
        case Right(value) => value
      output += new RelationValidationFold(
        partitions.axis.keys(heldOutPosition),
        trainingPositions.iterator.map(partitions.axis.keys).toVector,
        analysis,
        assessment
      )
      heldOutPosition += 1
    Right(output.result())

  private def referencesOf[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      generalizesOver: GeneralizationAxis
  ): Either[RelationValidationDesignError, Vector[DesignAxisReference]] =
    if partitions.name != generalizesOver.name then
      Right(Vector(partitions.reference, generalizesOver.reference).sortBy(_.name.value))
    else if partitions.axis.identity == generalizesOver.identity then Right(Vector(partitions.reference))
    else
      Left(
        RelationValidationDesignError.ConflictingAxisReference(
          partitions.name,
          partitions.axis.identity.fingerprint,
          generalizesOver.identity.fingerprint
        )
      )

  private def assignmentDigest(folds: Vector[RelationValidationFold]): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.int(folds.length)
    folds.foreach: fold =>
      writer.string(fold.heldOut.value)
      writer.int(fold.training.length)
      fold.training.foreach(partition => writer.string(partition.value))
      writer.int(fold.analysis.domain)
      fold.analysis.foreachIndex(writer.int)
      writer.int(fold.assessment.domain)
      fold.assessment.foreachIndex(writer.int)
    AxisDigest.sha256Hex(writer.result())

enum RelationValidationDesignError:
  case Identity(error: ScientificIdentityError)
  case InsufficientPartitions(actual: Int)
  case Resampling(detail: String)
  case ConflictingAxisReference(
      name: ScientificAxisName,
      first: AxisFingerprint,
      second: AxisFingerprint
  )

  def message: String =
    this match
      case Identity(error)                => error.message
      case InsufficientPartitions(actual) =>
        s"relation validation requires at least two partitions, obtained $actual"
      case Resampling(detail) =>
        s"invalid relation validation selection: $detail"
      case ConflictingAxisReference(name, first, second) =>
        s"design axis '${name.value}' refers to both ${first.value} and ${second.value}"
