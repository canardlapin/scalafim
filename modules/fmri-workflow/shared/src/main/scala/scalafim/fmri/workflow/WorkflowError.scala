package scalafim.fmri.workflow

enum WorkflowError:
  case InvalidValue(label: String, value: String, reason: String)
  case DuplicateValues(label: String, values: Vector[String])
  case InvalidRun(runId: String, reason: String)
  case InvalidUnit(unitId: String, reason: String)
  case InvalidCatalog(reason: String)
  case InvalidContrast(contrastId: String, reason: String)
  case InvalidGroupWorkflow(workflowId: String, reason: String)
  case InvalidOutput(reason: String)

  def message: String =
    this match
      case InvalidValue(label, value, reason) =>
        s"invalid $label '$value': $reason"
      case DuplicateValues(label, values) =>
        s"duplicate $label: ${values.mkString(", ")}"
      case InvalidRun(runId, reason) =>
        s"invalid run '$runId': $reason"
      case InvalidUnit(unitId, reason) =>
        s"invalid first-level unit '$unitId': $reason"
      case InvalidCatalog(reason) =>
        s"invalid study catalog: $reason"
      case InvalidContrast(contrastId, reason) =>
        s"invalid contrast '$contrastId': $reason"
      case InvalidGroupWorkflow(workflowId, reason) =>
        s"invalid group workflow '$workflowId': $reason"
      case InvalidOutput(reason) =>
        s"invalid workflow output: $reason"

private[workflow] object WorkflowValidation:
  def identifier(label: String, value: String): Either[WorkflowError, String] =
    val clean = value.trim
    if clean.isEmpty then Left(WorkflowError.InvalidValue(label, value, "must be non-empty"))
    else if clean.exists(character => !isIdentifierCharacter(character)) then
      Left(WorkflowError.InvalidValue(label, value, "may contain only letters, digits, '.', '_', and '-'"))
    else Right(clean)

  def label(label: String, value: String): Either[WorkflowError, String] =
    val clean = value.trim
    if clean.nonEmpty then Right(clean)
    else Left(WorkflowError.InvalidValue(label, value, "must be non-empty"))

  def duplicates[A](values: Vector[A]): Vector[A] =
    values
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .collect { case (value, count) if count > 1 => value }
      .toVector

  def traverse[A, B](values: Vector[A])(f: A => Either[WorkflowError, B]): Either[WorkflowError, Vector[B]] =
    values.foldLeft[Either[WorkflowError, Vector[B]]](Right(Vector.empty)) { (acc, value) =>
      for
        collected <- acc
        next <- f(value)
      yield collected :+ next
    }

  private def isIdentifierCharacter(character: Char): Boolean =
    character.isLetterOrDigit || character == '.' || character == '_' || character == '-'
