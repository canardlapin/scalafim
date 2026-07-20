package scalafim.fmri.group

import gale.linalg.LinAlgError

final case class CountMismatch(expected: Int, actual: Int, expectedLabel: String, actualLabel: String):
  def message: String =
    s"$actualLabel $actual do not match $expectedLabel $expected"

object CountMismatch:
  def subjects(expected: Int, actual: Int): CountMismatch =
    CountMismatch(expected, actual, "data subjects", "design subjects")

  def responseSubjects(expected: Int, actual: Int): CountMismatch =
    CountMismatch(expected, actual, "effect subjects", "variance subjects")

  def samples(expected: Int, actual: Int): CountMismatch =
    CountMismatch(expected, actual, "expected samples", "actual samples")

  def contrasts(expected: Int, actual: Int): CountMismatch =
    CountMismatch(expected, actual, "expected contrasts", "actual contrasts")

/** Failure modes of second-level (group) analysis, represented as values rather
  * than exceptions. Mirrors the `scalafim.fmri.fit.FitError` idiom.
  */
enum GroupError:
  case EmptyDesign
  case EmptyResponse
  case DuplicateSubjects(subjects: Vector[String])
  case DuplicateTerms(names: Vector[String])
  case DuplicateContrasts(names: Vector[String])
  case NonNumericColumn(column: String)
  case SubjectMismatch(mismatch: CountMismatch)
  case SampleMismatch(mismatch: CountMismatch)
  case ContrastMismatch(mismatch: CountMismatch)
  case MissingVariances(weighting: String)
  case NonFiniteData(what: String)
  case NonPositiveVariance
  case InvalidDegreesOfFreedom(value: Int)
  case InvalidPValue(value: Double)
  case InvalidLabel(kind: String, value: String)
  case InsufficientSubjects(subjects: Int, terms: Int)
  case SingularDesign(cause: LinAlgError)
  case UnknownColumn(column: String)
  case UnknownContrastTerm(term: String)
  case EmptyContrast(name: String)
  case UnknownContrast(name: String)
  case MissingSubjectContrast(subject: String, contrast: String)

  def message: String =
    this match
      case EmptyDesign =>
        "group design must have at least one subject row and one term column"
      case EmptyResponse =>
        "group response must have at least one subject and one sample"
      case DuplicateSubjects(subjects) =>
        s"subject ids must be unique: ${subjects.mkString(", ")}"
      case DuplicateTerms(names) =>
        s"group design term names must be unique: ${names.mkString(", ")}"
      case DuplicateContrasts(names) =>
        s"first-level contrast names must be unique: ${names.mkString(", ")}"
      case NonNumericColumn(column) =>
        s"covariate column '$column' is not numeric"
      case SubjectMismatch(mismatch) =>
        mismatch.message
      case SampleMismatch(mismatch) =>
        mismatch.message
      case ContrastMismatch(mismatch) =>
        mismatch.message
      case MissingVariances(weighting) =>
        s"weighting '$weighting' requires per-subject variances, but the data carries none"
      case NonFiniteData(what) =>
        s"$what must be finite"
      case NonPositiveVariance =>
        "per-subject variances must be strictly positive"
      case InvalidDegreesOfFreedom(value) =>
        s"degrees of freedom must be positive, got $value"
      case InvalidPValue(value) =>
        s"p-value must be finite and in [0, 1], got $value"
      case InvalidLabel(kind, value) =>
        s"$kind label must be non-empty, got '$value'"
      case InsufficientSubjects(subjects, terms) =>
        s"need more subjects than design terms for residual degrees of freedom (subjects=$subjects, terms=$terms)"
      case SingularDesign(cause) =>
        s"group design is singular or ill-conditioned: ${cause.getMessage}"
      case UnknownColumn(column) =>
        s"covariate table has no column '$column'"
      case UnknownContrastTerm(term) =>
        s"contrast references unknown design term: $term"
      case EmptyContrast(name) =>
        s"contrast '$name' has no non-zero weights"
      case UnknownContrast(name) =>
        s"result has no fit for contrast '$name'"
      case MissingSubjectContrast(subject, contrast) =>
        s"no first-level result for subject '$subject' contrast '$contrast'"

object GroupError:
  def subjectMismatch(expected: Int, actual: Int): GroupError =
    GroupError.SubjectMismatch(CountMismatch.subjects(expected, actual))

  def responseSubjectMismatch(expected: Int, actual: Int): GroupError =
    GroupError.SubjectMismatch(CountMismatch.responseSubjects(expected, actual))

  def sampleMismatch(expected: Int, actual: Int): GroupError =
    GroupError.SampleMismatch(CountMismatch.samples(expected, actual))

  def contrastMismatch(expected: Int, actual: Int): GroupError =
    GroupError.ContrastMismatch(CountMismatch.contrasts(expected, actual))
