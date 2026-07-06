package scalafim.fmri.group

import scalafim.linalg.LinearAlgebraError

/** Failure modes of second-level (group) analysis, represented as values rather
  * than exceptions. Mirrors the `scalafim.fmri.fit.FitError` idiom.
  */
enum GroupError:
  case EmptyDesign
  case EmptyResponse
  case DuplicateTerms(names: Vector[String])
  case DuplicateContrasts(names: Vector[String])
  case NonNumericColumn(column: String)
  case SubjectMismatch(designSubjects: Int, dataSubjects: Int)
  case SampleMismatch(expected: Int, actual: Int)
  case ContrastMismatch(expected: Int, actual: Int)
  case MissingVariances(weighting: String)
  case NonFiniteData(what: String)
  case NonPositiveVariance
  case InsufficientSubjects(subjects: Int, terms: Int)
  case SingularDesign(cause: LinearAlgebraError)
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
      case DuplicateTerms(names) =>
        s"group design term names must be unique: ${names.mkString(", ")}"
      case DuplicateContrasts(names) =>
        s"first-level contrast names must be unique: ${names.mkString(", ")}"
      case NonNumericColumn(column) =>
        s"covariate column '$column' is not numeric"
      case SubjectMismatch(designSubjects, dataSubjects) =>
        s"design subjects $designSubjects do not match data subjects $dataSubjects"
      case SampleMismatch(expected, actual) =>
        s"expected $expected samples but got $actual"
      case ContrastMismatch(expected, actual) =>
        s"expected $expected contrasts but got $actual"
      case MissingVariances(weighting) =>
        s"weighting '$weighting' requires per-subject variances, but the data carries none"
      case NonFiniteData(what) =>
        s"$what must be finite"
      case NonPositiveVariance =>
        "per-subject variances must be strictly positive"
      case InsufficientSubjects(subjects, terms) =>
        s"need more subjects than design terms for residual degrees of freedom (subjects=$subjects, terms=$terms)"
      case SingularDesign(cause) =>
        s"group design is singular or ill-conditioned: ${cause.message}"
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
