package scalafim.fmri.model

import scalafim.fmri.design.DesignError

enum ModelError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidParameter(name: String, detail: String)
  case InvalidFitConfig(engine: FitEngine, detail: String)
  case DesignRowMismatch(block: String, expected: Int, actual: Int)
  case DesignColumnMismatch(block: String, expected: Int, actual: Int)
  case DuplicateColumnId(id: String)
  case EmptyDesignBlock
  case VectorLengthMismatch(name: String, expected: Int, actual: Int)
  case MatrixRowMismatch(name: String, expected: Int, actual: Int)
  case TimepointOutOfBounds(name: String, value: Int, size: Int)
  case UnknownLssTrialTerm(term: String, known: Vector[String])
  case BuildFailed(detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind id '$value': $reason"
      case InvalidParameter(name, detail) =>
        s"invalid $name: $detail"
      case InvalidFitConfig(engine, detail) =>
        s"invalid $engine fit configuration: $detail"
      case DesignRowMismatch(block, expected, actual) =>
        s"$block design rows must match dataset timepoints: expected $expected, got $actual"
      case DesignColumnMismatch(block, expected, actual) =>
        s"$block design column names must match matrix columns: expected $expected, got $actual"
      case DuplicateColumnId(id) =>
        s"duplicate model design column id '$id'"
      case EmptyDesignBlock =>
        "model design block must contain at least one predictor"
      case VectorLengthMismatch(name, expected, actual) =>
        s"$name length must match model timepoints: expected $expected, got $actual"
      case MatrixRowMismatch(name, expected, actual) =>
        s"$name rows must match model timepoints: expected $expected, got $actual"
      case TimepointOutOfBounds(name, value, size) =>
        s"$name timepoint $value is outside model timepoint range [0, ${size - 1}]"
      case UnknownLssTrialTerm(term, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"unknown LSS trial term '$term'$suffix"
      case BuildFailed(detail) =>
        detail

object ModelError:
  def fromThrowable(t: Throwable): ModelError =
    val msg = Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)
    BuildFailed(msg)

  def fromDesignError(error: DesignError): ModelError =
    BuildFailed(error.message)
