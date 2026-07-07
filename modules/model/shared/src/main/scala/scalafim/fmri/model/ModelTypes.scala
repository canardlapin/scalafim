package scalafim.fmri.model

opaque type FormulaText = String

object FormulaText:
  def apply(value: String): Either[ModelError, FormulaText] =
    val trimmed = value.trim
    if trimmed.nonEmpty then Right(trimmed)
    else Left(ModelError.InvalidId("formula", value, "must be non-empty"))

  def unsafe(value: String): FormulaText =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (formula: FormulaText)
    inline def value: String = formula
