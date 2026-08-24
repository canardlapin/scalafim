package scalafim.fmri.design.formula

import scalafim.fmri.design.{ColumnId, DesignError, PhaseId, TermId}
import scalafim.fmri.design.data.DataTable
import scalafim.fmri.hrf.HrfKind

final case class ColumnRef(id: ColumnId):
  def name: String = id.value

object ColumnRef:
  def bind(id: ColumnId, data: DataTable): Either[DesignError, ColumnRef] =
    if data.contains(id) then Right(ColumnRef(id))
    else Left(DesignError.MissingColumn(id.value))

  def bindName(name: String, data: DataTable): Either[DesignError, ColumnRef] =
    ColumnId(name).flatMap(bind(_, data))

final case class BasisRef(kind: HrfKind, name: String)

object BasisRef:
  def bind(name: String): Either[DesignError, BasisRef] =
    HrfKind.fromString(name).left.map(_ => DesignError.UnknownBasis(name)).map(BasisRef(_, name))

final case class ContrastRef(name: String)

object ContrastRef:
  def bind(
      name: String,
      available: Set[String],
      requireKnown: Boolean
  ): Either[DesignError, ContrastRef] =
    if !requireKnown || available.contains(name) then Right(ContrastRef(name))
    else Left(DesignError.UnknownContrast(name, available.toVector.sorted))

final case class BoundPhaseRef(id: PhaseId, parent: ColumnRef)

sealed trait BoundTerm:
  def id: Option[TermId]

final case class BoundHrfTerm(
    id: Option[TermId],
    columns: Vector[ColumnRef],
    onset: ColumnRef,
    basis: Option[BasisRef],
    duration: Option[ColumnRef],
    phase: Option[BoundPhaseRef],
    contrast: Option[ContrastRef],
    raw: HrfCall
) extends BoundTerm

final case class BoundTrialwiseTerm(
    id: Option[TermId],
    onset: ColumnRef,
    basis: Option[BasisRef],
    duration: Option[ColumnRef],
    raw: TrialwiseCall
) extends BoundTerm

final case class BoundCovariateTerm(
    id: Option[TermId],
    columns: Vector[ColumnRef],
    raw: CovariateCall
) extends BoundTerm

final case class BoundFormula(onset: ColumnRef, terms: Vector[BoundTerm])

object BoundFormula:
  def bind(
      formula: ModelFormula,
      data: DataTable,
      availableContrastSets: Set[String] = Set.empty,
      requireKnownContrasts: Boolean = false
  ): Either[DesignError, BoundFormula] =
    for
      onset <- ColumnRef.bind(formula.onset, data)
      terms <- bindTerms(formula.terms, data, onset, availableContrastSets, requireKnownContrasts)
    yield BoundFormula(onset, terms)

  private def bindTerms(
      terms: Vector[TermCall],
      data: DataTable,
      onset: ColumnRef,
      availableContrastSets: Set[String],
      requireKnownContrasts: Boolean
  ): Either[DesignError, Vector[BoundTerm]] =
    val out = Vector.newBuilder[BoundTerm]
    var i = 0
    while i < terms.length do
      bindTerm(terms(i), data, onset, availableContrastSets, requireKnownContrasts) match
        case Left(error) => return Left(error)
        case Right(term) => out += term
      i += 1
    Right(out.result())

  private def bindTerm(
      term: TermCall,
      data: DataTable,
      onset: ColumnRef,
      availableContrastSets: Set[String],
      requireKnownContrasts: Boolean
  ): Either[DesignError, BoundTerm] =
    term match
      case h: HrfCall =>
        for
          columns <- bindArgColumns(h.vars, data)
          basis <- bindOptionalBasis(h.basis)
          duration <- bindOptionalColumn(h.durations, data, argName = "durations")
          phase <- bindOptionalPhase(h.phase, data)
          contrast <- bindOptionalContrast(h.contrasts, availableContrastSets, requireKnownContrasts)
        yield BoundHrfTerm(h.id.orElse(h.prefix), columns, onset, basis, duration, phase, contrast, h)

      case t: TrialwiseCall =>
        for
          basis <- bindOptionalBasis(t.basis)
          duration <- bindOptionalColumn(t.durations, data, argName = "durations")
        yield BoundTrialwiseTerm(t.label, onset, basis, duration, t)

      case c: CovariateCall =>
        bindArgColumns(c.vars, data).map(BoundCovariateTerm(c.id.orElse(c.prefix), _, c))

  private def bindOptionalBasis(value: Option[String]): Either[DesignError, Option[BasisRef]] =
    value match
      case None => Right(None)
      case Some(v) => BasisRef.bind(v).map(Some(_))

  private def bindOptionalPhase(value: Option[PhaseRef], data: DataTable): Either[DesignError, Option[BoundPhaseRef]] =
    value match
      case None => Right(None)
      case Some(ref) => ColumnRef.bind(ref.parent, data).map(parent => Some(BoundPhaseRef(ref.id, parent)))

  private def bindOptionalColumn(value: Option[ArgValue], data: DataTable, argName: String): Either[DesignError, Option[ColumnRef]] =
    value match
      case None => Right(None)
      case Some(ArgValue.Num(_)) => Right(None)
      case Some(ArgValue.Ident(id)) => ColumnRef.bind(id, data).map(Some(_))
      case Some(ArgValue.Str(name)) => ColumnRef.bindName(name, data).map(Some(_))
      case Some(other) =>
        Left(DesignError.FormulaBinding(s"$argName must be a column reference or numeric scalar, found $other"))

  private def bindOptionalContrast(
      value: Option[String],
      availableContrastSets: Set[String],
      requireKnownContrasts: Boolean
  ): Either[DesignError, Option[ContrastRef]] =
    value match
      case None       => Right(None)
      case Some(name) => ContrastRef.bind(name, availableContrastSets, requireKnownContrasts).map(Some(_))

  private def bindArgColumns(values: Vector[ArgValue], data: DataTable): Either[DesignError, Vector[ColumnRef]] =
    val out = Vector.newBuilder[ColumnRef]
    var i = 0
    while i < values.length do
      collectColumns(values(i), data) match
        case Left(error) => return Left(error)
        case Right(refs) => out ++= refs
      i += 1
    Right(out.result().distinct)

  private def collectColumns(value: ArgValue, data: DataTable): Either[DesignError, Vector[ColumnRef]] =
    value match
      case ArgValue.Ident(id) =>
        ColumnRef.bind(id, data).map(Vector(_))
      case ArgValue.Call(_, args) =>
        val out = Vector.newBuilder[ColumnRef]
        var i = 0
        while i < args.length do
          collectColumns(args(i).value, data) match
            case Left(error) => return Left(error)
            case Right(refs) => out ++= refs
          i += 1
        Right(out.result())
      case ArgValue.Num(_) | ArgValue.Str(_) | ArgValue.Bool(_) =>
        Right(Vector.empty)
