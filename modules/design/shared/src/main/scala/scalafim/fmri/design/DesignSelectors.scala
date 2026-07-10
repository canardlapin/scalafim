package scalafim.fmri.design

enum TermSelector:
  case AnyTerm
  case Tag(value: String)
  case Index(value: TermIndex)

  def matches(column: DesignColumnDescriptor): Boolean =
    this match
      case TermSelector.AnyTerm =>
        column.termTag.nonEmpty || column.termIndex.nonEmpty
      case TermSelector.Tag(value) =>
        column.termTag.contains(value)
      case TermSelector.Index(value) =>
        column.termIndex.contains(value)

object TermSelector:
  def tag(value: String): TermSelector =
    TermSelector.Tag(value)

  def indexOneBased(value: Int): Either[DesignError, TermSelector] =
    TermIndex.fromOneBased(value).map(TermSelector.Index.apply)

  inline def unsafeIndexOneBased(value: Int): TermSelector =
    TermSelector.Index(TermIndex.unsafeOneBased(value))

enum DesignColumnSelector:
  case All
  case Name(value: String)
  case Term(selector: TermSelector)
  case Role(value: ColumnRole)
  case Condition(value: String)
  case Run(value: RunIndex)
  case BasisName(value: String)
  case BasisIndex(value: scalafim.fmri.design.BasisIndex)
  case BasisLabel(value: String)
  case Source(value: ModelSource)
  case And(left: DesignColumnSelector, right: DesignColumnSelector)
  case Or(left: DesignColumnSelector, right: DesignColumnSelector)
  case Not(selector: DesignColumnSelector)

  def matches(column: DesignColumnDescriptor): Boolean =
    this match
      case DesignColumnSelector.All =>
        true
      case DesignColumnSelector.Name(value) =>
        column.name == value
      case DesignColumnSelector.Term(selector) =>
        selector.matches(column)
      case DesignColumnSelector.Role(value) =>
        column.role == value
      case DesignColumnSelector.Condition(value) =>
        column.condition.contains(value)
      case DesignColumnSelector.Run(value) =>
        column.run.contains(value)
      case DesignColumnSelector.BasisName(value) =>
        column.basis.exists(_.name == value)
      case DesignColumnSelector.BasisIndex(value) =>
        column.basis.flatMap(_.index).contains(value)
      case DesignColumnSelector.BasisLabel(value) =>
        column.basis.flatMap(_.label).contains(value)
      case DesignColumnSelector.Source(value) =>
        column.modelSource == value
      case DesignColumnSelector.And(left, right) =>
        left.matches(column) && right.matches(column)
      case DesignColumnSelector.Or(left, right) =>
        left.matches(column) || right.matches(column)
      case DesignColumnSelector.Not(selector) =>
        !selector.matches(column)

  infix def &&(right: DesignColumnSelector): DesignColumnSelector =
    (this, right) match
      case (DesignColumnSelector.All, other) => other
      case (other, DesignColumnSelector.All) => other
      case _                                => DesignColumnSelector.And(this, right)

  infix def ||(right: DesignColumnSelector): DesignColumnSelector =
    (this, right) match
      case (DesignColumnSelector.All, _) => DesignColumnSelector.All
      case (_, DesignColumnSelector.All) => DesignColumnSelector.All
      case _                            => DesignColumnSelector.Or(this, right)

  def unary_! : DesignColumnSelector =
    DesignColumnSelector.Not(this)

object DesignColumnSelector:
  def name(value: String): DesignColumnSelector =
    DesignColumnSelector.Name(value)

  def term(value: String): DesignColumnSelector =
    DesignColumnSelector.Term(TermSelector.Tag(value))

  def term(selector: TermSelector): DesignColumnSelector =
    DesignColumnSelector.Term(selector)

  def termIndexOneBased(value: Int): Either[DesignError, DesignColumnSelector] =
    TermSelector.indexOneBased(value).map(DesignColumnSelector.Term.apply)

  inline def unsafeTermIndexOneBased(value: Int): DesignColumnSelector =
    DesignColumnSelector.Term(TermSelector.unsafeIndexOneBased(value))

  def role(value: ColumnRole): DesignColumnSelector =
    DesignColumnSelector.Role(value)

  def condition(value: String): DesignColumnSelector =
    DesignColumnSelector.Condition(value)

  def run(index: RunIndex): DesignColumnSelector =
    DesignColumnSelector.Run(index)

  def runOneBased(value: Int): Either[DesignError, DesignColumnSelector] =
    RunIndex.fromOneBased(value).map(DesignColumnSelector.Run.apply)

  inline def unsafeRunOneBased(value: Int): DesignColumnSelector =
    DesignColumnSelector.Run(RunIndex.unsafeOneBased(value))

  def basisName(value: String): DesignColumnSelector =
    DesignColumnSelector.BasisName(value)

  def basisIndex(index: scalafim.fmri.design.BasisIndex): DesignColumnSelector =
    DesignColumnSelector.BasisIndex(index)

  def basisIndexOneBased(value: Int): Either[DesignError, DesignColumnSelector] =
    scalafim.fmri.design.BasisIndex.fromOneBased(value).map(DesignColumnSelector.BasisIndex.apply)

  inline def unsafeBasisIndexOneBased(value: Int): DesignColumnSelector =
    DesignColumnSelector.BasisIndex(scalafim.fmri.design.BasisIndex.unsafeOneBased(value))

  def basisLabel(value: String): DesignColumnSelector =
    DesignColumnSelector.BasisLabel(value)

  def source(value: ModelSource): DesignColumnSelector =
    DesignColumnSelector.Source(value)
