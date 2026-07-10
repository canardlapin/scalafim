package scalafim.fmri.design.contrast

import scalafim.fmri.design.{BasisIndex, DesignColumnIndex, FactorId, Names}
import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm}
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal
import scala.util.matching.Regex

enum ContrastError:
  case InvalidId(kind: String, value: String, reason: String)
  case NoCategoricalCells(contrast: String)
  case ContinuousTermUnsupported(contrast: String)
  case UnknownFactor(contrast: String, factor: String, known: Vector[String])
  case DuplicateFactors(contrast: String, factors: Vector[String])
  case InsufficientLevels(contrast: String, factor: String, observed: Int)
  case EmptySelection(contrast: String)
  case MaskLengthMismatch(contrast: String, expected: Int, actual: Int)
  case MaskOverlap(contrast: String)
  case InvalidBasisSelection(contrast: String, detail: String)
  case InvalidWeights(contrast: String, detail: String)
  case IncompatibleWeights(contrast: String, detail: String)
  case MissingConditionRow(contrast: String, row: String, known: Vector[String])
  case DuplicateContrasts(context: String, names: Vector[String])
  case UnknownTerm(termKey: String, known: Vector[String])
  case UnsupportedTerm(termKey: String, found: String)
  case CompileFailed(contrast: String, detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind '$value': $reason"
      case NoCategoricalCells(contrast) =>
        s"Contrast '$contrast': term has no categorical cells"
      case ContinuousTermUnsupported(contrast) =>
        s"Contrast '$contrast': categorical-only contrasts require categorical-only terms for now"
      case UnknownFactor(contrast, factor, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Contrast '$contrast': factor '$factor' not found$suffix"
      case DuplicateFactors(contrast, factors) =>
        s"Contrast '$contrast': factors must be distinct (${factors.mkString(", ")})"
      case InsufficientLevels(contrast, factor, observed) =>
        s"Contrast '$contrast': factor '$factor' needs at least 2 levels, found $observed"
      case EmptySelection(contrast) =>
        s"Contrast '$contrast': no conditions selected"
      case MaskLengthMismatch(contrast, expected, actual) =>
        s"Contrast '$contrast': mask length $actual does not match $expected conditions"
      case MaskOverlap(contrast) =>
        s"Contrast '$contrast': masks for group A and group B overlap"
      case InvalidBasisSelection(contrast, detail) =>
        s"Contrast '$contrast': $detail"
      case InvalidWeights(contrast, detail) =>
        s"Contrast '$contrast': $detail"
      case IncompatibleWeights(contrast, detail) =>
        s"Contrast '$contrast': $detail"
      case MissingConditionRow(contrast, row, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Contrast '$contrast': row '$row' not found in categorical condition names$suffix"
      case DuplicateContrasts(context, names) =>
        s"$context contains duplicate contrast names: ${names.mkString(", ")}"
      case UnknownTerm(termKey, known) =>
        val suffix = if known.isEmpty then "" else s" (known: ${known.mkString(", ")})"
        s"Unknown term key: '$termKey'$suffix"
      case UnsupportedTerm(termKey, found) =>
        s"Term '$termKey' does not support contrasts (found $found)"
      case CompileFailed(contrast, detail) =>
        s"Contrast '$contrast': $detail"

object ContrastError:
  def fromThrowable(contrast: String, throwable: Throwable): ContrastError =
    val msg = Option(throwable.getMessage).filter(_.nonEmpty).getOrElse(throwable.toString)
    CompileFailed(contrast, msg)

private def validateContrastId(kind: String, value: String): Either[ContrastError, String] =
  val trimmed = Option(value).fold("")(_.trim)
  if trimmed.isEmpty then Left(ContrastError.InvalidId(kind, value, "must be non-empty"))
  else Right(trimmed)

opaque type ContrastId = String

object ContrastId:
  def apply(value: String): Either[ContrastError, ContrastId] =
    validateContrastId("contrast id", value)

  inline def unsafe(value: String): ContrastId =
    value

  extension (id: ContrastId)
    inline def value: String = id

opaque type ContrastSetId = String

object ContrastSetId:
  def apply(value: String): Either[ContrastError, ContrastSetId] =
    validateContrastId("contrast set id", value)

  inline def unsafe(value: String): ContrastSetId =
    value

  extension (id: ContrastSetId)
    inline def value: String = id

opaque type EffectId = String

object EffectId:
  def apply(value: String): Either[ContrastError, EffectId] =
    validateContrastId("effect id", value)

  inline def unsafe(value: String): EffectId =
    value

  extension (id: EffectId)
    inline def value: String = id

opaque type LevelId = String

object LevelId:
  def apply(value: String): Either[ContrastError, LevelId] =
    validateContrastId("level id", value)

  inline def unsafe(value: String): LevelId =
    value

  extension (id: LevelId)
    inline def value: String = id

final case class FactorSelector(id: FactorId):
  infix def ===(level: String): CellSelector =
    CellSelector.Equals(id, LevelId.unsafe(level))

  infix def =!=(level: String): CellSelector =
    CellSelector.NotEquals(id, LevelId.unsafe(level))

  def in(levels: String*): CellSelector =
    CellSelector.In(id, levels.toVector.map(LevelId.unsafe).toSet)

enum CellSelector:
  case All
  case Equals(factor: FactorId, level: LevelId)
  case NotEquals(factor: FactorId, level: LevelId)
  case In(factor: FactorId, levels: Set[LevelId])
  case And(left: CellSelector, right: CellSelector)
  case Or(left: CellSelector, right: CellSelector)
  case Not(selector: CellSelector)
  case Custom(label: String, predicate: Cell => Boolean)

  infix def &&(other: CellSelector): CellSelector =
    CellSelector.And(this, other)

  infix def ||(other: CellSelector): CellSelector =
    CellSelector.Or(this, other)

  def unary_! : CellSelector =
    CellSelector.Not(this)

  def factors: Vector[FactorId] =
    this match
      case CellSelector.All               => Vector.empty
      case CellSelector.Equals(factor, _) => Vector(factor)
      case CellSelector.NotEquals(factor, _) =>
        Vector(factor)
      case CellSelector.In(factor, _) =>
        Vector(factor)
      case CellSelector.And(left, right) =>
        (left.factors ++ right.factors).distinct
      case CellSelector.Or(left, right) =>
        (left.factors ++ right.factors).distinct
      case CellSelector.Not(selector) =>
        selector.factors
      case CellSelector.Custom(_, _) =>
        Vector.empty

  def matches(cell: Cell, contrast: ContrastId = ContrastId.unsafe("selector")): Either[ContrastError, Boolean] =
    this match
      case CellSelector.All =>
        Right(true)
      case CellSelector.Equals(factor, level) =>
        valueFor(cell, contrast, factor).map(_ == level.value)
      case CellSelector.NotEquals(factor, level) =>
        valueFor(cell, contrast, factor).map(_ != level.value)
      case CellSelector.In(factor, levels) =>
        val allowed = levels.map(_.value)
        valueFor(cell, contrast, factor).map(allowed.contains)
      case CellSelector.And(left, right) =>
        for
          l <- left.matches(cell, contrast)
          r <- right.matches(cell, contrast)
        yield l && r
      case CellSelector.Or(left, right) =>
        for
          l <- left.matches(cell, contrast)
          r <- right.matches(cell, contrast)
        yield l || r
      case CellSelector.Not(selector) =>
        selector.matches(cell, contrast).map(!_ )
      case CellSelector.Custom(_, predicate) =>
        try Right(predicate(cell))
        catch case NonFatal(t) => Left(ContrastError.fromThrowable(contrast.value, t))

  def predicateUnsafe(contrast: ContrastId): Cell => Boolean =
    cell =>
      matches(cell, contrast) match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error.message)

  private def valueFor(cell: Cell, contrast: ContrastId, factor: FactorId): Either[ContrastError, String] =
    cell.get(factor.value) match
      case Some(value) => Right(value)
      case None        => Left(ContrastError.UnknownFactor(contrast.value, factor.value, cell.vars))

object CellSelector:
  def factor(name: String): Either[ContrastError, FactorSelector] =
    FactorId(name).left.map(err => ContrastError.InvalidId("factor id", name, err.message)).map(FactorSelector(_))

  def factorUnsafe(name: String): FactorSelector =
    FactorSelector(FactorId.unsafe(Names.sanitize(name, allowDot = true)))

  def custom(label: String)(predicate: Cell => Boolean): CellSelector =
    CellSelector.Custom(label, predicate)

enum BasisSelection:
  case All
  case Only(indices: Vector[BasisIndex])
  case WeightedAll(weights: Vector[Double])
  case Weighted(indices: Vector[BasisIndex], weights: Vector[Double])

  def legacyOptions(nbasis: Int, contrast: ContrastId): Either[ContrastError, (Option[Vector[Int]], Option[Vector[Double]])] =
    def validateIndices(indices: Vector[BasisIndex]): Either[ContrastError, Vector[Int]] =
      if indices.isEmpty then Left(ContrastError.InvalidBasisSelection(contrast.value, "basis selection must be non-empty"))
      else
        val oneBased = indices.map(_.oneBased).distinct.sorted
        oneBased.find(i => i < 1 || i > nbasis) match
          case Some(i) => Left(ContrastError.InvalidBasisSelection(contrast.value, s"basis index out of range: $i"))
          case None    => Right(oneBased)

    def validateWeights(weights: Vector[Double], expected: Int): Either[ContrastError, Vector[Double]] =
      if weights.length != expected then
        Left(ContrastError.InvalidBasisSelection(contrast.value, s"basisWeights length (${weights.length}) must match selected bases ($expected)"))
      else if !weights.forall(_.isFinite) then Left(ContrastError.InvalidBasisSelection(contrast.value, "basisWeights must be finite"))
      else if !weights.forall(_ >= 0.0) then Left(ContrastError.InvalidBasisSelection(contrast.value, "basisWeights must be non-negative"))
      else if weights.sum <= 0.0 then Left(ContrastError.InvalidBasisSelection(contrast.value, "basisWeights must sum to a positive value"))
      else Right(weights)

    this match
      case BasisSelection.All =>
        Right((None, None))
      case BasisSelection.Only(indices) =>
        validateIndices(indices).map(indices0 => (Some(indices0), None))
      case BasisSelection.WeightedAll(weights) =>
        validateWeights(weights, nbasis).map(weights0 => (None, Some(weights0)))
      case BasisSelection.Weighted(indices, weights) =>
        for
          indices0 <- validateIndices(indices)
          weights0 <- validateWeights(weights, indices0.length)
        yield (Some(indices0), Some(weights0))

object BasisSelection:
  val all: BasisSelection =
    BasisSelection.All

  def only(indices: BasisIndex*): BasisSelection =
    BasisSelection.Only(indices.toVector)

  def weightedAll(weights: Seq[Double]): BasisSelection =
    BasisSelection.WeightedAll(weights.toVector)

  def weighted(indices: Seq[BasisIndex], weights: Seq[Double]): BasisSelection =
    BasisSelection.Weighted(indices.toVector, weights.toVector)

  def fromLegacy(
      basis: Option[Seq[Int]],
      basisWeights: Option[Seq[Double]],
      nbasis: Int,
      contrast: ContrastId
  ): Either[ContrastError, BasisSelection] =
    def toBasisIndex(value: Int): Either[ContrastError, BasisIndex] =
      BasisIndex.fromOneBased(value).left.map(err => ContrastError.InvalidBasisSelection(contrast.value, err.message))

    val selectionEither =
      basis match
        case None =>
          basisWeights match
            case None    => Right(BasisSelection.All)
            case Some(w) => Right(BasisSelection.WeightedAll(w.toVector))
        case Some(values) =>
          values.toVector.foldLeft(Right(Vector.empty): Either[ContrastError, Vector[BasisIndex]]) {
            case (acc, value) =>
              for
                out <- acc
                ix <- toBasisIndex(value)
              yield out :+ ix
          }.map { indices =>
            basisWeights match
              case None    => BasisSelection.Only(indices)
              case Some(w) => BasisSelection.Weighted(indices, w.toVector)
          }

    selectionEither.flatMap { selection =>
      selection.legacyOptions(nbasis, contrast).map(_ => selection)
    }

sealed trait ContrastAxis
sealed trait LocalContrastAxis extends ContrastAxis
sealed trait DesignContrastAxis extends ContrastAxis

final case class TypedContrastWeights[Axis <: ContrastAxis](
    id: ContrastId,
    rowNames: Vector[String],
    contrastNames: Vector[String],
    weights: Mat,
    selectedRowNames: Vector[String]
):
  require(weights.rows == rowNames.length, "weights/rowNames row mismatch")
  require(weights.cols == contrastNames.length, "weights/contrastNames col mismatch")

  def toLegacy: ContrastWeights =
    ContrastWeights(
      name = id.value,
      condNames = rowNames,
      contrastNames = contrastNames,
      weights = weights,
      selectedCondNames = selectedRowNames
    )

type LocalContrastWeights = TypedContrastWeights[LocalContrastAxis]
type DesignContrastWeights = TypedContrastWeights[DesignContrastAxis]

object TypedContrastWeights:
  def fromLegacyLocal(weights: ContrastWeights): LocalContrastWeights =
    TypedContrastWeights[LocalContrastAxis](
      id = ContrastId.unsafe(weights.name),
      rowNames = weights.condNames,
      contrastNames = weights.contrastNames,
      weights = weights.weights,
      selectedRowNames = weights.selectedCondNames
    )

  def fromLegacyDesign(weights: ContrastWeights): DesignContrastWeights =
    TypedContrastWeights[DesignContrastAxis](
      id = ContrastId.unsafe(weights.name),
      rowNames = weights.condNames,
      contrastNames = weights.contrastNames,
      weights = weights.weights,
      selectedRowNames = weights.selectedCondNames
    )

enum ContrastSource:
  case User, GeneratedF, ColumnPattern, Legacy

final case class CompiledContrast(
    id: ContrastId,
    effect: Option[EffectId],
    source: ContrastSource,
    weights: DesignContrastWeights
):
  def toLegacy: ContrastWeights =
    weights.toLegacy

final case class ContrastFactor(id: FactorId, levels: Vector[LevelId], event: CategoricalEvent):
  def levelValues: Vector[String] =
    levels.map(_.value)

final case class ContrastColumn(
    index: DesignColumnIndex,
    name: String,
    condition: Option[String],
    basis: Option[BasisIndex]
):
  def semanticKey(nbasis: Int): Option[String] =
    condition.map { cond =>
      basis match
        case Some(ix) if nbasis > 1 => cond + Names.basisSuffix(ix.oneBased, nbasis)
        case _                      => cond
    }

final case class ContrastSpace(
    term: ConvolvedTerm,
    factors: Vector[ContrastFactor],
    cells: TermCells,
    baseConditionNames: Vector[String],
    columns: Vector[ContrastColumn]
):
  def nbasis: Int =
    term.hrf.nbasis

  def factor(id: FactorId): Option[ContrastFactor] =
    factors.find(_.id.value == id.value)

  def factorNames: Vector[String] =
    factors.map(_.id.value)

  def columnSemanticKeys: Vector[Option[String]] =
    columns.map(_.semanticKey(nbasis))

object ContrastSpace:
  def from(term: ConvolvedTerm): Either[ContrastError, ContrastSpace] =
    val factors = term.term.events.collect { case c: CategoricalEvent =>
      ContrastFactor(
        id = FactorId.unsafe(c.varName),
        levels = c.levels.map(LevelId.unsafe),
        event = c
      )
    }

    val columns =
      Vector.tabulate(term.columnNames.length) { i =>
        ContrastColumn(
          index = DesignColumnIndex.unsafeOneBased(i + 1),
          name = term.columnNames(i),
          condition =
            if term.columnConditions.nonEmpty && i < term.columnConditions.length then term.columnConditions(i)
            else None,
          basis =
            if term.columnBasisIx.nonEmpty && i < term.columnBasisIx.length then
              term.columnBasisIx(i).map(BasisIndex.unsafeOneBased)
            else None
        )
      }

    Right(
      ContrastSpace(
        term = term,
        factors = factors,
        cells = TermCells.from(term.term, dropEmpty = true),
        baseConditionNames = term.term.conditions,
        columns = columns
      )
    )

enum ContrastExpr:
  case Pair(
      contrastId: ContrastId,
      A: CellSelector,
      B: CellSelector,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  )
  case UnitContrast(
      contrastId: ContrastId,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  )
  case Oneway(
      contrastId: ContrastId,
      factor: FactorId,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  )
  case Interaction(
      contrastId: ContrastId,
      factor1: FactorId,
      factor2: FactorId,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  )
  case ColumnPattern(contrastId: ContrastId, patternA: Regex, patternB: Option[Regex] = None)

  def id: ContrastId =
    this match
      case Pair(contrastId, _, _, _, _)        => contrastId
      case UnitContrast(contrastId, _, _)      => contrastId
      case Oneway(contrastId, _, _, _)         => contrastId
      case Interaction(contrastId, _, _, _, _) => contrastId
      case ColumnPattern(contrastId, _, _)     => contrastId

  def compile(space: ContrastSpace): Either[ContrastError, CompiledContrast] =
    def withBasis(basis: BasisSelection): Either[ContrastError, (Option[Vector[Int]], Option[Vector[Double]])] =
      basis.legacyOptions(space.nbasis, id)

    def validateSelectors(selectors: CellSelector*): Either[ContrastError, Unit] =
      val known = space.factorNames.toSet
      selectors.iterator.flatMap(_.factors).find(f => !known.contains(f.value)) match
        case Some(factor) => Left(ContrastError.UnknownFactor(id.value, factor.value, space.factorNames))
        case None         => Right(())

    def compileLegacy(source: ContrastSource)(body: => ContrastWeights): Either[ContrastError, CompiledContrast] =
      try
        val legacy = body
        Right(
          CompiledContrast(
            id = id,
            effect = None,
            source = source,
            weights = TypedContrastWeights.fromLegacyDesign(legacy)
          )
        )
      catch case NonFatal(t) => Left(ContrastError.fromThrowable(id.value, t))

    this match
      case Pair(_, a, b, where, basis) =>
        for
          _ <- validateSelectors(a, b, where)
          opts <- withBasis(basis)
          compiled <- compileLegacy(ContrastSource.User) {
            val (basisIx, basisWeights) = opts
            ConvolvedContrastWeights.pair(
              term = space.term,
              name = id.value,
              A = a.predicateUnsafe(id),
              B = b.predicateUnsafe(id),
              where = where.predicateUnsafe(id),
              basis = basisIx,
              basisWeights = basisWeights
            )
          }
        yield compiled
      case UnitContrast(_, where, basis) =>
        for
          _ <- validateSelectors(where)
          opts <- withBasis(basis)
          compiled <- compileLegacy(ContrastSource.User) {
            val (basisIx, basisWeights) = opts
            ConvolvedContrastWeights.unit(
              term = space.term,
              name = id.value,
              where = where.predicateUnsafe(id),
              basis = basisIx,
              basisWeights = basisWeights
            )
          }
        yield compiled
      case Oneway(_, factor, where, basis) =>
        if !space.factorNames.contains(factor.value) then
          Left(ContrastError.UnknownFactor(id.value, factor.value, space.factorNames))
        else
          for
            _ <- validateSelectors(where)
            opts <- withBasis(basis)
            compiled <- compileLegacy(ContrastSource.User) {
              val (basisIx, basisWeights) = opts
              ConvolvedContrastWeights.oneway(
                term = space.term,
                name = id.value,
                factor = factor.value,
                where = where.predicateUnsafe(id),
                basis = basisIx,
                basisWeights = basisWeights
              )
            }
          yield compiled
      case Interaction(_, factor1, factor2, where, basis) =>
        val missing = Vector(factor1, factor2).filterNot(f => space.factorNames.contains(f.value))
        if factor1.value == factor2.value then Left(ContrastError.DuplicateFactors(id.value, Vector(factor1.value, factor2.value)))
        else if missing.nonEmpty then Left(ContrastError.UnknownFactor(id.value, missing.head.value, space.factorNames))
        else
          for
            _ <- validateSelectors(where)
            opts <- withBasis(basis)
            compiled <- compileLegacy(ContrastSource.User) {
              val (basisIx, basisWeights) = opts
              ConvolvedContrastWeights.interaction(
                term = space.term,
                name = id.value,
                factor1 = factor1.value,
                factor2 = factor2.value,
                where = where.predicateUnsafe(id),
                basis = basisIx,
                basisWeights = basisWeights
              )
            }
          yield compiled
      case ColumnPattern(_, patternA, patternB) =>
        compileLegacy(ContrastSource.ColumnPattern) {
          ConvolvedContrastWeights.column(space.term, id.value, patternA, patternB)
        }

object ContrastExpr:
  def pair(
      name: String,
      A: CellSelector,
      B: CellSelector,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  ): Either[ContrastError, ContrastExpr] =
    ContrastId(name).map(ContrastExpr.Pair(_, A, B, where, basis))

  def unit(
      name: String,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  ): Either[ContrastError, ContrastExpr] =
    ContrastId(name).map(ContrastExpr.UnitContrast(_, where, basis))

  def oneway(
      name: String,
      factor: String,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  ): Either[ContrastError, ContrastExpr] =
    for
      id <- ContrastId(name)
      fac <- FactorId(factor).left.map(err => ContrastError.InvalidId("factor id", factor, err.message))
    yield ContrastExpr.Oneway(id, fac, where, basis)

  def interaction(
      name: String,
      factor1: String,
      factor2: String,
      where: CellSelector = CellSelector.All,
      basis: BasisSelection = BasisSelection.All
  ): Either[ContrastError, ContrastExpr] =
    for
      id <- ContrastId(name)
      f1 <- FactorId(factor1).left.map(err => ContrastError.InvalidId("factor id", factor1, err.message))
      f2 <- FactorId(factor2).left.map(err => ContrastError.InvalidId("factor id", factor2, err.message))
    yield ContrastExpr.Interaction(id, f1, f2, where, basis)

  def column(name: String, patternA: Regex, patternB: Option[Regex] = None): Either[ContrastError, ContrastExpr] =
    ContrastId(name).map(ContrastExpr.ColumnPattern(_, patternA, patternB))

object ContrastCompiler:
  def compile(term: ConvolvedTerm, expr: ContrastExpr): Either[ContrastError, CompiledContrast] =
    for
      space <- ContrastSpace.from(term)
      compiled <- expr.compile(space)
    yield compiled

  def compileLegacy(
      term: ConvolvedTerm,
      name: String,
      source: ContrastSource = ContrastSource.Legacy
  )(body: => ContrastWeights): Either[ContrastError, CompiledContrast] =
    try
      val legacy = body
      Right(
        CompiledContrast(
          id = ContrastId.unsafe(name),
          effect = None,
          source = source,
          weights = TypedContrastWeights.fromLegacyDesign(legacy)
        )
      )
    catch case NonFatal(t) => Left(ContrastError.fromThrowable(name, t))

  def compileMap(values: VectorMap[String, ContrastWeights], source: ContrastSource): Either[ContrastError, VectorMap[String, CompiledContrast]] =
    val out = VectorMap.newBuilder[String, CompiledContrast]
    values.foreach { case (key, weights) =>
      val id = ContrastId.unsafe(key)
      out += key -> CompiledContrast(
        id = id,
        effect = Some(EffectId.unsafe(weights.name)),
        source = source,
        weights = TypedContrastWeights.fromLegacyDesign(weights)
      )
    }
    Right(out.result())
