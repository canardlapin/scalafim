package scalafim.bids

enum NaAction:
  case Leave
  case Zero
  case Median

enum ConfoundClean:
  case NoCleaning
  case ZeroVariance
  case Rank

enum ConfoundRole:
  case Confound
  case Pca
  case Raw

enum ConfoundDiagnosticReason:
  case ZeroVariance
  case RankDeficient

enum ConfoundDiagnosticAction:
  case Drop
  case Flag

opaque type PositiveInt = Int

object PositiveInt:
  def from(value: Int): Option[PositiveInt] =
    Option.when(value > 0)(value)

  def unsafe(value: Int): PositiveInt =
    require(value > 0, "value must be positive")
    value

  extension (value: PositiveInt)
    def toInt: Int = value

opaque type VariancePercent = Double

object VariancePercent:
  def from(value: Double): Option[VariancePercent] =
    Option.when(value.isFinite && value > 0.0 && value <= 100.0)(value)

  def unsafe(value: Double): VariancePercent =
    require(value.isFinite && value > 0.0 && value <= 100.0, "percent variance must be in (0, 100]")
    value

  extension (value: VariancePercent)
    def toDouble: Double = value

enum PcaRetention:
  case Components(n: PositiveInt)
  case Percent(value: VariancePercent)

object PcaRetention:
  def components(value: Int): Either[BidsError, PcaRetention] =
    PositiveInt
      .from(value)
      .map(PcaRetention.Components(_))
      .toRight(BidsError.InvalidConfoundStrategy("PCA retention", "component count must be positive"))

  def percent(value: Double): Either[BidsError, PcaRetention] =
    VariancePercent
      .from(value)
      .map(PcaRetention.Percent(_))
      .toRight(BidsError.InvalidConfoundStrategy("PCA retention", "percent variance must be in (0, 100]"))

final case class ConfoundDiagnostic(
    column: String,
    reason: ConfoundDiagnosticReason,
    action: ConfoundDiagnosticAction,
    role: ConfoundRole,
    sd: Double,
    rank: Option[Int] = None
)

final class ConfoundStrategy private (
    val name: String,
    val pcaVars: Vector[String],
    val rawVars: Vector[String],
    val pcaRetention: Option[PcaRetention]
):
  def npcs: Option[Int] =
    pcaRetention.collect { case PcaRetention.Components(value) => value.toInt }

  def percentVariance: Option[Double] =
    pcaRetention.collect { case PcaRetention.Percent(value) => value.toDouble }

  override def equals(other: Any): Boolean =
    other match
      case that: ConfoundStrategy =>
        name == that.name &&
          pcaVars == that.pcaVars &&
          rawVars == that.rawVars &&
          pcaRetention == that.pcaRetention
      case _ => false

  override def hashCode(): Int =
    (name, pcaVars, rawVars, pcaRetention).##

  override def toString: String =
    s"ConfoundStrategy($name,$pcaVars,$rawVars,$pcaRetention)"

object ConfoundStrategy:
  val PcaBasic80: ConfoundStrategy =
    unsafe(
      name = "pcabasic80",
      pcaVars = ConfoundSets.motion24 ++ Vector("csf", "white_matter", "a_comp_cor_*", "t_comp_cor_*"),
      rawVars = Vector("cosine_*", "cosine*"),
      pcaRetention = Some(PcaRetention.Percent(VariancePercent.unsafe(80.0)))
    )

  def from(
      name: String,
      pcaVars: Vector[String],
      rawVars: Vector[String] = Vector.empty,
      npcs: Option[Int] = None,
      percentVariance: Option[Double] = None
  ): Either[BidsError, ConfoundStrategy] =
    legacyRetention(name, npcs, percentVariance).flatMap { retention =>
      fromRetention(name, pcaVars, rawVars, retention)
    }

  def fromRetention(
      name: String,
      pcaVars: Vector[String],
      rawVars: Vector[String] = Vector.empty,
      pcaRetention: Option[PcaRetention] = None
  ): Either[BidsError, ConfoundStrategy] =
    validate(new ConfoundStrategy(name, pcaVars, rawVars, pcaRetention))

  def named(name: String): Either[BidsError, ConfoundStrategy] =
    name.trim.toLowerCase match
      case "pcabasic80" => validate(PcaBasic80)
      case other        => Left(BidsError.UnknownConfoundSet(other))

  def retention(strategy: ConfoundStrategy): Either[BidsError, Option[PcaRetention]] =
    Right(strategy.pcaRetention)

  private def legacyRetention(
      name: String,
      npcs: Option[Int],
      percentVariance: Option[Double]
  ): Either[BidsError, Option[PcaRetention]] =
    (npcs, percentVariance) match
      case (Some(_), Some(_)) =>
        Left(BidsError.InvalidConfoundStrategy(name, "choose either npcs or percentVariance, not both"))
      case (Some(n), None) =>
        PositiveInt.from(n) match
          case Some(value) => Right(Some(PcaRetention.Components(value)))
          case None => Left(BidsError.InvalidConfoundStrategy(name, "npcs must be positive"))
      case (None, Some(percent)) =>
        VariancePercent.from(percent) match
          case Some(value) => Right(Some(PcaRetention.Percent(value)))
          case None => Left(BidsError.InvalidConfoundStrategy(name, "percentVariance must be in (0, 100]"))
      case (None, None) =>
        Right(None)

  private[bids] def validate(strategy: ConfoundStrategy): Either[BidsError, ConfoundStrategy] =
    val cleanName = strategy.name.trim
    if cleanName.isEmpty then Left(BidsError.InvalidConfoundStrategy(strategy.name, "name must be non-empty"))
    else if strategy.pcaVars.isEmpty then Left(BidsError.InvalidConfoundStrategy(cleanName, "pcaVars must be non-empty"))
    else Right(new ConfoundStrategy(cleanName, strategy.pcaVars, strategy.rawVars, strategy.pcaRetention))

  private def unsafe(
      name: String,
      pcaVars: Vector[String],
      rawVars: Vector[String] = Vector.empty,
      pcaRetention: Option[PcaRetention] = None
  ): ConfoundStrategy =
    require(name.trim.nonEmpty, "confound strategy name must be non-empty")
    require(pcaVars.nonEmpty, "confound strategy PCA variables must be non-empty")
    new ConfoundStrategy(name.trim, pcaVars, rawVars, pcaRetention)

private[bids] object ConfoundAliases:
  val entries: Vector[(String, Vector[String])] =
    Vector(
      "csf" -> Vector("CSF", "csf"),
      "white_matter" -> Vector("WhiteMatter", "white_matter"),
      "global_signal" -> Vector("GlobalSignal", "global_signal"),
      "std_dvars" -> Vector("stdDVARS", "std_dvars"),
      "dvars" -> Vector("dvars", "non_std_dvars", "non.stdDVARS"),
      "non_std_dvars" -> Vector("non.stdDVARS", "non_std_dvars", "dvars"),
      "vx_wisestd_dvars" -> Vector("vx.wisestdDVARS", "vx_wisestd_dvars"),
      "framewise_displacement" -> Vector("FramewiseDisplacement", "framewise_displacement"),
      "t_comp_cor_00" -> Vector("tCompCor00", "t_comp_cor_00"),
      "t_comp_cor_01" -> Vector("tCompCor01", "t_comp_cor_01"),
      "t_comp_cor_02" -> Vector("tCompCor02", "t_comp_cor_02"),
      "t_comp_cor_03" -> Vector("tCompCor03", "t_comp_cor_03"),
      "t_comp_cor_04" -> Vector("tCompCor04", "t_comp_cor_04"),
      "t_comp_cor_05" -> Vector("tCompCor05", "t_comp_cor_05"),
      "a_comp_cor_00" -> Vector("aCompCor00", "a_comp_cor_00"),
      "a_comp_cor_01" -> Vector("aCompCor01", "a_comp_cor_01"),
      "a_comp_cor_02" -> Vector("aCompCor02", "a_comp_cor_02"),
      "a_comp_cor_03" -> Vector("aCompCor03", "a_comp_cor_03"),
      "a_comp_cor_04" -> Vector("aCompCor04", "a_comp_cor_04"),
      "a_comp_cor_05" -> Vector("aCompCor05", "a_comp_cor_05"),
      "trans_x" -> Vector("X", "trans_x"),
      "trans_y" -> Vector("Y", "trans_y"),
      "trans_z" -> Vector("Z", "trans_z"),
      "rot_x" -> Vector("RotX", "rot_x"),
      "rot_y" -> Vector("RotY", "rot_y"),
      "rot_z" -> Vector("RotZ", "rot_z")
    )

  val canonical: Vector[String] =
    entries.map(_._1)

  def aliasesFor(canonicalName: String): Vector[String] =
    entries.collectFirst { case (`canonicalName`, aliases) => aliases }.getOrElse(Vector(canonicalName))

  def canonicalFor(nameOrAlias: String): Option[String] =
    entries.collectFirst {
      case (canonicalName, aliases) if canonicalName == nameOrAlias || aliases.contains(nameOrAlias) => canonicalName
    }

object ConfoundSets:
  val motion6: Vector[String] =
    Vector("trans_x", "trans_y", "trans_z", "rot_x", "rot_y", "rot_z")

  val derivatives: Vector[String] = motion6.map(_ + "_derivative1")
  val powers: Vector[String] = motion6.map(_ + "_power2")
  val derivativePowers: Vector[String] = motion6.map(_ + "_derivative1_power2")
  val motion12: Vector[String] = motion6 ++ derivatives
  val motion24: Vector[String] = motion12 ++ powers ++ derivativePowers

  val global3: Vector[String] =
    Vector("csf", "white_matter", "global_signal")

  private val globalDerivatives = global3.map(_ + "_derivative1")
  private val globalPowers = global3.map(_ + "_power2")
  private val globalDerivativePowers = global3.map(_ + "_derivative1_power2")

  val legacyDefault: Vector[String] =
    ConfoundAliases.canonical

  def named(name: String, n: Option[Int] = None): Either[BidsError, Vector[String]] =
    n match
      case Some(k) if k <= 0 =>
        Left(BidsError.InvalidConfoundStrategy(name, "component cap must be positive"))
      case _ =>
        val capped = (prefix: String) => n.fold(s"${prefix}_*")(k => s"${prefix}_*[$k]")
        name.trim.toLowerCase match
          case "motion6" => Right(motion6)
          case "motion12" => Right(motion12)
          case "motion24" => Right(motion24)
          case "global3" => Right(global3)
          case "9p" => Right(motion6 ++ global3)
          case "36p" => Right(motion24 ++ global3 ++ globalDerivatives ++ globalPowers ++ globalDerivativePowers)
          case "acompcor" | "acomppcor" => Right(Vector(capped("a_comp_cor")))
          case "tcompcor" => Right(Vector(capped("t_comp_cor")))
          case "compcor" => Right(Vector(capped("a_comp_cor"), capped("t_comp_cor")))
          case "cosine" => Right(Vector("cosine_*", "cosine*"))
          case "outliers" => Right(Vector("framewise_displacement", "rmsd", "motion_outlier_*", "non_steady_state_outlier*"))
          case "dvars" | "std_dvars" => Right(Vector("std_dvars"))
          case "raw_dvars" => Right(Vector("dvars"))
          case "non_std_dvars" => Right(Vector("non_std_dvars"))
          case "vx_wisestd_dvars" => Right(Vector("vx_wisestd_dvars"))
          case "fd" => Right(Vector("framewise_displacement"))
          case "legacy_default" => Right(legacyDefault)
          case other => Left(BidsError.UnknownConfoundSet(other))

final case class ConfoundSelectionConfig(
    variables: Vector[String] = ConfoundSets.legacyDefault,
    naAction: NaAction = NaAction.Leave,
    clean: Vector[ConfoundClean] = Vector(ConfoundClean.ZeroVariance),
    role: ConfoundRole = ConfoundRole.Confound
)

final case class ConfoundSelection(
    table: BidsTable,
    requested: Vector[String],
    resolved: Vector[String],
    diagnostics: Vector[ConfoundDiagnostic],
    pca: Option[ConfoundPca] = None
)

final case class ConfoundPca(
    sourceColumns: Vector[String],
    componentNames: Vector[String],
    variances: Vector[Double],
    proportionVariance: Vector[Double],
    cumulativeProportion: Vector[Double],
    loadings: Vector[Vector[Double]]
)

object ConfoundSelector:
  private val Suffixes: Vector[String] =
    Vector("_derivative1_power2", "_derivative1", "_power2")

  def resolveVariables(variables: Vector[String], columns: Vector[String]): Vector[String] =
    distinctPreservingOrder(variables.flatMap(resolveOne(_, columns)))

  def select(
      table: BidsTable,
      config: ConfoundSelectionConfig = ConfoundSelectionConfig()
  ): Either[BidsError, ConfoundSelection] =
    val resolved = resolveVariables(config.variables, table.columns)
    if resolved.isEmpty then Left(BidsError.NoConfoundColumns(config.variables, table.columns))
    else
      for
        selected <- table.select(resolved)
        cleaned <- cleanTable(selected, config.clean, config.role)
        filled <- applyNaAction(cleaned.table, config.naAction)
      yield ConfoundSelection(
        table = filled,
        requested = config.variables,
        resolved = resolved,
        diagnostics = cleaned.diagnostics
      )

  def selectStrategy(
      table: BidsTable,
      strategy: ConfoundStrategy,
      naAction: NaAction = NaAction.Leave,
      clean: Vector[ConfoundClean] = Vector(ConfoundClean.ZeroVariance)
  ): Either[BidsError, ConfoundSelection] =
    ConfoundStrategy.validate(strategy).flatMap { strategy =>
      val pcaColumns = resolveVariables(strategy.pcaVars, table.columns)
      if pcaColumns.isEmpty then Left(BidsError.NoConfoundColumns(strategy.pcaVars, table.columns))
      else
        for
          selectedPca <- table.select(pcaColumns)
          cleanedPca <- cleanTable(selectedPca, clean, ConfoundRole.Pca)
          reduction <- PrincipalComponentConfoundReducer.reduce(cleanedPca.table, strategy)
          rawSelection <- selectRawStrategyColumns(table, strategy.rawVars, naAction, clean)
          joined <- appendTables(reduction.scores, rawSelection.table)
        yield ConfoundSelection(
          table = joined,
          requested = strategy.pcaVars ++ strategy.rawVars,
          resolved = pcaColumns ++ rawSelection.resolved,
          diagnostics = cleanedPca.diagnostics ++ rawSelection.diagnostics,
          pca = Some(reduction.pca)
        )
    }

  private final case class Cleaned(table: BidsTable, diagnostics: Vector[ConfoundDiagnostic])

  private def selectRawStrategyColumns(
      table: BidsTable,
      variables: Vector[String],
      naAction: NaAction,
      clean: Vector[ConfoundClean]
  ): Either[BidsError, ConfoundSelection] =
    if variables.isEmpty then
      BidsTable
        .fromRows(Vector.empty, Vector.fill(table.nrows)(Vector.empty))
        .map(empty => ConfoundSelection(empty, requested = Vector.empty, resolved = Vector.empty, diagnostics = Vector.empty))
    else
      val resolved = resolveVariables(variables, table.columns)
      if resolved.isEmpty then
        BidsTable
          .fromRows(Vector.empty, Vector.fill(table.nrows)(Vector.empty))
          .map(empty => ConfoundSelection(empty, requested = variables, resolved = Vector.empty, diagnostics = Vector.empty))
      else
        for
          selected <- table.select(resolved)
          cleaned <- cleanTable(selected, clean, ConfoundRole.Raw)
          filled <- applyNaAction(cleaned.table, naAction)
        yield ConfoundSelection(
          table = filled,
          requested = variables,
          resolved = resolved,
          diagnostics = cleaned.diagnostics
        )

  private def resolveOne(variable: String, columns: Vector[String]): Vector[String] =
    if variable.contains("*") then resolveWildcard(variable, columns)
    else resolveSuffixedAlias(variable, columns).orElse(resolveAlias(variable, columns)).toVector

  private def resolveWildcard(variable: String, columns: Vector[String]): Vector[String] =
    val (pattern, limit) =
      val marker = variable.lastIndexOf('[')
      if marker >= 0 && variable.endsWith("]") then
        val rawLimit = variable.substring(marker + 1, variable.length - 1)
        (variable.substring(0, marker), rawLimit.toIntOption.filter(_ > 0))
      else (variable, None)

    val prefix = pattern.takeWhile(_ != '*')
    val matches = columns.filter(_.startsWith(prefix)).distinct.sorted
    limit.fold(matches)(matches.take)

  private def resolveSuffixedAlias(variable: String, columns: Vector[String]): Option[String] =
    Suffixes
      .find(variable.endsWith)
      .flatMap { suffix =>
        val base = variable.dropRight(suffix.length)
        val bases =
          ConfoundAliases.canonicalFor(base).map(ConfoundAliases.aliasesFor).getOrElse(Vector(base))
        bases.map(_ + suffix).find(columns.contains)
      }

  private def resolveAlias(variable: String, columns: Vector[String]): Option[String] =
    ConfoundAliases.canonicalFor(variable) match
      case Some(canonical) => ConfoundAliases.aliasesFor(canonical).find(columns.contains)
      case None            => Option.when(columns.contains(variable))(variable)

  private def cleanTable(
      table: BidsTable,
      requested: Vector[ConfoundClean],
      role: ConfoundRole
  ): Either[BidsError, Cleaned] =
    val clean = normalizeClean(requested)
    if clean.isEmpty then Right(Cleaned(table, Vector.empty))
    else
      val afterZero =
        if clean.contains(ConfoundClean.ZeroVariance) then
          val dropped = table.columns.filter(column => isZeroVariance(columnValues(table, column)))
          val diagnostics = dropped.map { column =>
            val values = columnValues(table, column)
            ConfoundDiagnostic(
              column = column,
              reason = ConfoundDiagnosticReason.ZeroVariance,
              action = ConfoundDiagnosticAction.Drop,
              role = role,
              sd = sampleSd(finiteValues(values))
            )
          }
          dropColumns(table, dropped).map(Cleaned(_, diagnostics))
        else Right(Cleaned(table, Vector.empty))

      afterZero.flatMap { first =>
        if clean.contains(ConfoundClean.Rank) then
          val (dropped, rank) = rankDroppedColumns(first.table)
          val diagnostics = dropped.map { column =>
            ConfoundDiagnostic(
              column = column,
              reason = ConfoundDiagnosticReason.RankDeficient,
              action = ConfoundDiagnosticAction.Drop,
              role = role,
              sd = sampleSd(finiteValues(columnValues(first.table, column))),
              rank = Some(rank)
            )
          }
          dropColumns(first.table, dropped).map(table => Cleaned(table, first.diagnostics ++ diagnostics))
        else Right(first)
      }

  private def normalizeClean(clean: Vector[ConfoundClean]): Vector[ConfoundClean] =
    if clean.isEmpty || clean.contains(ConfoundClean.NoCleaning) then Vector.empty
    else clean.distinct

  private def dropColumns(table: BidsTable, dropped: Vector[String]): Either[BidsError, BidsTable] =
    val dropSet = dropped.toSet
    table.select(table.columns.filterNot(dropSet.contains))

  private def applyNaAction(table: BidsTable, action: NaAction): Either[BidsError, BidsTable] =
    action match
      case NaAction.Leave => Right(table)
      case NaAction.Zero | NaAction.Median =>
        val fills =
          table.columns.map { column =>
            val fill =
              action match
                case NaAction.Zero   => 0.0
                case NaAction.Median => median(finiteValues(columnValues(table, column)))
                case NaAction.Leave  => 0.0
            column -> fill
          }.toMap

        val rows =
          table.rows.map { row =>
            row.zip(table.columns).map {
              case (Some(value), _) => Some(value)
              case (None, column)   => Some(formatDouble(fills(column)))
            }
          }
        BidsTable.fromRows(table.columns, rows)

  private def isZeroVariance(column: Vector[Option[String]]): Boolean =
    finiteValues(column).distinct.length <= 1

  private def finiteValues(column: Vector[Option[String]]): Vector[Double] =
    column.flatMap(_.flatMap(parseFiniteDouble))

  private def columnValues(table: BidsTable, column: String): Vector[Option[String]] =
    table.column(column).getOrElse(Vector.empty)

  private def parseFiniteDouble(value: String): Option[Double] =
    value.trim.toDoubleOption.filter(d => !d.isNaN && !d.isInfinity)

  private def median(values: Vector[Double]): Double =
    if values.isEmpty then 0.0
    else
      val sorted = values.sorted
      val mid = sorted.length / 2
      if sorted.length % 2 == 1 then sorted(mid)
      else (sorted(mid - 1) + sorted(mid)) / 2.0

  private def sampleSd(values: Vector[Double]): Double =
    if values.length <= 1 then 0.0
    else
      val mean = values.sum / values.length
      val ss = values.map(value => math.pow(value - mean, 2.0)).sum
      math.sqrt(ss / (values.length - 1))

  private def rankDroppedColumns(table: BidsTable): (Vector[String], Int) =
    if table.columns.length <= 1 then (Vector.empty, table.columns.length)
    else
      val matrix = numericMatrix(table)
      var kept = Vector.empty[Int]
      var rank = 0
      val dropped = Vector.newBuilder[String]

      table.columns.indices.foreach { index =>
        val candidate = kept :+ index
        val candidateRank = matrixRank(selectColumns(matrix, candidate))
        if candidateRank > rank then
          kept = candidate
          rank = candidateRank
        else dropped += table.columns(index)
      }

      (dropped.result(), rank)

  private def numericMatrix(table: BidsTable): Vector[Vector[Double]] =
    val fills = table.columns.map(column => column -> median(finiteValues(columnValues(table, column)))).toMap
    table.rows.map { row =>
      row.zip(table.columns).map { case (cell, column) =>
        cell.flatMap(parseFiniteDouble).getOrElse(fills(column))
      }
    }

  private def appendTables(left: BidsTable, right: BidsTable): Either[BidsError, BidsTable] =
    if left.nrows != right.nrows then
      Left(BidsError.InvalidTable(s"cannot append tables with ${left.nrows} and ${right.nrows} rows"))
    else
      BidsTable.fromRows(
        left.columns ++ right.columns,
        left.rows.zip(right.rows).map { case (lrow, rrow) => lrow ++ rrow }
      )

  private def selectColumns(matrix: Vector[Vector[Double]], columns: Vector[Int]): Vector[Vector[Double]] =
    matrix.map(row => columns.map(row))

  private def matrixRank(matrix: Vector[Vector[Double]], tolerance: Double = 1e-10): Int =
    if matrix.isEmpty || matrix.headOption.forall(_.isEmpty) then 0
    else
      val a = matrix.map(_.toArray).toArray
      val rows = a.length
      val cols = a.head.length
      var rank = 0
      var col = 0

      while rank < rows && col < cols do
        val pivot = (rank until rows).maxBy(row => math.abs(a(row)(col)))
        if math.abs(a(pivot)(col)) <= tolerance then col += 1
        else
          val tmp = a(rank)
          a(rank) = a(pivot)
          a(pivot) = tmp

          val pivotValue = a(rank)(col)
          var j = col
          while j < cols do
            a(rank)(j) = a(rank)(j) / pivotValue
            j += 1

          var i = 0
          while i < rows do
            if i != rank then
              val factor = a(i)(col)
              j = col
              while j < cols do
                a(i)(j) = a(i)(j) - factor * a(rank)(j)
                j += 1
            i += 1

          rank += 1
          col += 1

      rank

  private def distinctPreservingOrder[A](values: Vector[A]): Vector[A] =
    values.foldLeft(Vector.empty[A]) { (acc, value) =>
      if acc.contains(value) then acc else acc :+ value
    }

  private def formatDouble(value: Double): String =
    val raw = f"$value%.12f"
    val stripped = raw.replaceAll("0+$", "").replaceAll("\\.$", "")
    if stripped.isEmpty || stripped == "-0" then "0" else stripped
