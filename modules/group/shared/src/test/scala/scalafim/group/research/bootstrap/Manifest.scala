package scalafim.group.research.bootstrap

/** Minimal canonical JSON: object keys keep their declared order, no whitespace,
  * numbers through `Canonical.number`. The canonical text is what gets hashed.
  */
enum Json:
  case Str(value: String)
  case Num(value: Double)
  case Integer(value: Long)
  case Bool(value: Boolean)
  case Null
  case Arr(items: Vector[Json])
  case Obj(fields: Vector[(String, Json)])

  def render: String =
    val out = new StringBuilder
    write(out)
    out.toString

  private def write(out: StringBuilder): Unit = this match
    case Str(s) => Json.quote(s, out)
    case Num(d) => out.append(Canonical.number(d))
    case Integer(i) => out.append(i.toString)
    case Bool(b) => out.append(if b then "true" else "false")
    case Null => out.append("null")
    case Arr(items) =>
      out.append('[')
      items.zipWithIndex.foreach { (item, k) =>
        if k > 0 then out.append(',')
        item.write(out)
      }
      out.append(']')
    case Obj(fields) =>
      out.append('{')
      fields.zipWithIndex.foreach { case ((key, value), k) =>
        if k > 0 then out.append(',')
        Json.quote(key, out)
        out.append(':')
        value.write(out)
      }
      out.append('}')

object Json:
  def obj(fields: (String, Json)*): Json = Obj(fields.toVector)
  def nums(values: Array[Double]): Json = Arr(values.toVector.map(Num(_)))
  def strs(values: Seq[String]): Json = Arr(values.toVector.map(Str(_)))

  private def quote(s: String, out: StringBuilder): Unit =
    out.append('"')
    s.foreach {
      case '"' => out.append("\\\"")
      case '\\' => out.append("\\\\")
      case c if c < ' ' || c > '~' => throw new IllegalArgumentException(s"manifest strings are printable ASCII, got code ${c.toInt}")
      case c => out.append(c)
    }
    out.append('"')

/** Declaration §5 cell manifest: every design matrix, sigma vector, contrast and
  * stream key, plus the frozen budgets and decision constants, in one canonical
  * serialization with a SHA-256. The committed copy lives at
  * tools/group-bootstrap-research/cells.json.
  */
object CellManifest:
  val Schema = "scalafim-group-bootstrap-cells/v1"
  val DeclarationSha256 = "0da17f37f48724110dd1fced88639eefa9fa2d7b5475622386c8f41602ee04c5"
  val Mote = "bd-01M21BNZR9ZBRAYY9JD5WCQ8KX"

  /** The six confirmation cells fixed now (§6), in declaration order; power uses 2, 3, 5, 6. */
  val FixedConfirmation: Vector[CellId] = Vector(
    "C-n80-DG-Vrev-T0-N8", "C-n8-DG-Vrev-T2-N8", "C-n20-DI-Vspread-T0-N8",
    "C-n8-DI-Vflat-T0-Ninf", "C-n20-DG-Vspread-T2-N40", "C-n80-DI-Vspread-T2-N8"
  ).map(CellId.unsafe)

  val PowerCells: Vector[CellId] = Vector(1, 2, 4, 5).map(FixedConfirmation)

  private def streams(phase: Phase): Json =
    Json.Obj(StreamKind.values.toVector.map(s => s.toString -> Json.Integer(phase.root(s))))

  private def nuJson(nu: NuLevel): Json = nu match
    case NuLevel.Infinite => Json.Str("inf")
    case NuLevel.Finite(v) => Json.Integer(v.toLong)

  def cellJson(cell: Cell): Json =
    val sigma = cell.sigma2 match
      case Some(values) => "sigma2" -> Json.nums(values)
      case None => "sigma2_law" -> Json.obj(
          "law" -> Json.Str("scaled-inv-chi2"), "df" -> Json.Integer(10), "scale" -> Json.Num(0.52),
          "draw" -> Json.Str("sigma2_i = 10 * 0.52 / chi2_10, per study, stream FirstLevel lane HierarchySigma")
        )
    val stressJson = cell.stress match
      case None => Json.Null
      case Some(kind) =>
        val detail = kind match
          case StressKind.Leverage => "z of subject 1 increased by 5"
          case StressKind.DominantPrecision => "subject 1 precision 10 (sigma2 .1); others precision .04 (sigma2 25)"
          case StressKind.LowDf => s"true and declared nu = ${cell.trueNu.code}"
          case StressKind.DeclaredDf4 => "true nu 8, declared 4"
          case StressKind.DeclaredDf16 => "true nu 8, declared 16"
          case StressKind.StudentT3 => "u and e are t3 scaled to variances tau2 and sigma2"
          case StressKind.Lognormal => "u and e are standardized lognormal(0,1) scaled to variances tau2 and sigma2"
          case StressKind.Autoregressive =>
            "first-level AR(1) rho .4 series, length 10, intercept + block regressor, OLS slope and naive variance with nominal df 8, scaled to sigma2"
        Json.obj("kind" -> Json.Str(kind.code), "detail" -> Json.Str(detail))
    Json.obj(
      "id" -> Json.Str(cell.id.value),
      "family" -> Json.Str(cell.family.toString),
      "stress" -> stressJson,
      "n" -> Json.Integer(cell.n.toLong),
      "p" -> Json.Integer(cell.p.toLong),
      "terms" -> Json.strs(cell.terms),
      "design_row_major" -> Json.nums(cell.designRowMajor),
      "contrast" -> Json.nums(cell.contrast),
      "b0" -> Json.Num(0.0),
      "tau2" -> Json.Num(cell.tau2.value),
      "nu_true" -> nuJson(cell.trueNu),
      "nu_declared" -> nuJson(cell.declaredNu),
      "df_admissible" -> Json.Bool(DfAdmission.floorAdmits(cell.declaredNu)),
      "error_law" -> Json.Str(cell.errorLaw.toString),
      sigma,
      "power_delta" -> cell.powerDelta.fold(Json.Null)(Json.Num(_)),
      "streams" -> Json.obj("pilot" -> streams(Phase.Pilot), "confirmation" -> streams(Phase.Confirmation))
    )

  def json: Json =
    Json.obj(
      "schema" -> Json.Str(Schema),
      "mote" -> Json.Str(Mote),
      "declaration_sha256" -> Json.Str(DeclarationSha256),
      "generator" -> Json.Str("modules/group/shared/src/test/scala/scalafim/group/research/bootstrap/Manifest.scala"),
      "rng" -> Json.obj(
        "generator" -> Json.Str("SplitMix64 (increment 0x9e3779b97f4a7c15, Stafford mix13)"),
        "stream_key" -> Json.Str("(root, cell id, study, stream); lane seed = mix(mix(mix(mix(mix(root) ^ fnv1a64(id)) + G*(study+1)) ^ (stream << 32)) + G*lane)"),
        "power_studies" -> Json.Str("every stream component of a power study uses the Power root"),
        "normal" -> Json.Str("Marsaglia polar"),
        "gamma" -> Json.Str("Marsaglia-Tsang; chi2_df = 2 Gamma(df/2)")
      ),
      "budgets" -> Json.obj(
        "pilot" -> Json.obj("studies" -> Json.Integer(2000), "draws" -> Json.Integer(499)),
        "confirmation" -> Json.obj("studies" -> Json.Integer(20000), "draws" -> Json.Integer(999), "cells" -> Json.Integer(12)),
        "sign_flip" -> Json.obj("enumerate_n" -> Json.Integer(8), "monte_carlo_draws" -> Json.Integer(999))
      ),
      "decision" -> Json.obj(
        "alpha" -> Json.Num(0.05),
        "margin" -> Json.Num(Decision.Margin),
        "tail_delta" -> Json.Num(Decision.Delta),
        "null_pass_max_k" -> Json.Integer(Decision.NullPassMax.toLong),
        "null_fail_min_k" -> Json.Integer(Decision.NullFailMin.toLong),
        "failure_pass_max_f" -> Json.Integer(Decision.FailurePassMax.toLong),
        "gain_margin" -> Json.Num(0.0),
        "non_loss_margin" -> Json.Num(0.02),
        "bound_subfamilies" -> Json.strs(Vector("n >= 20", "nu >= 40"))
      ),
      "candidates" -> Json.strs(Scheme.values.toVector.filter(_.role == SchemeRole.Candidate).map(_.code)),
      "controls" -> Json.Arr(Scheme.values.toVector.filter(_.role != SchemeRole.Candidate).map { s =>
        Json.obj("code" -> Json.Str(s.code), "role" -> Json.Str(s.role.toString), "prediction" -> Json.Str(s.prediction))
      }),
      "baselines" -> Json.strs(Vector("native PM/mKH t(n-p)", "equal-weight OLS + HC3 t(n-p)", "inverse-v WLS + HC3 t(n-p)", "oracle WLS z (non-feasible, diagnostic)")),
      "confirmation_fixed" -> Json.strs(FixedConfirmation.map(_.value)),
      "power_cells" -> Json.strs(PowerCells.map(_.value)),
      "cells" -> Json.Arr(Cell.all.map(cellJson))
    )

  lazy val canonical: String = json.render + "\n"

  lazy val sha256: String = Sha256.hex(canonical)
