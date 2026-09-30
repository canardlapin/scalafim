package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

/** Writes the explicit inputs of the independent R reference and the committed
  * cell manifest. Off by default; run with
  * -Dscalafim.group.bootstrapResearch.writeInputs=true, then run
  * tools/group-bootstrap-research/generate_reference_fixtures.R.
  */
class ReferenceInputWriter extends munit.FunSuite:
  import ReferenceInputWriter.*

  test("write the R reference inputs and tools/group-bootstrap-research/cells.json (opt-in)"):
    assume(sys.props.get("scalafim.group.bootstrapResearch.writeInputs").contains("true"), "pass -Dscalafim.group.bootstrapResearch.writeInputs=true to write inputs")
    val root = Paths.get(sys.props.getOrElse("user.dir", "."))
    val tools = root.resolve("tools/group-bootstrap-research")
    Files.createDirectories(tools.resolve("inputs"))
    write(tools.resolve("cells.json"), CellManifest.canonical)
    write(tools.resolve("cells.json.sha256"), s"${CellManifest.sha256}  cells.json\n")
    write(tools.resolve("inputs/bootstrap-cases.json"), casesJson)
    println(s"WROTE_INPUTS,${tools.resolve("inputs/bootstrap-cases.json")},sha256=${Sha256.hex(casesJson)}")

object ReferenceInputWriter:
  /** Fixture studies: harness-phase draws (never pilot or confirmation roots). */
  val FixtureStudies: Vector[(String, Int, Double)] = Vector(
    ("C-n8-DI-Vspread-T2-N8", 0, 0.0),
    ("C-n20-DG-Vrev-T0-N8", 0, 0.0),
    ("C-n20-DG-Vspread-T2-N40", 0, 0.0),
    ("C-n8-DG-Vflat-T2-N8", 1, 0.25)
  )
  val Draws = 39

  private def write(path: Path, text: String): Unit =
    val _ = Files.write(path, text.getBytes(StandardCharsets.UTF_8))

  private def num(d: Double): String =
    require(d.isFinite, s"inputs must be finite, got $d")
    java.lang.Double.toString(d)

  private def arr(values: Array[Double]): String = values.map(num).mkString("[", ",", "]")

  private def nuValue(d: Double): String = if d.isInfinite then "\"inf\"" else num(d)

  def casesJson: String =
    val cases = FixtureStudies.map { (id, index, b0) =>
      val c = ResearchTestSupport.cell(id)
      val sim0 = ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, index, c.researchDesign)
      val sim = sim0.copy(data = sim0.data.copy(b0 = b0))
      val engine = new BootstrapEngine(sim.data.design)
      val v = ResearchTestSupport.variates(sim, engine, Scheme.EmpiricalBayes, Draws)
      val eb = engine.hyperparameters(sim.data).toOption.flatten
      val post = if v.post.exists(_.isNaN) then "[]" else arr(v.post)
      s"""{"id":"$id-s$index","cell":"$id","n":${c.n},"p":${c.p},"terms":${c.terms.map(t => s"\"$t\"").mkString("[", ",", "]")},""" +
        s""""design_row_major":${arr(c.designRowMajor)},"contrast":${arr(c.contrast)},"b0":${num(b0)},""" +
        s""""y":${arr(sim.data.y)},"v":${arr(sim.data.v)},"nu":[${sim.data.nu.map(nuValue).mkString(",")}],"draws":$Draws,""" +
        s""""zu":${arr(v.zu)},"ze":${arr(v.ze)},"chi":${arr(v.chi)},"post":$post,""" +
        s""""scala_d0":${eb.fold("null")(f => if f.d0.isInfinite then "\"inf\"" else num(f.d0))},"scala_s0_squared":${eb.fold("null")(f => num(f.s0Squared))}}"""
    }
    s"""{"schema":"scalafim-group-bootstrap-reference-inputs/v1","source":"ReferenceInputWriter (Phase.Harness roots ${Phase.Harness.base + 1}-${Phase.Harness.base + 5})","cases":[${cases.mkString(",")}]}""" + "\n"
