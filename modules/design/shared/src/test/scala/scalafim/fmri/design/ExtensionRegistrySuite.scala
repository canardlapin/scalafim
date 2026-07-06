package scalafim.fmri.design

import scalafim.fmri.design.basis.{BasisModulation, BasisRegistry, ParametricBasis}
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.{DesignColmap, ModulationType}
import scalafim.fmri.design.event.{Event, EventModel, EventTerm}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.design.hrf.{ExternalHrfSpecRegistry, HrfBasisEntry, HrfBasisRegistry}
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

class ExtensionRegistrySuite extends munit.FunSuite:

  final case class DemoBasis(x: Vector[Double]) extends ParametricBasis:
    val argName: String = "rt"
    val name: String = "demo_rt"
    val basisClass: String = "DemoBasisXYZ"
    val y: Mat = Mat.unsafe(x.length, 1, x.toArray)
    val columns: Vector[String] = Vector("demo_rt")

    def subset(mask: Vector[Boolean]): ParametricBasis =
      DemoBasis(x.zip(mask).collect { case (v, true) => v })

  test("BasisRegistry exposes built-in basis metadata") {
    val registry = BasisRegistry.default
    val builtins = registry.listRegisteredBases()

    assert(Set("Poly", "BSpline", "Scale", "Standardized", "ScaleWithin", "RobustScale", "Ident").subsetOf(builtins.toSet))
    assertEquals(registry.getBasisEntry("Poly").flatMap(_.prefix), Some("poly"))
    assertEquals(registry.getBasisEntry("BSpline").flatMap(_.prefix), Some("bs"))
    assertEquals(registry.getBasisEntry("Scale").flatMap(_.prefix), Some("z"))
    assertEquals(registry.getBasisEntry("Ident").flatMap(_.prefix), None)
    assert(registry.parametricPrefixes.contains("poly_"))
    assert(registry.parametricPrefixes.contains("z_"))
  }

  test("custom basis metadata is reflected in designColmap") {
    val registry = BasisRegistry.default.registerBasis(
      className = "DemoBasisXYZ",
      prefix = Some("demo"),
      modulation = BasisModulation.Parametric,
      description = Some("test-only demo basis"),
      formulaNames = Seq("demo")
    )

    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))
    val term = EventTerm(
      events = Vector(Event.basis(DemoBasis(Vector(1.0, 2.0)))),
      onsets = Vector(0.0.s, 8.0.s),
      blockIds = Vector(0, 0),
      termTag = Some("demo_rt")
    )
    val model = EventModel.build(Seq(term.convolve(Hrfs.SPMG2, sf)), sf)
    val meta = DesignColmap.forEventModel(model, basisRegistry = registry)

    assertEquals(meta.map(_.modulationType).distinct, Vector(Some(ModulationType.Parametric)))
    assertEquals(meta.map(_.modulationId).distinct, Vector(Some("rt")))
    assertEquals(meta.map(_.basisLabel), Vector(Some("canonical"), Some("derivative")))
    assertEquals(meta.map(_.prettyName), Vector("rt_canonical", "rt_derivative"))
  }

  test("formula term tags use basis registry prefixes") {
    val registry = BasisRegistry.default.registerBasis(
      className = "Scale",
      prefix = Some("scaled"),
      modulation = BasisModulation.Parametric,
      description = Some("alternate Scale prefix"),
      formulaNames = Seq("scale")
    )
    val data = DataTable(
      nrows = 3,
      columns = Vector(
        "onset" -> Column.Doubles(Vector(0.0, 5.0, 10.0)),
        "rt" -> Column.Doubles(Vector(0.4, 0.5, 0.6))
      )
    )
    val sf = SamplingFrame(blockLens = Seq(20), tr = Seq(1.0))

    val model = EventModelBuilder.build(
      formula = "onset ~ hrf(Scale(rt))",
      data = data,
      samplingFrame = sf,
      blockIds = Seq(0, 0, 0),
      basisRegistry = registry
    )

    assertEquals(model.termKeys, Vector("scaled_rt"))
  }

  test("HrfBasisRegistry labels built-in and custom HRF bases") {
    val registry = HrfBasisRegistry.default.register(
      HrfBasisEntry(
        name = "demo_hrf",
        aliases = Vector("demo_hrf"),
        labels = Vector("early", "late")
      )
    )

    assertEquals(registry.labelForName("SPMG3", 1), "canonical")
    assertEquals(registry.labelForName("SPMG3", 3), "dispersion")
    assertEquals(registry.labelForName("fir", 2), "lag_01")
    assertEquals(registry.labelForName("demo_hrf", 2), "late")
    assertEquals(registry.labelForName("unregistered", 2), "component_02")
  }

  test("ExternalHrfSpecRegistry supports Scala-native extension introspection") {
    val registry = ExternalHrfSpecRegistry.empty
      .registerHrfSpecExtension(
        specClass = "test_hrfspec",
        packageName = "testPkg",
        convolvedClass = Some("test_convolved"),
        requiresExternalProcessing = true,
        formulaFunctions = Seq("test_hrf", "test_trialwise")
      )
      .registerHrfSpecExtension(
        specClass = "afni_hrfspec",
        packageName = "afnireg"
      )

    val info = registry.getExternalHrfSpecInfo("test_hrfspec").get
    assertEquals(info.specClass, "test_hrfspec")
    assertEquals(info.packageName, "testPkg")
    assertEquals(info.convolvedClass, Some("test_convolved"))
    assertEquals(info.requiresExternalProcessing, true)
    assertEquals(info.formulaFunctions, Vector("test_hrf", "test_trialwise"))

    assert(registry.isExternalHrfSpec(Seq("other", "test_hrfspec")))
    assert(registry.requiresExternalProcessing("test_hrfspec"))
    assertEquals(registry.getExternalHrfSpecFunctions("test_hrfspec"), Vector("test_hrf", "test_trialwise"))
    assertEquals(registry.getExternalHrfSpecFunctions("afni_hrfspec"), Vector("afni_hrf"))
    assert(registry.getAllExternalHrfFunctions().contains("test_hrf"))
  }
