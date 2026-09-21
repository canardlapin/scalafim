package scalafim.fmri.workflow

import bids4s.*
import scalafim.dataset.{DatasetId,DatasetShape}
import scalafim.image.NeuroSpace

class LinkedBidsStudyCompilerSuite extends munit.FunSuite:
  private val raw = BidsRoot.raw("raw","/study/source").toOption.get
  private val first = BidsRoot.derivative("first","/study/external",PipelineName("custom"),raw.alias).toOption.get
  private val second = BidsRoot.derivative("second","/study/other",PipelineName("custom"),raw.alias).toOption.get
  private val prefix = "sub-01/func/sub-01_task-demo_run-01"
  private val rawBold = prefix + "_bold.nii"
  private val bold = prefix + "_space-MNI_desc-preproc_bold.nii"
  private val mask = prefix + "_space-MNI_desc-brain_mask.nii"
  private val confound = prefix + "_desc-confounds_timeseries.tsv"
  private val events = prefix + "_events.tsv"
  private val shape = DatasetShape.unsafe(NeuroSpace(Vector(2,2,1)),3)
  private def project(root: BidsRoot, paths: Vector[String], sidecars: Map[BidsPath,JsonValue.Obj] = Map.empty): BidsProject =
    BidsProject(root=root.location,description=None,participants=Vector("01"),derivatives=Vector.empty,
      manifest=BidsManifest.fromRootPathsChecked(paths,root.kind).value,sidecars=sidecars)
  private def fixture(firstOmit: Set[String] = Set.empty, omitEvents: Boolean = false): LinkedBidsProject =
    val rawJson = rawBold.stripSuffix(".nii") + ".json"
    val rawProject = project(raw,Vector(rawBold,rawJson) ++ (if omitEvents then Vector.empty else Vector(events)),
      Map(BidsPath(rawJson) -> BidsJson.parseObject("{\"RepetitionTime\":2.0}").toOption.get))
    val projects = Vector(raw -> rawProject,first -> project(first,Vector(bold,mask,confound,events).filterNot(firstOmit)),
      second -> project(second,Vector(bold,mask,confound)))
    val files = projects.flatMap((root,p) => p.manifest.files.map(file =>
      LinkedBidsFile(root,file,BidsPath(root.location.value + "/" + file.path.value))))
    LinkedBidsProject.make(projects.map(_._1),files,projects.map((r,p) => r.alias -> p).toMap).toOption.get
  private def recipe(root: BidsRoot = first): LinkedDatasetRecipe =
    LinkedDatasetRecipe.unsafe(DatasetId("linked"),root.alias,
      BidsQuery.unsafe(filename=Vector("bold\\.nii$"),scope=BidsScope.All),
      maskPolicy=MaskPolicy.IntersectRunMasks,confounds=Some(ConfoundSelectionConfig(variables=Vector("trans_x"))))
  private val headers = LinkedImageHeaderCatalog(Vector(first,second).flatMap(root => Vector(
    LinkedBidsFileKey(root.alias,BidsPath(bold)) -> ImageHeaderDescriptor(shape),
    LinkedBidsFileKey(root.alias,BidsPath(mask)) -> ImageHeaderDescriptor(DatasetShape.unsafe(shape.space,1)))).toMap)
  private def compile(project: LinkedBidsProject, selected: BidsRoot = first): StudyCatalog =
    BidsStudyCompiler.compile(project,recipe(selected),headers).fold(r => fail(r.issues.mkString("; ")),identity)

  test("broad query selects only chosen derivative and raw-source TR and events") {
    val catalog = compile(fixture())
    assertEquals(catalog.units.size,1)
    val unit = catalog.units.head
    assert(unit.id.value.contains("first"))
    assertEquals(unit.runs.size,1)
    assertEqualsDouble(unit.runs.head.repetitionTime.seconds,2.0,0.0)
    assertEquals(unit.runs.head.bold.location.value,"file:///study/external/"+bold)
    assertEquals(unit.runs.head.events.location.value,"file:///study/source/"+events)
    assertEquals(unit.runs.head.confounds.get.location.value,"file:///study/external/"+confound)
    assert(unit.mask.artifacts.forall(_.location.value == "file:///study/external/"+mask))
  }
  test("same producer and entities in another root retain distinct unit and artifact identity") {
    val linked = fixture()
    val a = compile(linked)
    val b = compile(linked,second)
    assertNotEquals(a.units.head.id,b.units.head.id)
    assertNotEquals(a.units.head.runs.head.bold,b.units.head.runs.head.bold)
    val reversed = LinkedBidsProject.make(linked.roots.reverse,linked.files.reverse,linked.projects).toOption.get
    assertEquals(compile(reversed),a)
  }
  test("missing selected-root companions cannot fall back to competing producer or derivative events") {
    for (omitted,code) <- Vector(mask -> CatalogIssueCode.MissingMask,confound -> CatalogIssueCode.MissingConfounds) do
      val failure = BidsStudyCompiler.compile(fixture(Set(omitted)),recipe(),headers).left.toOption.get
      assert(failure.errors.exists(_.code == code))
    val noEvents = BidsStudyCompiler.compile(fixture(omitEvents=true),recipe(),headers).left.toOption.get
    assert(noEvents.errors.exists(_.code == CatalogIssueCode.MissingEvents))
    assert(LinkedBidsProject.make(Vector(first),Vector.empty).isLeft)
  }
  test("selected raw root retains events and explicit mask without derivative linkage") {
    val linked = fixture()
    val selected = LinkedDatasetRecipe.unsafe(DatasetId("raw"),raw.alias,BidsQuery.unsafe(filename=Vector("bold\\.nii$")),
      maskPolicy=MaskPolicy.Explicit(WorkflowArtifactRef.unsafe[MaskImageResource]("file:///explicit-mask.nii")))
    val actual = BidsStudyCompiler.compile(linked,selected,LinkedImageHeaderCatalog(Map(
      LinkedBidsFileKey(raw.alias,BidsPath(rawBold)) -> ImageHeaderDescriptor(shape)))).fold(r => fail(r.issues.mkString("; ")),identity)
    assertEquals(actual.units.head.mask.artifacts.head.location.value,"file:///explicit-mask.nii")
    assertEquals(actual.units.head.runs.head.events.location.value,"file:///study/source/"+events)
  }
