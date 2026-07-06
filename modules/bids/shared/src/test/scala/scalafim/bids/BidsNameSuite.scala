package scalafim.bids

class BidsNameSuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("parse and render raw functional names"):
    val name = value(BidsName.parse("sub-01_task-rest_run-01_bold.nii.gz"))

    assertEquals(name.datatype, Some("func"))
    assertEquals(name.entities(EntityKey.Subject), "01")
    assertEquals(name.entities(EntityKey.Task), "rest")
    assertEquals(name.entities(EntityKey.Run), "01")
    assertEquals(name.kind, "bold")
    assertEquals(name.extension, "nii.gz")
    assertEquals(name.fileName, "sub-01_task-rest_run-01_bold.nii.gz")

  test("parse and render session-bearing events files"):
    val name = value(BidsName.parse("sub-01_ses-pre_task-nback_run-02_events.tsv"))

    assertEquals(name.datatype, Some("func"))
    assertEquals(name.entities(EntityKey.Session), "pre")
    assertEquals(name.kind, "events")
    assertEquals(name.fileName, "sub-01_ses-pre_task-nback_run-02_events.tsv")

  test("derivative entities select fMRIPrep datatype instead of raw func"):
    val name = value(BidsName.parse("sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz"))

    assertEquals(name.datatype, Some("funcprep"))
    assertEquals(name.entities(EntityKey.Space), "MNI152NLin2009cAsym")
    assertEquals(name.entities(EntityKey.Description), "preproc")

  test("decode-style entity update preserves BIDS order"):
    val name = value(BidsName.parse("sub-01_task-rest_run-01_bold.nii.gz"))
    val updated = value(name.withEntity(EntityKey.Description, "smooth6mm"))

    assertEquals(updated.fileName, "sub-01_task-rest_run-01_desc-smooth6mm_bold.nii.gz")

  test("invalid raw names are rejected by datatype specs"):
    assert(BidsSpecs.Func.validate(value(BidsName.parseGeneric("sub-01_task-rest_space-MNI_bold.nii.gz"))).isLeft)
    assert(BidsName.parse("sub-01_task-rest_run-a_bold.nii.gz").isLeft)

  test("path-aware parsing validates datatype folder and exposes typed role"):
    val name = value(BidsRegistry.Builtin.parsePath(BidsPath("sub-01/func/sub-01_task-rest_bold.nii.gz")))
    val role = value(BidsSpecs.Func.roleFor(name))

    assertEquals(role.datatypeName, "func")
    assertEquals(role.folderName, "func")
    assertEquals(role.suffixName, "bold")
    assertEquals(role.formatName, "nii.gz")
    assert(BidsRegistry.Builtin.parsePath(BidsPath("sub-01/anat/sub-01_task-rest_bold.nii.gz")).isLeft)

    val manifest = BidsManifest.fromRelativePaths(Vector("sub-01/anat/sub-01_task-rest_bold.nii.gz"))
    assertEquals(manifest.files.head.parsed, None)
    assertEquals(
      manifest.paths(
        BidsQuery(filename = Vector("bold\\.nii\\.gz$"), filters = Vector(EntityFilter(EntityKey.Task, "rest")))
      ),
      Vector.empty
    )

  test("name constructor has a total validation path"):
    assert(BidsName.from(BidsEntities.Empty, "", "tsv").isLeft)
    assert(BidsName.from(BidsEntities.Empty, "events", "").isLeft)
    assert(BidsKind.from("", Vector("tsv")).isLeft)
    assert(BidsKind.from("events", Vector.empty).isLeft)
    assert(BidsPath.from(" ").isLeft)
    assert(PipelineName.from(" ").isLeft)
    intercept[IllegalArgumentException](BidsPath(" "))
    intercept[IllegalArgumentException](PipelineName(" "))
