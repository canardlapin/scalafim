package scalafim.bids.io

import scalafim.bids.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

class BidsProjectLoaderSuite extends munit.FunSuite:
  private def value[A](result: Either[BidsError, A]): A =
    result.fold(err => fail(err.message), identity)

  private def withBidsFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-bids-")
    try body(root)
    finally deleteRecursive(root)

  private def write(path: Path, text: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, text, StandardCharsets.UTF_8)

  private def touch(path: Path): Unit =
    Files.createDirectories(path.getParent)
    Files.write(path, Array.emptyByteArray)

  private def deleteRecursive(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try stream.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally stream.close()

  private def writeCoreFixture(root: Path): Unit =
    write(
      root.resolve("dataset_description.json"),
      """{"Name":"Fixture","BIDSVersion":"1.10.0","DatasetType":"raw"}"""
    )
    write(root.resolve("participants.tsv"), "participant_id\tage\tgroup\nsub-01\t25\tcontrol\nsub-02\t30\tpatient\n")
    touch(root.resolve("sub-01/anat/sub-01_T1w.nii.gz"))
    touch(root.resolve("sub-01/func/sub-01_task-rest_run-01_bold.nii.gz"))
    write(root.resolve("sub-01/func/sub-01_task-rest_run-01_events.tsv"), "onset duration trial_type\n0 1 go\n")
    write(
      root.resolve("derivatives/fmriprep/dataset_description.json"),
      """{"Name":"fMRIPrep","BIDSVersion":"1.10.0","DatasetType":"derivative"}"""
    )
    touch(root.resolve("derivatives/fmriprep/sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz"))
    write(
      root.resolve("derivatives/fmriprep/sub-01/func/sub-01_task-rest_run-01_desc-confounds_timeseries.tsv"),
      """CSF	WhiteMatter	FramewiseDisplacement	X	Y	Z	RotX	RotY	RotZ	cosine00	cosine01
        |0.1	1.0	n/a	0.0	0.1	0.2	0.01	0.02	0.03	-1	0
        |0.2	1.1	0.2	0.1	0.0	0.3	0.02	0.01	0.04	0	0
        |0.3	1.2	0.4	0.3	-0.1	0.5	0.04	0.03	0.01	1	0
        |""".stripMargin
    )

  test("loader discovers participants, raw files, derivatives, and dataset description"):
    withBidsFixture { root =>
      writeCoreFixture(root)

      val project = value(BidsProjectLoader.load(root))
      assertEquals(project.description.flatMap(_.name), Some("Fixture"))
      assertEquals(project.description.flatMap(_.bidsVersion), Some("1.10.0"))
      assertEquals(project.participants, Vector("01", "02"))
      val participants = project.participantsTable.getOrElse(fail("expected participants table"))
      assertEquals(participants.columns, Vector("participant_id", "age", "group"))
      assertEquals(value(participants.column("group")), Vector(Some("control"), Some("patient")))
      assertEquals(value(BidsProjectLoader.readParticipantsTable(project)).map(_.columns), Some(Vector("participant_id", "age", "group")))
      assertEquals(value(BidsProjectLoader.readTable(project, BidsPath("participants.tsv"))).nrows, 2)
      assertEquals(project.derivatives.map(_.pipeline.value), Vector("fmriprep"))

      val rawBold =
        project.paths(BidsQuery(filename = Vector("bold\\.nii\\.gz$"), scope = BidsScope.Raw))
      assertEquals(rawBold.map(_.value), Vector("sub-01/func/sub-01_task-rest_run-01_bold.nii.gz"))
      assertEquals(project.funcScans(task = "rest").map(_.path.value), Vector("sub-01/func/sub-01_task-rest_run-01_bold.nii.gz"))
      assertEquals(project.anatScans().map(_.path.value), Vector("sub-01/anat/sub-01_T1w.nii.gz"))

      val derivativeBold =
        project.paths(
          BidsQuery(
            filename = Vector("bold\\.nii\\.gz$"),
            scope = BidsScope.Derivatives,
            pipeline = Some(PipelineName("fmriprep"))
          )
        )
      assertEquals(
        derivativeBold.map(_.value),
        Vector("derivatives/fmriprep/sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz")
      )
      assertEquals(
        project.preprocScans(space = "MNI").map(_.path.value),
        Vector("derivatives/fmriprep/sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz")
      )
    }

  test("loader preserves strict and lax behavior for missing participants.tsv"):
    withBidsFixture { root =>
      write(
        root.resolve("dataset_description.json"),
        """{"Name":"Fixture","BIDSVersion":"1.10.0","DatasetType":"raw"}"""
      )
      touch(root.resolve("sub-01/anat/sub-01_T1w.nii.gz"))

      assert(BidsProjectLoader.load(root).isLeft)

      val lax = value(BidsProjectLoader.load(root, BidsLoadConfig(strictParticipants = false)))
      assertEquals(lax.participants, Vector("01"))
      assertEquals(lax.participantsTable, None)
      assertEquals(value(BidsProjectLoader.readParticipantsTable(lax)), None)
    }

  test("loader rejects participants.tsv without participant_id"):
    withBidsFixture { root =>
      write(
        root.resolve("dataset_description.json"),
        """{"Name":"Fixture","BIDSVersion":"1.10.0","DatasetType":"raw"}"""
      )
      write(root.resolve("participants.tsv"), "age\tgroup\n25\tcontrol\n")
      assert(BidsProjectLoader.load(root).isLeft)
    }

  test("generic table reader rejects paths that escape the project root"):
    withBidsFixture { root =>
      writeCoreFixture(root)

      val project = value(BidsProjectLoader.load(root))
      assert(BidsProjectLoader.readTable(project, BidsPath("../outside.tsv")).isLeft)
      assert(BidsProjectLoader.readTable(project, BidsPath("/tmp/outside.tsv")).isLeft)
    }

  test("metadata inheritance merges root, datatype, and file sidecars"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      write(
        root.resolve("task-rest_bold.json"),
        """{"TaskName":"Rest","RepetitionTime":1.5,"Nested":{"A":1}}"""
      )
      write(root.resolve("sub-01/func/task-rest_bold.json"), """{"SliceTiming":[0,0.5],"Nested":{"B":2}}""")
      write(root.resolve("sub-01/func/sub-01_task-rest_run-01_bold.json"), """{"RepetitionTime":2.0}""")

      val project = value(BidsProjectLoader.load(root))
      val meta = value(project.metadata(BidsPath("sub-01/func/sub-01_task-rest_run-01_bold.nii.gz")))
      val records = value(project.funcScanMetadata(subid = "01", task = "rest", run = "01"))
      val tr = value(project.inferRepetitionTime(subid = "01", task = "rest", run = "01"))

      assertEquals(meta.fields("TaskName").asString, Some("Rest"))
      assertEquals(meta.fields("RepetitionTime").asNumber, Some(2.0))
      assert(meta.fields.contains("SliceTiming"))
      val nested = meta.fields("Nested").asObject.getOrElse(fail("Nested metadata should be an object"))
      assertEquals(nested("A").asNumber, Some(1.0))
      assertEquals(nested("B").asNumber, Some(2.0))
      assertEquals(records.map(_.path.value), Vector("sub-01/func/sub-01_task-rest_run-01_bold.nii.gz"))
      assertEquals(records.head.number("RepetitionTime"), Some(2.0))
      assertEquals(tr, Some(2.0))
    }

  test("loader reads event tables with whitespace sniffing"):
    withBidsFixture { root =>
      writeCoreFixture(root)
      write(
        root.resolve("sub-02/func/sub-02_task-rest_run-01_events.tsv"),
        "onset duration trial_type\n0 1 go\n2 n/a stop\n"
      )

      val project = value(BidsProjectLoader.load(root))
      val tables = value(BidsProjectLoader.readEventTables(project))
      val byPath = tables.toMap
      val table = byPath(BidsPath("sub-02/func/sub-02_task-rest_run-01_events.tsv"))
      val tableFiles = value(BidsProjectLoader.readEventTableFiles(project))
      val tableFile = tableFiles.find(_.path == BidsPath("sub-02/func/sub-02_task-rest_run-01_events.tsv")).getOrElse(fail("missing event table file"))

      assertEquals(table.columns, Vector("onset", "duration", "trial_type"))
      assertEquals(value(table.column("duration")), Vector(Some("1"), None))
      assertEquals(tableFile.subject, Some("02"))
      assertEquals(tableFile.task, Some("rest"))
      assertEquals(tableFile.run, Some("01"))
      assertEquals(tableFile.context.scope, BidsScope.Raw)
    }

  test("loader discovers fMRIPrep confounds and applies shared selection policy"):
    withBidsFixture { root =>
      writeCoreFixture(root)

      val project = value(BidsProjectLoader.load(root))
      val files = project.confoundFiles(subid = "01", task = "rest", run = "01")
      assertEquals(
        files.map(_.path.value),
        Vector("derivatives/fmriprep/sub-01/func/sub-01_task-rest_run-01_desc-confounds_timeseries.tsv")
      )

      val tables = value(BidsProjectLoader.readConfoundTables(project, subid = "01", task = "rest", run = "01"))
      val tableFiles = value(BidsProjectLoader.readConfoundTableFiles(project, subid = "01", task = "rest", run = "01"))
      assertEquals(tableFiles.head.subject, Some("01"))
      assertEquals(tableFiles.head.task, Some("rest"))
      assertEquals(tableFiles.head.run, Some("01"))
      assertEquals(tableFiles.head.context.pipeline, Some(PipelineName("fmriprep")))
      assertEquals(tableFiles.head.context.kind, Some("timeseries"))

      val motion6 =
        value(
          BidsProjectLoader.readConfounds(
            project,
            cvars = ConfoundSets.named("motion6"),
            subid = "01",
            task = "rest",
            run = "01"
          )
        )
      assertEquals(
        motion6.map(_.path.value),
        Vector("derivatives/fmriprep/sub-01/func/sub-01_task-rest_run-01_desc-confounds_timeseries.tsv")
      )
      assertEquals(motion6.head.subject, Some("01"))
      assertEquals(motion6.head.table.columns, Vector("X", "Y", "Z", "RotX", "RotY", "RotZ"))
      assertEquals(motion6.head.requested, ConfoundSets.motion6)
      assertEquals(motion6.head.resolved, Vector("X", "Y", "Z", "RotX", "RotY", "RotZ"))

      val fd =
        value(
          BidsProjectLoader.readConfounds(
            project,
            cvars = ConfoundSets.named("fd"),
            naAction = NaAction.Zero,
            subid = "01",
            task = "rest",
            run = "01"
          )
        )
      assertEquals(value(fd.head.table.column("FramewiseDisplacement")).head, Some("0"))

      val selection =
        value(
          ConfoundSelector.select(
            tables.head._2,
            ConfoundSelectionConfig(
              variables = Vector("csf", "white_matter", "framewise_displacement", "cosine*"),
              naAction = NaAction.Zero
            )
          )
        )

      assertEquals(selection.table.columns, Vector("CSF", "WhiteMatter", "FramewiseDisplacement", "cosine00"))
      assertEquals(value(selection.table.column("FramewiseDisplacement")).head, Some("0"))
      assertEquals(selection.diagnostics.map(_.column), Vector("cosine01"))

      val strategySelection =
        value(
          BidsProjectLoader
            .readConfoundStrategy(
              project,
              ConfoundStrategy.named("pcabasic80"),
              subid = "01",
              task = "rest",
              run = "01"
            )
            .map(_.head.selection)
        )
      val componentNames = strategySelection.pca.map(_.componentNames).getOrElse(fail("expected PCA metadata"))
      assert(componentNames.nonEmpty)
      assertEquals(strategySelection.table.columns, componentNames :+ "cosine00")
      assertEquals(
        strategySelection.pca.map(_.sourceColumns),
        Some(Vector("X", "Y", "Z", "RotX", "RotY", "RotZ", "CSF", "WhiteMatter"))
      )
    }
