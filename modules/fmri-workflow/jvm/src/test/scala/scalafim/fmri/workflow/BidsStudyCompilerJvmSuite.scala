package scalafim.fmri.workflow

import munit.FunSuite
import bids4s.*
import bids4s.io.BidsProjectLoader
import scalafim.dataset.DatasetId
import scalafim.image.{Axis, PrimitiveBuffers, NeuroSpace, NeuroVec, NeuroVol}
import scalafim.image.io.Nifti

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scala.jdk.CollectionConverters.*

class BidsStudyCompilerJvmSuite extends FunSuite:
  test("JVM compiler reads headers only for a multi-subject multi-run fMRIPrep project") {
    withFixture { root =>
      writeProject(root)
      val projectReport = BidsProjectLoader.loadChecked(root).toOption.get
      val project = projectReport.value
      val recipe = DatasetRecipe.unsafe(
        datasetId = DatasetId("fixture"),
        project = WorkflowArtifactRef.unsafe[BidsProjectResource](root.toUri.toString.stripSuffix("/")),
        boldQuery = BidsQuery.from(
          filename = Vector("desc-preproc_bold\\.nii(\\.gz)?$"),
          scope = BidsScope.Derivatives,
          pipeline = Some(PipelineName("fmriprep"))
        ).toOption.get,
        maskPolicy = MaskPolicy.IntersectRunMasks,
        confounds = Some(ConfoundSelectionConfig(variables = Vector("motion6")))
      )

      val compilation = BidsStudyCompilerJvm.compileChecked(projectReport, recipe).toOption.get
      val catalog = compilation.catalog
      assertEquals(compilation.issues, Vector.empty)

      assertEquals(catalog.units.map(_.subject.value), Vector("01", "02"))
      assertEquals(catalog.units.map(_.runs.length), Vector(2, 2))
      assertEquals(catalog.units.map(_.shape.timepoints), Vector(6, 6))
      assert(catalog.units.forall(_.mask.isInstanceOf[UnitMask.Intersection]))
      assertEquals(catalog.participants.map(_.subject.value), Vector("01", "02"))
      assertEquals(catalog.participants.map(_.values("age")), Vector(Some("25"), Some("30")))

      val selected = project.query(recipe.boldQuery)
      assertEquals(selected.length, 4)
      selected.foreach { file =>
        val size = Files.size(root.resolve(file.path.value))
        assert(size <= 400L, clues(file.path.value, size))
      }
    }
  }

  test("external and competing roots compile header-only files with URI and QC identity") {
    withFixture { root =>
      val rawPath = root.resolve("raw")
      writeProject(rawPath)
      val external = root.resolve("external #1% café")
      Files.move(rawPath.resolve("derivatives/fmriprep"),external)
      val competing = root.resolve("competing")
      val stream = Files.walk(external)
      try stream.iterator.asScala.foreach { path =>
        val target = competing.resolve(external.relativize(path))
        if Files.isDirectory(path) then Files.createDirectories(target) else Files.copy(path,target)
      }
      finally stream.close()
      val raw = BidsRoot.raw("raw",rawPath.toString).toOption.get
      val selected = BidsRoot.derivative("selected",external.toString,PipelineName("custom"),raw.alias).toOption.get
      val other = BidsRoot.derivative("other",competing.toString,PipelineName("custom"),raw.alias).toOption.get
      val roots = Vector(raw,selected,other)
      def load = BidsProjectLoader.loadLinkedChecked(roots).fold(e => fail(e.message),identity)
      val recipe = LinkedDatasetRecipe.unsafe(DatasetId("linked"),selected.alias,
        BidsQuery.from(filename=Vector("desc-preproc_bold\\.nii(\\.gz)?$"),scope=BidsScope.All).toOption.get,
        maskPolicy=MaskPolicy.IntersectRunMasks,confounds=Some(ConfoundSelectionConfig(variables=Vector("trans_x"))))
      def compile =
        val report = load
        BidsStudyCompilerJvm.compileChecked(report.value,recipe,report.issues)
      val compilation = compile.fold(r => fail(r.issues.mkString("; ")),identity)
      assertEquals(compilation.catalog.units.size,2)
      assert(compilation.catalog.units.forall(_.runs.size == 2))
      compilation.catalog.units.foreach { unit =>
        unit.runs.foreach { run =>
          assert(Path.of(java.net.URI.create(run.bold.location.value)).startsWith(external))
          assert(Path.of(java.net.URI.create(run.events.location.value)).startsWith(rawPath))
        }
        val qc = FirstLevelCompanionReviewJvm.inspectLinked(roots,selected.alias,unit).fold(e => fail(e.message),identity)
        assert(qc.forall(_.columns.exists(_.name == "trans_x")))
        assert(qc.forall(_.location.exists(location => Path.of(java.net.URI.create(location.value)).startsWith(external))))
      }
      val reversed = BidsProjectLoader.loadLinkedChecked(roots.reverse).toOption.get
      assertEquals(BidsStudyCompilerJvm.compileChecked(reversed.value,recipe,reversed.issues),Right(compilation))
      write(competing.resolve("sub-01/func/sub-01_task-demo_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.json"),"{\"RepetitionTime\":9.0}")
      assertEquals(compile,Right(compilation))
      val sidecar = external.resolve("sub-01/func/sub-01_task-demo_run-01_space-MNI152NLin2009cAsym_desc-preproc_bold.json")
      write(sidecar,"{\"RepetitionTime\":4.0}")
      assertNotEquals(compile.toOption.get.catalog,compilation.catalog)
      val mask = external.resolve("sub-01/func/sub-01_task-demo_run-01_space-MNI152NLin2009cAsym_desc-brain_mask.nii")
      Files.delete(mask)
      assert(compile.left.toOption.get.errors.exists(_.code == CatalogIssueCode.MissingMask))
    }
  }

  private def writeProject(root: Path): Unit =
    write(root.resolve("dataset_description.json"), """{"Name":"Workflow fixture","BIDSVersion":"1.10.0","DatasetType":"raw"}""")
    write(root.resolve("participants.tsv"), "participant_id\tage\nsub-01\t25\nsub-02\t30\n")
    write(
      root.resolve("derivatives/fmriprep/dataset_description.json"),
      """{"Name":"fMRIPrep","BIDSVersion":"1.10.0","DatasetType":"derivative"}"""
    )

    Vector("01", "02").foreach { subject =>
      Vector("01", "02").foreach { run =>
        val rawPrefix = s"sub-${subject}_task-demo_run-${run}"
        val derivativePrefix = s"${rawPrefix}_space-MNI152NLin2009cAsym"
        val func = root.resolve(s"derivatives/fmriprep/sub-$subject/func")
        val boldNii = func.resolve(s"${derivativePrefix}_desc-preproc_bold.nii")
        val mask = func.resolve(s"${derivativePrefix}_desc-brain_mask.nii")
        val compressed = subject == "02" && run == "02"
        writeHeaderOnlyBold(boldNii)
        val boldPath =
          if compressed then
            val gzipPath = boldNii.resolveSibling(boldNii.getFileName.toString + ".gz")
            gzip(boldNii, gzipPath)
            Files.delete(boldNii)
            gzipPath
          else boldNii
        write(
          boldPath.resolveSibling(boldPath.getFileName.toString.replace(".nii.gz", ".json").replace(".nii", ".json")),
          """{"RepetitionTime":2.0}"""
        )
        writeHeaderOnlyMask(mask)
        write(
          func.resolve(s"${rawPrefix}_desc-confounds_timeseries.tsv"),
          "trans_x\ttrans_y\n0\t0\n0\t0\n0\t0\n"
        )
        write(
          root.resolve(s"sub-$subject/func/${rawPrefix}_events.tsv"),
          "onset\tduration\ttrial_type\n0\t1\tstim\n"
        )
      }
    }

  private def writeHeaderOnlyBold(path: Path): Unit =
    Files.createDirectories(path.getParent)
    val values = PrimitiveBuffers.fromArray(Array.tabulate(12)(_.toDouble))
    val space = NeuroSpace(Vector(2, 2, 1)).addDim(3, Some(Axis.Time))
    Nifti.writeVec(path, NeuroVec.fromLinear(values, space, "bold"))
    Files.write(path, Files.readAllBytes(path).take(352))

  private def writeHeaderOnlyMask(path: Path): Unit =
    Files.createDirectories(path.getParent)
    val values = PrimitiveBuffers.fromArray(Array(1.0, 1.0, 1.0, 1.0))
    val space = NeuroSpace(Vector(2, 2, 1))
    Nifti.writeVol(path, NeuroVol.fromLinear(values, space, "mask"))
    Files.write(path, Files.readAllBytes(path).take(352))

  private def gzip(source: Path, target: Path): Unit =
    val input = Files.newInputStream(source)
    val output = new GZIPOutputStream(Files.newOutputStream(target))
    try input.transferTo(output)
    finally
      output.close()
      input.close()

  private def write(path: Path, value: String): Unit =
    Files.createDirectories(path.getParent)
    Files.writeString(path, value, StandardCharsets.UTF_8)

  private def withFixture[A](body: Path => A): A =
    val root = Files.createTempDirectory("scalafim-workflow-bids-")
    try body(root)
    finally
      val files = Files.walk(root)
      try files.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)
      finally files.close()
