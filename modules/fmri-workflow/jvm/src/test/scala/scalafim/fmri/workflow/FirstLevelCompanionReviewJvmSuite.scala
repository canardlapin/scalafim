package scalafim.fmri.workflow

import scalafim.dataset.*
import scalafim.image.NeuroSpace
import java.nio.file.{Files,Path}
import scala.jdk.CollectionConverters.*

class FirstLevelCompanionReviewJvmSuite extends munit.FunSuite:
  test("optional QC uses source units and rows, independently of nuisance or readable BOLD") {
    val root = Files.createTempDirectory("scalafim-qc-")
    def write(path: String,text: String): Path =
      val target = root.resolve(path)
      val _ = Files.createDirectories(target.getParent)
      Files.writeString(target,text)
    try
      val _ = write("dataset_description.json","""{"Name":"QC","BIDSVersion":"1.10.0","DatasetType":"raw"}""")
      val _ = write("derivatives/custom/dataset_description.json","""{"Name":"Custom producer","BIDSVersion":"1.10.0","DatasetType":"derivative"}""")
      val prefix = "derivatives/custom/sub-01/func/sub-01_task-demo_run-01"
      val bold = write(prefix+"_space-MNI_desc-preproc_bold.nii.gz","Deliberately invalid BOLD payload")
      val events = write("sub-01/func/sub-01_task-demo_run-01_events.tsv","onset\tduration\ttrial_type\n0\t0\tA\n")
      val mask = root.resolve("mask.nii")
      val unit = FirstLevelUnit.unsafe(FirstLevelUnitId.unsafe("selected"),SubjectId("01"),None,TaskId("demo"),SpaceId("MNI"),
        DatasetShape.unsafe(NeuroSpace(Vector(2,1,1)),2),Vector(RunInput.unsafe(RunId("01"),RepetitionTime.unsafe(1.0),2,
          WorkflowArtifactRef.unsafe[BoldImageResource](bold.toUri.toString),WorkflowArtifactRef.unsafe[EventsTableResource](events.toUri.toString))),
        UnitMask.Single(WorkflowArtifactRef.unsafe[MaskImageResource](mask.toUri.toString)))
      assert(!FirstLevelCompanionReviewJvm.inspect(root,unit).toOption.get.head.available)
      val table = write(prefix+"_desc-confounds_timeseries.tsv","trans_x\tframewise_displacement\n1.5\tn/a\n-2\t0.4\n")
      val _ = write(prefix+"_desc-confounds_timeseries.json","""{"trans_x":{"Units":"mm"}}""")
      val first = FirstLevelCompanionReviewJvm.inspect(root,unit).fold(e => fail(e.message),identity).head
      assertEquals(first.columns.head.units,Some("mm"))
      assertEquals(first.columns(1).values,Right(Vector(None,Some(0.4))))
      assert(unit.runs.head.confounds.isEmpty)
      val _ = Files.writeString(table,"trans_x\tframewise_displacement\n1.7\tn/a\n-2\t0.4\n")
      assertNotEquals(FirstLevelCompanionReviewJvm.inspect(root,unit).toOption.get.head.sourceSha256,first.sourceSha256)
      assert(FirstLevelCompanionReviewJvm.inspect(root,unit,maximumTableBytes=2).isLeft)
      val _ = Files.writeString(table,"trans_x\n0\n")
      assert(FirstLevelCompanionReviewJvm.inspect(root,unit).isLeft)
      val _ = Files.writeString(table,"trans_x\n0\n1\n")
      val _ = write(prefix+"_desc-confounds_regressors.tsv","trans_x\n0\n1\n")
      assert(FirstLevelCompanionReviewJvm.inspect(root,unit).isLeft)
    finally
      val paths = Files.walk(root)
      try paths.iterator().asScala.toVector.reverse.foreach(path => { val _ = Files.deleteIfExists(path) }) finally paths.close()
  }
