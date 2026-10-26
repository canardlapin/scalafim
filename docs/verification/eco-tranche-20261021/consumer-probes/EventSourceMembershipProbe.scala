package glm.review.fx

import bids4s.BidsTable
import glm.review.*
import javafx.application.{Application, Platform}
import javafx.scene.Scene
import javafx.scene.control.TableView
import javafx.stage.Stage
import scalafim.dataset.RunId
import scalafim.fmri.design.*
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame
import scala.jdk.CollectionConverters.*

/** Exercises the actual unchanged consumer pane; all inputs are synthetic. */
object EventSourceMembershipProbe:
  def main(args: Array[String]): Unit = Application.launch(classOf[EventSourceMembershipProbeApp], args*)

final class EventSourceMembershipProbeApp extends Application:
  override def start(stage: Stage): Unit =
    try
      check()
      Platform.exit()
    catch case error: Throwable =>
      error.printStackTrace()
      System.exit(1)

  private def check(): Unit =
    val draft = ModelDraft(response = "FIR", firBins = "1", firWindow = "1",
      eventTerms = EventTermsDraft(subset = EventSubsetDraft(rules = Vector(
        EventSubsetRuleDraft("rating", "At least", "Number", "3")))))
    val validated = draft.validate.toOption.get
    val spec = validated.specification
    val onsets = Vector(1.0, 3.0, 5.0, 1.0, 3.0, 5.0, 7.0)
    val ratings = Vector(3.0, 1.0, 4.0, 1.0, 3.0, 1.0, 4.0)
    val data = DataTable.fromColumns(
      "onset" -> Column.Doubles(onsets),
      "trial_type" -> Column.Strings(Vector.fill(7)("A")),
      "rating" -> Column.Doubles(ratings))
    val frame = SamplingFrame(blockLens = Seq(10, 10), tr = Seq(1.0, 1.0))
    val model = EventModelBuilder.build(spec.formula, data, frame,
      blockIds = Vector(0, 0, 0, 1, 1, 1, 1), defaultHrf = spec.defaultHrf,
      precision = spec.precision, missingValuePolicy = spec.missingValuePolicy,
      degenerateModulatorPolicy = spec.degenerateModulatorPolicy)
    val schema = model.designSchema
    require(schema.audit.sourceEvents.map(_.sourceRow) == Vector(0, 2, 4, 6))
    val names = Vector("alpha", "beta")
    val scans = (1 to 20).map(ScanIndex.unsafeOneBased).toVector
    val reviews = names.zipWithIndex.map: (name, index) =>
      val source = DesignReview.forRun(schema, frame, RunIndex.unsafeOneBased(index + 1),
        scans, DesignReviewScope.SourceColumns).toOption.get
      FirstLevelRunReview(name, source, Left("No fit requested"), FitReadiness.Unavailable, Left("No QC requested"))
    def table(indices: Range): BidsTable =
      val text = "onset\tduration\ttrial_type\trating\n" + indices.map(i =>
        s"${onsets(i)}\t0\tA\t${ratings(i)}\n").mkString
      BidsTable.parse(text).toOption.get
    val tables = Vector(RunId("alpha") -> table(0 until 3), RunId("beta") -> table(3 until 7))
    val pane = new EventSourcePane()
    val scene = new Scene(pane.node, 900, 500)
    def membership(run: String): Vector[String] =
      pane.reveal(RunId(run), -1)
      val view = scene.lookup("#glm-events-table").asInstanceOf[TableView[Integer]]
      val column = view.getColumns.asScala.find(_.getText == "Task terms").get
      view.getItems.asScala.indices.map(i => column.getCellObservableValue(i).getValue.toString).toVector
    for displayed <- Vector(tables, tables.reverse) do
      pane.show(validated, displayed, "Synthetic membership", reviews.reverse)
      require(membership("alpha") == Vector("Modeled", "Not in task terms", "Modeled"))
      require(membership("beta") == Vector("Not in task terms", "Modeled", "Not in task terms", "Modeled"))
    pane.show(validated, tables.reverse, "Missing review", reviews.take(1))
    require(membership("alpha").forall(_ == "Not reviewed"))
    require(membership("beta").forall(_ == "Not reviewed"))
    println("PASS: original task membership across unequal runs and reversed display/review order; incomplete evidence stays unreviewed")
