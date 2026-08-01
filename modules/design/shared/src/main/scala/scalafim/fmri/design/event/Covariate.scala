package scalafim.fmri.design.event

import scalafim.fmri.design.{ColumnId, DesignError, Names}
import scalafim.fmri.design.data.DataTable
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

final case class CovariateSpec(
    vars: Vector[ColumnId],
    data: DataTable,
    id: Option[String] = None,
    prefix: Option[String] = None
):
  require(vars.nonEmpty, "`vars` must be non-empty")

  val columnNames: Vector[String] =
    val base = vars.map(v => Names.sanitize(v.value, allowDot = true))
    prefix match
      case None    => base
      case Some(p) => base.map(v => s"${Names.sanitize(p, allowDot = true)}_$v")

  val name: String = columnNames.mkString("::")
  val termId: String = id.getOrElse(name)

  def construct(samplingFrame: SamplingFrame): Either[DesignError, CovariateConvolvedTerm] =
    val nRows = samplingFrame.blockLens.sum
    if data.nrows != nRows then
      Left(
        DesignError.InvalidSchedule(
          s"Covariate term '$termId' has ${data.nrows} rows but samplingFrame expects $nRows"
        )
      )
    else
      val nCols = vars.length
      val out = new Array[Double](nRows * nCols)

      var c = 0
      while c < nCols do
        data.get[Double](vars(c)) match
          case Left(error) => return Left(error)
          case Right(xs) =>
            var r = 0
            while r < nRows do
              out(r * nCols + c) = xs(r)
              r += 1
        c += 1

      Right(
        CovariateConvolvedTerm(
          id = termId,
          name = name,
          spec = this,
          data = Mat.unsafe(nRows, nCols, out),
          columnNames = columnNames
        )
      )

final case class CovariateConvolvedTerm(
    id: String,
    name: String,
    spec: CovariateSpec,
    data: Mat,
    columnNames: Vector[String]
) extends EventModelTerm:
  val columnRoles: Vector[EventTermColumnRole] =
    Vector.fill(data.cols)(EventTermColumnRole.Covariate)

  requireColumnMetadata()

  def keyHint: Option[String] = Some(id)
  def hrfOpt: Option[scalafim.fmri.hrf.Hrf] = None
  def role: EventTermRole = EventTermRole.Covariate
