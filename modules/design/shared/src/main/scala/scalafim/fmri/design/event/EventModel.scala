package scalafim.fmri.design.event

import scalafim.fmri.design.{DesignError, Names}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat

import scala.collection.immutable.VectorMap
import scala.util.control.NonFatal

enum EventModelDiagnosticKind:
  case BasisDegeneracy, DegenerateModulator, NonFiniteModulator, OnsetOutOfBounds

final case class EventModelDiagnostic(
    kind: EventModelDiagnosticKind,
    term: String,
    message: String
)

final case class EventModel(
    terms: Vector[(String, EventModelTerm)],
    samplingFrame: SamplingFrame,
    designMatrix: Mat,
    columnNames: Vector[String],
    termSpans: Vector[(Int, Int)],
    colIndices: Map[String, Vector[Int]],
    contrastSetsByTerm: VectorMap[String, ContrastSpec.ContrastSet] = VectorMap.empty,
    diagnostics: Vector[EventModelDiagnostic] = Vector.empty
)
:
  def termKeys: Vector[String] = terms.map(_._1)

object EventModel:

  def build(terms: Seq[ConvolvedTerm], samplingFrame: SamplingFrame): EventModel =
    buildTerms(terms.toVector, samplingFrame)

  def buildTermsEither(terms: Seq[EventModelTerm], samplingFrame: SamplingFrame): Either[DesignError, EventModel] =
    try Right(buildTerms(terms, samplingFrame))
    catch
      case NonFatal(t) => Left(DesignError.fromThrowable(t))

  def buildTerms(terms: Seq[EventModelTerm], samplingFrame: SamplingFrame): EventModel =
    val ts0 = terms.toVector
    val totalRows = samplingFrame.blockLens.sum
    ts0.foreach { t =>
      require(t.data.rows == totalRows, "term matrix row mismatch with samplingFrame")
      t.requireColumnMetadata()
    }

    val rawKeys =
      ts0.zipWithIndex.map { case (t, i) =>
        t.keyHint.getOrElse(s"term_${i + 1}")
      }
    val keys = Names.makeUniqueTags(rawKeys)

    val totalCols = ts0.map(_.data.cols).sum
    val out = new Array[Double](totalRows * totalCols)
    val colNames = Vector.newBuilder[String]

    val spans = Vector.newBuilder[(Int, Int)]
    val indices = scala.collection.mutable.LinkedHashMap.empty[String, Vector[Int]]
    val outTerms = Vector.newBuilder[(String, EventModelTerm)]

    var colOffset = 0
    var i = 0
    while i < ts0.length do
      val term = ts0(i)
      val cols = term.data.cols
      val key = keys(i)

      // Copy into the combined matrix (row-major cbind).
      var r = 0
      while r < totalRows do
        System.arraycopy(term.data.data, r * cols, out, r * totalCols + colOffset, cols)
        r += 1

      val term0 =
        (term, term.keyHint) match
          case (ct: ConvolvedTerm, Some(old)) if old != key =>
            val ren = ct.columnNames.map { cn =>
              if cn.startsWith(old + "_") then key + cn.drop(old.length) else cn
            }
            ct.copy(term = ct.term.copy(termTag = Some(key)), columnNames = ren)
          case (cv: CovariateConvolvedTerm, Some(old)) if old != key =>
            cv.copy(id = key, spec = cv.spec.copy(id = Some(key)))
          case _ => term

      outTerms += (key -> term0)
      colNames ++= term0.columnNames

      val start = colOffset
      val endExcl = colOffset + cols
      spans += ((start, endExcl))

      indices.update(key, (start until endExcl).toVector)

      colOffset = endExcl
      i += 1

    EventModel(
      terms = outTerms.result(),
      samplingFrame = samplingFrame,
      designMatrix = Mat.unsafe(totalRows, totalCols, out),
      columnNames = colNames.result(),
      termSpans = spans.result(),
      colIndices = indices.toMap
    )
