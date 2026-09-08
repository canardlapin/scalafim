package scalafim.fmri.workflow

import bids4s.*
import bids4s.io.BidsProjectLoader
import java.net.URI
import java.nio.file.{Files,Path}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.util.control.NonFatal
import scala.util.Using

/** Optional table-only inspection; never opens or stages a BOLD payload. */
object FirstLevelCompanionReviewJvm:
  def inspect(root: Path, unit: FirstLevelUnit, maximumTableBytes: Long = 64L*1024*1024): Either[WorkflowError,Vector[FirstLevelCompanionReview]] =
    def failed(message: String) = WorkflowError.InvalidUnit(unit.id.value,message)
    def load = BidsProjectLoader.loadChecked(root).left.map(e => failed(e.message)).map(_.value)
    def companion(project: BidsProject,run: RunInput): Either[WorkflowError,Option[BidsFile]] =
      val path = Path.of(URI.create(run.bold.location.value)).toAbsolutePath.normalize()
      val base = root.toAbsolutePath.normalize()
      if !path.startsWith(base) then Left(failed("BOLD artifact is outside the dataset root"))
      else BidsStudyCompiler.confoundCompanion(project,BidsPath(base.relativize(path).toString)).left.map(e => failed(e.message))
    def digest(bytes: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
    def readBounded(path: Path): Either[WorkflowError,Array[Byte]] =
      if Files.size(path) > maximumTableBytes then Left(failed(s"QC table exceeds $maximumTableBytes bytes"))
      else
        val bytes = Using.resource(Files.newInputStream(path))(_.readNBytes((maximumTableBytes+1).toInt))
        Either.cond(bytes.length <= maximumTableBytes,bytes,failed("QC table grew beyond its byte budget"))
    try
      for
        _ <- Either.cond(maximumTableBytes > 0 && maximumTableBytes < Int.MaxValue,(),failed("QC table budget must be positive and below Int.MaxValue"))
        project <- load
        before <- WorkflowValidation.traverse(unit.runs)(run => companion(project,run))
        result <- WorkflowValidation.traverse(unit.runs) { run =>
          if Thread.currentThread().isInterrupted then throw InterruptedException("QC review cancelled")
          for
            file <- companion(project,run)
            result <- file match
              case None => Right(FirstLevelCompanionReview(run.id,None,None,Vector.empty,0))
              case Some(source) =>
                val path = root.resolve(source.path.value)
                for
                  bytes <- readBounded(path)
                  table <- BidsTable.parse(String(bytes,StandardCharsets.UTF_8)).left.map(e => failed(e.message))
                  metadata <- project.metadata(source.path).left.map(e => failed(e.message))
                  units = metadata.fields.toVector.flatMap { (name,value) => value match
                    case entry: JsonValue.Obj => entry.fields.get("Units").collect { case JsonValue.Str(unit) => name -> unit }
                    case _ => None
                  }.toMap
                  columns <- FirstLevelCompanionReview.bind(run.id,run.timepoints,table,units)
                  sha = digest(bytes)
                  verified <- readBounded(path)
                  _ <- Either.cond(digest(verified) == sha,(),failed("QC table changed during review"))
                  current <- load
                  latest <- companion(current,run)
                  latestMetadata <- current.metadata(source.path).left.map(e => failed(e.message))
                  _ <- Either.cond(latest == file && latestMetadata == metadata,(),failed("QC companion selection or metadata changed during review"))
                yield FirstLevelCompanionReview(run.id,Some(ArtifactLocation.unsafe(path.toAbsolutePath.normalize().toUri.toString)),Some(sha),columns,table.nrows)
          yield result
        }
        current <- load
        after <- WorkflowValidation.traverse(unit.runs)(run => companion(current,run))
        _ <- Either.cond(before == after,(),failed("Optional QC companion membership changed during review"))
      yield result
    catch case NonFatal(error) => Left(failed(Option(error.getMessage).getOrElse(error.toString)))
