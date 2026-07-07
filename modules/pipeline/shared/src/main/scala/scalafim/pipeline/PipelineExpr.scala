package scalafim.pipeline

import scala.util.control.NonFatal

sealed trait PipelineExpr[A]:
  private[pipeline] def dependencies: Vector[ArtifactRef[?]]
  private[pipeline] def read(table: ArtifactTable): Either[PipelineError, A]

  def label: String

  def map[B](label: String)(f: A => B): PipelineExpr[B] =
    PipelineExpr.Mapped(this, label, f)

  def zip[B](that: PipelineExpr[B]): PipelineExpr[(A, B)] =
    PipelineExpr.Zipped(this, that)

object PipelineExpr:
  def ref[A](artifact: ArtifactRef[A]): PipelineExpr[A] =
    Ref(artifact)

  def const[A](value: A, label: String): PipelineExpr[A] =
    Const(value, label)

  private final case class Ref[A](artifact: ArtifactRef[A]) extends PipelineExpr[A]:
    override private[pipeline] def dependencies: Vector[ArtifactRef[?]] =
      Vector(artifact)

    override private[pipeline] def read(table: ArtifactTable): Either[PipelineError, A] =
      table.get(artifact)

    override def label: String =
      artifact.displayName

  private final case class Const[A](value: A, label: String) extends PipelineExpr[A]:
    override private[pipeline] def dependencies: Vector[ArtifactRef[?]] =
      Vector.empty

    override private[pipeline] def read(table: ArtifactTable): Either[PipelineError, A] =
      Right(value)

  private final case class Mapped[A, B](
      source: PipelineExpr[A],
      label: String,
      f: A => B
  ) extends PipelineExpr[B]:
    override private[pipeline] def dependencies: Vector[ArtifactRef[?]] =
      source.dependencies

    override private[pipeline] def read(table: ArtifactTable): Either[PipelineError, B] =
      source.read(table).flatMap { value =>
        try Right(f(value))
        catch
          case NonFatal(t) =>
            val reason = Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.toString)
            Left(PipelineError.ExpressionFailed(label, reason))
      }

  private final case class Zipped[A, B](
      left: PipelineExpr[A],
      right: PipelineExpr[B]
  ) extends PipelineExpr[(A, B)]:
    override private[pipeline] def dependencies: Vector[ArtifactRef[?]] =
      left.dependencies ++ right.dependencies

    override private[pipeline] def read(table: ArtifactTable): Either[PipelineError, (A, B)] =
      for
        a <- left.read(table)
        b <- right.read(table)
      yield (a, b)

    override def label: String =
      s"(${left.label}, ${right.label})"
