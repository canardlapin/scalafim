package scalafim.fmri.motion

import scalafim.image.{NeuroVec, NeuroVol}

final case class MotionQc(
    fd: Vector[Double],
    dvars: Vector[Double],
    robustDvars: Vector[Double],
    costDrop: Vector[Double],
    motionSpike: Vector[Boolean],
    fitFailure: Vector[Boolean],
    censorSuggest: Vector[Boolean]
)

object MotionQc:
  def from(
      run: NeuroVec[Double],
      trace: MotionTrace,
      corrected: Option[NeuroVec[Double]] = None,
      mask: Option[NeuroVol[Boolean]] = None,
      costInit: Option[Vector[Double]] = None,
      costFinal: Option[Vector[Double]] = None,
      radius: HeadRadius = HeadRadius.default
  ): Either[MotionError, MotionQc] =
    if trace.length != run.nVolumes then Left(MotionError.TraceLengthMismatch(trace.length, run.nVolumes))
    else
      corrected match
        case Some(corr) if corr.space.dims.take(4) != run.space.dims.take(4) =>
          Left(MotionError.ShapeMismatch("corrected", run.space.dims.take(4), corr.space.dims.take(4)))
        case _ =>
          val nt = run.nVolumes
          val init = costInit.getOrElse(Vector.fill(nt)(0.0))
          val fin = costFinal.getOrElse(Vector.fill(nt)(0.0))
          if init.length != nt then Left(MotionError.ShapeMismatch("costInit", Vector(nt), Vector(init.length)))
          else if fin.length != nt then Left(MotionError.ShapeMismatch("costFinal", Vector(nt), Vector(fin.length)))
          else
            val qcRun = corrected.getOrElse(run)
            for
              dv <- MotionMetrics.dvars(qcRun, mask, robust = false)
              rdv <- MotionMetrics.dvars(qcRun, mask, robust = true)
            yield
              val fd = MotionMetrics.framewiseDisplacement(trace, radius)
              val costDrop = Vector.tabulate(nt)(t => init(t) - fin(t))
              val motionSpike = fd.map(_ > 0.5)
              val fitFailure = Vector.tabulate(nt)(t => fin(t) > init(t) + 1e-8)
              val censor = Vector.tabulate(nt)(t => motionSpike(t) || fitFailure(t))
              MotionQc(
                fd = fd,
                dvars = dv,
                robustDvars = rdv,
                costDrop = costDrop,
                motionSpike = motionSpike,
                fitFailure = fitFailure,
                censorSuggest = censor
              )
