package scalafim.fmri.motion

private[motion] object MotionPlatform:
  val parallelFramesSupported: Boolean = false

  def mapOrdered[A, B](values: Vector[A], nThreads: Int)(f: A => B): Vector[B] =
    values.map(f)
