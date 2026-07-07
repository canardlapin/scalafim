package scalafim.fmri.motion

import java.util.concurrent.{Callable, ExecutionException, Executors}

private[motion] object MotionPlatform:
  val parallelFramesSupported: Boolean = true

  def mapOrdered[A, B](values: Vector[A], nThreads: Int)(f: A => B): Vector[B] =
    if values.isEmpty then Vector.empty
    else if nThreads <= 1 || values.length == 1 then values.map(f)
    else
      val pool = Executors.newFixedThreadPool(math.min(nThreads, values.length))
      try
        val futures =
          values.map { value =>
            pool.submit(
              new Callable[B]:
                override def call(): B = f(value)
            )
          }
        futures.map { future =>
          try future.get()
          catch
            case e: ExecutionException =>
              e.getCause match
                case runtime: RuntimeException => throw runtime
                case error: Error => throw error
                case cause => throw new RuntimeException(cause)
        }
      finally pool.shutdown()
