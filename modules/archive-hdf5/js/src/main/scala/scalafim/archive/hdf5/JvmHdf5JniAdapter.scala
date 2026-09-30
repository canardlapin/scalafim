package scalafim.archive.hdf5

object JvmHdf5JniAdapter:
  def open(limits: Hdf5Limits = Hdf5Limits.bounded): Either[Hdf5Error, Hdf5Archive] =
    Left(Hdf5Error.MissingCapability("HDFGroup official JNI 2.2.0", "Scala.js", "inapplicable", "JVM-native capability required"))
