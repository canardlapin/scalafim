package scalafim.phrfcmp.exec

import java.io.{FileInputStream, FileOutputStream, IOException, InputStream, OutputStream}
import java.nio.charset.StandardCharsets.US_ASCII
import java.security.MessageDigest

/** Why root intake failed. Messages never carry the root. */
enum IntakeError(val message: String):
  case MissingVariable(name: String) extends IntakeError(s"environment variable $name is not set")
  case BadDescriptor(name: String) extends IntakeError(s"$name is not a usable descriptor number")
  case DescriptorUnavailable extends IntakeError("cannot open the inherited descriptor through /dev/fd")
  case ReadFailed extends IntakeError("could not read the root line")
  case BadLine extends IntakeError("root line is not an unsigned 64-bit decimal followed by a newline")
  case AckFailed extends IntakeError("could not write the acknowledgement")

/** Opens inherited descriptors by number; replaceable so the intake order can be tested with fakes. */
trait FdIo:
  def input(fd: Int): Either[IntakeError, InputStream]
  def output(fd: Int): Either[IntakeError, OutputStream]

object FdIo:
  /** Attaches by path (`/dev/fd/N`), which works on macOS and Linux. Opening that path yields a stream on the same
    * pipe through a fresh descriptor; closing the stream closes that new descriptor. The inherited descriptor number
    * itself cannot be closed from Java, so it stays open until the JVM exits. That is harmless to the session (it
    * reads exactly 64 ack bytes and does not wait for EOF; the parent closed its write end of the root pipe) and
    * ProcessBuilder never passes descriptors above 2 to children. The real Python `session.launch` interop test
    * verifies the behaviour end to end.
    */
  val system: FdIo = new FdIo:
    def input(fd: Int): Either[IntakeError, InputStream] =
      try Right(new FileInputStream(s"/dev/fd/$fd"))
      catch case _: IOException => Left(IntakeError.DescriptorUnavailable)
    def output(fd: Int): Either[IntakeError, OutputStream] =
      try Right(new FileOutputStream(s"/dev/fd/$fd"))
      catch case _: IOException => Left(IntakeError.DescriptorUnavailable)

/** Root intake per the sealed-format spec section 8, in this order and nothing in between:
  *   1. read the line once; 2. close `PHRF_ROOT_FD`; 3. write `hex(SHA-256(line))` to `PHRF_ROOT_ACK_FD` and close it.
  * The root is never logged, stored or forwarded except into [[PilotRoot]]. The JVM cannot unset its own environment;
  * [[ChildProcess]] scrubs both variables from every child it starts, and nothing here starts a process or a thread.
  */
object RootIntake:
  val RootVar = "PHRF_ROOT_FD"
  val AckVar = "PHRF_ROOT_ACK_FD"

  final class Received(val root: PilotRoot, val ackHex: String):
    override def toString: String = "Received(redacted)"

  private def fdNumber(env: collection.Map[String, String], name: String): Either[IntakeError, Int] =
    env.get(name).toRight(IntakeError.MissingVariable(name)).flatMap { v =>
      v.toIntOption.filter(_ > 2).toRight(IntakeError.BadDescriptor(name))
    }

  private def parse(line: Array[Byte]): Option[PilotRoot] =
    val s = new String(line, US_ASCII)
    if s.matches("[0-9]{1,20}\n") then
      try Some(new PilotRoot(java.lang.Long.parseUnsignedLong(s.dropRight(1))))
      catch case _: NumberFormatException => None
    else None

  def receive(env: collection.Map[String, String] = sys.env, io: FdIo = FdIo.system): Either[IntakeError, Received] =
    for
      rootFd <- fdNumber(env, RootVar)
      ackFd <- fdNumber(env, AckVar)
      in <- io.input(rootFd)
      line <- readLineThenClose(in)
      root <- parse(line).toRight(IntakeError.BadLine)
      ack = Fs.hex(MessageDigest.getInstance("SHA-256").digest(line))
      out <- io.output(ackFd)
      _ <- writeAckThenClose(out, ack)
    yield new Received(root, ack)

  /** One logical read of the line (looping only until the newline, EOF or 64 bytes), then the descriptor is closed. */
  private def readLineThenClose(in: InputStream): Either[IntakeError, Array[Byte]] =
    val buf = new Array[Byte](64)
    var n = 0
    var done = false
    try
      while !done && n < buf.length do
        val r = in.read(buf, n, buf.length - n)
        if r < 0 then done = true
        else
          n += r
          if buf.take(n).contains('\n'.toByte) then done = true
      Right(buf.take(n))
    catch case _: IOException => Left(IntakeError.ReadFailed)
    finally
      try in.close()
      catch case _: IOException => ()

  private def writeAckThenClose(out: OutputStream, ack: String): Either[IntakeError, Unit] =
    try
      out.write(ack.getBytes(US_ASCII))
      out.flush()
      Right(())
    catch case _: IOException => Left(IntakeError.AckFailed)
    finally
      try out.close()
      catch case _: IOException => ()
