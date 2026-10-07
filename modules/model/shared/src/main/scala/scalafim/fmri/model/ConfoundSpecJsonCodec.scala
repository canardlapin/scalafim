package scalafim.fmri.model

import ujson.*
import scala.util.control.NonFatal

enum ConfoundSpecJsonError:
  case Invalid(path: String, detail: String)
  def message: String = this match
    case Invalid(path, detail) => s"$path: $detail"

/** Strict, versioned portable encoding for the typed confound preparation spec. */
object ConfoundSpecJsonCodec:
  private val Version = 1
  def encode(spec: ConfoundSpec): String =
    val censor = spec.censor match
      case None => Null
      case Some(value) => Obj("threshold" -> Num(value.threshold), "before" -> Num(value.before), "after" -> Num(value.after), "minimumRetainedSegment" -> Num(value.minimumRetainedSegment), "maximumCensoredFraction" -> Num(value.maximumCensoredFraction))
    write(Obj("version" -> Num(Version), "motion" -> Str(spec.motion.toString), "acompcorComponents" -> Num(spec.acompcorComponents), "includeWhiteMatter" -> Bool(spec.includeWhiteMatter), "includeCsf" -> Bool(spec.includeCsf), "includeGlobalSignal" -> Bool(spec.includeGlobalSignal), "censor" -> censor))

  def decode(text: String): Either[ConfoundSpecJsonError, ConfoundSpec] =
    try decodeValue(read(text)) catch case NonFatal(error) => Left(ConfoundSpecJsonError.Invalid("$", Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def decodeValue(value: Value): Either[ConfoundSpecJsonError, ConfoundSpec] =
    value match
      case objectValue: Obj =>
        val allowed = Set("version", "motion", "acompcorComponents", "includeWhiteMatter", "includeCsf", "includeGlobalSignal", "censor")
        objectValue.obj.keys.find(key => !allowed(key)).map(key => Left(ConfoundSpecJsonError.Invalid("$", s"unknown field '$key'"))).getOrElse {
          for
            version <- integer(objectValue, "version")
            _ <- if version == Version then Right(()) else Left(ConfoundSpecJsonError.Invalid("$.version", s"expected $Version"))
            motionText <- string(objectValue, "motion")
            motion <- motionText match
              case "Raw6" => Right(MotionExpansion.Raw6); case "RawAndDerivative12" => Right(MotionExpansion.RawAndDerivative12); case "Friston24" => Right(MotionExpansion.Friston24)
              case other => Left(ConfoundSpecJsonError.Invalid("$.motion", s"unknown motion expansion '$other'"))
            acompcor <- integer(objectValue, "acompcorComponents")
            wm <- bool(objectValue, "includeWhiteMatter")
            csf <- bool(objectValue, "includeCsf")
            global <- bool(objectValue, "includeGlobalSignal")
            censor <- field(objectValue, "censor").flatMap {
              case Null => Right(None)
              case entry: Obj => policy(entry).map(Some.apply)
              case _ => Left(ConfoundSpecJsonError.Invalid("$.censor", "expected object or null"))
            }
            spec <- ConfoundSpec.make(motion, acompcor, wm, csf, global, censor).left.map(error => ConfoundSpecJsonError.Invalid("$", error.message))
          yield spec
        }
      case _ => Left(ConfoundSpecJsonError.Invalid("$", "expected object"))

  private def policy(value: Obj): Either[ConfoundSpecJsonError, FdCensorPolicy] =
    val allowed = Set("threshold", "before", "after", "minimumRetainedSegment", "maximumCensoredFraction")
    value.obj.keys.find(key => !allowed(key)).map(key => Left(ConfoundSpecJsonError.Invalid("$.censor", s"unknown field '$key'"))).getOrElse {
      for
        threshold <- number(value, "threshold", "$.censor")
        before <- integer(value, "before", "$.censor")
        after <- integer(value, "after", "$.censor")
        minimum <- integer(value, "minimumRetainedSegment", "$.censor")
        maximum <- number(value, "maximumCensoredFraction", "$.censor")
        policy <- FdCensorPolicy.make(threshold, before, after, minimum, maximum).left.map(error => ConfoundSpecJsonError.Invalid("$.censor", error.message))
      yield policy
    }
  private def field(value: Obj, name: String, path: String = "$"): Either[ConfoundSpecJsonError, Value] = value.obj.get(name).toRight(ConfoundSpecJsonError.Invalid(s"$path.$name", "missing field"))
  private def string(value: Obj, name: String, path: String = "$"): Either[ConfoundSpecJsonError, String] = field(value, name, path).flatMap { case Str(text) => Right(text); case _ => Left(ConfoundSpecJsonError.Invalid(s"$path.$name", "expected string")) }
  private def bool(value: Obj, name: String, path: String = "$"): Either[ConfoundSpecJsonError, Boolean] = field(value, name, path).flatMap { case Bool(flag) => Right(flag); case _ => Left(ConfoundSpecJsonError.Invalid(s"$path.$name", "expected boolean")) }
  private def number(value: Obj, name: String, path: String): Either[ConfoundSpecJsonError, Double] = field(value, name, path).flatMap { case Num(number) if number.isFinite => Right(number); case _ => Left(ConfoundSpecJsonError.Invalid(s"$path.$name", "expected finite number")) }
  private def integer(value: Obj, name: String, path: String = "$"): Either[ConfoundSpecJsonError, Int] = number(value, name, path).flatMap(number => if number.isValidInt && number == number.toInt then Right(number.toInt) else Left(ConfoundSpecJsonError.Invalid(s"$path.$name", "expected integer")))
