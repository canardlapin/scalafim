package scalafim.locus

final class Injection[X, Y] private (
    val mapping: TotalMap[X, Y]
)

object Injection:
  def validate[X, Y](mapping: TotalMap[X, Y]): Either[MapEvidenceError, Injection[X, Y]] =
    val firstSource = Array.fill(mapping.to.size)(-1)
    val targets = mapping.targetOrdinals
    var source = 0
    var error = Option.empty[MapEvidenceError]
    while source < targets.length && error.isEmpty do
      val target = targets(source)
      if firstSource(target) >= 0 then
        error = Some(MapEvidenceError.DuplicateTarget(target, firstSource(target), source))
      else
        firstSource(target) = source
      source += 1

    error match
      case Some(value) => Left(value)
      case None => Right(new Injection(mapping))

final class Surjection[X, Y] private (
    val mapping: TotalMap[X, Y]
)

object Surjection:
  def validate[X, Y](mapping: TotalMap[X, Y]): Either[MapEvidenceError, Surjection[X, Y]] =
    val seen = Array.fill(mapping.to.size)(false)
    val targets = mapping.targetOrdinals
    var source = 0
    while source < targets.length do
      seen(targets(source)) = true
      source += 1

    var target = 0
    var missing = -1
    while target < seen.length && missing < 0 do
      if !seen(target) then missing = target
      target += 1

    if missing >= 0 then Left(MapEvidenceError.MissingTarget(missing))
    else Right(new Surjection(mapping))

final class Bijection[X, Y] private (
    val mapping: TotalMap[X, Y],
    val injection: Injection[X, Y],
    val surjection: Surjection[X, Y]
)

object Bijection:
  def validate[X, Y](mapping: TotalMap[X, Y]): Either[MapEvidenceError, Bijection[X, Y]] =
    for
      injection <- Injection.validate(mapping)
      surjection <- Surjection.validate(mapping)
    yield new Bijection(mapping, injection, surjection)
