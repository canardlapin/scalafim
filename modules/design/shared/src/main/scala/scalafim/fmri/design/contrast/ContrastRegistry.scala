package scalafim.fmri.design.contrast

import scalafim.fmri.design.Validate
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel}

import scala.collection.immutable.VectorMap

object ContrastRegistry:

  private def unsafe[A](result: Either[ContrastError, A]): A =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (model: EventModel)

    def withContrastSet(termKey: String, set: ContrastSpec.ContrastSet, merge: Boolean = true): EventModel =
      unsafe(withContrastSetEither(termKey, set, merge = merge))

    def withContrastSetEither(termKey: String, set: ContrastSpec.ContrastSet, merge: Boolean = true): Either[ContrastError, EventModel] =
      model.terms.collectFirst { case (k, t) if k == termKey => t } match
        case None =>
          Left(ContrastError.UnknownTerm(termKey, model.terms.map(_._1)))
        case Some(_: ConvolvedTerm) =>
          val merged =
            if merge then model.contrastSetsByTerm.get(termKey).map(_ ++ set).getOrElse(set)
            else set

          Right(model.copy(contrastSetsByTerm = model.contrastSetsByTerm.updated(termKey, merged)))
        case Some(other) =>
          Left(ContrastError.UnsupportedTerm(termKey, other.toString))

    def contrastSpecs: VectorMap[String, ContrastSpec] =
      if model.contrastSetsByTerm.isEmpty then VectorMap.empty
      else
        val out = VectorMap.newBuilder[String, ContrastSpec]
        var i = 0
        while i < model.terms.length do
          val (termKey, _) = model.terms(i)
          model.contrastSetsByTerm.get(termKey).foreach { set =>
            set.contrasts.foreach { spec =>
              out += (s"$termKey#${spec.name}" -> spec)
            }
          }
          i += 1
        out.result()

    /** Flattened contrast weights embedded in the full design matrix columns.
      *
      * Keys follow the R convention: `"<termKey>#<contrastName>"`.
      */
    def contrastWeights: VectorMap[String, ContrastWeights] =
      unsafe(contrastWeightsEither)

    def contrastWeightsEither: Either[ContrastError, VectorMap[String, ContrastWeights]] =
      compiledContrasts.map { compiled =>
        VectorMap.from(compiled.iterator.map { case (key, contrast) => key -> contrast.toLegacy })
      }

    def compiledContrasts: Either[ContrastError, VectorMap[String, CompiledContrast]] =
      if model.contrastSetsByTerm.isEmpty then Right(VectorMap.empty)
      else
        model.terms.foldLeft(Right(VectorMap.empty): Either[ContrastError, VectorMap[String, CompiledContrast]]) {
          case (acc, (termKey, term)) =>
            model.contrastSetsByTerm.get(termKey) match
              case None => acc
              case Some(set) =>
                term match
                  case ct: ConvolvedTerm =>
                    for
                      out <- acc
                      local <- set.compileEither(ct)
                    yield
                      local.foldLeft(out) { case (acc0, (contrastName, compiled)) =>
                        val key = s"$termKey#$contrastName"
                        val embedded = compiled.toLegacy.embedIn(model.columnNames)
                        acc0.updated(
                          key,
                          compiled.copy(
                            id = ContrastId.unsafe(key),
                            effect = Some(EffectId.unsafe(contrastName)),
                            weights = TypedContrastWeights.fromLegacyDesign(embedded)
                          )
                        )
                      }
                  case other =>
                    Left(ContrastError.UnsupportedTerm(termKey, other.toString))
        }

    def validateAttachedContrasts(
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Vector[Validate.ContrastValidation] =
      unsafe(validateAttachedContrastsEither(tol = tol, rankTol = rankTol))

    def validateAttachedContrastsEither(
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Either[ContrastError, Vector[Validate.ContrastValidation]] =
      contrastWeightsEither.map { ws =>
        if ws.isEmpty then Vector.empty
        else
          ws.iterator.flatMap { case (key, cw) =>
            Validate.validateContrasts(
              design = model.designMatrix,
              columnNames = model.columnNames,
              weights = cw.weights,
              name = key,
              tol = tol,
              rankTol = rankTol
            )
          }.toVector.sortBy(_.name)
      }

    /** Model-level R-style F-contrasts for categorical predictors (main effects + interactions). */
    def fContrastWeights(maxInter: Int = 4): VectorMap[String, ContrastWeights] =
      FContrasts.fContrasts(model)(maxInter = maxInter)

    def compiledFContrastWeights(maxInter: Int = 4): Either[ContrastError, VectorMap[String, CompiledContrast]] =
      FContrasts.compiledFContrasts(model)(maxInter = maxInter)

    def validateFContrasts(
        maxInter: Int = 4,
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Vector[Validate.ContrastValidation] =
      val ws = fContrastWeights(maxInter = maxInter)
      if ws.isEmpty then Vector.empty
      else
        ws.iterator.flatMap { case (key, cw) =>
          Validate.validateContrasts(
            design = model.designMatrix,
            columnNames = model.columnNames,
            weights = cw.weights,
            name = key,
            tol = tol,
            rankTol = rankTol
          )
        }.toVector.sortBy(_.name)

    def validateAllContrasts(
        maxInter: Int = 4,
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Vector[Validate.ContrastValidation] =
      unsafe(validateAllContrastsEither(maxInter = maxInter, tol = tol, rankTol = rankTol))

    def validateAllContrastsEither(
        maxInter: Int = 4,
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Either[ContrastError, Vector[Validate.ContrastValidation]] =
      validateAttachedContrastsEither(tol = tol, rankTol = rankTol).map { attached =>
        val fcons = validateFContrasts(maxInter = maxInter, tol = tol, rankTol = rankTol)
        (attached ++ fcons).sortBy(_.name)
      }
