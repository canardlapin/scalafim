package scalafim.fmri.design.contrast

import scalafim.fmri.design.Validate
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel}

import scala.collection.immutable.VectorMap

object ContrastRegistry:

  extension (model: EventModel)

    def withContrastSet(termKey: String, set: ContrastSpec.ContrastSet, merge: Boolean = true): EventModel =
      val term = model.terms.collectFirst { case (k, t) if k == termKey => t }.getOrElse {
        throw new IllegalArgumentException(s"Unknown term key: '$termKey'")
      }
      term match
        case _: ConvolvedTerm => ()
        case other            => throw new IllegalArgumentException(s"Term '$termKey' does not support contrasts (found $other)")

      val merged =
        if merge then model.contrastSetsByTerm.get(termKey).map(_ ++ set).getOrElse(set)
        else set

      model.copy(contrastSetsByTerm = model.contrastSetsByTerm.updated(termKey, merged))

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
      if model.contrastSetsByTerm.isEmpty then VectorMap.empty
      else
        val out = VectorMap.newBuilder[String, ContrastWeights]
        var i = 0
        while i < model.terms.length do
          val (termKey, term) = model.terms(i)
          model.contrastSetsByTerm.get(termKey).foreach { set =>
            term match
              case ct: ConvolvedTerm =>
                val local = set.weights(ct)
                local.foreach { case (contrastName, cw) =>
                  out += (s"$termKey#$contrastName" -> cw.embedIn(model.columnNames))
                }
              case other =>
                throw new IllegalArgumentException(s"Term '$termKey' does not support contrasts (found $other)")
          }
          i += 1
        out.result()

    def validateAttachedContrasts(
        tol: Double = Validate.DefaultTol,
        rankTol: Double = Validate.DefaultRankTol
    ): Vector[Validate.ContrastValidation] =
      val ws = contrastWeights
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

    /** Model-level R-style F-contrasts for categorical predictors (main effects + interactions). */
    def fContrastWeights(maxInter: Int = 4): VectorMap[String, ContrastWeights] =
      FContrasts.fContrasts(model)(maxInter = maxInter)

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
      val attached = validateAttachedContrasts(tol = tol, rankTol = rankTol)
      val fcons = validateFContrasts(maxInter = maxInter, tol = tol, rankTol = rankTol)
      (attached ++ fcons).sortBy(_.name)
