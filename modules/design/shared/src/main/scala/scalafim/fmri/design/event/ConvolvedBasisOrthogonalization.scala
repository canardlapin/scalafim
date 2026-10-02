package scalafim.fmri.design.event

import gale.linalg.DMat
import gale.spectral.SpectralBackend.given
import scalafim.fmri.design.{BasisOrthogonalizationGroupReceipt, BasisOrthogonalizationReceipt, DesignError, HrfColumnScale}
import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.linalg.Mat

/** Serially orthogonalizes basis columns on the realized, scaled design.
  * The recorded transform maps the original scaled columns to the returned
  * columns; effective HRFs transport that transform back through the original
  * column divisors for structural response readout.
  */
object ConvolvedBasisOrthogonalization:
  def apply(term: ConvolvedTerm): Either[DesignError, (ConvolvedTerm, Vector[BasisOrthogonalizationReceipt])] =
    if term.eventHrfs.nonEmpty then Left(DesignError.InvalidSchema("basis orthogonalization requires a shared HRF per column group, not per-event HRFs"))
    else if term.eventPeakScales.nonEmpty then Left(DesignError.InvalidSchema("basis orthogonalization does not yet support per-event peak scales"))
    else if term.data.data.exists(value => !value.isFinite) then Left(DesignError.InvalidSchema("basis orthogonalization requires finite design values"))
    else if term.columnCells.length != term.data.cols || term.columnModulators.length != term.data.cols then Left(DesignError.InvalidSchema("basis orthogonalization requires cell and modulator metadata"))
    else if term.columnBasisIx.length != term.data.cols then Left(DesignError.InvalidSchema("basis orthogonalization requires basis-column metadata"))
    else
      val groups = term.columnCells.indices.groupBy(i => term.columnCells(i) -> term.columnModulators(i)).toVector.sortBy { case ((cell, modulator), _) =>
        cell.fold("")(_.canonical) -> modulator.fold("")(_.value)
      }
      val output = term.data.data.clone()
      val effectiveHrfs = term.columnHrfs.padTo(term.data.cols, term.hrf).toArray
      val effectiveScales = term.columnScales.padTo(term.data.cols, HrfColumnScale.identity).toArray
      val receipts = Vector.newBuilder[BasisOrthogonalizationGroupReceipt]
      var failed: Option[DesignError] = None
      groups.foreach { case ((cell, modulator), raw) =>
        val columns = raw.toVector.sortBy(i => term.columnBasisIx(i).getOrElse(Int.MaxValue))
        if columns.lengthCompare(2) >= 0 && failed.isEmpty then
          val hrf = term.hrfForColumn(columns.head)
          val basis = hrf.basisElementsValidated.left.map(error => DesignError.InvalidSchema(error.message))
          if basis.isLeft || columns.length != hrf.nbasis || columns.map(i => term.columnBasisIx(i)).toSet != (1 to hrf.nbasis).map(Some(_)).toSet then
            failed = Some(basis.fold(identity, _ => DesignError.InvalidSchema("basis orthogonalization requires each basis index exactly once")))
          else if columns.exists(i => term.hrfForColumn(i).descriptor != hrf.descriptor) || columns.exists(i => term.hrfForColumn(i).nbasis != hrf.nbasis) then
            failed = Some(DesignError.InvalidSchema("basis orthogonalization group must share one source HRF and width"))
          else
            val p = columns.length
            val transform = Array.tabulate(p * p)(i => if i / p == i % p then 1.0 else 0.0)
            var target = 1
            var finalRank = 0
            while target < p && failed.isEmpty do
              val refs = (0 until target).toVector
              val x = DMat.tabulate(term.data.rows, refs.length)((r, c) => output(r * term.data.cols + columns(refs(c))))
              val y = DMat.tabulate(term.data.rows, 1)((r, _) => output(r * term.data.cols + columns(target)))
              x.svd match
                case Left(error) => failed = Some(DesignError.BuildFailed(error.toString))
                case Right(svd) =>
                  val sigmaMax = if svd.size == 0 then 0.0 else svd.singularValues(0)
                  val cutoff = math.max(x.rows, x.cols).toDouble * sigmaMax * 2.220446049250313e-16
                  val kept = (0 until svd.size).filter(i => svd.singularValues(i) > cutoff).toVector
                  finalRank = kept.length
                  val beta = refs.indices.map { c => kept.map { k =>
                    var dot = 0.0; var r = 0
                    while r < x.rows do { dot += svd.u(r, k) * y(r, 0); r += 1 }
                    svd.vt(k, c) * dot / svd.singularValues(k)
                  }.sum }.toVector
                  if beta.exists(value => !value.isFinite) then failed = Some(DesignError.InvalidSchema("basis orthogonalization produced non-finite coefficients"))
                  var r = 0
                  while r < term.data.rows do
                    var projection = 0.0; var c = 0
                    while c < refs.length do { projection += x(r, c) * beta(c); c += 1 }
                    output(r * term.data.cols + columns(target)) = y(r, 0) - projection
                    r += 1
                  var base = 0
                  while base < p do
                    var value = if base == target then 1.0 else 0.0
                    var c = 0
                    while c < refs.length do { value -= transform(base * p + refs(c)) * beta(c); c += 1 }
                    transform(base * p + target) = value
                    base += 1
              target += 1
            val divisors = columns.map(i => term.scaleForColumn(i).divisor)
            val weights = Vector.tabulate(p)(col => Vector.tabulate(p)(row => transform(row * p + col) / divisors(row)))
            if transform.exists(value => !value.isFinite) || weights.flatten.exists(value => !value.isFinite) || output.exists(value => !value.isFinite) then
              failed = Some(DesignError.InvalidSchema("basis orthogonalization produced a non-finite transform or design"))
            else if failed.isEmpty then
              val effective = weights.zipWithIndex.map { (values, column) =>
                hrf.withCoefficients(values.toArray, Some(s"${hrf.name}_orth_${column + 1}"))
              }
              val bound = bindBasis(effective, Some(s"${hrf.name}_orth"))
              columns.foreach { i => effectiveHrfs(i) = bound; effectiveScales(i) = HrfColumnScale.identity }
              receipts += BasisOrthogonalizationGroupReceipt(cell, modulator, columns, basis.toOption.get.map(_.id.value), (0 until term.data.rows).toVector, columns.map(term.scaleForColumn), transform.toVector, finalRank)

      }
      failed.fold[Either[DesignError, (ConvolvedTerm, Vector[BasisOrthogonalizationReceipt])]](
        if receipts.result().isEmpty then Left(DesignError.FormulaBinding("basis orthogonalization requires a stream with at least two basis columns"))
        else
          val groups0 = receipts.result()
          val changed = term.copy(hrf = effectiveHrfs.headOption.getOrElse(term.hrf), data = Mat.unsafe(term.data.rows, term.data.cols, output), columnHrfs = effectiveHrfs.toVector, columnScales = effectiveScales.toVector)
          Right(changed -> Vector(BasisOrthogonalizationReceipt(term.term.termTag.flatMap(scalafim.fmri.design.TermId(_).toOption), groups0)))
      )(Left(_))
