package scalafim.fmri.design.hrf

import scalafim.fmri.design.data.DataTable
import scalafim.fmri.hrf.Hrf

enum HrfSelection:
  case Shared(hrf: Hrf)
  case PerEvent(hrfs: Vector[Hrf])

object HrfSelection:
  def perEvent(hrfs: Seq[Hrf]): HrfSelection =
    PerEvent(hrfs.toVector)

/** Per-onset HRF generator (`hrf_fun=`). */
type HrfFun = DataTable => HrfSelection

object HrfFun:
  def shared(f: DataTable => Hrf): HrfFun =
    d => HrfSelection.Shared(f(d))

  def perEvent(f: DataTable => Seq[Hrf]): HrfFun =
    d => HrfSelection.perEvent(f(d))
