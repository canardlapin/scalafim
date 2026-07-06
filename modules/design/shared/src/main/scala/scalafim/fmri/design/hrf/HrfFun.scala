package scalafim.fmri.design.hrf

import scalafim.fmri.design.data.DataTable
import scalafim.fmri.hrf.Hrf

/** Per-onset HRF generator (`hrf_fun=`). */
type HrfFun = DataTable => (Hrf | Seq[Hrf])
