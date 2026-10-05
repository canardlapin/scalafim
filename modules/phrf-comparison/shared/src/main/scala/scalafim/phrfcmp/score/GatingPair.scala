package scalafim.phrfcmp.score

/** The 23 gating pairs of the pilot whitelist (12 condition, 9 trial, 2 T-G). A closed enum: no other pair can be
  * named, so no other pair can be reported.
  */
enum GatingPair(val cell: PilotCell, val comparator: Method):
  case CTX5Can extends GatingPair(PilotCell.CTX5, Method.Can)
  case CTX5Inf3 extends GatingPair(PilotCell.CTX5, Method.Inf3)
  case CTX5Fir extends GatingPair(PilotCell.CTX5, Method.Fir)
  case CTS1Can extends GatingPair(PilotCell.CTS1, Method.Can)
  case CTS1Inf3 extends GatingPair(PilotCell.CTS1, Method.Inf3)
  case CTS1Fir extends GatingPair(PilotCell.CTS1, Method.Fir)
  case CTS5Can extends GatingPair(PilotCell.CTS5, Method.Can)
  case CTS5Inf3 extends GatingPair(PilotCell.CTS5, Method.Inf3)
  case CTS5Fir extends GatingPair(PilotCell.CTS5, Method.Fir)
  case CTG5Can extends GatingPair(PilotCell.CTG5, Method.Can)
  case CTG5Inf3 extends GatingPair(PilotCell.CTG5, Method.Inf3)
  case CTG5Fir extends GatingPair(PilotCell.CTG5, Method.Fir)
  case TTXFastLsa extends GatingPair(PilotCell.TTXFast, Method.Lsa)
  case TTXFastLss extends GatingPair(PilotCell.TTXFast, Method.Lss)
  case TTXFastRlss extends GatingPair(PilotCell.TTXFast, Method.Rlss)
  case TTXJitLsa extends GatingPair(PilotCell.TTXJit, Method.Lsa)
  case TTXJitLss extends GatingPair(PilotCell.TTXJit, Method.Lss)
  case TTXJitRlss extends GatingPair(PilotCell.TTXJit, Method.Rlss)
  case TTSFastLsa extends GatingPair(PilotCell.TTSFast, Method.Lsa)
  case TTSFastLss extends GatingPair(PilotCell.TTSFast, Method.Lss)
  case TTSFastRlss extends GatingPair(PilotCell.TTSFast, Method.Rlss)
  case TTXFastGlmsD extends GatingPair(PilotCell.TTXFast, Method.GlmsD)
  case TTXJitGlmsD extends GatingPair(PilotCell.TTXJit, Method.GlmsD)

  /** The worst-in-family imputation pool of this pair. */
  def family: Family = (cell, comparator) match
    case (PilotCell.CTX5, _)                          => Family.CTx
    case (PilotCell.CTS1 | PilotCell.CTS5, _)         => Family.CTs
    case (PilotCell.CTG5, _)                          => Family.CTg
    case (_, Method.GlmsD)                            => Family.TG
    case (PilotCell.TTSFast, _)                       => Family.TTs
    case _                                            => Family.TTx

  def isCondition: Boolean = cell.isCondition

object GatingPair:
  val all: Vector[GatingPair] = GatingPair.values.toVector
