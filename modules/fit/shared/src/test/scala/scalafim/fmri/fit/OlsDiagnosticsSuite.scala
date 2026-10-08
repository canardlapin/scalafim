package scalafim.fmri.fit

import scalafim.fmri.design.RankToleranceConvention

class OlsDiagnosticsSuite extends munit.FunSuite:
  private def diagnostic(rank: Int, pivots: Vector[Int]): OlsDiagnostics =
    OlsDiagnostics(OlsSolveMethod.QrRankRevealing, 2, rank, OlsSolvePolicy.Default,
      RankDiagnostics.fromPivotedQr(2, rank, 1e-12, pivots, Vector(2.0, if rank == 2 then 1.0 else 0.0),
        RankToleranceConvention.ScaleAware))

  test("full-rank voxelwise factors remain compatible when whitening changes pivot order") {
    assert(diagnostic(2, Vector(0, 1)).structurallyCompatible(diagnostic(2, Vector(1, 0))))
  }

  test("different alias partitions and ranks cannot be merged") {
    assert(!diagnostic(1, Vector(0, 1)).structurallyCompatible(diagnostic(1, Vector(1, 0))))
    assert(!diagnostic(2, Vector(0, 1)).structurallyCompatible(diagnostic(1, Vector(0, 1))))
  }
