package scalafim.image

import image4s.Continuous
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.D3
import image4s.geometry.Frame
import ravel.Rank

/** Restores the closed continuous semantic refinement erased only by the
  * temporary `NeuroVol[Double]` and `NeuroVec[Double]` compatibility names.
  */
private[image] object ContinuousImageRefinement:
  inline def volume(
      value: NeuroVol[Double]
  ): Sampled[
    SampleSpace[Frame[D3], D3],
    Double,
    Continuous,
    Rank[3]
  ] =
    value.sampled.asInstanceOf[
      Sampled[
        SampleSpace[Frame[D3], D3],
        Double,
        Continuous,
        Rank[3]
      ]
    ]

  inline def series(
      value: NeuroVec[Double]
  ): Sampled[
    SampleSpace[Frame[D3], D3],
    Double,
    Continuous,
    Rank[4]
  ] =
    value.sampled.asInstanceOf[
      Sampled[
        SampleSpace[Frame[D3], D3],
        Double,
        Continuous,
        Rank[4]
      ]
    ]
