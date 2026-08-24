package scalafim.image

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import locus4s.FiniteDomain
import locus4s.Region
import locus4s.SpaceMismatch
import locus4s.data.Field

/** Neuroimaging algorithms over the provider-owned exact grid domain. */
object GridDomainOps:
  extension [F <: Frame[D3], S](domain: GridDomain[F, D3, S])
    /** Spatial-only sample-space view of the exact live grid owner. */
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(
        SampleSpaces.fromCanonical(
          SampleSpace.create(domain.grid, NonSpatialAxes.empty)
        )
      )

    /** Zero-copy field exposure checked against the exact live grid owner. */
    def fieldOf[A, Sem](
        volume: SomeNeuroVolume[A, Sem]
    ): Either[GridDomainError, Field[S, A]] =
      domain.spatialField(volume.sampled).map(identity)

    def supportWhere[A](
        field: Field[S, A]
    )(
        predicate: A => Boolean
    ): Either[SpaceMismatch, Region[S]] =
      checkSpace(domain.space, field.space).map: _ =>
        Region.tabulate(domain.space)(index => predicate(field(index)))

  private def checkSpace[S, T](
      expected: FiniteDomain[S],
      actual: FiniteDomain[T]
  ): Either[SpaceMismatch, Unit] =
    if expected.sameRuntimeOwnerAs(actual) then Right(())
    else Left(SpaceMismatch.between(expected, actual))
