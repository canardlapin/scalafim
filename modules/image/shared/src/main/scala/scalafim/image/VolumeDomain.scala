package scalafim.image

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.GridDomainError
import image4s.locus.GridDomainRecord
import image4s.locus.GridDomainResolution
import locus4s.DomainRegistry
import locus4s.FiniteDomain
import locus4s.Region
import locus4s.SpaceMismatch
import locus4s.data.Field

/** Direct image4s-locus domain for a D3 voxel grid.
  *
  * This is a type alias, not a ScalaFIM wrapper. Grid identity, canonical
  * ordinal layout, and the path-dependent voxel type are owned by
  * `image4s.locus.GridDomain`.
  */
type VolumeDomain[S] = GridDomain[? <: Frame[D3], D3, S]

/** Existential result of registering or restoring a D3 grid domain. */
type SomeVolumeDomain = GridDomainResolution[? <: Frame[D3], D3]

object VolumeDomain:
  /** Register the canonical versioned domain of this exact live grid. */
  def register(
      volumeSpace: VolumeSpace,
      domainName: String,
      registry: DomainRegistry
  ): Either[GridDomainError, SomeVolumeDomain] =
    GridDomain.register(
      volumeSpace.sampleSpace.grid,
      domainName,
      registry
    )

  /** Restore serialized grid-domain evidence against this exact live grid. */
  def restore(
      record: GridDomainRecord,
      volumeSpace: VolumeSpace,
      registry: DomainRegistry
  ): Either[GridDomainError, SomeVolumeDomain] =
    GridDomain.restore(
      record,
      volumeSpace.sampleSpace.grid,
      registry
    )

  extension [S](domain: VolumeDomain[S])
    /** Reconstruct a spatial-only sample space around the bridge's exact grid
      * owner. No geometry or storage is copied.
      */
    def volumeSpace: VolumeSpace =
      VolumeSpace.unsafe(
        NeuroSpace.fromCanonical(
          SampleSpace.create(domain.grid, NonSpatialAxes.empty)
        )
      )

    /** Zero-copy field exposure checked against the bridge's exact live grid
      * owner.
      */
    def fieldOf[A](
        volume: NeuroVol[A]
    ): Either[GridDomainError, Field[S, A]] =
      domain.spatialField(volume.sampled).map(identity)

    def supportWhere[A](
        field: Field[S, A]
    )(
        predicate: A => Boolean
    ): Either[SpaceMismatch, Region[S]] =
      checkSpace(domain, field.space).map: _ =>
        Region.tabulate(domain.space)(index => predicate(field(index)))

  private def checkSpace[S, T](
      domain: VolumeDomain[S],
      actual: FiniteDomain[T]
  ): Either[SpaceMismatch, Unit] =
    if domain.space.sameRuntimeOwnerAs(actual) then Right(())
    else Left(SpaceMismatch.between(domain.space, actual))
