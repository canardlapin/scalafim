package scalafim.image

import locus4s.DomainRegistry

/** Compatibility name for the checked image4s-locus ordinal bridge.
  *
  * There is no second ScalaFIM volume-domain implementation or identity.
  */
type VolumeDomain[S] = VolumeOrdinalBridge[S, ?]

object VolumeDomain:
  def canonical(
      volumeSpace: VolumeSpace
  ): SomeVolumeDomain =
    canonicalIn(DomainRegistry.empty, volumeSpace)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def canonicalIn(
      registry: DomainRegistry,
      volumeSpace: VolumeSpace
  ): Either[VolumeOrdinalBridgeError, SomeVolumeDomain] =
    VolumeOrdinalBridge.register(volumeSpace, registry)

type SomeVolumeDomain = VolumeOrdinalBridgeResolution

object SomeVolumeDomain:
  def canonical(
      volumeSpace: VolumeSpace
  ): SomeVolumeDomain =
    VolumeDomain.canonical(volumeSpace)

  def canonicalIn(
      registry: DomainRegistry,
      volumeSpace: VolumeSpace
  ): Either[VolumeOrdinalBridgeError, SomeVolumeDomain] =
    VolumeOrdinalBridge.register(volumeSpace, registry)
