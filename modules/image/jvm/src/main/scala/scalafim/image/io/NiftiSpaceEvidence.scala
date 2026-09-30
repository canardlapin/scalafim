package scalafim.image.io

import image4s.geometry.{Affine, D3}
import image4s.nifti.{DecodedNifti, NiftiAffinePolicy, NiftiAffineSource}
import scalafim.image.world.{
  DatasetNamespace,
  GeometryDigest,
  NativeContext,
  ReferenceAcquisition,
  SessionId,
  SpaceError,
  SpaceEvidence,
  SubjectId,
  XformCode
}

/** World-space evidence carried by a NIfTI header: the xform code of the affine that the read policy selects.
  *
  * The other xform code is deliberately not evidence: when the policy chooses the sform, the qform code says nothing about
  * the coordinates the data were placed in.
  */
object NiftiSpaceEvidence:
  def fromHeader(header: NiftiHeader, policy: NiftiAffinePolicy): Either[SpaceError, SpaceEvidence] =
    fromSelection(header.native, selectedSource(header.native, policy)._1)

  /** Evidence for the affine a read actually selected, e.g. `DecodedNifti.affineSelection.source`. */
  def fromSelection(header: image4s.nifti.NiftiHeader, source: NiftiAffineSource): Either[SpaceError, SpaceEvidence] =
    source match
      case NiftiAffineSource.Sform    => XformCode.fromNifti(header.sformCode).map(code => SpaceEvidence(xform = Some(code)))
      case NiftiAffineSource.Qform    => XformCode.fromNifti(header.qformCode).map(code => SpaceEvidence(xform = Some(code)))
      case NiftiAffineSource.Fallback => Right(SpaceEvidence(xform = Some(XformCode.Unknown)))
      case NiftiAffineSource.Explicit => Right(SpaceEvidence())

  /** Lossless digest of the selected geometry, for anchoring a native [[scalafim.image.world.ReferenceAcquisition]]. */
  def geometry(header: NiftiHeader, policy: NiftiAffinePolicy): Either[SpaceError, GeometryDigest] =
    val native = header.native
    val (_, affine) = selectedSource(native, policy)
    GeometryDigest(native.spatialShape, affine.rowMajor, native.qformCode, native.sformCode)

  /** Lossless digest of the geometry a completed read placed its data with. */
  def geometry(decoded: DecodedNifti[?]): Either[SpaceError, GeometryDigest] =
    val native = decoded.header
    GeometryDigest(native.spatialShape, decoded.affineSelection.affine.rowMajor, native.qformCode, native.sformCode)

  /** A native context anchored by this file: its selected geometry plus the caller's BIDS identity. Use it when the
    * file is the reference acquisition (a T1w, or a boldref) of the native space it defines.
    */
  def nativeContext(
      header: NiftiHeader,
      policy: NiftiAffinePolicy,
      namespace: DatasetNamespace,
      subject: SubjectId,
      session: Option[SessionId],
      entities: Map[String, String]
  ): Either[SpaceError, NativeContext] =
    for
      digest    <- geometry(header, policy)
      reference <- ReferenceAcquisition(entities, digest)
    yield NativeContext(namespace, subject, session, reference)

  /** Which stored affine a read under `policy` places the data with.
    *
    * This mirrors the private selection in image4s-nifti, `image4s.nifti.Nifti.selectAffine`
    * (`modules/image4s-nifti/shared/src/main/scala/image4s/nifti/Nifti.scala` in image4s): sform before qform for
    * PreferSform and RequireAgreement, qform before sform for PreferQform, else the scaling fallback. Agreement failures
    * are the reader's concern and surface there, not as evidence. `NiftiSpaceEvidenceSuite` pins the mirror to the
    * reader's actual selection for every qform/sform/scaling case, so an upstream change fails a test instead of
    * drifting. Code that holds a `DecodedNifti` should use [[fromSelection]] with its recorded selection instead.
    */
  private[io] def selectedSource(header: image4s.nifti.NiftiHeader, policy: NiftiAffinePolicy): (NiftiAffineSource, Affine[D3]) =
    def sformFirst =
      header.sform.map(NiftiAffineSource.Sform -> _)
        .orElse(header.qform.map(NiftiAffineSource.Qform -> _))
        .getOrElse(NiftiAffineSource.Fallback -> header.fallbackAffine)
    policy match
      case NiftiAffinePolicy.PreferSform | NiftiAffinePolicy.RequireAgreement(_) => sformFirst
      case NiftiAffinePolicy.PreferQform =>
        header.qform.map(NiftiAffineSource.Qform -> _)
          .orElse(header.sform.map(NiftiAffineSource.Sform -> _))
          .getOrElse(NiftiAffineSource.Fallback -> header.fallbackAffine)
      case NiftiAffinePolicy.UseExplicit(affine) => NiftiAffineSource.Explicit -> affine
