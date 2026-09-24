package scalafim.image.io

import image4s.geometry.{Affine, D3}
import image4s.nifti.{NiftiAffinePolicy, NiftiAffineSource}
import scalafim.image.world.{GeometryDigest, SpaceError, SpaceEvidence, XformCode}

/** World-space evidence carried by a NIfTI header: the xform code of the affine that the read policy selects.
  *
  * The other xform code is deliberately not evidence: when the policy chooses the sform, the qform code says nothing about
  * the coordinates the data were placed in.
  */
object NiftiSpaceEvidence:
  def fromHeader(header: NiftiHeader, policy: NiftiAffinePolicy): Either[SpaceError, SpaceEvidence] =
    val native = header.native
    selectedSource(native, policy) match
      case (NiftiAffineSource.Sform, _)    => XformCode.fromNifti(native.sformCode).map(code => SpaceEvidence(xform = Some(code)))
      case (NiftiAffineSource.Qform, _)    => XformCode.fromNifti(native.qformCode).map(code => SpaceEvidence(xform = Some(code)))
      case (NiftiAffineSource.Fallback, _) => Right(SpaceEvidence(xform = Some(XformCode.Unknown)))
      case (NiftiAffineSource.Explicit, _) => Right(SpaceEvidence())

  /** Lossless digest of the selected geometry, for anchoring a native [[scalafim.image.world.ReferenceAcquisition]]. */
  def geometry(header: NiftiHeader, policy: NiftiAffinePolicy): Either[SpaceError, GeometryDigest] =
    val native = header.native
    val (_, affine) = selectedSource(native, policy)
    GeometryDigest(native.spatialShape, affine.rowMajor, native.qformCode, native.sformCode)

  /** Which stored affine a read under `policy` places the data with. Mirrors image4s-nifti's (private) selection order:
    * sform before qform for PreferSform and RequireAgreement, qform before sform for PreferQform, else the fallback.
    * Agreement failures are the reader's concern and surface there, not as evidence.
    */
  private def selectedSource(header: image4s.nifti.NiftiHeader, policy: NiftiAffinePolicy): (NiftiAffineSource, Affine[D3]) =
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
