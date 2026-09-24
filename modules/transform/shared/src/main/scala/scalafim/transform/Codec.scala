package scalafim.transform

import image4s.geometry.{D3, Frame}
import scalafim.image.world.{FreeSurferVolumeGeometry, FslVolumeGeometry}

/** File content handed to a codec. Decompression and HDF5 parsing are platform container concerns; what reaches a
  * codec is already platform-neutral.
  */
enum TransformSource:
  case Text(content: String)
  case Binary(bytes: IArray[Byte])

/** A lossless model of one toolkit file format: `decode` then `encode` preserves every value (not lexical layout). */
trait TransformCodec[N]:
  def format: TransformFormat
  def decode(source: TransformSource): Either[TransformIoError, N]
  def encode(native: N): Either[TransformIoError, TransformSource]

/** What a native file means, given the context its format needs. `Ctx` and `Out` are indexed by both endpoint frames,
  * so a context built for other frames does not type-check.
  */
trait Interpretation[N, Ctx[_ <: Frame[D3], _ <: Frame[D3]], Out[_ <: Frame[D3], _ <: Frame[D3]]]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](native: N, context: Ctx[S, T]): Either[TransformError, Out[S, T]]

/** Writing a world transform into a format. Separate from [[Interpretation]]: not every transform is expressible in
  * every format (see the D4b conversion matrix in the plan).
  */
trait Expression[N, Ctx[_ <: Frame[D3], _ <: Frame[D3]], In[_ <: Frame[D3], _ <: Frame[D3]]]:
  def express[S <: Frame[D3], T <: Frame[D3]](transform: In[S, T], context: Ctx[S, T]): Either[TransformError, N]

/** Endpoint frames alone: formats whose files carry all geometry they need (ITK, LTA, xfm, X5, AFNI warps). */
final case class Frames[S <: Frame[D3], T <: Frame[D3]](source: S, target: T)

/** FLIRT and FNIRT: FSL's own geometry of the source (input) and reference volumes, each tied to its world frame. */
final case class FslGrids[S <: Frame[D3], T <: Frame[D3]](
    source: S,
    sourceGeometry: FslVolumeGeometry,
    reference: T,
    referenceGeometry: FslVolumeGeometry
)

/** Whether to undo AFNI's oblique "cardinal" coordinate convention when reading `.aff12.1D` affines. */
enum CardinalCorrection derives CanEqual:
  case Off
  case On(sourceObliquity: Option[AfniObliquity], baseObliquity: Option[AfniObliquity])

/** An AFNI dataset's cardinal-to-real mapping (its `IJK_TO_DICOM_REAL` relative to `IJK_TO_DICOM`). */
final case class AfniObliquity(cardinalToReal: image4s.geometry.Affine[D3])

final case class AfniContext[S <: Frame[D3], T <: Frame[D3]](frames: Frames[S, T], correction: CardinalCorrection)

/** tkregister `register.dat`: FreeSurfer geometry of the movable and target volumes. */
final case class TkRegGrids[S <: Frame[D3], T <: Frame[D3]](
    movable: S,
    movableGeometry: FreeSurferVolumeGeometry,
    target: T,
    targetGeometry: FreeSurferVolumeGeometry
)

/** A series of affines sharing endpoints, e.g. AFNI volreg or MCFLIRT output: element `i` maps volume `i` to the base. */
final case class LinearSeries[S <: Frame[D3], T <: Frame[D3]](transforms: Vector[WorldTransform.Linear[S, T]])

/** A chain whose intermediate stages stay visible for provenance, e.g. ITK composites and multi-node X5 files.
  * `composed` is the whole chain as one transform.
  */
final case class TransformChain[S <: Frame[D3], T <: Frame[D3]](stages: Vector[TransformChain.Stage], composed: WorldTransform[S, T])

object TransformChain:
  final case class Stage(kind: String, provenance: TransformProvenance)
