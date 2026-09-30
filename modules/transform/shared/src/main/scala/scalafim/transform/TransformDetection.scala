package scalafim.transform

import scalafim.transform.nifti.NiftiRaw

/** Identifies a transform file from its content; a file name only breaks ties, never overrides content. Content that
  * fits several formats is a typed [[TransformIoError.Ambiguous]] rather than a guess.
  */
object TransformDetection:
  def detect(source: TransformSource, fileName: Option[String] = None): Either[TransformIoError, TransformFormat] =
    val name = fileName.map(_.toLowerCase).getOrElse("")
    unsupportedByName(name) match
      case Some(error) => Left(error)
      case None =>
        source match
          case TransformSource.Text(text)    => detectText(text, name)
          case TransformSource.Binary(bytes) => detectBinary(bytes, name)

  private def unsupportedByName(name: String): Option[TransformIoError] =
    if name.endsWith(".brik") || name.endsWith(".head") || name.endsWith(".brik.gz") then
      Some(TransformIoError.Unsupported(UnsupportedFormat.AfniBrik, "AFNI BRIK/HEAD datasets are out of scope; convert warps to NIfTI"))
    else if name.endsWith(".m3z") then Some(TransformIoError.Unsupported(UnsupportedFormat.FreeSurferM3z, "FreeSurfer .m3z morphs are out of scope"))
    else None

  private def detectText(text: String, name: String): Either[TransformIoError, TransformFormat] =
    val lines = text.linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    val content = lines.filterNot(_.startsWith("#"))
    def numeric(line: String): Option[Vector[Double]] =
      val tokens = line.split("\\s+").toVector.filter(_.nonEmpty)
      val parsed = tokens.flatMap(_.toDoubleOption)
      Option.when(tokens.nonEmpty && parsed.size == tokens.size)(parsed)
    if text.contains("#Insight Transform File") then Right(TransformFormat.ItkText)
    else if lines.headOption.exists(_.startsWith("MNI Transform File")) then Right(TransformFormat.FreeSurferXfm)
    else if content.exists(_.startsWith("(Transform ")) then
      Left(TransformIoError.Unsupported(UnsupportedFormat.Elastix, "elastix parameter files are out of scope"))
    else if content.exists(l => l.matches("type\\s*=\\s*\\d+.*")) && content.exists(l => l.startsWith("nxforms") || l.startsWith("mean")) then
      Right(TransformFormat.FreeSurferLta)
    else
      val rows = content.map(numeric)
      if rows.nonEmpty && rows.forall(_.exists(_.size == 12)) then Right(TransformFormat.AfniAff12)
      else if rows.size == 4 && rows.forall(_.exists(_.size == 4)) then Right(TransformFormat.FslFlirt)
      else if isRegisterDat(content, numeric) then Right(TransformFormat.FreeSurferRegisterDat)
      else Left(TransformIoError.Undetectable(s"text does not match any transform format${if name.nonEmpty then s" ($name)" else ""}"))

  /** subject name, in-plane resolution, between-plane resolution, intensity, 4x4 matrix, optional `round`/`tkregister`. */
  private def isRegisterDat(lines: Vector[String], numeric: String => Option[Vector[Double]]): Boolean =
    lines.size >= 8 &&
      numeric(lines(0)).isEmpty &&
      (1 to 3).forall(i => numeric(lines(i)).exists(_.size == 1)) &&
      (4 to 7).forall(i => numeric(lines(i)).exists(_.size == 4))

  private def detectBinary(bytes: IArray[Byte], name: String): Either[TransformIoError, TransformFormat] =
    if bytes.length >= 2 && (bytes(0) & 0xff) == 0x1f && (bytes(1) & 0xff) == 0x8b then
      Left(TransformIoError.Undetectable("gzip-compressed content; decompress before detection"))
    else if isHdf5(bytes) then
      if name.endsWith(".x5") then Right(TransformFormat.X5)
      else if name.endsWith(".h5") || name.endsWith(".hdf5") then Right(TransformFormat.ItkHdf5)
      else Left(TransformIoError.Ambiguous(Vector(TransformFormat.ItkHdf5, TransformFormat.X5), "HDF5 container; inspect its groups or name the file .h5/.x5"))
    else if isMatlabV4(bytes) then Right(TransformFormat.ItkMatlab)
    else
      NiftiRaw.parse(bytes) match
        case Right(nifti) => detectNifti(nifti, name)
        case Left(_) =>
          // text formats sometimes arrive as bytes
          val text = String(IArray.genericWrapArray(bytes).toArray, "UTF-8")
          if text.forall(c => c == '\n' || c == '\r' || c == '\t' || (c >= ' ' && c < 127) || c > 127) then detectText(text, name)
          else Left(TransformIoError.Undetectable("binary content is not NIfTI-1, HDF5 or MATLAB v4"))

  private def detectNifti(nifti: NiftiRaw, name: String): Either[TransformIoError, TransformFormat] =
    val shape = nifti.shape
    nifti.intentCode match
      // FSL intent codes (fslpy fsl.data.constants): 2006 FNIRT displacement field, 2007 cubic and 2009 quadratic spline
      // coefficients, 2008 DCT coefficients, 2016/2017 TOPUP spline coefficients, 2018 TOPUP field.
      case 2006        => Right(TransformFormat.FslFnirtField)
      case 2007 | 2009 => Right(TransformFormat.FslFnirtCoefficients)
      case 2008 =>
        Left(TransformIoError.Unsupported(UnsupportedFormat.FslDctCoefficients, "FNIRT DCT-basis coefficients (intent 2008) are out of scope"))
      case 2016 | 2017 | 2018 =>
        Left(TransformIoError.Unsupported(UnsupportedFormat.FslTopup, s"TOPUP intent ${nifti.intentCode} is an off-resonance field, not a spatial transform"))
      case _ if shape.size == 5 && shape(3) == 1 && shape(4) == 3 =>
        // 3dQwarp writes <prefix>_WARP.nii; ANTs writes <prefix>[0-9]Warp.nii.gz / InverseWarp.nii.gz.
        if name.contains("qwarp") || name.contains("_warp.") then Right(TransformFormat.AfniQwarp)
        else if name.contains("warp") then Right(TransformFormat.AntsDisplacementNifti)
        else
          Left(TransformIoError.Ambiguous(Vector(TransformFormat.AntsDisplacementNifti, TransformFormat.AfniQwarp), "5D (x,y,z,1,3) displacement field: ANTs and AFNI write the same layout and LPS displacement convention but choose between qform and sform differently; name the file or state the format"))
      case _ if shape.size == 4 && shape(3) == 3 => Right(TransformFormat.FslFnirtField)
      case other =>
        Left(TransformIoError.Undetectable(s"NIfTI with intent $other and shape ${shape.mkString("x")} is not a transform container"))

  private def isHdf5(bytes: IArray[Byte]): Boolean =
    val signature = Array[Int](0x89, 'H', 'D', 'F', 0x0d, 0x0a, 0x1a, 0x0a)
    bytes.length >= 8 && signature.indices.forall(i => (bytes(i) & 0xff) == signature(i))

  /** MATLAB v4 matrix header: type (MOPT), rows, cols, imagf, name length, then an ASCII variable name. */
  private def isMatlabV4(bytes: IArray[Byte]): Boolean =
    def int32(at: Int, le: Boolean): Int =
      if le then (bytes(at) & 0xff) | ((bytes(at + 1) & 0xff) << 8) | ((bytes(at + 2) & 0xff) << 16) | ((bytes(at + 3) & 0xff) << 24)
      else ((bytes(at) & 0xff) << 24) | ((bytes(at + 1) & 0xff) << 16) | ((bytes(at + 2) & 0xff) << 8) | (bytes(at + 3) & 0xff)
    bytes.length >= 20 && Vector(true, false).exists: le =>
      val mopt = int32(0, le)
      val rows = int32(4, le)
      val cols = int32(8, le)
      val imagf = int32(12, le)
      val nameLength = int32(16, le)
      mopt >= 0 && mopt < 5000 && (mopt / 1000) <= 4 && rows >= 0 && rows < 1_000_000 && cols >= 0 && cols < 1_000_000 &&
        (imagf == 0 || imagf == 1) && nameLength > 1 && nameLength < 256 && bytes.length >= 20 + nameLength &&
        (0 until nameLength - 1).forall(i => { val c = bytes(20 + i) & 0xff; c >= 32 && c < 127 })
