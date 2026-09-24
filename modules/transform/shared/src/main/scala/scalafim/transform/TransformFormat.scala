package scalafim.transform

/** Toolkit transform file formats ScalaFIM reads and writes. */
enum TransformFormat derives CanEqual:
  /** ITK/ANTs `#Insight Transform File` text (`.txt`, `.tfm`, text `.mat`). */
  case ItkText

  /** ITK/ANTs MATLAB v4 binary `.mat` (`*GenericAffine.mat`). */
  case ItkMatlab

  /** ITK/ANTs HDF5 transform or composite (`.h5`). */
  case ItkHdf5

  /** ITK/ANTs displacement field NIfTI (`*Warp.nii.gz`, 5D `(x,y,z,1,3)`, intent 1007). */
  case AntsDisplacementNifti

  /** FSL FLIRT 4x4 matrix in scaled-voxel coordinates. */
  case FslFlirt

  /** FSL FNIRT dense field (`--fout`, relative or absolute). */
  case FslFnirtField

  /** FSL FNIRT spline coefficients (`--cout`, intent 2007-2009). */
  case FslFnirtCoefficients

  /** AFNI `.aff12.1D` affine, one or more rows. */
  case AfniAff12

  /** AFNI 3dQwarp displacement NIfTI. */
  case AfniQwarp

  /** FreeSurfer linear transform array (`.lta`). */
  case FreeSurferLta

  /** MNI/FreeSurfer `.xfm` (e.g. `talairach.xfm`). */
  case FreeSurferXfm

  /** FreeSurfer `register.dat` (tkregister). */
  case FreeSurferRegisterDat

  /** X5 transform file (HDF5). */
  case X5

/** Formats deliberately not supported (ADR s5); detection names them so the refusal is explicit. */
enum UnsupportedFormat derives CanEqual:
  case ItkBSpline, AfniBrik, FreeSurferM3z, Elastix
