package scalafim

/** Toolkit spatial-transform formats and their interpretation as typed world-space transforms.
  *
  * Frame identity and coordinate conventions live in `scalafim.image.world`; generic transform
  * algebra (composition, inversion, dense maps, resampling) is owned by reframe4s. This package
  * holds the neuroimaging layer between them: faithful models of ITK/ANTs, FSL, AFNI, FreeSurfer
  * and X5 files, what those files mean, and conversion between them.
  */
package object transform
