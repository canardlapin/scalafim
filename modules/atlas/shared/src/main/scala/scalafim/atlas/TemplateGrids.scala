package scalafim.atlas

import image4s.geometry.{Affine, D3, Grid, GridId}
import scalafim.image.GridSpec
import scalafim.image.world.Spaces as WorldFrames

/** Stock voxel grids of the MNI templates, exactly as TemplateFlow's `tpl-<template>_res-0N_*.nii.gz` headers place
  * them (sform = qform, code 4, RAS millimetres). Each grid is typed at its template's world frame, so a grid of one
  * template cannot be passed where another's is expected, and carries a persistent grid id
  * (`templateflow:tpl-<template>_res-0N`), so evidence recorded on it (e.g. a numerical inverse's domain) persists.
  */
object TemplateGrids:
  /** MNI152NLin2009cAsym `res-01`: 193 x 229 x 193 at 1 mm, first voxel at (-96, -132, -78). */
  val mni2009cRes1: GridSpec[WorldFrames.Mni2009c] =
    GridSpec.fromGrid(
      persistent("templateflow:tpl-MNI152NLin2009cAsym_res-01")(
        Grid.createPersistent[D3, WorldFrames.Mni2009c](_, WorldFrames.MNI152NLin2009cAsym)(Vector(193, 229, 193), cardinal(1.0, -96.0, -132.0, -78.0))
      )
    )

  /** MNI152NLin2009cAsym `res-02`: 97 x 115 x 97 at 2 mm, first voxel at (-96.5, -132.5, -78.5). */
  val mni2009cRes2: GridSpec[WorldFrames.Mni2009c] =
    GridSpec.fromGrid(
      persistent("templateflow:tpl-MNI152NLin2009cAsym_res-02")(
        Grid.createPersistent[D3, WorldFrames.Mni2009c](_, WorldFrames.MNI152NLin2009cAsym)(Vector(97, 115, 97), cardinal(2.0, -96.5, -132.5, -78.5))
      )
    )

  /** MNI152NLin6Asym `res-01`: 182 x 218 x 182 at 1 mm, first voxel at (-91, -126, -72). */
  val mni6Res1: GridSpec[WorldFrames.Mni6] =
    GridSpec.fromGrid(
      persistent("templateflow:tpl-MNI152NLin6Asym_res-01")(
        Grid.createPersistent[D3, WorldFrames.Mni6](_, WorldFrames.MNI152NLin6Asym)(Vector(182, 218, 182), cardinal(1.0, -91.0, -126.0, -72.0))
      )
    )

  /** MNI152NLin6Asym `res-02`: 91 x 109 x 91 at 2 mm, first voxel at (-90, -126, -72). */
  val mni6Res2: GridSpec[WorldFrames.Mni6] =
    GridSpec.fromGrid(
      persistent("templateflow:tpl-MNI152NLin6Asym_res-02")(
        Grid.createPersistent[D3, WorldFrames.Mni6](_, WorldFrames.MNI152NLin6Asym)(Vector(91, 109, 91), cardinal(2.0, -90.0, -126.0, -72.0))
      )
    )

  private def persistent[A](id: String)(create: GridId => Either[image4s.geometry.GeometryError, A]): A =
    GridId.parse(id).flatMap(create).fold(error => throw new IllegalStateException(error.message), identity)

  private def cardinal(spacing: Double, x: Double, y: Double, z: Double): Affine[D3] =
    Affine
      .fromRowMajor[D3](
        Vector(spacing, 0.0, 0.0, x, 0.0, spacing, 0.0, y, 0.0, 0.0, spacing, z, 0.0, 0.0, 0.0, 1.0)
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
