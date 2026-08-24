package scalafim.dataset

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.GridCongruence

/** Bind provider congruence evidence to the exact erased grid operands.
  *
  * This checks only certificate endpoint identity. Coordinate comparison remains exclusively in
  * image4s `Grid.exactCongruence` and `Grid.approximateCongruence`.
  */
private[dataset] def bindGridCongruence(
    congruence: GridCongruence[D3, ? <: Frame[D3], ? <: Frame[D3]],
    expectedLeft: Grid[? <: Frame[D3], D3],
    expectedRight: Grid[? <: Frame[D3], D3]
): Either[DatasetError, Unit] =
  if !(congruence.left eq expectedLeft) then
    Left(DatasetError.CongruenceEndpointMismatch(CongruenceEndpoint.Left))
  else if !(congruence.right eq expectedRight) then
    Left(DatasetError.CongruenceEndpointMismatch(CongruenceEndpoint.Right))
  else Right(())
