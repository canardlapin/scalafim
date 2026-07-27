package scalafim.latent

import scalafim.archive.lna.SharedBasisId
import scalafim.image.NeuroSpace
import gale.linalg.{DMat, DVec}

class RadialBasisSuite extends munit.FunSuite:
  private val center = WorldCoordinate3D.unsafe(0.0, 0.0, 0.0)

  test("Gaussian kernel evaluates in millimeter-scaled distance") {
    val sigma = RadialSigmaMm.unsafe(2.0)
    val expected = math.exp(-0.5 * 0.5 * 0.5)
    val actual = RadialKernel.Gaussian.value(1.0, sigma).fold(err => fail(err.message), identity)

    assertEqualsDouble(actual, expected, 1e-15)
    assertEqualsDouble(
      RadialKernel.Gaussian.value(0.0, sigma).fold(err => fail(err.message), identity),
      1.0,
      0.0
    )
  }

  test("Wendland kernels have compact support and distinct C4/C6 formulas") {
    val sigma = RadialSigmaMm.unsafe(2.0)
    val r = 0.5
    val c4Expected = math.pow(1.0 - r, 6.0) * (35.0 * r * r + 18.0 * r + 3.0) / 3.0
    val c6Expected = math.pow(1.0 - r, 8.0) * (32.0 * r * r * r + 25.0 * r * r + 8.0 * r + 1.0)
    val c4 = RadialKernel.WendlandC4.value(1.0, sigma).fold(err => fail(err.message), identity)
    val c6 = RadialKernel.WendlandC6.value(1.0, sigma).fold(err => fail(err.message), identity)

    assertEqualsDouble(c4, c4Expected, 1e-15)
    assertEqualsDouble(c6, c6Expected, 1e-15)
    assert(math.abs(c4 - c6) > 1e-6)
    assertEqualsDouble(
      RadialKernel.WendlandC4.value(2.0, sigma).fold(err => fail(err.message), identity),
      0.0,
      0.0
    )
    assertEqualsDouble(
      RadialKernel.WendlandC6.value(3.0, sigma).fold(err => fail(err.message), identity),
      0.0,
      0.0
    )
  }

  test("explicit Gaussian atom loadings match fmrilatent HRBF fixture values") {
    val basis =
      RadialBasis
        .fromAtoms(
          atoms = Vector(
            RadialAtom(WorldCoordinate3D.unsafe(0.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0)),
            RadialAtom(WorldCoordinate3D.unsafe(2.0, 0.0, 0.0), RadialSigmaMm.unsafe(2.0))
          ),
          activeCoordinates = Vector(
            WorldCoordinate3D.unsafe(0.0, 0.0, 0.0),
            WorldCoordinate3D.unsafe(1.0, 0.0, 0.0),
            WorldCoordinate3D.unsafe(2.0, 0.0, 0.0)
          ),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)

    assertRowsEqual(
      basis.loadings.toRows,
      Vector(
        Vector(1.0000000000000000, 0.6065306597126334),
        Vector(0.6065306597126334, 0.8824969025845955),
        Vector(0.1353352832366127, 1.0000000000000000)
      ),
      1e-15
    )
  }

  test("Wendland atom loadings match fmrilatent HRBF fixture values") {
    val activeCoordinates =
      Vector(
        WorldCoordinate3D.unsafe(0.0, 0.0, 0.0),
        WorldCoordinate3D.unsafe(0.5, 0.0, 0.0),
        WorldCoordinate3D.unsafe(1.0, 0.0, 0.0),
        WorldCoordinate3D.unsafe(1.5, 0.0, 0.0)
      )
    val atom = Vector(RadialAtom(WorldCoordinate3D.unsafe(0.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0)))
    val c4 =
      RadialBasis
        .fromAtoms(atom, activeCoordinates, RadialKernel.WendlandC4)
        .fold(err => fail(err.message), identity)
    val c6 =
      RadialBasis
        .fromAtoms(atom, activeCoordinates, RadialKernel.WendlandC6)
        .fold(err => fail(err.message), identity)

    val combined =
      Vector.tabulate(activeCoordinates.length) { row =>
        Vector(c4.loadings(row, 0), c6.loadings(row, 0))
      }
    assertRowsEqual(
      combined,
      Vector(
        Vector(1.0, 1.0),
        Vector(0.1080729166666667, 0.0595703125000000),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      ),
      1e-15
    )
  }

  test("builds active-voxel by atom loadings for explicit atoms") {
    val basis =
      RadialBasis
        .fromAtoms(
          atoms = Vector(
            RadialAtom(center, RadialSigmaMm.unsafe(1.0)),
            RadialAtom(WorldCoordinate3D.unsafe(2.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0))
          ),
          activeCoordinates = Vector(
            WorldCoordinate3D.unsafe(0.0, 0.0, 0.0),
            WorldCoordinate3D.unsafe(1.0, 0.0, 0.0),
            WorldCoordinate3D.unsafe(2.0, 0.0, 0.0)
          ),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)

    assertEquals(basis.loadings.rows, 3)
    assertEquals(basis.loadings.cols, 2)
    assertEqualsDouble(basis.loadings(0, 0), 1.0, 0.0)
    assertEqualsDouble(basis.loadings(0, 1), math.exp(-2.0), 1e-15)
    assertEqualsDouble(basis.loadings(1, 0), math.exp(-0.5), 1e-15)
    assertEqualsDouble(basis.loadings(1, 1), math.exp(-0.5), 1e-15)
    assertEqualsDouble(basis.loadings(2, 0), math.exp(-2.0), 1e-15)
    assertEqualsDouble(basis.loadings(2, 1), 1.0, 0.0)
    assertEquals(basis.metadata("kernel"), "gaussian")
    assertEquals(basis.metadata("n_atoms"), "2")
  }

  test("thresholding zeros small values without changing matrix shape") {
    val basis =
      RadialBasis
        .fromAtoms(
          atoms = Vector(RadialAtom(center, RadialSigmaMm.unsafe(1.0))),
          activeCoordinates = Vector(
            WorldCoordinate3D.unsafe(0.0, 0.0, 0.0),
            WorldCoordinate3D.unsafe(3.0, 0.0, 0.0)
          ),
          kernel = RadialKernel.Gaussian,
          threshold = RadialValueThreshold.unsafe(0.05)
        )
        .fold(err => fail(err.message), identity)

    assertEquals(basis.loadings.rows, 2)
    assertEquals(basis.loadings.cols, 1)
    assertEqualsDouble(basis.loadings(0, 0), 1.0, 0.0)
    assertEqualsDouble(basis.loadings(1, 0), 0.0, 0.0)
    assertEquals(basis.metadata("threshold"), "0.05")
  }

  test("builds active coordinates from NeuroSpace and preserves sparse index order") {
    val space =
      NeuroSpace(
        dims = Vector(4, 1, 1),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0))
      )
    val atom = RadialAtom(WorldCoordinate3D.unsafe(10.0, 20.0, 30.0), RadialSigmaMm.unsafe(2.0))
    val basis =
      RadialBasis
        .fromSpaceIndices(
          atoms = Vector(atom),
          space = space,
          activeIndices = Vector(3, 1),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)

    assertEquals(basis.activeIndices, Some(Vector(3, 1)))
    assertEquals(basis.activeCoordinates.map(_.toVector), Vector(Vector(16.0, 20.0, 30.0), Vector(12.0, 20.0, 30.0)))
    assertEqualsDouble(basis.loadings(0, 0), math.exp(-4.5), 1e-15)
    assertEqualsDouble(basis.loadings(1, 0), math.exp(-0.5), 1e-15)
  }

  test("active voxel construction rejects duplicate and out-of-bounds indices") {
    val space = NeuroSpace(Vector(3, 1, 1))
    val duplicate = RadialActiveVoxels.fromIndices(space, Vector(1, 1))
    val outOfBounds = RadialActiveVoxels.fromIndices(space, Vector(3))

    assert(duplicate.isLeft)
    assert(duplicate.left.toOption.exists(_.message.contains("duplicate index 1")))
    assert(outOfBounds.isLeft)
    assert(outOfBounds.left.toOption.exists(_.message.contains("out of bounds")))
  }

  test("shared-basis artifact conversion canonicalizes rows to mask order") {
    val space =
      NeuroSpace(
        dims = Vector(4, 1, 1),
        spacing = Some(Vector(2.0, 1.0, 1.0)),
        origin = Some(Vector(0.0, 0.0, 0.0))
      )
    val basis =
      RadialBasis
        .fromSpaceIndices(
          atoms = Vector(RadialAtom(center, RadialSigmaMm.unsafe(2.0))),
          space = space,
          activeIndices = Vector(3, 1),
          kernel = RadialKernel.Gaussian,
          threshold = RadialValueThreshold.unsafe(0.01)
        )
        .fold(err => fail(err.message), identity)
    val artifact =
      basis
        .toSharedBasisArtifact(space.spatialDims, params = Map("source" -> "suite"))
        .fold(err => fail(err.message), identity)

    assertEquals(basis.activeIndices, Some(Vector(3, 1)))
    val order =
      basis
        .activeMaskOrder
        .fold(err => fail(err.message), identity)
    assertEquals(order.activeToFullGrid, Vector(3, 1))
    assertEquals(order.activeRowsInMaskOrder, Vector(1, 0))
    assertEquals(order.maskOrderSelection.ordinals.toVector, Vector(1, 0))
    assertEquals(order.maskValues(space.spatialDims.product), Right(Vector(false, true, false, true)))
    assertEquals(order.activeRowsForFullGrid(Vector(1, 3)), Right(Vector(1, 0)))
    assertEquals(
      order.vectorInActiveOrderFromMaskOrder(DVec.fromSeq(Vector(10.0, 20.0))).map(_.toVector),
      Right(Vector(20.0, 10.0))
    )
    assertEquals(artifact.kind, "hrbf")
    assertEquals(artifact.mask.values, Vector(false, true, false, true))
    assertEquals(artifact.params("source"), "suite")
    assertEquals(artifact.params("radial.kernel"), "gaussian")
    assertEquals(artifact.params("radial.threshold"), "0.01")
    assertEqualsDouble(artifact.loadings(0, 0), basis.loadings(1, 0), 0.0)
    assertEqualsDouble(artifact.loadings(1, 0), basis.loadings(0, 0), 0.0)
    assert(RadialMaskOrder.fromActiveIndices(Vector(1, 1)).isLeft)
  }

  test("radial locus order is an exact active-to-full injection with checked reverse") {
    val order =
      RadialMaskOrder
        .fromActiveIndices(Vector(3, 1))
        .fold(error => fail(error.message), identity)
    val locus =
      order
        .locus(maskSize = 4)
        .fold(error => fail(error.message), identity)

    assertEquals(
      locus.activeSelection.ordinals.toVector,
      Vector(3, 1)
    )
    assertEquals(
      locus.activeToFull.mapping.targetOrdinals.toVector,
      Vector(3, 1)
    )
    val firstActive = order.activeSpace.point(0).get
    assertEquals(locus.fullPointFor(firstActive).ordinal, 3)
    assertEquals(
      locus.activePointFor(locus.fullGridSpace.point(1).get).map(_.ordinal),
      Right(1)
    )
    assert(
      locus
        .activePointFor(locus.fullGridSpace.point(2).get)
        .left
        .exists(_.message.contains("not active"))
    )
  }

  test("radial basis encoder delegates through shared-basis projection and reconstructs data in span") {
    val space = NeuroSpace(Vector(3, 1, 1))
    val radial =
      RadialBasis
        .fromSpaceIndices(
          atoms = Vector(
            RadialAtom(WorldCoordinate3D.unsafe(0.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0)),
            RadialAtom(WorldCoordinate3D.unsafe(2.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0))
          ),
          space = space,
          activeIndices = Vector(0, 1, 2),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)
    val coefficients =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(-0.5, 0.25),
          Vector(3.0, -1.0)
        )
      )
    val data = LatentNumerics.multiply(coefficients, radial.loadings.transpose)
    val encoding =
      RadialBasisEncoder
        .encode(
          data = data,
          radialBasis = radial,
          maskDims = space.spatialDims,
          basisId = SharedBasisId.unsafe("hrbf_suite_basis"),
          center = false,
          metadata = Map("experiment" -> "synthetic")
        )
        .fold(err => fail(err.message), identity)
    val reconstructed =
      encoding.response
        .reconstruct()
        .fold(err => fail(err.message), identity)

    assertRowsEqual(reconstructed.toRows, data.toRows, 1e-12)
    assertRowsEqual(encoding.coefficients.toRows, coefficients.toRows, 1e-12)
    assertEquals(encoding.artifact.kind, "hrbf")
    assertEquals(encoding.artifact.params("radial.kernel"), "gaussian")
    assertEquals(encoding.response.metadata("basis.kind"), "hrbf")
    assertEquals(encoding.response.metadata("radial.kernel"), "gaussian")
    assertEquals(encoding.response.metadata("experiment"), "synthetic")
  }

  test("partial decode supports typed active voxel and time selections") {
    val space = NeuroSpace(Vector(4, 1, 1))
    val radial =
      RadialBasis
        .fromSpaceIndices(
          atoms = Vector(
            RadialAtom(WorldCoordinate3D.unsafe(0.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0)),
            RadialAtom(WorldCoordinate3D.unsafe(3.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0))
          ),
          space = space,
          activeIndices = Vector(0, 1, 2, 3),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)
    val coefficients =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(-0.5, 0.25),
          Vector(3.0, -1.0)
        )
      )
    val selection =
      RadialDecodeSelection
        .checked(timepoints = Some(Vector(2, 0)), activeVoxels = Some(Vector(3, 1)))
        .fold(err => fail(err.message), identity)
    val partial =
      radial.decode(coefficients, selection).fold(err => fail(err.message), identity)
    val full = LatentNumerics.multiply(coefficients, radial.loadings.transpose)

    assertRowsEqual(
      partial.toRows,
      Vector(
        Vector(full(2, 3), full(2, 1)),
        Vector(full(0, 3), full(0, 1))
      ),
      1e-12
    )
  }

  test("partial decode maps full-grid active voxel indices without materializing full output") {
    val space = NeuroSpace(Vector(4, 4, 2))
    val active =
      Vector(
        space.gridToIndex3D(0, 0, 0),
        space.gridToIndex3D(2, 3, 0),
        space.gridToIndex3D(3, 1, 1),
        space.gridToIndex3D(1, 2, 1)
      )
    val radial =
      RadialBasis
        .fromSpec(
          space = space,
          activeIndices = active,
          spec = RadialBasisSpec(sigma0 = 1.0, levels = 0, seed = 3L).fold(err => fail(err.message), identity)
        )
        .fold(err => fail(err.message), identity)
    val coefficients =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0, -0.5, 2.0),
          Vector(0.25, 3.0, 1.5, -1.0),
          Vector(-2.0, 0.5, 0.75, 1.0)
        )
      )
    val offset = DVec.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))
    val selection =
      RadialDecodeSelection
        .checked(timepoints = Some(Vector(1, 2)), fullGridVoxels = Some(Vector(active(3), active(1))))
        .fold(err => fail(err.message), identity)
    val partial =
      radial.decode(coefficients, selection, Some(offset)).fold(err => fail(err.message), identity)
    val full =
      radial
        .decode(coefficients, RadialDecodeSelection.All, Some(offset))
        .fold(err => fail(err.message), identity)

    assertRowsEqual(
      partial.toRows,
      Vector(
        Vector(full(1, 3), full(1, 1)),
        Vector(full(2, 3), full(2, 1))
      ),
      1e-12
    )
    assert(RadialDecodeSelection.checked(fullGridVoxels = Some(Vector(active(0), active(0)))).flatMap(radial.decode(coefficients, _)).isLeft)
    assert(RadialDecodeSelection.checked(fullGridVoxels = Some(Vector(space.gridToIndex3D(0, 1, 0)))).flatMap(radial.decode(coefficients, _)).isLeft)
  }

  test("radial encoding exposes partial decode with centered offsets") {
    val space = NeuroSpace(Vector(3, 1, 1))
    val active = Vector(2, 0, 1)
    val radial =
      RadialBasis
        .fromSpec(
          space = space,
          activeIndices = active,
          spec = RadialBasisSpec(sigma0 = 1.0, levels = 0, seed = 1L).fold(err => fail(err.message), identity)
        )
        .fold(err => fail(err.message), identity)
    val raw =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(2.0, 4.0, 6.0),
          Vector(4.0, 6.0, 8.0),
          Vector(6.0, 8.0, 10.0)
        )
      )
    val encoding =
      RadialBasisEncoder
        .encode(
          data = raw,
          radialBasis = radial,
          maskDims = space.spatialDims,
          basisId = SharedBasisId.unsafe("partial_decode_hrbf"),
          center = true
        )
        .fold(err => fail(err.message), identity)
    val selection =
      RadialDecodeSelection
        .checked(timepoints = Some(Vector(2, 0)), activeVoxels = Some(Vector(2, 0)))
        .fold(err => fail(err.message), identity)
    val partial = encoding.decode(selection).fold(err => fail(err.message), identity)

    assertEquals(encoding.offset.map(_.toVector), Some(Vector(4.0, 6.0, 8.0)))
    assertEquals(encoding.encoding.offset.map(_.toVector), Some(Vector(6.0, 8.0, 4.0)))
    assertRowsEqual(
      partial.toRows,
      Vector(
        Vector(raw(2, 2), raw(2, 0)),
        Vector(raw(0, 2), raw(0, 0))
      ),
      1e-8
    )
  }

  test("fmrilatent tiny sparse HRBF fixture roundtrips through encoder and archive") {
    val space = NeuroSpace(Vector(5, 5, 5))
    val active =
      Vector(
        space.gridToIndex3D(0, 0, 0),
        space.gridToIndex3D(2, 3, 1),
        space.gridToIndex3D(4, 1, 4),
        space.gridToIndex3D(1, 4, 3),
        space.gridToIndex3D(3, 2, 2)
      )
    val spec =
      RadialBasisSpec(sigma0 = 1.0, levels = 0, radiusFactor = 2.5, seed = 19L)
        .fold(err => fail(err.message), identity)
    val radial =
      RadialBasis
        .fromSpec(space, active, spec)
        .fold(err => fail(err.message), identity)
    val coefficients =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0, -1.0, 0.5, 3.0),
          Vector(-2.0, 0.25, 1.5, -0.75, 0.0)
        )
      )
    val data =
      radial.decode(coefficients).fold(err => fail(err.message), identity)
    val basisId = SharedBasisId.unsafe("fmrilatent_tiny_sparse_hrbf")
    val encoding =
      RadialBasisEncoder
        .encode(data, radial, space.spatialDims, basisId, center = false)
        .fold(err => fail(err.message), identity)

    assertRowsEqual(encoding.coefficients.toRows, coefficients.toRows, 1e-8)
    assertRowsEqual(encoding.decode().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-8)
    assertRowsEqual(encoding.response.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-8)

    val archive =
      LegacyLatentArchiveCodec
        .toRadialBasisArchive(data, space, radial, space.spatialDims, basisId, center = false)
        .fold(err => fail(err.message), identity)
    val decoded =
      LegacyLatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    decoded match
      case LatentArchiveResponse.SharedBasis(shared) =>
        val artifact =
          radial
            .toSharedBasisArtifact(space.spatialDims)
            .fold(err => fail(err.message), identity)
        val materialized =
          shared
            .materialize(artifact, Some(space))
            .fold(err => fail(err.message), identity)
        val dataInMaskOrder =
          radial
            .dataInMaskOrder(data)
            .fold(err => fail(err.message), identity)
        assertRowsEqual(materialized.reconstruct().fold(err => fail(err.message), identity).toRows, dataInMaskOrder.toRows, 1e-8)
      case other =>
        fail(s"expected shared-basis radial archive, found $other")
  }

  test("spec generation uses identity-like atoms for tiny sparse masks") {
    val space = NeuroSpace(Vector(5, 5, 5))
    val active =
      Vector(
        space.gridToIndex3D(0, 0, 0),
        space.gridToIndex3D(2, 3, 1),
        space.gridToIndex3D(4, 1, 4),
        space.gridToIndex3D(1, 4, 3),
        space.gridToIndex3D(3, 2, 2)
      )
    val spec =
      RadialBasisSpec(sigma0 = 1.0, levels = 0, seed = 19L)
        .fold(err => fail(err.message), identity)
    val basis =
      RadialBasis
        .fromSpec(space, active, spec)
        .fold(err => fail(err.message), identity)

    assertEquals(basis.activeIndices, Some(active))
    assertEquals(basis.nAtoms, active.length)
    assertEquals(basis.atomLevels.distinct, Vector(0))
    assert(basis.atoms.forall(_.sigma.value == 0.01))
    var i = 0
    while i < basis.nAtoms do
      assertEqualsDouble(basis.loadings(i, i), 1.0, 1e-12)
      assertEqualsDouble(columnNorm(basis.loadings, i), 1.0, 1e-12)
      i += 1
  }

  test("spec generation samples disconnected components independently") {
    val space = NeuroSpace(Vector(10, 1, 1))
    val active = Vector(0, 1, 8, 9)
    val spec =
      RadialBasisSpec(
        sigma0 = 10.0,
        levels = 0,
        radiusFactor = 10.0,
        seed = 5L,
        tinyMaskIdentity = false
      ).fold(err => fail(err.message), identity)
    val basis =
      RadialBasis
        .fromSpec(space, active, spec)
        .fold(err => fail(err.message), identity)
    val xs = basis.atoms.map(_.center.x).sorted

    assertEquals(basis.nAtoms, 2)
    assert(xs.head <= 1.0)
    assert(xs.last >= 8.0)
  }

  test("spec generation is deterministic and halves sigma by level") {
    val space = NeuroSpace(Vector(100, 1, 1))
    val active = 0 until 100
    val spec =
      RadialBasisSpec(
        sigma0 = 8.0,
        levels = 1,
        extraFineLevels = 1,
        radiusFactor = 2.0,
        seed = 42L,
        tinyMaskIdentity = false
      ).fold(err => fail(err.message), identity)
    val first = RadialBasis.fromSpec(space, active, spec).fold(err => fail(err.message), identity)
    val second = RadialBasis.fromSpec(space, active, spec).fold(err => fail(err.message), identity)
    val changedSeed =
      RadialBasis
        .fromSpec(space, active, RadialBasisSpec(sigma0 = 8.0, levels = 1, extraFineLevels = 1, radiusFactor = 2.0, seed = 43L, tinyMaskIdentity = false).fold(err => fail(err.message), identity))
        .fold(err => fail(err.message), identity)

    assertEquals(first.atoms.map(_.center.toVector), second.atoms.map(_.center.toVector))
    assertNotEquals(first.atoms.map(_.center.toVector), changedSeed.atoms.map(_.center.toVector))
    assertEquals(first.atomLevels.distinct.sorted, Vector(0, 1, 2))
    first.atoms.foreach { atom =>
      assertEqualsDouble(atom.sigma.value, 8.0 / math.pow(2.0, atom.level.toDouble), 1e-12)
    }
    assertMinimumLevelSpacing(first, spec)
  }

  test("validated scalar and coordinate constructors reject invalid inputs") {
    assert(RadialSigmaMm(0.0).isLeft)
    assert(RadialSigmaMm(Double.NaN).isLeft)
    assert(RadialValueThreshold(-0.1).isLeft)
    assert(RadialValueThreshold(Double.PositiveInfinity).isLeft)
    assert(RadialBasisSpec(sigma0 = 0.0).isLeft)
    assert(RadialBasisSpec(levels = -1).isLeft)
    assert(RadialBasisSpec(radiusFactor = 0.0).isLeft)
    assert(RadialBasisSpec(extraFineLevels = -1).isLeft)
    assert(RadialBasisSpec(threshold = -1.0).isLeft)
    assert(RadialBasisSpec(tinyMaskLimit = 0).isLeft)
    assert(RadialBasisSpec(tinySigmaScale = Double.NaN).isLeft)
    assert(WorldCoordinate3D(0.0, Double.NaN, 1.0).isLeft)
    assert(RadialKernel.Gaussian.value(-1.0, RadialSigmaMm.unsafe(1.0)).isLeft)
  }

  test("rejects empty atom or active-coordinate inputs") {
    val atom = RadialAtom(center, RadialSigmaMm.unsafe(1.0))
    assert(RadialBasis.fromAtoms(Vector.empty, Vector(center), RadialKernel.Gaussian).isLeft)
    assert(RadialBasis.fromAtoms(Vector(atom), Vector.empty, RadialKernel.Gaussian).isLeft)
    assert(RadialBasis.fromAtoms(Vector(atom), Vector(center), RadialKernel.Gaussian, activeIndices = Some(Vector(0, 1))).isLeft)
  }

  private def assertRowsEqual(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }

  private def columnNorm(matrix: DMat, col: Int): Double =
    var sum = 0.0
    var row = 0
    while row < matrix.rows do
      val value = matrix(row, col)
      sum += value * value
      row += 1
    math.sqrt(sum)

  private def assertMinimumLevelSpacing(
      basis: RadialBasis,
      spec: RadialBasisSpec
  ): Unit =
    spec.metadata
    val byLevel = basis.atoms.groupBy(_.level)
    byLevel.foreach { case (level, atoms) =>
      val radius = spec.radiusFactor.value * spec.sigma0.value / math.pow(2.0, level.toDouble)
      var i = 0
      while i < atoms.length do
        var j = i + 1
        while j < atoms.length do
          val distance = atoms(i).center.distanceTo(atoms(j).center)
          assert(distance + 1e-12 >= radius, clue = s"level $level atoms too close: $distance < $radius")
          j += 1
        i += 1
    }
