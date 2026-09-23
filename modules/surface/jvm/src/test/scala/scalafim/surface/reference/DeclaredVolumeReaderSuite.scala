package scalafim.surface.reference

import image4s.geometry.{Affine, D3}
import scalafim.image.*
import scalafim.image.SampleSpaces.*
import scalafim.image.io.Nifti

import java.nio.file.{Files, Path}

class DeclaredVolumeReaderSuite extends munit.FunSuite:
  private val nlin2009c = TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-24.2.0")
  private val affine = Affine.fromRowMajor[D3](Vector(
    -2.0, 0.0, 0.0, 90.0, 0.0, 2.0, 0.0, -126.0, 0.0, 0.0, 2.0, -72.0, 0.0, 0.0, 0.0, 1.0)).toOption.get
  private val space = SampleSpaces(Vector(3, 4, 5), affine = Some(affine))
  private val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.tabulate(60) { ordinal =>
    val g = space.indexToGrid3D(ordinal)
    (g(0) + 10 * g(1) + 100 * g(2)).toDouble
  }, space, "group")

  private def withNifti[A](name: String)(f: Path => A): A =
    val dir = Files.createTempDirectory("scalafim-declared-volume-")
    val path = dir.resolve(name)
    try
      assert(Nifti.writeVolume(path, volume).isRight)
      f(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(dir)

  private def declaration(bytes: Array[Byte]): FrameDeclaration =
    val bundle = DataAsset.make("synthetic-bundle", "1" * 64).toOption.get
    FrameDeclaration.make(nlin2009c, FrameBasis.derived("synthetic group analysis", Vector(bundle)).toOption.get,
      DataAsset.make("group.nii", AssetSha256.of(bytes).value).toOption.get).toOption.get

  test("a NIfTI source loads only when its bytes match the declared digest"):
    for name <- Vector("group.nii", "group.nii.gz") do
      withNifti(name) { path =>
        val bytes = Files.readAllBytes(path)
        val declared = DeclaredVolumeReader.readNifti(path, declaration(bytes)).fold(e => fail(e.message), d => d)
        assertEquals(declared.frame, nlin2009c)
        assertEquals(declared.volume(VoxelCoord(2, 3, 4)), 432.0)
        val reference = VolumeReference.make(nlin2009c, space).toOption.get
        assert(reference.sharesGrid(declared.volume.grid), s"$name: reloaded grid must match the written grid")
        val other = declaration(bytes.updated(bytes.length - 1, (bytes.last ^ 1).toByte))
        assert(DeclaredVolumeReader.readNifti(path, other).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
      }
    assert(DeclaredVolumeReader.readNifti(Path.of("/nonexistent/group.nii"), declaration(Array.emptyByteArray))
      .left.exists(_.isInstanceOf[ReferenceError.AssetReadFailure]))
