package scalafim.fmri.workflow

import bids4s.*
import munit.FunSuite

class LinkedBidsArtifactResolverSuite extends FunSuite:
  test("resolver preserves the selected root location for equal BIDS paths") {
    val raw = BidsRoot.raw("raw", "/study/raw").toOption.get
    val source = BidsRootAlias.unsafe("raw")
    val first = BidsRoot.derivative("first", "/study/first", PipelineName("fmriprep"), source).toOption.get
    val second = BidsRoot.derivative("second", "/study/second", PipelineName("fmriprep"), source).toOption.get
    val path = BidsPath("sub-01/func/sub-01_task-rest_space-MNI_desc-preproc_bold.nii.gz")
    val file = BidsFile(path, None, BidsScope.Derivatives, Some(PipelineName("fmriprep")))
    val selected = LinkedBidsFile(second, file, BidsPath("/study/second/" + path.value))
    val catalog = LinkedBidsProject.make(Vector(raw, first, second), Vector(selected)).toOption.get
    val resolved = LinkedBidsArtifactResolver.resolve[BoldImageResource](catalog, selected).toOption.get
    assertEquals(resolved.location.value, "file:///study/second/" + path.value)
  }

  test("resolver percent-encodes UTF-8 path segments") {
    val raw = BidsRoot.raw("raw", "/folder #1% café").toOption.get
    val path = BidsPath("sub-01/func/file ?#.nii")
    val file = BidsFile(path, None, BidsScope.Raw)
    val linked = LinkedBidsFile(raw, file, BidsPath("/folder #1% café/" + path.value))
    val project = LinkedBidsProject.make(Vector(raw), Vector(linked)).toOption.get
    val result = LinkedBidsArtifactResolver.resolve[BoldImageResource](project, linked).toOption.get
    assertEquals(result.location.value, "file:///folder%20%231%25%20caf%C3%A9/sub-01/func/file%20%3F%23.nii")
  }
