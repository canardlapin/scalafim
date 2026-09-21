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
    assertEquals(resolved.location.value, "/study/second/" + path.value)
  }
