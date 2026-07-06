package scalafim.bids

class BidsUriSuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("parse current-dataset BIDS URI"):
    val uri = value(BidsUri.parse("bids::sub-01/fmap/sub-01_epi.nii.gz"))

    assertEquals(uri.datasetName.value, "")
    assertEquals(uri.relativePath.value, "sub-01/fmap/sub-01_epi.nii.gz")
    assertEquals(uri.value, "bids::sub-01/fmap/sub-01_epi.nii.gz")

  test("resolve current-dataset BIDS URI from parent directory"):
    val desc = DatasetDescription(
      name = Some("Base"),
      bidsVersion = Some("1.9.0"),
      parentDirectory = Some(BidsPath("/tmp/base"))
    )

    assertEquals(value(desc.resolve(value(BidsUri.parse("bids::sub-01/anat/T1w.nii.gz")))), "/tmp/base/sub-01/anat/T1w.nii.gz")

  test("resolve named DatasetLinks including remote links"):
    val desc = DatasetDescription(
      name = Some("Base"),
      bidsVersion = Some("1.9.0"),
      datasetLinks = Map(
        DatasetName("deriv1") -> "/tmp/deriv1",
        DatasetName("remote") -> "s3://bucket/ds"
      )
    )

    assertEquals(value(desc.resolve(value(BidsUri.parse("bids:deriv1:sub-01/anat/T1w.nii.gz")))), "/tmp/deriv1/sub-01/anat/T1w.nii.gz")
    assertEquals(value(desc.resolve(value(BidsUri.parse("bids:remote:sub-01/anat/T1w.nii.gz")))), "s3://bucket/ds/sub-01/anat/T1w.nii.gz")

  test("reject absolute BIDS URI relative paths"):
    assert(BidsUri.parse("bids:ds:/absolute/path.nii.gz").isLeft)
    assert(BidsUri.parse("bids:ds:../outside.tsv").isLeft)
    assert(BidsPath.relative("sub-01/../outside.tsv").isLeft)
    assert(BidsUri.parse("notbids:ds:file.nii.gz").isLeft)
