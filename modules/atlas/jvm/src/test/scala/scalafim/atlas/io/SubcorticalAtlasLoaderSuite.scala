package scalafim.atlas.io

import scalafim.atlas.*

class SubcorticalAtlasLoaderSuite extends munit.FunSuite:
  private def right[A](value: Either[AtlasAcquisitionError, A]): A = value.fold(error => fail(error.message), identity)

  test("strict parsers preserve source IDs, names, and laterality only where supplied"):
    val cit = right(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.Cit168, "0 Pu\n1 Ca\n2 NAC\n3 EXA\n4 GPe\n5 GPi\n6 SNc\n7 RN\n8 SNr\n9 PBP\n10 VTA\n11 VeP\n12 HN\n13 HTH\n14 MN\n15 STH"))
    assertEquals(cit.map(_.id.value), (1 to 16).toVector)
    assert(cit.forall(_.hemisphere.isEmpty))
    val thalamus = right(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.HcpThalamic,
      "0 Unknown 0 0 0 0\n1 LH-Pulvinar 255 0 0 0\n8 RH-Pulvinar 255 0 0 0"))
    assertEquals(thalamus.map(_.hemisphere), Vector(Some(Hemisphere.Left), Some(Hemisphere.Right)))
    val mdtb = right(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.Mdtb10,
      "index\tname\tcolor\n1\tRegion1\t#2ea698\n2\tRegion2\t#549720"))
    assertEquals(mdtb.map(_.id.value), Vector(1, 2))
    val hp = right(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.HcpHippocampusAmygdala,
      "index\tlabel\tcifti_label\tcolor_red\tcolor_green\tcolor_blue\topacity\n17\tHIPPOCAMPUS_LEFT\tHIPPOCAMPUS_LEFT\t1\t1\t1\t255\n18\tAMYGDALA_LEFT\tAMYGDALA_LEFT\t1\t1\t1\t255\n53\tHIPPOCAMPUS_RIGHT\tHIPPOCAMPUS_RIGHT\t1\t1\t1\t255\n54\tAMYGDALA_RIGHT\tAMYGDALA_RIGHT\t1\t1\t1\t255"))
    assertEquals(hp.map(_.id.value), Vector(17, 18, 53, 54))

  test("malformed source metadata fails closed"):
    assert(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.Cit168, "0 Pu").isLeft)
    assert(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.HcpThalamic, "1 neither 1 1 1 0").isLeft)
    assert(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.Mdtb10, "wrong").isLeft)
    assert(SubcorticalAtlasLoader.parse(SubcorticalAtlasFamily.HcpHippocampusAmygdala, "index\tlabel").isLeft)
