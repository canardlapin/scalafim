package scalafim.fmri.hrf

import scalafim.fmri.hrf.design.SamplingFrame

class SamplingFrameSuite extends munit.FunSuite:

  test("sampling frame constructor works") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))
    assertEquals(sf.nBlocks, 2)
    assertEquals(sf.tr.map(_.value), Vector(2.0, 2.0))
    assertEquals(sf.startTime.map(_.value), Vector(1.0, 1.0))

    val sf2 = SamplingFrame(blockLens = Seq(100, 200), tr = Seq(2.0, 1.5))
    assertEquals(sf2.tr.map(_.value), Vector(2.0, 1.5))
    assertEquals(sf2.startTime.map(_.value), Vector(1.0, 0.75))
  }

  test("regular sampling frame sugar preserves default and explicit acquisition origins") {
    val default =
      SamplingFrame
        .regular(tr = 0.8, nScans = 600)
        .fold(error => fail(error.message), identity)
    assertEquals(default.blockLens, Vector(600))
    assertEquals(default.tr.map(_.value), Vector(0.8))
    assertEquals(default.startTime.map(_.value), Vector(0.4))

    val explicit =
      SamplingFrame
        .regular(tr = 0.8, nScans = 600, startTime = 0.0, precision = 0.01)
        .fold(error => fail(error.message), identity)
    assertEquals(explicit.startTime.map(_.value), Vector(0.0))
    assertEquals(explicit.precision.value, 0.01)
  }

  test("sampling frame validates inputs") {
    intercept[IllegalArgumentException] {
      SamplingFrame(blockLens = Seq(-1, 100), tr = Seq(2.0))
    }
    intercept[IllegalArgumentException] {
      SamplingFrame(blockLens = Seq(100, 100), tr = Seq(-1.0))
    }
    intercept[IllegalArgumentException] {
      SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0), precision = 3.0)
    }
    intercept[IllegalArgumentException] {
      SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0, 2.0, 2.0))
    }
    intercept[IllegalArgumentException] {
      SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0), startTime = Seq(0.0, 0.0, 0.0))
    }
  }

  test("samples are correct length and global offsets") {
    val sf = SamplingFrame(blockLens = Seq(3, 3), tr = Seq(1.0))
    val rel = sf.samples()
    val glob = sf.samples(global = true)
    assertEquals(rel.length, 6)
    assertEquals(glob.length, 6)
    assertEquals(rel.take(3).map(_.value), Vector(0.5, 1.5, 2.5))
    assertEquals(glob.take(3).map(_.value), Vector(0.5, 1.5, 2.5))
    assertEquals(glob.drop(3).map(_.value), Vector(3.5, 4.5, 5.5))
  }

  test("samples local vs global timing and block selection") {
    val sf = SamplingFrame(blockLens = Seq(100, 100), tr = Seq(2.0))
    val rel = sf.samples(global = false)
    assertEquals(rel.take(5).map(_.value), Vector(1.0, 3.0, 5.0, 7.0, 9.0))

    val glob = sf.samples(global = true)
    assertEquals(glob.length, 200)
    assert(math.abs(glob(100).value - glob(99).value - 2.0) < 1e-10)

    val block1 = sf.samples(blocks = Seq(0))
    assertEquals(block1.length, 100)
  }

  test("global onsets converts block-relative times") {
    val sf = SamplingFrame(blockLens = Seq(3, 3), tr = Seq(1.0))
    val ons: Seq[Seconds] = Seq(0.0.s, 1.0.s, 0.0.s, 2.0.s)
    val blk = Seq(0, 0, 1, 1)
    val g = sf.globalOnsets(ons, blk).map(_.value)
    assertEquals(g, Vector(0.0, 1.0, 3.0, 5.0))

    intercept[IllegalArgumentException] {
      sf.globalOnsets(ons, Seq(0, 2, 1, 1))
    }
  }

  test("sampling frame temporal consistency") {
    val sf = SamplingFrame(blockLens = Seq(10, 10, 10), tr = Seq(2.0))
    val glob = sf.samples(global = true).map(_.value)
    val bids = sf.blockIdsPerSample

    for b <- 0 until sf.nBlocks do
      val idx = bids.zipWithIndex.collect { case (bb, i) if bb == b => i }
      val diffs = idx.sliding(2).collect { case Seq(i0, i1) => glob(i1) - glob(i0) }.toVector
      assert(diffs.forall(d => math.abs(d - 2.0) < 1e-10))

    val ends = sf.blockLens.scanLeft(0)(_ + _).tail
    for i <- 0 until ends.length - 1 do
      val timeDiff = glob(ends(i)) - glob(ends(i) - 1)
      assert(math.abs(timeDiff - 2.0) < 1e-10)
  }

  test("samples with blocks parameter mirrors R blockids cases") {
    val sf = SamplingFrame(blockLens = Seq(10, 10, 10), tr = Seq(2.0))

    val b1Local = sf.samples(blocks = Seq(0), global = false).map(_.value)
    val b2Local = sf.samples(blocks = Seq(1), global = false).map(_.value)
    val b3Local = sf.samples(blocks = Seq(2), global = false).map(_.value)
    assertEquals(b1Local, b2Local)
    assertEquals(b2Local, b3Local)

    val b1Global = sf.samples(blocks = Seq(0), global = true).map(_.value)
    val b2Global = sf.samples(blocks = Seq(1), global = true).map(_.value)
    val b3Global = sf.samples(blocks = Seq(2), global = true).map(_.value)
    assertEquals(b1Global.head, 1.0)
    assertEquals(b2Global.head, 21.0)
    assertEquals(b3Global.head, 41.0)

    val blocks12 = sf.samples(blocks = Seq(0, 1), global = true).map(_.value)
    assertEquals(blocks12.take(10), b1Global)
    assertEquals(blocks12.drop(10), b2Global)

    val blocks13 = sf.samples(blocks = Seq(0, 2), global = true).map(_.value)
    assertEquals(blocks13.take(10), b1Global)
    assertEquals(blocks13.drop(10), b3Global)

    val all = sf.samples(global = true)
    assert(blocks13.length < all.length)
  }

  test("samples respects varying TR/startTime per block") {
    val sf = SamplingFrame(
      blockLens = Seq(10, 10, 10),
      tr = Seq(2.0, 1.5, 3.0),
      startTime = Seq(1.0, 0.75, 1.5)
    )

    val b1Local = sf.samples(blocks = Seq(0), global = false).map(_.value)
    val b2Local = sf.samples(blocks = Seq(1), global = false).map(_.value)
    val b3Local = sf.samples(blocks = Seq(2), global = false).map(_.value)
    assert(math.abs((b1Local(1) - b1Local(0)) - 2.0) < 1e-10)
    assert(math.abs((b2Local(1) - b2Local(0)) - 1.5) < 1e-10)
    assert(math.abs((b3Local(1) - b3Local(0)) - 3.0) < 1e-10)
    assertEquals(b1Local.head, 1.0)
    assertEquals(b2Local.head, 0.75)
    assertEquals(b3Local.head, 1.5)

    val b1Global = sf.samples(blocks = Seq(0), global = true).map(_.value)
    val b2Global = sf.samples(blocks = Seq(1), global = true).map(_.value)
    val b3Global = sf.samples(blocks = Seq(2), global = true).map(_.value)
    assertEquals(b1Global.head, 1.0)
    assertEquals(b2Global.head, 20.75)
    assertEquals(b3Global.head, 36.5)
  }

  test("edge cases for blocks parameter") {
    val sf = SamplingFrame(blockLens = Seq(5, 10, 15), tr = Seq(1.0))
    assertEquals(sf.samples(blocks = Seq.empty).length, 0)

    val outOrder = sf.samples(blocks = Seq(2, 0, 1), global = true)
    assertEquals(outOrder.length, 30)

    val repeated = sf.samples(blocks = Seq(0, 0), global = true)
    assertEquals(repeated.length, 10)

    val middle = sf.samples(blocks = Seq(1), global = true).map(_.value)
    assertEquals(middle.length, 10)
    assertEquals(middle.head, 5.5)
  }

  test("acquisitionOnsets parity with samples(global=true)") {
    val sf1 = SamplingFrame(blockLens = Seq(10), tr = Seq(2.0))
    assertEquals(sf1.acquisitionOnsets().map(_.value), sf1.samples(global = true).map(_.value))

    val sf2 = SamplingFrame(blockLens = Seq(10, 15, 20), tr = Seq(2.0))
    assertEquals(sf2.acquisitionOnsets().length, 45)

    val sf3 = SamplingFrame(blockLens = Seq(10, 10), tr = Seq(2.0, 1.5))
    assertEquals(sf3.acquisitionOnsets().map(_.value), sf3.samples(global = true).map(_.value))
  }

  test("acquisitionOnsets handles default and custom start times") {
    val sfDefault = SamplingFrame(blockLens = Seq(5), tr = Seq(2.0))
    val onDefault = sfDefault.acquisitionOnsets().map(_.value)
    assertEquals(onDefault.head, 1.0)
    assert(onDefault.sliding(2).forall(w => w.length == 2 && math.abs((w(1) - w(0)) - 2.0) < 1e-10))

    val sf0 = SamplingFrame(blockLens = Seq(5), tr = Seq(2.0), startTime = Seq(0.0))
    assertEquals(sf0.acquisitionOnsets().map(_.value), Vector(0.0, 2.0, 4.0, 6.0, 8.0))

    val sf3 = SamplingFrame(blockLens = Seq(3, 3), tr = Seq(2.0), startTime = Seq(0.0, 5.0))
    val on3 = sf3.acquisitionOnsets().map(_.value)
    assertEquals(on3.take(3), Vector(0.0, 2.0, 4.0))
    assertEquals(on3.drop(3), Vector(11.0, 13.0, 15.0))
  }

  test("acquisitionOnsets handles variable TR and multi-block transitions") {
    val sf = SamplingFrame(blockLens = Seq(5, 5), tr = Seq(2.0, 3.0))
    val on = sf.acquisitionOnsets().map(_.value)
    assertEquals(on.take(5), Vector(1.0, 3.0, 5.0, 7.0, 9.0))
    assertEquals(on.drop(5), Vector(11.5, 14.5, 17.5, 20.5, 23.5))

    val sfMulti = SamplingFrame(blockLens = Seq(100, 120, 80), tr = Seq(2.0))
    val om = sfMulti.acquisitionOnsets().map(_.value)
    assertEquals(om.length, 300)
    assertEquals(om(99), 199.0)
    assertEquals(om(100), 201.0)
    assertEquals(om(219), 439.0)
    assertEquals(om(220), 441.0)
  }

  test("acquisitionOnsets edge cases and standard fMRI timing") {
    val sf1 = SamplingFrame(blockLens = Seq(1), tr = Seq(2.0))
    assertEquals(sf1.acquisitionOnsets().map(_.value), Vector(1.0))

    val sfShort = SamplingFrame(blockLens = Seq(5), tr = Seq(0.5))
    assertEquals(sfShort.acquisitionOnsets().map(_.value), Vector(0.25, 0.75, 1.25, 1.75, 2.25))

    val sfMany = SamplingFrame(blockLens = Seq.fill(10)(10), tr = Seq(1.0))
    assertEquals(sfMany.acquisitionOnsets().length, 100)

    val sfStd = SamplingFrame(blockLens = Seq(150, 150), tr = Seq(2.0), startTime = Seq(0.0))
    val os = sfStd.acquisitionOnsets().map(_.value)
    assertEquals(os.head, 0.0)
    assertEquals(os(149), 298.0)
    assertEquals(os(150), 300.0)
    assertEquals(os(299), 598.0)
    assertEquals(os.max, 598.0)
  }
