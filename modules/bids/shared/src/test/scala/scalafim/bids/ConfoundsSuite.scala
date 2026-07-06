package scalafim.bids

class ConfoundsSuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def doubles(values: Vector[Option[String]]): Vector[Double] =
    values.map {
      case Some(value) => value.toDouble
      case None        => fail("expected finite numeric cell")
    }

  test("motion and global confound sets match bidser sizes"):
    assertEquals(value(ConfoundSets.named("motion6")).length, 6)
    assertEquals(value(ConfoundSets.named("motion24")).length, 24)
    assertEquals(value(ConfoundSets.named("9p")).length, 9)
    assertEquals(value(ConfoundSets.named("36p")).length, 36)

  test("CompCor confound sets support component caps"):
    assertEquals(value(ConfoundSets.named("acompcor", n = Some(6))), Vector("a_comp_cor_*[6]"))
    assertEquals(value(ConfoundSets.named("compcor", n = Some(3))), Vector("a_comp_cor_*[3]", "t_comp_cor_*[3]"))

  test("DVARS and legacy default preserve bidser names"):
    assertEquals(value(ConfoundSets.named("dvars")), Vector("std_dvars"))
    assertEquals(value(ConfoundSets.named("raw_dvars")), Vector("dvars"))
    assertEquals(ConfoundSets.legacyDefault.length, 26)
    assert(ConfoundSets.legacyDefault.contains("a_comp_cor_05"))
    assert(ConfoundSets.legacyDefault.contains("rot_z"))

  test("pcabasic80 strategy carries PCA and raw families"):
    val strategy = value(ConfoundStrategy.named("pcabasic80"))

    assertEquals(strategy.percentVariance, Some(80.0))
    assert(strategy.pcaVars.contains("trans_x"))
    assert(strategy.pcaVars.contains("a_comp_cor_*"))
    assert(!strategy.pcaVars.contains("global_signal"))
    assert(strategy.rawVars.contains("cosine_*"))

  test("confound resolver supports aliases, wildcards, caps, and derivative suffix aliases"):
    val columns =
      Vector(
        "CSF",
        "WhiteMatter",
        "FramewiseDisplacement",
        "a_comp_cor_00",
        "a_comp_cor_01",
        "a_comp_cor_02",
        "X_derivative1",
        "cosine00"
      )

    val resolved =
      ConfoundSelector.resolveVariables(
        Vector("csf", "white_matter", "framewise_displacement", "a_comp_cor_*[2]", "trans_x_derivative1", "cosine*"),
        columns
      )

    assertEquals(
      resolved,
      Vector("CSF", "WhiteMatter", "FramewiseDisplacement", "a_comp_cor_00", "a_comp_cor_01", "X_derivative1", "cosine00")
    )

  test("confound selector fills missing values and drops zero-variance columns"):
    val table =
      value(
        BidsTable.fromRows(
          Vector("CSF", "WhiteMatter", "FramewiseDisplacement", "cosine00", "cosine01"),
          Vector(
            Vector(Some("0.1"), Some("1.0"), None, Some("-1"), Some("0")),
            Vector(Some("0.2"), Some("1.1"), Some("0.2"), Some("0"), Some("0")),
            Vector(Some("0.3"), Some("1.2"), Some("0.4"), Some("1"), Some("0"))
          )
        )
      )

    val selected =
      value(
        ConfoundSelector.select(
          table,
          ConfoundSelectionConfig(
            variables = Vector("csf", "white_matter", "framewise_displacement", "cosine*"),
            naAction = NaAction.Median
          )
        )
    )

    assertEquals(selected.table.columns, Vector("CSF", "WhiteMatter", "FramewiseDisplacement", "cosine00"))
    assertEquals(value(selected.table.column("FramewiseDisplacement")), Vector(Some("0.3"), Some("0.2"), Some("0.4")))
    assertEquals(selected.diagnostics.map(_.column), Vector("cosine01"))
    assertEquals(selected.diagnostics.map(_.reason), Vector(ConfoundDiagnosticReason.ZeroVariance))

  test("confound selector can drop rank-deficient columns deterministically"):
    val table =
      value(
        BidsTable.fromRows(
          Vector("a", "b", "sum"),
          Vector(
            Vector(Some("1"), Some("0"), Some("1")),
            Vector(Some("0"), Some("1"), Some("1")),
            Vector(Some("1"), Some("1"), Some("2"))
          )
        )
      )

    val selected =
      value(
        ConfoundSelector.select(
          table,
          ConfoundSelectionConfig(
            variables = Vector("a", "b", "sum"),
            clean = Vector(ConfoundClean.Rank)
          )
        )
      )

    assertEquals(selected.table.columns, Vector("a", "b"))
    assertEquals(selected.diagnostics.map(_.column), Vector("sum"))
    assertEquals(selected.diagnostics.flatMap(_.rank), Vector(2))

  test("confound strategy reduces PCA columns and appends raw columns"):
    val table =
      value(
        BidsTable.fromRows(
          Vector("CSF", "WhiteMatter", "cosine00"),
          Vector(
            Vector(Some("1"), Some("2"), Some("-1")),
            Vector(Some("2"), Some("4"), Some("0")),
            Vector(Some("3"), Some("6"), Some("1")),
            Vector(Some("4"), Some("8"), Some("2"))
          )
        )
      )
    val strategy =
      ConfoundStrategy(
        name = "test-pca",
        pcaVars = Vector("csf", "white_matter"),
        rawVars = Vector("cosine*"),
        percentVariance = Some(80.0)
      )

    val selected = value(ConfoundSelector.selectStrategy(table, strategy))

    assertEquals(selected.table.columns, Vector("PC1", "cosine00"))
    assertEquals(selected.resolved, Vector("CSF", "WhiteMatter", "cosine00"))
    assertEquals(selected.pca.map(_.sourceColumns), Some(Vector("CSF", "WhiteMatter")))
    assertEquals(selected.pca.map(_.componentNames), Some(Vector("PC1")))
    assertEqualsDouble(selected.pca.get.proportionVariance.head, 1.0, 1e-10)

    val pc1 = doubles(value(selected.table.column("PC1")))
    assert(math.abs(pc1.sum) < 1e-10)
    assert(pc1.head < pc1.last)

    val directReduction =
      value(PrincipalComponentConfoundReducer.reduce(value(table.select(Vector("CSF", "WhiteMatter"))), strategy))
    assertEquals(directReduction.scores.columns, Vector("PC1"))
    assertEquals(directReduction.pca.sourceColumns, Vector("CSF", "WhiteMatter"))

  test("confound strategy honours explicit PCA component count"):
    val table =
      value(
        BidsTable.fromRows(
          Vector("a_comp_cor_00", "a_comp_cor_01", "cosine00"),
          Vector(
            Vector(Some("1"), Some("0"), Some("1")),
            Vector(Some("0"), Some("1"), Some("0")),
            Vector(Some("-1"), Some("0"), Some("-1")),
            Vector(Some("0"), Some("-1"), Some("0"))
          )
        )
      )
    val strategy =
      ConfoundStrategy(
        name = "two-pc",
        pcaVars = Vector("a_comp_cor_*"),
        rawVars = Vector("cosine*"),
        npcs = Some(2)
      )

    val selected = value(ConfoundSelector.selectStrategy(table, strategy))

    assertEquals(selected.table.columns, Vector("PC1", "PC2", "cosine00"))
    assertEquals(selected.pca.get.componentNames, Vector("PC1", "PC2"))
    assertEquals(selected.pca.get.loadings.length, 2)
