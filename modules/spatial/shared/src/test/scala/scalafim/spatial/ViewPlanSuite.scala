package scalafim.spatial

class ViewPlanSuite extends munit.FunSuite:

  private def domainId(name: String): DomainId =
    DomainId(name) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def rootId(name: String): FieldRootId =
    FieldRootId(name) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def value[A](result: Either[ViewPlanError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  test("field root ids and root plans validate identity and shape"):
    val id = rootId("  run-01-bold  ")
    val domain = domainId("native-bold")
    val plan = value(ViewPlan.root(id, domain, sampleCount = 12, observations = 8))

    assertEquals(id.value, "run-01-bold")
    assert(plan.isRoot)
    assertEquals(plan.rootId, id)
    assertEquals(plan.rootDomain, domain)
    assertEquals(plan.currentDomain, domain)
    assertEquals(plan.rootSampleCount, 12)
    assertEquals(plan.sampleCount, 12)
    assertEquals(plan.observations, 8)
    assertEquals(plan.steps, Vector.empty)
    assertEquals(FieldRootId("  ").left.toOption, Some(ViewPlanError.EmptyRootId))
    assertEquals(
      ViewPlan.root(id, domain, sampleCount = -1, observations = 8).left.toOption,
      Some(ViewPlanError.InvalidRootShape(-1, 8))
    )

  test("chained reexpression remains one root-first intent with inspectable steps"):
    val rootDomain = domainId("root")
    val midDomain = domainId("mid")
    val targetDomain = domainId("target")
    val root = value(ViewPlan.root(rootId("bold-run"), rootDomain, 10, 3))

    val mid = value(
      root.reexpress(
        midDomain,
        targetSampleCount = 8,
        routing = RoutingPolicy.Anatomical,
        sampling = SamplingPolicy.Nearest,
        allowInverses = true
      )
    )
    val target = value(mid.reexpress(targetDomain, targetSampleCount = 6))

    assert(target.isView)
    assertEquals(target.rootId, root.rootId)
    assertEquals(target.rootDomain, rootDomain)
    assertEquals(target.currentDomain, targetDomain)
    assertEquals(target.sampleCount, 6)
    assertEquals(target.intent.root, rootDomain)
    assertEquals(target.intent.target, targetDomain)
    assertEquals(target.intent.targetSampleCount, 6)
    assertEquals(target.steps.length, 2)
    target.steps match
      case Vector(
            ViewPlanStep.Reexpress(firstSource, firstTarget, _, _, _, _, ViewStepOrigin.Descriptive),
            ViewPlanStep.Reexpress(secondSource, secondTarget, _, _, _, _, ViewStepOrigin.Descriptive)
          ) =>
        assertEquals(firstSource, rootDomain)
        assertEquals(firstTarget, midDomain)
        assertEquals(secondSource, midDomain)
        assertEquals(secondTarget, targetDomain)
      case other =>
        fail(s"unexpected plan steps: $other")

  test("terminal row selections compose into absolute target rows"):
    val rootDomain = domainId("root")
    val targetDomain = domainId("target")
    val root = value(ViewPlan.root(rootId("bold-run"), rootDomain, 5, 2))
    val target = value(root.reexpress(targetDomain, targetSampleCount = 6))
    val first = value(target.selectRows(RowSelection.Rows(Vector(4, 1, 3))))
    val second = value(first.selectRows(RowSelection.Rows(Vector(2, 0))))

    assertEquals(first.sampleCount, 3)
    assertEquals(first.intent.rowSelection, RowSelection.Rows(Vector(4, 1, 3)))
    assertEquals(second.sampleCount, 2)
    assertEquals(second.intent.rowSelection, RowSelection.Rows(Vector(3, 4)))
    assertEquals(second.rootId, root.rootId)
    assertEquals(second.steps.length, 3)
    assertEquals(
      second.steps.last,
      ViewPlanStep.SelectRows(
        targetDomain,
        RowSelection.Rows(Vector(2, 0)),
        Vector(3, 4)
      )
    )

  test("invalid targets and non-terminal selections return typed plan errors"):
    val rootDomain = domainId("root")
    val targetDomain = domainId("target")
    val nextDomain = domainId("next")
    val root = value(ViewPlan.root(rootId("bold-run"), rootDomain, 5, 2))
    val target = value(root.reexpress(targetDomain, targetSampleCount = 4))
    val selected = value(target.selectRows(RowSelection.Rows(Vector(3, 1))))

    assertEquals(
      root.reexpress(targetDomain, targetSampleCount = -1).left.toOption,
      Some(ViewPlanError.InvalidTargetShape(targetDomain, -1))
    )
    assertEquals(
      selected.reexpress(nextDomain, targetSampleCount = 3).left.toOption,
      Some(ViewPlanError.SelectionMustBeTerminal(targetDomain))
    )
    assertEquals(
      target.selectRows(RowSelection.Rows(Vector(4))).left.toOption,
      Some(ViewPlanError.InvalidSelection(SpatialError.InvalidRoiRow(4, 4)))
    )

  test("selecting all is a semantic no-op"):
    val root = value(ViewPlan.root(rootId("bold-run"), domainId("root"), 5, 2))
    val selected = value(root.selectRows(RowSelection.All))

    assertEquals(selected, root)
