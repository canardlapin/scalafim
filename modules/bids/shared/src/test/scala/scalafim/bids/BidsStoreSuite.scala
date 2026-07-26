package scalafim.bids

import cats.Id

class BidsStoreSuite extends munit.FunSuite:
  private def value[A](result: Either[BidsError, A]): A =
    result.fold(error => fail(error.message), identity)

  test("in-memory store lists normalized entries deterministically"):
    val store =
      value(
        InMemoryBidsStore.fromText[Id](
          Vector(
            "sub-02/func/sub-02_task-rest_events.tsv" -> "onset\tduration\n0\t1\n",
            "dataset_description.json" -> "{}",
            "sub-01/func/sub-01_task-rest_events.tsv" -> "onset\tduration\n0\t1\n"
          )
        )
      )

    assertEquals(
      value(store.entries).map(_.path.value),
      Vector(
        "dataset_description.json",
        "sub-01/func/sub-01_task-rest_events.tsv",
        "sub-02/func/sub-02_task-rest_events.tsv"
      )
    )
    assertEquals(value(store.readUtf8(BidsPath("dataset_description.json"))), "{}")

  test("in-memory store injects listing and read failures deterministically"):
    val readError = BidsError.Io("a.json", "injected read failure")
    val store =
      value(
        InMemoryBidsStore.fromText[Id](
          Vector("a.json" -> "{}", "b.json" -> "{}"),
          readFailures = Map("a.json" -> readError)
        )
      )
    assertEquals(store.readUtf8(BidsPath("a.json")), Left(readError))
    assertEquals(value(store.readUtf8(BidsPath("b.json"))), "{}")

    val listingError = BidsError.Io("memory", "injected listing failure")
    val failed = value(InMemoryBidsStore.fromText[Id](Vector("a.json" -> "{}"), listingFailure = Some(listingError)))
    assertEquals(failed.entries, Left(listingError))
    assert(InMemoryBidsStore.fromText[Id](Vector("../escape.json" -> "{}")).isLeft)
