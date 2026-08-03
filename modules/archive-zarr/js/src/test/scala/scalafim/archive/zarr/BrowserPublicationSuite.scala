package scalafim.archive.zarr

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import zarr4s.*

class BrowserPublicationSuite extends munit.FunSuite:
  private def zvalue[A](result: Either[ZarrError, A]): A = result match
    case Right(found) => found
    case Left(error) => fail(error.message)

  test("Scala.js opens a complete published canonical fixture without listing"):
    val (store, objectCount) = fixture(dropFirstObject = false)
    BrowserNeuroArchiveZarr.openCanonical(store).map:
      case Left(error) => fail(error.message)
      case Right(opened) =>
        assertEquals(opened.canonical.array.shape.toVector, Vector(2L, 2L, 2L, 3L))
        assertEquals(opened.publication.objects.length, objectCount)
        assertEquals(store.trace.collect { case ObjectRequest.Length(_) => 1 }.sum, objectCount)

  test("Scala.js rejects a publication with a missing receipt-listed object"):
    val (store, _) = fixture(dropFirstObject = true)
    BrowserNeuroArchiveZarr.openCanonical(store).map: result =>
      assert(result.isLeft)

  test("portable async profile reads a complete sharded publication through a revision cache"):
    val store = zvalue(AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore))
    val cache = ObjectReadCache(
      zvalue(CacheNamespace.from("neuroarchive:fixture-v1")),
      CacheLimits(32, zvalue(ByteCount(8192L)))
    )
    val reader = CachingAsyncObjectReader(store, cache)
    BrowserNeuroArchiveZarr.openCanonical(reader).flatMap:
      case Left(error) => fail(error.message)
      case Right(opened) =>
        val region = zvalue(Region.within(
          opened.canonical.array.shape,
          zvalue(Coordinate(0L, 0L, 0L, 0L)),
          opened.canonical.array.shape
        ))
        store.clearTrace()
        opened.array.readRegion(region).flatMap:
          case Left(error) => fail(error.message)
          case Right(first) =>
            val trace = store.trace
            val values = first.block match
              case PrimitiveBlock.Int16(found) => found.toArray.toVector
              case _ => fail("expected int16 result")
            assertEquals(values, Vector.tabulate(24)(_.toShort))
            assertEquals(trace.collect { case ObjectRequest.Range(_, _) => 1 }.sum, 2)
            opened.array.readRegion(region).map:
              case Left(error) => fail(error.message)
              case Right(second) =>
                val repeated = second.block match
                  case PrimitiveBlock.Int16(found) => found.toArray.toVector
                  case _ => fail("expected int16 result")
                assertEquals(repeated, values)
                assertEquals(store.trace, trace)

  test("browser profile performs the same cached bounded reads over Fetch"):
    withServer(ProfileFixtures.completeStartIndexedStore): (base, requests) =>
      val transport = FetchStore(base).fold(fail(_), identity)
      val cache = ObjectReadCache(
        zvalue(CacheNamespace.from("neuroarchive:fetch-fixture-v1")),
        CacheLimits(32, zvalue(ByteCount(8192L)))
      )
      val reader = CachingAsyncObjectReader(transport, cache)
      BrowserNeuroArchiveZarr.openCanonical(reader).flatMap:
        case Left(error) => fail(error.message)
        case Right(opened) =>
          assertEquals(requests.toVector, ProfileFixtures.expectedHttpOpenTrace)
          val region = zvalue(Region.within(
            opened.canonical.array.shape,
            zvalue(Coordinate(0L, 0L, 0L, 0L)),
            opened.canonical.array.shape
          ))
          requests.clear()
          opened.array.readRegion(region).flatMap:
            case Left(error) => fail(error.message)
            case Right(first) =>
              val firstTrace = requests.toVector
              val values = first.block match
                case PrimitiveBlock.Int16(found) => found.toArray.toVector
                case _ => fail("expected int16 result")
              assertEquals(values, Vector.tabulate(24)(_.toShort))
              assertEquals(firstTrace, ProfileFixtures.expectedHttpDataTrace)
              opened.array.readRegion(region).map:
                case Left(error) => fail(error.message)
                case Right(second) =>
                  val repeated = second.block match
                    case PrimitiveBlock.Int16(found) => found.toArray.toVector
                    case _ => fail("expected int16 result")
                  assertEquals(repeated, values)
                  assertEquals(requests.toVector, firstTrace)

  private def fixture(dropFirstObject: Boolean): (AsyncMemoryStore, Int) =
    val descriptor = ProfileFixtures.descriptor()
    val manifestJson = NeuroArchiveManifestCodec.render(ProfileFixtures.manifest)
    val canonicalJson = ProfileFixtures.directMetadata
    val objects = ProfileFixtures.publicationObjects(descriptor)
    val receipt = ProfileFixtures.publication(descriptor, manifestJson, canonicalJson, objects)
    val base = Map(
      "zarr.json" -> ProfileFixtures.bytes(NeuroArchiveRootMetadata.render),
      "neuroarchive.json" -> ProfileFixtures.bytes(manifestJson),
      "publication.json" -> ProfileFixtures.bytes(PublicationReceiptCodec.render(receipt)),
      "canonical/zarr.json" -> ProfileFixtures.bytes(canonicalJson)
    )
    val payloads = objects.zipWithIndex.collect:
      case (objectValue, index) if !dropFirstObject || index != 0 =>
        objectValue.key.value -> OwnedBytes.copyOf(Array(0.toByte))
    val store = AsyncMemoryStore(base ++ payloads)
      .fold(error => throw IllegalArgumentException(error.message), identity)
    store -> objects.length

  private def withServer[A](
      objects: Map[String, OwnedBytes]
  )(
      body: (String, mutable.ArrayBuffer[String]) => Future[A]
  ): Future[A] =
    val requests = mutable.ArrayBuffer.empty[String]
    val http = js.Dynamic.global.require("http")
    val handler: js.Function2[js.Dynamic, js.Dynamic, Unit] = (request, response) =>
      val path = request.url.asInstanceOf[String].stripPrefix("/store/")
      val method = request.method.asInstanceOf[String]
      val rangeValue = request.headers.selectDynamic("range")
      val range = if js.isUndefined(rangeValue) then "-" else rangeValue.asInstanceOf[String]
      requests += s"$method $path $range"
      objects.get(path) match
        case None =>
          response.statusCode = 404
          response.end()
        case Some(payload) =>
          val all = payload.toArray
          if method == "HEAD" then
            response.statusCode = 200
            response.setHeader("Content-Length", all.length.toString)
            response.end()
          else if range != "-" then
            val bounds = range.stripPrefix("bytes=").split("-").map(_.toInt)
            val selected = all.slice(bounds(0), bounds(1) + 1)
            response.statusCode = 206
            response.setHeader(
              "Content-Range",
              s"bytes ${bounds(0)}-${bounds(1)}/${all.length}"
            )
            response.setHeader("Content-Length", selected.length.toString)
            response.end(uint8(selected))
          else
            response.statusCode = 200
            response.setHeader("Content-Length", all.length.toString)
            response.end(uint8(all))
    val server = http.createServer(handler)
    val started = Promise[String]()
    server.listen(0, "127.0.0.1", () =>
      val port = server.address().port.asInstanceOf[Int]
      started.success(s"http://127.0.0.1:$port/store/")
    )
    started.future.flatMap(base => body(base, requests)).transformWith: result =>
      val closed = Promise[Unit]()
      server.close(() => closed.success(()))
      closed.future.transform(_ => result)

  private def uint8(bytes: Array[Byte]): Uint8Array =
    val result = new Uint8Array(bytes.length)
    var index = 0
    while index < bytes.length do
      result(index) = bytes(index)
      index += 1
    result
