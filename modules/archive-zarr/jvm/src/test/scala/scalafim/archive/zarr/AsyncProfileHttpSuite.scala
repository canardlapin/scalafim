package scalafim.archive.zarr

import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer
import java.net.InetSocketAddress
import java.net.URI
import java.util.concurrent.Executors
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scalafim.zarr.*

class AsyncProfileHttpSuite extends munit.FunSuite:
  private def zvalue[A](result: Either[ZarrError, A]): A = result match
    case Right(found) => found
    case Left(error) => fail(error.message)

  test("portable async NeuroArchive profile performs cached bounded JVM HTTP reads"):
    withServer(ProfileFixtures.completeStartIndexedStore): (base, requests) =>
      val blocking = JvmHttpStore(base).fold(fail(_), identity)
      val ioExecutor = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))
      val codecExecutor = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
      try
        given ExecutionContext = ioExecutor
        val remote = BlockingObjectReaderAdapter(blocking, ioExecutor)
        val cache = ObjectReadCache(
          zvalue(CacheNamespace.from("neuroarchive:http-fixture-v1")),
          CacheLimits(32, zvalue(ByteCount(8192L)))
        )
        val reader = CachingAsyncObjectReader(remote, cache)
        val result = AsyncNeuroArchiveZarr.openCanonical(
          reader,
          runtime = JvmAsyncCodecRuntime.portable(codecExecutor)
        ).flatMap:
          case Left(error) => fail(error.message)
          case Right(opened) =>
            assertEquals(snapshot(requests), ProfileFixtures.expectedHttpOpenTrace)
            val region = zvalue(Region.within(
              opened.canonical.array.shape,
              zvalue(Coordinate(0L, 0L, 0L, 0L)),
              opened.canonical.array.shape
            ))
            clear(requests)
            opened.array.readRegion(region).flatMap:
              case Left(error) => fail(error.message)
              case Right(first) =>
                val firstTrace = snapshot(requests)
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
                    assertEquals(snapshot(requests), firstTrace)
        Await.result(result, Duration("10s"))
      finally
        ioExecutor.shutdown()
        codecExecutor.shutdown()

  private def withServer(objects: Map[String, OwnedBytes])(
      body: (URI, mutable.ArrayBuffer[String]) => Unit
  ): Unit =
    val requests = mutable.ArrayBuffer.empty[String]
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/store/", new HttpHandler:
      def handle(exchange: HttpExchange): Unit =
        try
          val name = exchange.getRequestURI.getPath.stripPrefix("/store/")
          val range = exchange.getRequestHeaders.getFirst("Range")
          requests.synchronized:
            requests += s"${exchange.getRequestMethod} $name ${Option(range).getOrElse("-")}"
          objects.get(name) match
            case None => exchange.sendResponseHeaders(404, -1L)
            case Some(owned) =>
              val all = owned.toArray
              if exchange.getRequestMethod == "HEAD" then
                exchange.getResponseHeaders.set("Content-Length", all.length.toString)
                exchange.sendResponseHeaders(200, -1L)
              else if range == null then
                exchange.getResponseHeaders.set("Content-Length", all.length.toString)
                exchange.sendResponseHeaders(200, all.length.toLong)
                exchange.getResponseBody.write(all)
              else
                val bounds = range.stripPrefix("bytes=").split("-").map(_.toInt)
                val selected = all.slice(bounds(0), bounds(1) + 1)
                exchange.getResponseHeaders.set(
                  "Content-Range",
                  s"bytes ${bounds(0)}-${bounds(1)}/${all.length}"
                )
                exchange.sendResponseHeaders(206, selected.length.toLong)
                exchange.getResponseBody.write(selected)
        finally exchange.close()
    )
    server.start()
    try
      val base = URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/store/")
      body(base, requests)
    finally server.stop(0)

  private def snapshot(requests: mutable.ArrayBuffer[String]): Vector[String] =
    requests.synchronized:
      requests.toVector

  private def clear(requests: mutable.ArrayBuffer[String]): Unit =
    requests.synchronized:
      requests.clear()
