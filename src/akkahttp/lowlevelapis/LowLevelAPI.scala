package akkahttp.lowlevelapis

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.IncomingConnection
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Location
import akka.stream.scaladsl.{Flow, Sink}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object LowLevelAPI extends App {

  /**
   * Akka http is a suite of libraries
   * designed for both the servers and clients
   * based on akka streams and akka actors
   * It is not a
   * framework : Play is a framework or lagom for micro services
   * It comes with backpressure for free till the tcp level
   * It has multiple API levels to choose what we want to go forth and about
   * The structure is http response and request
   * Data to wire understandable json formatting
   *
   * the server receives HTTPRequests
   * It goes through the flow we write
   * Gives the response out
   */

  implicit val system: ActorSystem = ActorSystem("LowLevelServerAPI")

  import system.dispatcher

  // This is a source of http connections. This is going to bind and generate connections
  val serverSource = Http().newServerAt("localhost", 8000).connectionSource()
  val connectionSink = Sink.foreach[IncomingConnection] { connection =>
    println(s"Accepted incoming connection from: ${connection.remoteAddress}")
  }

  // server binding the left materialization value
  //  val serverBindingFuture = serverSource.to(connectionSink).run()
  //  serverBindingFuture.onComplete {
  //    case Success(binding) =>
  //      println("Server binding successful.")
  //      binding.terminate(6 seconds)
  //    case Failure(ex) => println(s"Server binding failed: $ex")
  //  }

  /*
    Method 1: synchronously serve HTTP responses
   */
  //  val requestHandler: HttpRequest => HttpResponse = {
  //    case HttpRequest(HttpMethods.GET, _, _, _, _) =>
  //      HttpResponse(
  //        StatusCodes.OK, // HTTP 200
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   Hello from Akka HTTP!
  //            | </body>
  //            |</html>
  //          """.stripMargin
  //        )
  //      )
  //
  //    case request: HttpRequest =>
  //      // what is this discardEntityBytes?
  //      request.discardEntityBytes()
  //      HttpResponse(
  //        StatusCodes.NotFound, // 404
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   OOPS! The resource can't be found.
  //            | </body>
  //            |</html>
  //          """.stripMargin
  //        )
  //      )
  //  }
  //
  //  val httpSyncConnectionHandler = Sink.foreach[IncomingConnection] { connection =>
  //    connection.handleWithSyncHandler(requestHandler)
  //  }
  //
  //  serverSource.runWith(httpSyncConnectionHandler)

  // shorthand version:
  //  Http().newServerAt("localhost", 8000).bindSync(requestHandler)


  /**
   * Method 2: serve back HTTP response ASYNCHRONOUSLY
   * Have a dedicated dispatcher
   */
  //  val asyncRequestHandler: HttpRequest => Future[HttpResponse] = {
  //    case HttpRequest(HttpMethods.GET, Uri.Path("/home"), _, _, _) => // method, URI, HTTP headers, content and the protocol (HTTP1.1/HTTP2.0)
  //      // very important - Have a dedicated dispatcher
  //      Future(HttpResponse(
  //        StatusCodes.OK, // HTTP 200
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   Hello from Akka HTTP!
  //            | </body>
  //            |</html>
  //            """.stripMargin
  //        )
  //      ))
  //
  //    case request: HttpRequest =>
  //      request.discardEntityBytes()
  //      Future(HttpResponse(
  //        StatusCodes.NotFound, // 404
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   OOPS! The resource can't be found.
  //            | </body>
  //            |</html>
  //            """.stripMargin
  //        )
  //      ))
  //  }
  //
  //  val httpAsyncConnectionHandler = Sink.foreach[IncomingConnection] { connection =>
  //    connection.handleWithAsyncHandler(asyncRequestHandler)
  //  }

  // shorthand version
  //  Http().newServerAt("localhost", 8000).bind(asyncRequestHandler)

  /*
    Method 3: async via Akka streams
   */
  //  val streamsBasedRequestHandler: Flow[HttpRequest, HttpResponse, _] = Flow[HttpRequest].map {
  //    case HttpRequest(HttpMethods.GET, Uri.Path("/home"), _, _, _) => // method, URI, HTTP headers, content and the protocol (HTTP1.1/HTTP2.0)
  //      HttpResponse(
  //        StatusCodes.OK, // HTTP 200
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   Hello from Akka HTTP!
  //            | </body>
  //            |</html>
  //            """.stripMargin
  //        )
  //      )
  //
  //    case request: HttpRequest =>
  //      request.discardEntityBytes()
  //      HttpResponse(
  //        StatusCodes.NotFound, // 404
  //        entity = HttpEntity(
  //          ContentTypes.`text/html(UTF-8)`,
  //          """
  //            |<html>
  //            | <body>
  //            |   OOPS! The resource can't be found.
  //            | </body>
  //            |</html>
  //            """.stripMargin
  //        )
  //      )
  //  }
  //
  //  // "manual" version
  //  Http().newServerAt("localhost", 8082).connectionSource().runForeach {
  //    _.handleWith(streamsBasedRequestHandler)
  //  }
  // shorthand version

  //  Http().newServerAt("localhost", 8082).bindFlow(streamsBasedRequestHandler)

  /**
   * Exercise: create your own HTTP server running on localhost on 8388, which replies
   *   - with a welcome message on the "front door" localhost:8388
   *   - with a proper HTML on localhost:8388/about
   *   - with a 404 message otherwise
   */

  val syncExerciseHandler: HttpRequest => HttpResponse = {
    case HttpRequest(HttpMethods.GET, Uri.Path("/"), _, _, _) =>
      HttpResponse(
        // status code OK (200) is default
        entity = HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          "Hello from the exercise front door!"
        )
      )

    case HttpRequest(HttpMethods.GET, Uri.Path("/about"), _, _, _) =>
      HttpResponse(
        // status code OK (200) is default
        entity = HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          """
            |<html>
            | <body>
            |   <div style="color: red">
            |     Hello from the about page!
            |   <div>
            | </body>
            |</html>
            """.stripMargin
        )
      )

    // path /search redirects to some other part of our website/webapp/microservice
    case HttpRequest(HttpMethods.GET, Uri.Path("/search"), _, _, _) =>
      HttpResponse(
        StatusCodes.Found,
        headers = List(Location("http://google.com"))
      )

    case request: HttpRequest =>
      request.discardEntityBytes()
      HttpResponse(
        StatusCodes.NotFound,
        entity = HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          "OOPS, you're in no man's land, sorry."
        )
      )
  }

  val bindingFuture = Http().newServerAt("localhost", 8388).bindSync(syncExerciseHandler)

  // shutdown the server:
  //  bindingFuture
  //    .flatMap(_.unbind)
  //    .onComplete(_ => system.terminate())

}

