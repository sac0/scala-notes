package akkahttp.highlevelapis

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Route

object HighLevelIntro extends App {

  implicit val system: ActorSystem = ActorSystem("HighLevelAPIs")

  // directive

  import akka.http.scaladsl.server.Directives._

  /**
   * route is RequestContext => Future[RouteResult]
   * RequestContext contains all the ActorSystem, materialize , routing information etc, loggers
   * RequestContext can
   * Complete synchronously
   * asynchronously
   * handle it by returning to source (advanced) -?
   * reject and pass on
   * fail it
   */

  val simpleRoute: Route = path("home") {
    //directives specify what happens to request and when
    // path checks if the path has home or not
    complete(StatusCodes.OK)
  }

  val pathGetRoute: Route = path("home") {
    //directives specify what happens to request and when
    // path checks if the path has home or not
    // if we do a post request get returns 405 method not allowed
    get {
      complete(StatusCodes.OK)
    }
  }

  // chaining directive with ~
  val chainedRoute: Route =
    path("endpoint") {
      get {
        complete(StatusCodes.OK)
      } ~ post {
        complete(StatusCodes.Forbidden)
      }
    } ~ path("home") {
      complete(
        HttpEntity(
          ContentTypes.`text/html(UTF-8)`,
          """
            |<html>
            | <body>
            |   Hello from the high level Akka HTTP!
            | </body>
            |</html>
          """.stripMargin
        )
      )
    } // Routing tree

  // this simple route can be added to the bind instance
  // Routes are converted to a flow implicitly
  Http().newServerAt("localhost", 8080).bind(simpleRoute)


}
