package akkaessentials.patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}


class AskSpec extends TestKit(ActorSystem("AskSpec"))
  with ImplicitSender with AnyWordSpecLike with BeforeAndAfterAll {
  override def afterAll(): Unit = TestKit.shutdownActorSystem(system)

  import AskSpec._

  "An authenticator" should {
    authenticatorTestSuite(Props[AuthManager])
  }

  "A piped authenticator" should {
    authenticatorTestSuite(Props[PipedAuthManager])
  }

  def authenticatorTestSuite(props: Props): Unit = {
    import AuthManager._

    "fail to authenticate a non-registered user" in {
      val authManager = system.actorOf(props)
      authManager ! Authenticate("daniel", "rtjvm")
      expectMsg(AuthFailure(AUTH_FAILURE_NOT_FOUND))
    }

    "fail to authenticate if invalid password" in {
      val authManager = system.actorOf(props)
      authManager ! RegisterUser("daniel", "rtjvm")
      authManager ! Authenticate("daniel", "iloveakka")
      expectMsg(AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT))
    }

    "successfully authenticate a registered user" in {
      val authManager = system.actorOf(props)
      authManager ! RegisterUser("daniel", "rtjvm")
      authManager ! Authenticate("daniel", "rtjvm")
      expectMsg(AuthSuccess)
    }
  }

}

object AskSpec {

  case class Read(key: String)

  case class Write(key: String, value: String)

  class KVActor extends Actor with ActorLogging {
    override def receive: Receive = online(Map())

    def online(kv: Map[String, String]): Receive = {
      case Read(key) =>
        log.info(s"trying to read details of $key")
        sender() ! kv.get(key)
      case Write(key, value) =>
        log.info(s"trying to write details of $key - $value")
        context.become(online(kv + (key -> value)))
    }
  }

  case class RegisterUser(username: String, password: String)

  case class Authenticate(username: String, password: String)

  case class AuthFailure(message: String)

  case object AuthSuccess

  object AuthManager {
    val AUTH_FAILURE_NOT_FOUND = "AUTH_FAILURE_NOT_FOUND"
    val AUTH_FAILURE_PASSWORD_INCORRECT = "AUTH_FAILURE_PASSWORD_INCORRECT"
    val AUTH_FAILURE_SYSTEM = "AUTH_FAILURE_SYSTEM"
  }

  class AuthManager extends Actor with ActorLogging {
    protected val authDb: ActorRef = context.actorOf(Props[KVActor])

    implicit val timeout: Timeout = Timeout(1 second)


    override def receive: Receive = {
      /**
       * normally do
       * authDb ! Read(username)
       * context.become(waitingForPassword(username,sender())
       * def waitingForPassword(str:String, ref:ActorRef):Receive => {
       * case password:Option[String]
       * }
       */

      /**
       * Problems
       * Which user tried to authenticate with what password and at what time
       * this logic becomes complicated
       *
       * this sends me back an option string
       * Which user tried with what password - so i have to extend the kv actor to my personal use case
       * But that might be written by some one else or a library
       */

      /**
       * solutions
       * we will use the ask method
       * we need import akka pattern ask
       */

      case RegisterUser(username, password) => authDb ! Write(username, password)
      case Authenticate(username, password) => handleAuthentication(username, password)
    }

    def handleAuthentication(username: String, password: String): Unit = {
      val originalSender = sender()
      val future = authDb ? Read(username)
      future.onComplete {
        //          case Success(None) => sender() ! AuthFailure("password not found")
        //          case Success(Some(dbPassword)) =>
        //            if (dbPassword == password) sender() ! AuthSuccess
        //            else sender() ! AuthFailure("password incorrect")
        // step 5 most important
        // NEVER CALL METHODS ON THE ACTOR INSTANCE OR ACCESS MUTABLE STATE IN ONCOMPLETE.
        // avoid closing over the actor instance or mutable state
        case Success(None) => originalSender ! AuthFailure(AuthManager.AUTH_FAILURE_NOT_FOUND)
        case Success(Some(dbPassword)) =>
          if (dbPassword == password) originalSender ! AuthSuccess
          else originalSender ! AuthFailure(AuthManager.AUTH_FAILURE_PASSWORD_INCORRECT)
        case Failure(_) => originalSender ! AuthFailure(AuthManager.AUTH_FAILURE_SYSTEM)

      }(context.dispatcher)
    }
  }

  class PipedAuthManager extends AuthManager {

    import AuthManager._

    override def handleAuthentication(username: String, password: String): Unit = {
      // step 3 - ask the actor
      val future = authDb ? Read(username) // Future[Any]
      // step 4 - process the future until you get the responses you will send back
      val passwordFuture = future.mapTo[Option[String]] // Future[Option[String]]
      val responseFuture = passwordFuture.map {
        case None => AuthFailure(AUTH_FAILURE_NOT_FOUND)
        case Some(dbPassword) =>
          if (dbPassword == password) AuthSuccess
          else AuthFailure(AUTH_FAILURE_PASSWORD_INCORRECT)
      }(context.dispatcher) // Future[Any] - will be completed with the response I will send back

      // step 5 - pipe the resulting future to the actor you want to send the result to
      /*
        When the future completes, send the response to the actor ref in the arg list.
       */
      responseFuture pipeTo sender()
    }
  }


}
