package AkkaEssentials.Testing

import akka.actor.Props.default.withDispatcher
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.Duration

// we do not need testkit if we are doing synchronous testing
class SynchronousTestSpec extends AnyWordSpecLike with BeforeAndAfterAll {

  implicit val system = ActorSystem("SynchronousTestSpec")

  override def afterAll(): Unit = {
    system.terminate()
  }

  import SynchronousTestSpec._

  "A Counter" should {
    "synchronously increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Inc
      // after this this means that we have synchronously sent the message
      // here it is sent. It happens in the calling thread so this happens in this running thread
      assert(counter.underlyingActor.count  == 1)
    }
    "synchronously increase its counter using receive" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter.receive(Inc)
      // after this this means that we have synchronously sent the message
      assert(counter.underlyingActor.count  == 1)
    }
    "work on the calling thread dispatcher" in {
      // dispatcher makes the thread the calling thread as the one that runs it
      val counter  = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()
      probe.send(counter,Read)
      probe.expectMsg(Duration.Zero , 0)
    }
  }


}

object SynchronousTestSpec {
  case object Inc
  case object Read
  class Counter extends Actor {
    var count =0

    override def receive: Receive = {
      case Inc => count += 1
      case Read => sender() ! count
    }

  }
}
