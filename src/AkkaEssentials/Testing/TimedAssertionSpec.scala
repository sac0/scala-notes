package AkkaEssentials.Testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.DurationInt
import scala.util.Random

class TimedAssertionSpec extends TestKit(ActorSystem("TimedAssertionSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import TimedAssertionSpec._

  "a worker actor" should {
    val workerActor = system.actorOf(Props[WorkerActor])


    "reply with the meaning within 500ms" in {

      // Time boxed expect should be after 500 millis only. if it finishes early it is an error
      within(500 millis, 1 second) {
        workerActor ! "work"
        expectMsg(WorkResult(42))
      }
    }

    "reply with valid work in a time interval" in {
      workerActor ! "workSequence"
      val results: Seq[Int] = receiveWhile[Int](max = 2 seconds, idle = 500 millis, messages = 10) {
        case WorkResult(result) => result
      }
      assert(results.sum > 5)
    }
    "reply to the test probe in a timely manner" in {
      within(1 second) {
        val probe = TestProbe()
        probe.send(workerActor,"work")
        //can have its own timeout of 300 ms from the akka configuration
        probe.expectMsg(WorkResult(42))
      }
    }
  }

}

object TimedAssertionSpec {

  case class WorkResult(result: Int)

  class WorkerActor extends Actor {
    override def receive: Receive = {
      case "work" =>
        // long computation. This simulates something that takes as a bit of time to finish
        Thread.sleep(500)
        sender() ! WorkResult(42)
      case "workSequence" =>
        val random = new Random()
        for (_ <- 1 to 10) {
          Thread.sleep(random.nextInt(50))
          sender() ! WorkResult(1)
        }
    }


  }

}
