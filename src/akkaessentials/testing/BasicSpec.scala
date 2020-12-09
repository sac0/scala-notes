package akkaessentials.testing

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.DurationInt
import scala.util.Random


/**
 * Recommend naming the classes ending with Spec
 * Extends TestKit - It is initialized with a ActorSystem
 * Implicit sender which is used for send reply scenarios
 * AnyWordSpecLike is a trail to allow the test cases to input as readable strings - behavioural testing
 * BeforeAndAfterAll gives some hooks into the akka system
 */

/**
 * In the companion object store all the methods and values that are being used in the tests
 * afterAll is a teardown method
 */
class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
  import BasicSpec._

  // String has an implicit conversion to a class that has the should method
  "An echo actor" should {
    "send back the same message" in {
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "Hello Test"
      echoActor ! message
      expectMsg(message)

      /**
       * Who is receiving the message and who is sending here
       * testActor is a member of the testkit library and that is added
       * as a semde
       */
    }
  }
  "An black Hole actor" should {
    "send back the same message" in {
      val blackHole = system.actorOf(Props[BlackHole])
      val message = "Hello Test"
      blackHole ! message
      // By default we wait for 3 seconds while waiting fot a time
      // akka.test.single-expect-default is where this timeout is picked from

      // uncomment the below to see an error
      // expectMsg(message)
      expectNoMessage(1 second)

    }
  }
  "An lab test actor" should {

    val labTestActor = system.actorOf(Props[LabTestActor])
    "send an uppercase message" in {
      val message = "Hello Test"
      labTestActor ! message
      // By default we wait for 3 seconds while waiting fot a time
      // akka.test.single-expect-default is where this timeout is picked from

      // uncomment the below to see an error
      // expectMsg(message)
      // expectMsg(message.toUpperCase)
      val reply = expectMsgType[String]
      assert(reply == message.toUpperCase)
    }
    // any one of multiple message check
    "greeting message return a greeting" in {
      val message = "greeting"
      labTestActor ! message
      expectMsgAnyOf("hi","hello")
    }
    // multiple in order check
    "replies with two messages" in {
      labTestActor ! "favoriteTech"
      expectMsgAllOf("Scala","Akka")
    }
    "reply with cool tech in different way" in {
      labTestActor ! "favoriteTech"
      val messages = receiveN(2)
      // feel free to do more complicated assertions
    }
    "reply with cool tech in fancy way" in {
      labTestActor ! "favoriteTech"
      //only care that PF is defined
      expectMsgPF() {
        case "Scala" =>
        case "Akka" =>
      }
    }
  }
}

object BasicSpec {
  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }
  class BlackHole extends Actor {
    // default behaviour of not doing anything
    override def receive: Receive = Actor.emptyBehavior
  }
  class LabTestActor extends Actor {
    // default behaviour of not doing anything
    val random = new Random()
    override def receive: Receive = {
      case "greeting" =>
        if(random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case "favoriteTech" =>
        sender() ! "Scala"
        sender() ! "Akka"
      case message :String => sender() ! message.toUpperCase
    }
  }
}
