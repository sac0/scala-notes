package AkkaEssentials.Infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

object Dispatchers extends App {

  class Counter extends Actor with ActorLogging {
    var count = 0

    override def receive: Receive = {
      case message =>
        count += 1
        log.info(s"$count - $message")
    }
  }

  val system = ActorSystem("DispatcherDemo")
  //  , ConfigFactory.load().getConfig("dispatchersDemo"))
  val actors = for (i <- 1 to 5) yield system.actorOf(Props[Counter].withDispatcher("my-dispatcher"), s"counter_$i")
  val r = new Random()
  for (i <- 1 to 200) {
    actors(r.nextInt(5)) ! s"message $i"
  }
  /**
   * the actors need a thread to attach themselves and do a task
   * dispatcher gives them that thread
   * once a throughput is achieved the dispatcher gives that thread to someone else
   */

  // method -2
  val rtjvmActor = system.actorOf(Props[Counter], "rtjvm")

  /**
   * Dispatcher implement ExecutorContext trait
   */
  class DBActor extends Actor with ActorLogging {
    implicit val executionContext: ExecutionContext = context.system.dispatchers.lookup("my-dispatcher")
    // we can have custom dispatchers for long running tasks
    // context.dispatcher
    // Solution 2 PinnedDispatcher CallingThreadDispatcher - all invocations happens on the calling thread
    override def receive: Receive = {
      case message => Future {
        //wait for a resource
        Thread.sleep(1500)
        log.info(s"Success $message")
      }
    }
  }

  val dbActor = system.actorOf(Props[DBActor])
  dbActor ! "the meaning of life is 42"
  val nonBlockingActor = system.actorOf(Props[Counter])
  for (i<- 1 to 1000) {
    val message = s"important message $i"
    dbActor ! message
    nonBlockingActor ! message
  }

}
