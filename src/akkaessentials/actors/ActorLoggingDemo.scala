package akkaessentials.actors

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}

object ActorLoggingDemo extends App {

  class SimpleActorWithLogging extends  Actor {
    // 1. explicit logging
    val logger: LoggingAdapter = Logging(context.system, this)

    override def receive: Receive = {
      case message => logger.info(message.toString)
    }

  }

  val system: ActorSystem = ActorSystem("LoggingDemo")
  val actor: ActorRef = system.actorOf(Props[SimpleActorWithLogging])

  actor ! "Logging a message"

  // 2. Actor With Logging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      // templates in actor log function
      case (a,b) => log.info("Tuple {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }


  val actorWithLogging: ActorRef = system.actorOf(Props[ActorWithLogging])
  actorWithLogging ! "logging a message with actor logging"
  actorWithLogging ! (42,65)



}
