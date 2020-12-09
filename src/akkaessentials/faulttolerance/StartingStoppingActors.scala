package akkaessentials.faulttolerance

import akkaessentials.faulttolerance.StartingStoppingActors.Parent.StartChild
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Kill, PoisonPill, Props, Terminated}

object StartingStoppingActors extends App {

  val system = ActorSystem("StoppingActorsDemo")

  object Parent {

    case class StartChild(name: String)

    case class StopChild(name: String)

    case object Stop

  }

  class Parent extends Actor with ActorLogging {

    import Parent._

    override def receive: Receive = withChildren(Map())

    def withChildren(children: Map[String, ActorRef]): Receive = {
      case StartChild(name) =>
        log.info(s"Starting child $name")
        context.become(withChildren(children + (name -> context.actorOf(Props[Child], name))))
      case StopChild(name) =>
        log.info(s"Stopping child $name")
        val childOption = children.get(name)
        childOption.foreach(context.stop)
      //        context.become(withChildren(children.remo))
      case Stop =>
        // waits until all children die and then kills itself
        context.stop(self)
    }

  }

  class Child extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

//  val parent = system.actorOf(Props[Parent], "parent")
//  parent ! Parent.StartChild("child1")
//
//  val child = system.actorSelection("/user/parent/child1")
//  child ! "hi kid!"

//  parent ! Parent.StopChild("child1")
  // receives quite a few messages before stopping
  //  for (_ <- 1 to 50) child ! "are you still there?"

  /**
   * Parent creates a second child
   * send a message to the second child
   * Stop parent
   * Send messages to both parent and child
   */

//  parent ! Parent.StartChild("child2")
//  val child2 = system.actorSelection("/user/parent/child2")
//  child2 ! "hi kid2!"
//
//  parent ! Parent.Stop
//  for(_ <- 1 to 20) parent ! "parent are you there"
//  for(_ <- 1 to 20) child2 ! s"child2 are you there"

  /**
   *
   * Method -2 using special messages
   * PoisonPill is better than Kill. Kill abruptly stops raising an exception
   */
//
//  val looseActor = system.actorOf(Props[Child])
//
//  looseActor ! "hello loose actor"
//  looseActor ! PoisonPill
//  looseActor ! "loose actor are you still there"
//
//  val abruptTermination = system.actorOf(Props[Child])
//
//  abruptTermination ! "hello to be terminated"
//  abruptTermination ! Kill
//  abruptTermination ! "you have been terminated"


  /**
   * Death Watcher
   */

  class Watcher extends Actor with ActorLogging {

    import Parent._

    override def receive: Receive = {
      case StartChild(name) =>
        log.info(s"Watcher -> Starting child $name")
        context.watch(context.actorOf(Props[Child], name))
        // there is a context.unwatch
      case Terminated(ref) =>
        log.info(s"Watcher -> the ref $ref has been terminated")
    }
  }

  val watcher = system.actorOf(Props[Watcher],"watcher")
  watcher ! StartChild("watchedChild")
  val watchedChild = system.actorSelection("/user/watcher/watchedChild")
  Thread.sleep(500)
  watchedChild ! PoisonPill


}
