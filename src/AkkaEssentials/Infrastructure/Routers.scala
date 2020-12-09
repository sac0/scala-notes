package AkkaEssentials.Infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing.{ActorRefRoutee, Broadcast, FromConfig, RoundRobinGroup, RoundRobinPool, RoundRobinRoutingLogic, Router}

object Routers extends App {

  /**
   * Manual routing
   */

  class Master extends Actor {

    // step 1 create the refs to route
    private val slaves = for (_ <- 1 to 5) yield {
      val slave = context.actorOf(Props[Slave])
      context.watch(slave)
      ActorRefRoutee(slave)
    }

    // step -2 define router and a seq of routees
    private var router = Router(RoundRobinRoutingLogic(), slaves)

    override def receive: Receive = {

      // step - 3 handle termination for the routee
      case Terminated(ref) =>
        println(s"Terminated $ref")
        router = router.removeRoutee(ref)
        val newSlave = context.actorOf(Props[Slave])
        context.watch(newSlave)
        router = router.addRoutee(newSlave)
      case message => {
        // step 4 route to the slaves passing the sender
        router.route(message, sender())
      }
    }


  }

  class Slave extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("RoutersDemo")
  val masterActor = system.actorOf(Props[Master])

  // mesages are sent in round robin but received out of order
  for (i <- 1 to 10) masterActor ! s"Random Message $i"

  /**
   * round robin
   * random
   * smallest mailbox
   * broadcast
   * consistent hashing
   * tail-chopping
   * scatter gather
   */

  /**
   * Method 2 a router actor with its own children
   * Pool router
   */
  // 2.1 from code
  val poolMaster = system.actorOf(RoundRobinPool(5).props(Props[Slave]),"simplePoolMaster")
  for(i <- 1 to 10) poolMaster ! s"$i hello pool master"
  // 2.2 from routers demo
  val poolMaster2 = system.actorOf(FromConfig.props(Props[Slave]), "poolMaster2")

  /**
   * Method #3 - router with actors created elsewhere
   * GROUP router
   */
  // .. in another part of my application
  val slaveList = (1 to 5).map(i => system.actorOf(Props[Slave], s"slave_$i")).toList

  // need their paths
  val slavePaths = slaveList.map(_.path.toString)

  // 3.1 in the code
  val groupMaster = system.actorOf(RoundRobinGroup(slavePaths).props())
  //  for (i <- 1 to 10) {
  //    groupMaster ! s"[$i] Hello from the world"
  //  }

  // 3.2 from configuration
  val groupMaster2 = system.actorOf(FromConfig.props(), "groupMaster2")
  for (i <- 1 to 10) {
    groupMaster2 ! s"[$i] Hello from the world"
  }

  /**
   * Special messages
   */
  groupMaster2 ! Broadcast("hello, everyone")

  // PoisonPill and Kill are NOT routed
  // AddRoutee, Remove, Get handled only by the routing actor

}
