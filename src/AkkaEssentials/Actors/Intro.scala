package AkkaEssentials.Actors
import akka.actor.ActorSystem

object Intro extends App {

  // Everything starts with creating an actor system
  // Actorsystem is heavy weight data structure. It creates and controls threads that allocate to actors
  // recommended to have only one actor system unless with good reason
  // we cannot have non alphanumeric characters in the name
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

}
