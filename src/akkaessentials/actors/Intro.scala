package akkaessentials.actors

import akkaessentials.recap.ScalaAdvancedRecap.Person
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Intro extends App {

  // Everything starts with creating an actor system
  // Actorsystem is heavy weight data structure. It creates and controls threads that allocate to actors
  // recommended to have only one actor system unless with good reason
  // we cannot have non alphanumeric characters in the name

  // check akka scala target version before adding
  // 1. create actor systems
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // actors are uniquely identified
  // messages are asynchronous
  // actors are really encapsulated. we cannot read their state without they giving us entry
  // each actor might act differently on a message

  // 2. create actors

  class WordCountActor extends Actor {

    // accumulator
    var wordCount = 0

    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received $message")
        wordCount += message.split(" ").length
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // 3. We cannot instantiate the actors with the new keyword
  // instead we use the actor System to do so to us - we use actorOf
  // same name restrictions apply on the naming here, we need to pass a props object to the initializer
  //we can only communicate to the instance through this actor reference
  val wordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "wordcounter")
  // the name of each actor has to be different else it throws a message
  val anotherWordCounter: ActorRef = actorSystem.actorOf(Props[WordCountActor], "anotherwordcounter")

  // 4. communicate
  // we use ! to send the messages to the actor reference
  wordCounter ! "I'm learning akka and it is pretty damn cool"
  anotherWordCounter ! "A different message"

  // we cannot do new WordCountActor
  // https://github.com/akka/akka/blob/master/akka-actor/src/main/scala/akka/actor/Actor.scala
  // refer by searching ActorCell
  // ! is known as tell. receive is Any to Unit it is also aliased by receive which is the same thing
  // How do we instantiate with plain classes

  // how do we instantiate an actor
  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"Hi, my name is $name")
      case _ => // do nothing
    }
  }

  // implicitly takes a class and returns an implicit defined for it. it is in predef

  val person = actorSystem.actorOf(Props(new Person("bob")))
  person ! "hi"

  // the best practice is to prevent any closure over the constructor since it delays the execution
  // remember the mutable example counter
  // we can use
  /*
    object DemoActor {
      def props(magicNumber: Int): Props = Props(classOf[DemoActor], magicNumber)
    }
   */
  // But that catches no matching constructor in runtime only citing an illegalArgumentException

  /*

    final class ParentActor extends Actor {
      var counter = 0
      override def receive = {
        case Increment   => counter++
        case CreateChild => context.actorOf(Props(new ChildActor(counter)))
      }
    }

    final class ChildActor(value: Int) extends Actor {
      println(value)
      override def receive = Actor.emptyBehavior
    }
    object ChildActor {
      final val Name = "child-actor"
      def apply(value: Int): Props = Props(new ChildActor(value))
    }

    final class ParentActor extends Actor {
      var counter = 0
      override def receive = {
        case Increment   => counter++
        case CreateChild => context.actorOf(ChildActor(counter), ChildActor.Name))
      }
    }
   */
  // Best practice example

  object Person {
    def props(name:String) = Props(new Person(name))
  }
  val improvedPerson = actorSystem.actorOf(Person.props("Sachin"))

  improvedPerson ! "hi"



}
