package AkkaEssentials.Actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorBehaviour extends App {

  /**
   * Kid accepts to play or rejects on ask by mother on the basis of whether Vegetables or chocolates
   * was given as food
   */
  object FussyKid {

    case object KidAccept

    case object KidReject

    val HAPPY: String = "happy"
    val SAD: String = "sad"
  }

  class FussyKid extends Actor {

    var state: String = FussyKid.HAPPY

    override def receive: Receive = {
      case Mother.Food(Mother.VEGETABLE) => state = FussyKid.SAD
      case Mother.Food(Mother.CHOCOLATE) => state = FussyKid.HAPPY
      case Mother.Ask(_) =>
        if (state == FussyKid.HAPPY) sender() ! FussyKid.KidAccept
        else sender() ! FussyKid.KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Mother.Food(Mother.VEGETABLE) => context.become(sadReceive) // change my handler to sad receive
      case Mother.Food(Mother.CHOCOLATE) => //stay happy
      case Mother.Ask(_) => sender() ! FussyKid.KidAccept
    }

    def sadReceive: Receive = {
      case Mother.Food(Mother.VEGETABLE) => // stay sad
      case Mother.Food(Mother.CHOCOLATE) => context.become(happyReceive) // change my handler to happy receive
      case Mother.Ask(_) => sender() ! FussyKid.KidReject
    }
  }

  object Mother {

    case class Food(food: String)

    case class Ask(message: String)

    case class Start(kidRef: ActorRef)

    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolates"
  }

  class Mother extends Actor {
    override def receive: Receive = {
      case Mother.Start(kidRef) =>
        kidRef ! Mother.Food(Mother.VEGETABLE)
        kidRef ! Mother.Ask("Do you want to play")
      case FussyKid.KidReject => println("My kid is sad")
      case FussyKid.KidAccept => println("My kid is happy")
    }
  }

  val system = ActorSystem("changingActorBehaviour")
  val fussyKid = system.actorOf(Props[FussyKid])
  val mother = system.actorOf(Props[Mother])

  mother ! Mother.Start(fussyKid)

  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])
  mother ! Mother.Start(statelessFussyKid)

  // we can stack handlers with become and unbecome
  // the kid becomes increasingly fussy as he keeps getting vegetables and becomes better on chocolates

  class StatelessIncrementallyFussyKid extends Actor {
    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Mother.Food(Mother.VEGETABLE) => context.become(sadReceive, discardOld = false) // change my handler to sad receive
      case Mother.Food(Mother.CHOCOLATE) => // context.unbecome() understand context become - this example is bad
      case Mother.Ask(_) => sender() ! FussyKid.KidAccept
    }

    def sadReceive: Receive = {
      case Mother.Food(Mother.VEGETABLE) => context.become(sadReceive, discardOld = false)
      case Mother.Food(Mother.CHOCOLATE) => context.unbecome() // change my handler to happy receive
      case Mother.Ask(_) => sender() ! FussyKid.KidReject
    }
  }

  val statelessIncrementallyFussyKid = system.actorOf(Props[StatelessIncrementallyFussyKid])
  mother ! Mother.Start(statelessIncrementallyFussyKid)


  /**
   * 1 - recreate the counter actor with context become - no mutable state
   * 2 - simplified voting system
   */

  object Counter {

    case object Increment

    case object Decrement

    case object Print

  }

  class Counter extends Actor {

    // we create a countReceive handler that takes the count and gives a receive method
    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Counter.Increment => context.become(countReceive(currentCount + 1))
      case Counter.Decrement => context.become(countReceive(currentCount - 1))
      case Counter.Print => println(s"Currently counter is $currentCount")
    }
  }

  val counter = system.actorOf(Props[Counter])

  counter ! Counter.Increment
  counter ! Counter.Increment
  counter ! Counter.Decrement
  counter ! Counter.Print

  /**
   * Simplified voting system
   */

  case class Vote(candidate: String)

  case object VoteStatusRequest

  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {

    def voted(candidate: String): Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }

    override def receive: Receive = {
      case Vote(c) => context.become(voted(c))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    /*
      // old implementation wit state
      // var candidate:Option[String] = None
      override def receive: Receive = {
        case Vote(c) => candidate = Some(c)
        case VoteStatusRequest => sender() ! VoteStatusReply(candidate)
      }
     */
  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {
    //    var stillWaiting : Set[ActorRef] = Set()
    //    var currentStats : Map[String,Int] = Map()
    //    override def receive: Receive = {
    //      case AggregateVotes(citizens) =>
    //        stillWaiting = citizens
    //        citizens.foreach(_ ! VoteStatusRequest)
    //      case VoteStatusReply(None) =>  // sender() ! VoteStatusRequest // This is a infinite loop
    //      case VoteStatusReply(Some(c)) =>
    //        stillWaiting -= sender()
    //        val newVotesOfCandidate = currentStats.getOrElse(c,0) + 1
    //        currentStats += (c-> newVotesOfCandidate)
    //        if(stillWaiting.isEmpty) println(s"aggregator -> poll stats $currentStats")
    //    }

    override def receive: Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(_ ! VoteStatusRequest)
        context.become(awaitingStatus(citizens, Map()))
    }

    def awaitingStatus(stillWaiting: Set[ActorRef], currentStats: Map[String, Int]): Receive = {
      case VoteStatusReply(None) => // sender() ! VoteStatusRequest // This is a infinite loop
      case VoteStatusReply(Some(c)) =>
        val newStillWaiting = stillWaiting - sender()
        val newVotesOfCandidate = currentStats.getOrElse(c, 0) + 1
        val newCurrentStats = currentStats + (c -> newVotesOfCandidate)
        if (newStillWaiting.isEmpty) println(s"aggregator -> poll stats $currentStats")
        else context.become(awaitingStatus(newStillWaiting, newCurrentStats))

    }
  }

  val alice = system.actorOf(Props[Citizen])
  val charles = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val sachin = system.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  charles ! Vote("Jonas")
  bob ! Vote("Martin")
  sachin ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, charles, bob, sachin))

  // refactoring to not use state means to check the places state is being used and
  // replace it with context.become instead
}
