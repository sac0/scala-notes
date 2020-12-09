package akkaessentials.actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors extends App {

  //   Actors can create other actors
  object Parent {

    case class CreateChild(name: String)

    case class TellChild(message: String)

  }

  class Parent extends Actor {

    import Parent._

    var child: ActorRef = _

    override def receive: Receive = {
      case CreateChild(name) =>
        // creating child here
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(child: ActorRef): Receive = {
      // for safety use if(child != null)
      case TellChild(message) => child forward message

    }
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"${self.path} I got $message")
    }
  }

  val system = ActorSystem("ParentChildDemo")
  val parent = system.actorOf(Props[Parent], "parent")

  parent ! Parent.CreateChild("child")
  parent ! Parent.TellChild("Hey kid!")

  // actor hierarchies
  // parent -> child -> grandchild

  /**
   * Guardian actors. There are three guardian actors
   * There are 3 guardian actos
   * - /user - for all user created threads
   * - /system - system guardian
   * - / = root guardian
   */

  /** Actor selection */

  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you!"

  //  val invalidChildSelection = system.actorSelection("/user/parent/childe")
  //  the ones that doesn't exists in a path gives a ref that goes to dead letters
  //  invalidChildSelection ! "I found you!"

  // Do not pass this in the actor to the children

  object NaiveBankAccount {

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object InitializeAccount

  }

  class NaiveBankAccount extends Actor {

    var amount = 0

    override def receive: Receive = {
      case NaiveBankAccount.InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        println(creditCardRef)
        creditCardRef ! CreditCard.AttachToAccount(this) // this is a problem
      case NaiveBankAccount.Deposit(funds) => deposit(funds)
      case NaiveBankAccount.Withdraw(funds) => withdraw(funds)
    }

    def deposit(funds: Int): Unit = {
      println(s"${self.path} Depositing $funds on $amount")
      amount += funds
    }

    def withdraw(funds: Int): Unit = {
      println(s"${self.path} Withdrawing $funds from $amount")
      amount -= funds
    }
  }

  object CreditCard {

    case class AttachToAccount(bankAccount: NaiveBankAccount)

    case object CheckStatus

  }

  class CreditCard extends Actor {
    override def receive: Receive = {
      case CreditCard.AttachToAccount(account) =>
        context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CreditCard.CheckStatus => println(s"${self.path} your message has been processed")
        account.withdraw(1) // because i can
    }
  }

  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! NaiveBankAccount.InitializeAccount
  bankAccountRef ! NaiveBankAccount.Deposit(100)

  Thread.sleep(500)
  val ccSelection = system.actorSelection("/user/account/card")
  ccSelection ! CreditCard.CheckStatus

  // Never close over mutable state or this
}
