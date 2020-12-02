package AkkaEssentials.Actors

import AkkaEssentials.Actors.ActorCapabilities.SimpleBankAccount.{Deposit, TransactionFailure, TransactionSuccess, Withdraw}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.util.{Failure, Success}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello there!"
      case "print-context" => println(s"simple-actor -> context is ${context.self}")
      case message: String => println(s"simple-actor -> i $self have received $message")
      case integer: Int => println(s"simple-actor -> i have received number $integer")
      case SimpleMessage(contents) => println(s"simple-actor -> simple message $contents")
      case SendMessageToYourself(contents) => {
        // this will trigger the message:String case class
        self ! contents
      }
      case SayHiTo(ref) => ref ! "Hi!"
      case WirelessPhoneMessage(contents, ref) => ref forward (contents + "s")
    }
  }

  val system = ActorSystem("actorSystemCapabilities")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")
  simpleActor ! "hello actor"

  // 1. Messages can be any type
  // when we send 42 it will invoke the partial function. It can send any primitive data by default
  // messages must be immutable and serializable
  // in practice use only case classes and case objects

  simpleActor ! 42

  case class SimpleMessage(contents: String)

  simpleActor ! SimpleMessage("Some simple message")

  //2. Context
  // each actor has a member called context. It is a comlex data structure that has information that actor runs on
  // it has information of actor system and has a reference to actor reference
  // context.self is this -> refers to the current actorRef
  simpleActor ! "print-context"
  println(simpleActor) // similar to the print from the akka

  case class SendMessageToYourself(contents: String)

  simpleActor ! SendMessageToYourself("I am self message")

  // 3. Actors can reply to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  // use reference to send hi
  case class SayHiTo(ref: ActorRef)

  alice ! SayHiTo(bob)
  // def !(message : scala.Any)(implicit sender : akka.actor.ActorRef = { /* compiled code */ })
  // reference to alice is passed as an implicit val in the ! function
  // at any point of time context.sender is the actor ref that has sent the message
  // in sayHaiTo case the message was sent to bob inside the actor system where self is passed as the sender
  // when called from outside the sender is null or Actor.noSender - so Hi! handler will not have context.sender

  // 4 dead letter
  alice ! "Hi!" // reply to me
  // this will deliver to a dead letter queue
  // 5 - Forwarding Messages
  // D -> A -> B
  // sending a message with the original message

  case class WirelessPhoneMessage(contents: String, ref: ActorRef)

  // forward preserves the actual sender in sender object
  alice ! WirelessPhoneMessage("Hi", bob)

  /*
  *
  *
  * 1. Counter Actor
  *   - respond to increment to decrement counter
  *   - Print the current counter at its current value
  *
  * 2. Bank Account
  *     probably hold an internal amount for bank account
  *   - Deposit an Amount
  *   - Withdraw an Amount
  *   - Statement
  *   replied back with a
  *     Success/Failure
  *
  *  Interact with the person actor
  * */
  class SimpleCounter extends Actor {

    var counter = 0;

    override def receive: Receive = {
      case ("decrement", amount: Int) => {
        counter -= amount
      }
      case ("increment", amount: Int) => {
        counter += amount
      }
      case "print" => println(s"Current count is $counter")
    }
  }

  val simpleCounter = system.actorOf(Props[SimpleCounter], "simplecounter");

  simpleCounter ! ("increment", 80)
  simpleCounter ! ("decrement", 40)
  simpleCounter ! "print"


  object SimpleBankAccount {

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case object Statement

    case class TransactionSuccess(message: String)

    case class TransactionFailure(reason: String)

  }

  class SimpleBankAccount extends Actor {
    var balance = 0

    override def receive: Receive = {
      case Withdraw(amount) => {
        if (amount < 0 || amount > balance) {
          sender() ! TransactionFailure(s"Invalid Amount $amount")
        } else {
          balance -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }
      }
      case Deposit(amount) => {
        if (amount < 0) sender() ! TransactionFailure(s"Invalid Amount $amount")
        else {
          balance += amount
          sender() ! TransactionSuccess(s"Successfully deposited $amount")
        }
      }
      case SimpleBankAccount.Statement => sender() ! TransactionSuccess(s"Current balance is $balance")
    }
  }

  object SimpleBankCustomer {

    case class AccountRef(accountRef: ActorRef)

  }

  class SimpleBankCustomer extends Actor {
    override def receive: Receive = {
      case SimpleBankCustomer.AccountRef(accountRef) => {
        accountRef ! Deposit(1000)
        accountRef ! Withdraw(500)
        accountRef ! Deposit(-1000)
        accountRef ! Withdraw(200)
        accountRef ! SimpleBankAccount.Statement
      }
      case message => println(s"simple-bank-account -> $message")
    }
  }

  val simpleBankAccount = system.actorOf(Props[SimpleBankAccount], "bankaccount")
  val simpleBankCustomer = system.actorOf(Props[SimpleBankCustomer], "bankcustomer")

  simpleBankCustomer ! SimpleBankCustomer.AccountRef(simpleBankAccount)

  /**
   * Can we assume any ordering of messages
   * arent we causing race conditions
   * what does asynchronous mean here
   */
}
