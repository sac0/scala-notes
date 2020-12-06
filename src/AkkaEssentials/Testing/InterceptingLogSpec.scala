package AkkaEssentials.Testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class InterceptingLogSpec extends TestKit(ActorSystem("InterceptingLogSpec"))
  with ImplicitSender
  with BeforeAndAfterAll
  with AnyWordSpecLike {
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }
}

object InterceptingLogSpec {

  case class Checkout(item: String, creditCard: String)

  case class AuthorizeCard(creditCard: String)

  case object PaymentAccepted

  case object PaymentDenied

  case class DispatchOrder(item: String)

  case object OrderConfirmed

  class CheckoutActor extends Actor {
    private val paymentManager = context.actorOf(Props[PaymentManager])
    private val fulfilmentManager = context.actorOf(Props[FulfilmentManager])

    override def receive: Receive = awaitingCheckout

    def awaitingCheckout: Receive = {
      case Checkout(item, card) =>
        paymentManager ! AuthorizeCard(card)
        context.become(pendingPayment(item))
    }

    def pendingPayment(item: String): Receive = {
      case PaymentAccepted =>
        fulfilmentManager ! DispatchOrder(item)
        context.become(pendingFulfilMent(item))
      case PaymentDenied => println("Payment Denied")

    }

    def pendingFulfilMent(item: String): Receive = {
      case OrderConfirmed => context.become(awaitingCheckout)
    }

  }

  class PaymentManager extends Actor {
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card startsWith "0") sender() ! PaymentDenied
        else sender() ! PaymentAccepted
    }

  }

  class FulfilmentManager extends Actor with ActorLogging {
    var orderId = 43;

    override def receive: Receive = {
      case DispatchOrder(item: String) => orderId += 1
        log.info(s"Order $orderId for the item $item has been dispatched")
        sender() ! OrderConfirmed
    }

  }

  /**
   * We have a number of actors interacting with each other. the actors are creating 2 children.
   * It is difficult to test with a test probe in this case
   */

}
