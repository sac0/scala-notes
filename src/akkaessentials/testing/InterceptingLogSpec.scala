package akkaessentials.testing

import akkaessentials.testing.InterceptingLogSpec.{Checkout, CheckoutActor}
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{EventFilter, ImplicitSender, TestKit}
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

class InterceptingLogSpec extends TestKit(ActorSystem("InterceptingLogSpec", ConfigFactory.load().getConfig("interceptingLogMessages")))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll {
  // order of with traits natters a lot
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val item = "Akka Course"
  val creditCard = "1234-1234-1234-1234"
  val invalidCreditCard = "0234-1234-1234-1234"
  "A checkout Flow" should {
    "correct log the dispatch of an order" in {
      EventFilter.info(pattern = s"Order [0-9]+ for the item $item has been dispatched", occurrences = 1) intercept {
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! Checkout(item, creditCard)
      }
    }
    "freak out on payment denied" in {
      EventFilter[RuntimeException](occurrences = 1) intercept {
        val checkoutRef = system.actorOf(Props[CheckoutActor])
        checkoutRef ! Checkout(item, invalidCreditCard)
      }
    }
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
        context.become(pendingFulfilment)
      case PaymentDenied => throw new RuntimeException("PaymentDenied")
    }

    def pendingFulfilment: Receive = {
      case OrderConfirmed => context.become(awaitingCheckout)
    }
  }

  class PaymentManager extends Actor {
    override def receive: Receive = {
      case AuthorizeCard(card) =>
        if (card startsWith "0") sender() ! PaymentDenied
        else {
          // intercept method waits for only 4 seconds
          // chage filter-leeway for 5 seconds and uncomment this
//          Thread.sleep(4000)
          sender() ! PaymentAccepted
        }
    }

  }

  class FulfilmentManager extends Actor with ActorLogging {
    var orderId = 43

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
