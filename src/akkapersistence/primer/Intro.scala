package akkapersistence.primer

import java.util.Date

import akka.actor.{ActorLogging, ActorSystem, Props}
import akka.persistence.PersistentActor

object Intro extends App {

  /**
   * tracing orders in an online store
   * transactions history in a bank
   * document versioning in a dropbox like system
   *
   * Problems
   * How did we arrive at the current state
   * How do we query a previous state
   */

  /**
   *
   * Since all the events are only appended we can get a highly optimized writes
   * avoids relational stores and ORM entirely
   * full traae of every state
   * This fits the akka model perfectly
   * querying a state is expensive since we have to relay sll the events
   * potential performance issues with ong term streams - we sue snapshotting for this
   * schema evolution
   * a very different model to storing data
   */

  /**
   * Persistent actors can do everything the a normal actor can do
   * send recieve messages and hold internal state
   * run in parallel like other actors
   *
   * Extra capabilities
   * has a persistence id and saves into a persistent store
   * recover state by replaying events
   * When an actor starts restarts -> it will start from by playing all the events
   */

  /**
   * An accountant keeps track of invoices
   */

  case class Invoice(recipient: "the sofa company", date: Date, amount: Int)

  case class InvoiceRecorded(id: Int, recipient: "the sofa company", date: Date, amount: Int)

  class Accountant extends PersistentActor with ActorLogging {
    var latestInvoiceId = 0
    var totalAmount = 0

    override def persistenceId: String = "simple-accountant" // make this unique as a best practice
    // this is the receive command for this
    override def receiveCommand: Receive = {
      /**
       * Create an event to persist
       * persist the event and pass a callback that is called on successful persist
       * we update the actors state when the event has persisted
       */
      /**
       * Persistence is async. Never call methods in callbacks. but in this case it is safe
       * correctly identify the sender in the callback
       * Closing upon is not a problem here in this callback like increase the var totalAmount
       * Can persist only selective messages. ignore messages like print
       * But we can receive other messages withing the time gap to persist
       */
      case Invoice(recepient, date, amount) =>
        log.info(s"Receive invoice for amount $amount")
        persist(InvoiceRecorded(latestInvoiceId, recepient, date, amount)) { event =>

          // best practice do it in the handler  after persistence
          latestInvoiceId += 1
          totalAmount += amount
          log.info(s"persisted $event for total amount $totalAmount")
        }
        //act like a normal actor here in some cases
      case "print" =>
        log.info(s"current amount $totalAmount and last invoice id of $latestInvoiceId")

    }

    // this is how this actor will recover
    override def receiveRecover: Receive = {
      case InvoiceRecorded(id, _, _, amount) =>
        log.info(s"recovering $amount for id $id")
        latestInvoiceId = id
        totalAmount += amount
    }
  }

  val system: ActorSystem = ActorSystem("IntroPersistence")
  val accountant = system.actorOf(Props[Accountant], "simpleaccountant")

  for (i <- 1 to 10) {
    accountant ! Invoice("the sofa company", new Date, i * 100)
  }


}
