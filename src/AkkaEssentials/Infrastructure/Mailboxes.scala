package AkkaEssentials.Infrastructure

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.dispatch.{PriorityGenerator, UnboundedPriorityMailbox}
import com.typesafe.config.Config

object Mailboxes extends App {
  
  class SimpleActor extends  Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(message.toString)
    }
  }

  val system = ActorSystem("MailboxesDemo")

  /**
   * prioritize p0 message ang then p1,p2,p3 etc
   */

  class SupportTicketPriorityMailbox(settings: ActorSystem.Settings, config: Config)
  extends UnboundedPriorityMailbox(
    PriorityGenerator {
      case message : String if(message.startsWith("p0")) => 0
      case message : String if(message.startsWith("p1")) => 1
      case message : String if(message.startsWith("p2")) => 2
      case message : String if(message.startsWith("p3")) => 3
    }
  )

  val supportTicketLogger = system.actorOf(Props[SimpleActor].withDispatcher("support-ticket-dispatcher"))
  supportTicketLogger ! "p3 -> nice to have"
  supportTicketLogger ! "p0 -> need now"
  supportTicketLogger ! "p1 -> do this when we have time"

}
