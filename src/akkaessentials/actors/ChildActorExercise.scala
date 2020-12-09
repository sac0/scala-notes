package akkaessentials.actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorExercise extends App {

  // distributed word counting
  /**
   * WordCountMaster, Worker
   * create a master - initialize message 10
   * it creates 10 workers
   * send a text `Akka is Awesome` to wordCountMaster
   * requester sends a text
   * round robin logic
   *
   */

  val system = ActorSystem("ChildExercise")

  object WordCounterMaster {

    case class Initialize(nWorkers: Int)

    case class WordCountTask(text: String)

    case class WordCountReply(count: Int)

  }

  class WordCounterMaster extends Actor {
    override def receive: Receive = {
      case WordCounterMaster.Initialize(nWorkers) => {
        val workerRefList: IndexedSeq[ActorRef] = 1 to nWorkers map { _ => system.actorOf(Props[WordCounterWorker]) }
        context.become(assignTasks(workerRefList, 0, 0))
      }
    }

    def assignTasks(workerRefList: IndexedSeq[ActorRef], currentIndex: Int, currentCounter: Int): Receive = {
      case WordCounterMaster.WordCountTask(text) => {
        workerRefList(currentIndex) ! WordCounterMaster.WordCountTask(text)
        val nextIndex: Int = (currentIndex + 1) % workerRefList.length
        context.become(assignTasks(workerRefList, nextIndex, currentCounter))
      }
      case WordCounterMaster.WordCountReply(count: Int) => {
        println(currentCounter+count)
        context.become(assignTasks(workerRefList, currentIndex, currentCounter + count))
      }
    }
  }

  class WordCounterWorker extends Actor {
    override def receive: Receive = {
      case WordCounterMaster.WordCountTask(text: String) => sender() ! WordCounterMaster.WordCountReply(text.split(' ').length)
    }
  }

  // round robin

}
