package akkastreams.primer

import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}

object BackpressureBasics extends App {
  /**
   * elements flow as demand from consumers
   * consumers ask and the data flows
   * backpressure is all about maintenance of speed among the async components
   * Fast Consumers : all is well
   * Slow Consumers : issue
   * consumer will send a signal to producers to slow down
   */

  implicit val system: ActorSystem = ActorSystem("BackpressureBasics")

  val fastSource = Source(1 to 1000).map({
    x => {
//      println(s"x$x")
      x
    }
  })
    val slowSink = Sink.foreach[Int] { x =>
      Thread.sleep(1000)
      println(s"Sink $x")
    }

  // this is fusion and not backpressure run once a second
  //  fastSource.to(slowSink).run()

  // this is backpressure even if we have intermediate flows like in the next example. backpressure is transparent
  // fastSource.async.to(slowSink).run()

  val simpleFlow = Flow[Int].map { x =>
    println(s"Incoming $x")
    x + 1
  }
  /**
   * we see a batch of incoming and sink. from 16 it becomes only 8. default buffer is 16 elements
   * Buffering is a reaction to backpressure
   */

//  fastSource.async
//    .via(simpleFlow).async
//    .to(slowSink)
//    .run()
  /**
   * Reactions to Backpressure
   * slow down if possible
   * buffer elements
   * drop elements on overflow
   * tear down the stream failure
   */
  val bufferedFlow = simpleFlow.buffer(4, OverflowStrategy.backpressure)
    fastSource.async
      .via(bufferedFlow).async
      .to(slowSink)
      .run
  /*
  1-16: nobody is backpressured
  17-26: flow will buffer, flow will start dropping at the next element
  26-1000: flow will always drop the oldest element
    => 991-1000 => 992 - 1001 => sink
 */

  /*
    overflow strategies:
    - drop head = oldest
    - drop tail = newest
    - drop new = exact element to be added = keeps the buffer
    - drop the entire buffer
    - backpressure signal
    - fail
   */

  // throttling

  //  import scala.concurrent.duration._
  //  fastSource.throttle(10, 1 second).runWith(Sink.foreach(println))

}
