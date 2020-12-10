package akkastreams.primer

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FirstPrinciples extends App {
  /**
   * Read and design asynchronous streams handling back pressure
   *
   * concepts
   * publisher -> emits events asynchronously
   * subscriber -> receives elements
   * transformer -> transforms the elements
   * async -> executed at no proper time
   * backpressure -> This buildup of data/messages within a stream is called backpressure.
   * Because a stream in SocketCluster can have multiple loops (aka consumers) iterating over it,
   * and because each loop can consume data from the stream at a different rate,
   * the slowest consumer will determine the backpressure of the stream
   *
   * Reactive Streams defines the concepts and how they work
   * It is a service interface kind of thing
   * libraries are free to implement their own thing
   *
   * source - may or may not stop - generates messages in rx terms emits
   * sink receives elements terminates only on the publisher termination
   * flow - processor
   *
   * direction
   * upstream -> towards the source
   * downstream -> towards the sink
   *
   */

  implicit val system: ActorSystem = ActorSystem("FirstPrinciples")

  val source = Source(1 to 10)
  val sink = Sink.foreach[Int](println)

  val graph = source.to(sink)
  //  graph.run()

  //flows transform elements
  val flow = Flow[Int].map(_ + 1)
  val sourceWithFlow = source.via(flow)
  val flowWithSink = flow.to(sink)

  // all valid ways to run a stream
  sourceWithFlow.to(sink).run()
  source.to(flowWithSink).run()
  source.via(flow).to(sink).run()

  // any data is allowed in streams
  // nulls are not allowed
  /**
   * This throws an error but the streams from before are completed
   * They happen in an async way too
   */

  //  val illegalSource = Source.single[String](null)
  //  illegalSource.to(Sink.foreach(println)).run()

  //various kind of sources

  val finiteSource = Source.single(1)
  val anotherFiniteSource = Source(List(1, 2, 3))
  val emptySource = Source.empty[Int]
  val infiniteSource = Source(LazyList.from(1))
  val futureSource = Source.future(Future(42))

  // sinks
  val theMostBoringSink = Sink.ignore
  val forEachSink = Sink.foreach[String](println)
  val headSink = Sink.head[Int]
  val foldSink = Sink.fold[Int, Int](0)(_ + _)

  // flows
  val mapFlow = Flow[Int].map(2 * _)
  val takeFlow = Flow[Int].take(5)
  // drop filter
  // doesnt have flatmap

  // can go through multiple flows
  val doubleFlowGraph = source.via(mapFlow).via(takeFlow).to(sink)
  doubleFlowGraph.run()

  val mapSource = Source(1 to 10).map(2 * _) // Source(1 to 10).via(Flow[Int].map(2*_))
  mapSource.runForeach(println) // mapSource.to(Sink.foreach[Int](println)).run()

  // operators - components

  val namesSource = Source(List("sachin", "rock", "miller", "cool", "cathy"))
  namesSource.filter(_.length > 4).take(2).runForeach(println)

}
