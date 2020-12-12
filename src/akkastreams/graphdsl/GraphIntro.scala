package akkastreams.graphdsl

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ClosedShape
import akka.stream.scaladsl.{Balance, Broadcast, Flow, GraphDSL, Merge, RunnableGraph, Sink, Source, Zip}

import scala.concurrent.duration.DurationInt

object GraphIntro extends App {

  implicit val system: ActorSystem = ActorSystem("GraphIntro")
  val input = Source(1 to 1000)
  val incrementer = Flow[Int].map(_ + 1)
  val multiplier = Flow[Int].map(_ * 10)
  val output = Sink.foreach[(Int, Int)](println)

  val graph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
      // this builder is a mutable data structure
      // this is the basis of the graph
      import GraphDSL.Implicits._ // brings some operators in to scope
      // add necessary components into the graph
      val broadcast = builder.add(Broadcast[Int](2)) // fan out
      val zip = builder.add(Zip[Int, Int])

      // forming the graph
      input ~> broadcast
      broadcast.out(0) ~> incrementer ~> zip.in0
      broadcast.out(1) ~> multiplier ~> zip.in1
      zip.out ~> output

      //return a closed shape
      // when we return the builder we freeze the shape
      ClosedShape
    } // graph - static graph
  ) // runnable graph
  /**
   * feed a source to 2 sinks at the same time
   */


  val firstSink = Sink.foreach[Int](x => println(s"First sink $x"))
  val secondSink = Sink.foreach[Int](x => println(s"Second sink $x"))
  val sourceTo2Sinks = RunnableGraph.fromGraph(
    GraphDSL.create() {
      implicit builder: GraphDSL.Builder[NotUsed] =>
        import GraphDSL.Implicits._

        val broadcast = builder.add(Broadcast[Int](2))
        input ~> broadcast
        broadcast.out(0) ~> firstSink
        broadcast.out(1) ~> secondSink
        ClosedShape
    }
  )

  /**
   * fast ans slow sources
   * both are merged and forwarded from to a single output
   * Balance picks it up from there and gives the output to 2 sinks
   */

  import scala.concurrent.duration._
  val fastSource = input.throttle(5, 1 second)
  val slowSource = input.throttle(2, 1 second)

  val sink1 = Sink.fold[Int, Int](0)((count, _) => {
    println(s"Sink 1 number of elements: $count")
    count + 1
  })

  val sink2 = Sink.fold[Int, Int](0)((count, _) => {
    println(s"Sink 2 number of elements: $count")
    count + 1
  })

  // step 1
  val balanceGraph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._


      // step 2 -- declare components
      val merge = builder.add(Merge[Int](2))
      val balance = builder.add(Balance[Int](2))


      // step 3 -- tie them up
      fastSource ~> merge ~>  balance ~> sink1
      slowSource ~> merge
      balance ~> sink2
      // step 4
      ClosedShape
    }
  )

  balanceGraph.run()


}
