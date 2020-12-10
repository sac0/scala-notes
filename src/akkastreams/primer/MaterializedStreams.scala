package akkastreams.primer

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object MaterializedStreams extends App {

  /**
   * graph is a blueprint of a stream
   * running a graph is called materializing the stream
   * materializing the graph means materializing all components
   * each component produces a materialized values
   * the graph finally is one value
   * our job to choose what
   *
   * A component can materialize multiple things
   * we can reuse this materialization for future stuff
   */

  implicit val system: ActorSystem = ActorSystem("MaterializedStreams")
  //  val simpleGraph = Source(1 to 10).to(Sink.foreach(println))
  //  val simpleMaterializedValue = simpleGraph.run()


  // we can have some meaningful value returns as well
  val source = Source(1 to 10)
  val sink = Sink.reduce[Int](_ + _)
  val sumFuture = source.runWith(sink)
  sumFuture onComplete {
    case Success(value) => println(s"the sum of the numbers is $value")
    case Failure(ex) => println(s"Error in running the sum $ex")
  }

  // choosing materialized values
  val simpleSource = Source(1 to 10)
  val simpleFlow = Flow[Int].map(_ + 1)
  val simpleSink = Sink.foreach[Int](a => a)
  val graph = simpleSource.viaMat(simpleFlow)(Keep.right).toMat(simpleSink)(Keep.right)
  graph.run().onComplete {
    case Success(_) => println(s"Stream processed")
    case Failure(exception) => println(s"Stream processing failed with $exception")
  }

  // sugars
  Source(1 to 10).runWith(Sink.reduce[Int](_ + _))
  Source(1 to 10).runReduce(_ + _)

  // source to sink syntax keeps the left values materialized

  // backwards
  Sink.foreach[Int](println).runWith(Source.single(42))
  /**
   * return the last element of a source
   * return word count of a stream of sentences
   */

  val f1 = Source(1 to 10).toMat(Sink.last[Int])(Keep.right).run()
  println(f1)
  val f2 = Source(1 to 10).runWith(Sink.last[Int])

  val sentenceSource = Source(List(
    "Sachin is learning scala",
    "i can touch type now",
    "I do not understand materialization"
  ))

  val wordCountSink = Sink.fold[Int, String](0)(_ + _.split(" ").length)
  val g1 = sentenceSource.toMat(wordCountSink)(Keep.right).run()
  val g2 = sentenceSource.runWith(wordCountSink)
  val g3 = sentenceSource.runFold(0)(_ + _.split(" ").length)
  val wordCountFlow = Flow[String].fold[Int](0)(_ + _.split(" ").length)
  val g4 = sentenceSource.via(wordCountFlow).toMat(Sink.head[Int])(Keep.right).run()
  val g5 = sentenceSource.viaMat(wordCountFlow)(Keep.left).toMat(Sink.head)(Keep.right).run()
  val g6 = sentenceSource.via(wordCountFlow).runWith(Sink.head)
  println(g1, g2, g3, g4, g5, g6)


}
