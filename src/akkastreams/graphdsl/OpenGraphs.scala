package akkastreams.graphdsl

import java.util.Date

import akka.actor.ActorSystem
import akka.stream.{ClosedShape, FanOutShape2, FlowShape, SinkShape, SourceShape, UniformFanInShape}
import akka.stream.scaladsl.{Broadcast, Concat, Flow, GraphDSL, RunnableGraph, Sink, Source, ZipWith}

object OpenGraphs extends App {

  implicit val system: ActorSystem = ActorSystem("OpenGraphs")

  /**
   * Composite source that concatenates the 2 sources
   * emits all the elements from the first route
   * then emits all the elements of the second graph
   */

  val firstSource = Source(1 to 10)
  val secondSource = Source(42 to 120)
  val sourceGraph = Source.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      val concat = builder.add(Concat[Int](2))
      firstSource ~> concat
      secondSource ~> concat
      SourceShape(concat.out)
    }
  )
  sourceGraph.runForeach(println)

  /**
   * Complex link
   */

  val sink1 = Sink.foreach[Int](x => s"first sink $x")
  val sink2 = Sink.foreach[Int](x => s"first sink $x")

  val sinkGraph = Sink.fromGraph(
    GraphDSL.create() {
      implicit builder =>
        import GraphDSL.Implicits._

        val broadcast = builder.add(Broadcast[Int](2))
        broadcast ~> sink1
        broadcast ~> sink2
        SinkShape(broadcast.in)
    }
  )

  //  firstSource.runWith(sinkGraph)

  /**
   * Challenge
   * Write your own flow that is composed of two other flows
   * One that adds one to number and one that does number*10
   */

  val flowGraph = Flow.fromGraph(
    GraphDSL.create() {
      implicit builder =>
        import GraphDSL.Implicits._
        // only shapes have ~> builder.add return a shape

        val incrementer = builder.add(Flow[Int].map(_ + 1))
        val multiplier = builder.add(Flow[Int].map(_ * 10))
        incrementer ~> multiplier
        // tells how to build the build data structure
        FlowShape(incrementer.in, multiplier.out)
      // this function takes a mutable data structure and mutate the builder to build the appropriate shape in the builder
      // from graph creates an actual container graph from the mutable data structure builder

      // -> belongs to Akka implicit conversions ->first source in the above way is implicitly converted to something and that worked
      // if both components need to be converted implicitly recursive implicits will be enabled and that is illegal for various reasons
      // in the scala compiler
    }
  )

  /**
   * flow from a sink and a source
   */

  def fromSinkAndSource[A, B](sink: Sink[A, _], source: Source[B, _]): Flow[A, B, _] =
    Flow.fromGraph(GraphDSL.create() {
      implicit builder =>
        val sourceShape = builder.add(source)
        val sinkShape = builder.add(sink)
        FlowShape(sinkShape.in, sourceShape.out)
    })

  // this is technically a flow because that has an input and output -> this has distinct and unconnected components
  // This kind of function is in akka stream api
  val f = Flow.fromSinkAndSource(Sink.foreach[String](println), Source(1 to 10))
  // if the elements going into the sink are finished the source has no way of stopping the stream
  // we have a sink and source coupled to get backpressure and termination signals


  /*
  Example: Max3 operator
  - 3 inputs of type int
  - the maximum of the 3
 */

  // step 1
  val max3StaticGraph = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    // step 2 - define aux SHAPES
    val max1 = builder.add(ZipWith[Int, Int, Int]((a, b) => Math.max(a, b)))
    val max2 = builder.add(ZipWith[Int, Int, Int]((a, b) => Math.max(a, b)))

    // step 3
    max1.out ~> max2.in0

    // step 4

    UniformFanInShape(max2.out, max1.in0, max1.in1, max2.in1)
  }

  val source1 = Source(1 to 10)
  val source2 = Source((1 to 10).map(_ => 5))
  val source3 = Source((1 to 10).reverse)

  val maxSink = Sink.foreach[Int](x => println(s"Max is: $x"))

  // step 1
  val max3RunnableGraph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2 - declare SHAPES
      val max3Shape = builder.add(max3StaticGraph)

      // step 3 - tie
      source1 ~> max3Shape.in(0)
      source2 ~> max3Shape.in(1)
      source3 ~> max3Shape.in(2)
      max3Shape.out ~> maxSink

      // step 4
      ClosedShape
    }
  )

  //  max3RunnableGraph.run()

  // same for UniformFanOutShape

  /*
    Non-uniform fan out shape
    Processing bank transactions
    Txn suspicious if amount > 10000
    Streams component for txns
    - output1: let the transaction go through
    - output2: suspicious txn ids
   */

  case class Transaction(id: String, source: String, recipient: String, amount: Int, date: Date)

  val transactionSource = Source(List(
    Transaction("5273890572", "Paul", "Jim", 100, new Date),
    Transaction("3578902532", "Daniel", "Jim", 100000, new Date),
    Transaction("5489036033", "Jim", "Alice", 7000, new Date)
  ))

  val bankProcessor = Sink.foreach[Transaction](println)
  val suspiciousAnalysisService = Sink.foreach[String](txnId => println(s"Suspicious transaction ID: $txnId"))

  // step 1
  val suspiciousTxnStaticGraph = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    // step 2 - define SHAPES
    val broadcast = builder.add(Broadcast[Transaction](2))
    val suspiciousTxnFilter = builder.add(Flow[Transaction].filter(txn => txn.amount > 10000))
    val txnIdExtractor = builder.add(Flow[Transaction].map[String](txn => txn.id))

    // step 3 - tie SHAPES
    broadcast.out(0) ~> suspiciousTxnFilter ~> txnIdExtractor

    // step 4
    new FanOutShape2(broadcast.in, broadcast.out(1), txnIdExtractor.out)
  }

  // step 1
  val suspiciousTxnRunnableGraph = RunnableGraph.fromGraph(
    GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // step 2
      val suspiciousTxnShape = builder.add(suspiciousTxnStaticGraph)

      // step 3
      transactionSource ~> suspiciousTxnShape.in
      suspiciousTxnShape.out0 ~> bankProcessor
      suspiciousTxnShape.out1 ~> suspiciousAnalysisService

      // step 4
      ClosedShape
    }
  )

  suspiciousTxnRunnableGraph.run()


}
