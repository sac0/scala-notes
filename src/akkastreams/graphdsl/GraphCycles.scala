package akkastreams.graphdsl

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, Merge, MergePreferred, Source, Zip, ZipWith}
import akka.stream.{ClosedShape, OverflowStrategy, UniformFanInShape}

object GraphCycles extends App {

  implicit val system: ActorSystem = ActorSystem("GraphCycles")

  val accelerator = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    val sourceShape = builder.add(Source(1 to 100))
    val mergeShape = builder.add(Merge[Int](2))
    val incrementerShape = builder.add(Flow[Int].map { x =>
      println(s"Accelerating $x")
      x + 1
    })

    sourceShape ~> mergeShape ~> incrementerShape
    mergeShape <~ incrementerShape

    ClosedShape
  }

  //  RunnableGraph.fromGraph(accelerator).run()
  // graph cycle deadlock!

  /*
    Solution 1: MergePreferred
   */
  val actualAccelerator = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    val sourceShape = builder.add(Source(1 to 100))
    val mergeShape = builder.add(MergePreferred[Int](1))
    val incrementerShape = builder.add(Flow[Int].map { x =>
      println(s"Accelerating $x")
      x + 1
    })

    sourceShape ~> mergeShape ~> incrementerShape
    mergeShape.preferred <~ incrementerShape

    ClosedShape
  }

  //  RunnableGraph.fromGraph(actualAccelerator).run()


  /*
    Solution 2: buffers
   */
  val bufferedRepeater = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._
    val sourceShape = builder.add(Source(1 to 100))
    val mergeShape = builder.add(Merge[Int](2))
    val repeaterShape = builder.add(Flow[Int].buffer(10, OverflowStrategy.dropHead).map { x =>
      println(s"Accelerating $x")
      Thread.sleep(100)
      x
    })

    sourceShape ~> mergeShape ~> repeaterShape
    mergeShape <~ repeaterShape

    ClosedShape
  }

  //  RunnableGraph.fromGraph(bufferedRepeater).run()

  /*
    cycles risk deadlocking
    - add bounds to the number of elements in the cycle
    boundedness vs liveness
   */

  /**
   * Challenge: create a fan-in shape
   * - two inputs which will be fed with EXACTLY ONE number (1 and 1)
   * - output will emit an INFINITE FIBONACCI SEQUENCE based off those 2 numbers
   * 1, 2, 3, 5, 8 ...
   *
   * Hint: Use ZipWith and cycles, MergePreferred
   */

  val fibonacciGenerator = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    val zip = builder.add(Zip[BigInt, BigInt])
    val mergePreferred = builder.add(MergePreferred[(BigInt, BigInt)](1))
    val fiboLogic = builder.add(Flow[(BigInt, BigInt)].map { pair =>
      val last = pair._1
      val previous = pair._2

      Thread.sleep(100)

      (last + previous, last)
    })
    val broadcast = builder.add(Broadcast[(BigInt, BigInt)](2))
    val extractLast = builder.add(Flow[(BigInt, BigInt)].map(_._1))

    zip.out ~> mergePreferred ~> fiboLogic ~> broadcast ~> extractLast
    mergePreferred.preferred <~ broadcast

    UniformFanInShape(extractLast.out, zip.in0, zip.in1)
  }

  //  val fiboGraph = RunnableGraph.fromGraph(
  //    GraphDSL.create() { implicit builder =>
  //      import GraphDSL.Implicits._
  //
  //      val source1 = builder.add(Source.single[BigInt](1))
  //      val source2 = builder.add(Source.single[BigInt](1))
  //      val sink = builder.add(Sink.foreach[BigInt](println))
  //      val fibo = builder.add(fibonacciGenerator)
  //
  //      source1 ~> fibo.in(0)
  //      source2 ~> fibo.in(1)
  //      fibo.out ~> sink
  //
  //      ClosedShape
  //    }
  //  )
  //
  //  fiboGraph.run()

  /**
   * ********************
   * Old solution (more complicated)
   * ********************
   */

  val complicatedFibonacciGenerator = GraphDSL.create() { implicit builder =>
    import GraphDSL.Implicits._

    // two big feeds: one with the "last" number, and one with the "next-to-last" (previous) Fibonacci number
    val lastFeed = builder.add(MergePreferred[BigInt](1))
    val previousFeed = builder.add(MergePreferred[BigInt](1))

    /*
      The "last" feed will be split into 3:
      - the final output of the shape
      - a zip to sum with the previousFeed
      - a feedback loop to the previousFeed (the current "last" will become the next "previous")
    */
    val broadcastLast = builder.add(Broadcast[BigInt](3))

    // the actual Fibonacci logic
    val fiboLogic = builder.add(ZipWith((last: BigInt, previous: BigInt) => {
      Thread.sleep(100) // so you can actually see the result growing
      last + previous
    }))

    // hopefully connections are traceable on paper

    broadcastLast ~> previousFeed.preferred // feedback loop: current "last" becomes next "previous"
    lastFeed ~> broadcastLast ~> fiboLogic.in0
    previousFeed ~> fiboLogic.in1
    lastFeed.preferred <~ fiboLogic.out // feedback loop: next "last" is the sum of current "last" and "previous"

    UniformFanInShape(
      broadcastLast.out(2), // the unconnected output
      lastFeed.in(0),
      previousFeed.in(0) // and the regular ports of the MergePreferred components
    )

    // So as you can see, quite involved. But it gives the same output!
  }
}

