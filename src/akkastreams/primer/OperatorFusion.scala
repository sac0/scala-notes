package akkastreams.primer

import akka.actor.{Actor, ActorSystem, Props}
import akka.stream.scaladsl.{Flow, Sink, Source}

object OperatorFusion extends App {
  implicit val system: ActorSystem = ActorSystem("OperatorFusion")

  val simpleSource = Source(1 to 1000)
  val simpleFlow = Flow[Int].map(_ + 1)
  val simpleFlow2 = Flow[Int].map(_ * 10)
  val simpleSink = Sink.foreach[Int](println)

  // this runs on the same actor
  //  simpleSource.via(simpleFlow).via(simpleFlow2).to(simpleSink).run()

  // if we connect components with via or viaMat by default run in the same actor
  // this is called operator fusion

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case x: Int =>
        //flow operations
        val x2 = x + 1
        val y = x2 * 10
        //sink
        println(y)
    }
  }

  val simpleActor = system.actorOf(Props[SimpleActor])
  //  (1 to 1000).foreach(simpleActor ! _)
  // single cpu will be used to calculate all the processing in this above case
  // when they are quick this fusion is good. But if the operations are time expensive it does a bit of harm
  // lets see more complex architecture

  val complexFlow = Flow[Int].map { x =>
    Thread.sleep(1000)
    x + 1
  }


  val complexFlow2 = Flow[Int].map { x =>
    Thread.sleep(1000)
    x * 10
  }

  // there is a 2 second boundary between every number
  // simpleSource.via(complexFlow).via(complexFlow2).to(simpleSink).run()

  //  lets create an async boundary
  //  simpleSource.via(complexFlow).async // runs on an actor
  //    .via(complexFlow2).async // runs on another actor
  //    .to(simpleSink) // // runs on a third actor
  //    .run()
  // we can introduce as many async boundaries as we want

  // ordering guarantees with or without async boundaries
  //  Source(1 to 5)
  //    .map(x => {println(s"Flow A $x");x})
  //    .map(x => {println(s"Flow B $x");x})
  //    .map(x => {println(s"Flow C $x");x})
  //    .runWith(Sink.ignore)


  // relative order of the stream elements doesnt change. this means that 1 will go a-b-c in that order for sure
  Source(1 to 5)
    .map(x => {
      println(s"Flow A $x"); x
    }).async
    .map(x => {
      println(s"Flow B $x"); x
    }).async
    .map(x => {
      println(s"Flow C $x"); x
    }).async
    .runWith(Sink.ignore)

}
