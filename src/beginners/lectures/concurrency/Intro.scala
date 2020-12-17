package beginners.lectures.concurrency

object Intro extends App {


  val runnable = new Runnable{
    override def run(): Unit = println("Running in Parallel")
  }
  val aThread = new Thread(runnable )
  aThread.start()
  runnable.run() // doesnt run in parallel

  val threadHello = new Thread(()=>(1 to 5).foreach(_=>println("hello")))
  val threadBye = new Thread(()=>(1 to 5).foreach(_=>println("bye")))
  threadHello.start()
  threadBye.start()

}
