package AkkaEssentials.Recap

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object MultiThreadingRecap extends App {


  // lets launch 2 threads that runs an unit
  val aThread = new Thread(() => println("I am running in parallel"))

  // wait makes the main thread for that thread
  aThread.start()
  aThread.join()

  val threadHello = new Thread(() => (1 to 1000).foreach(a => println(s"Hello-$a")))
  val threadGoodBye = new Thread(() => (1 to 1000).foreach(a => println(s"goodbye-$a")))

//  threadHello.start()
//  threadGoodBye.start()


  class BankAccount(private var amount:Int) {
    // volatile amount - but work for primitive types only

    def withdraw (money:Int) = this.amount -= money
    def safeWithdraw (money:Int) = this.synchronized {
      this.amount -= money
    }

  }

  // join can be used on each thread and then the other threads started but that is cumbersome
  // Each run gives a different output - causing a lot of headaches

  // wait notify
  val future = Future {42}
  future.onComplete {
    case Success(value) => println(s"I have found $value")
    case Failure(exception) => println(exception.toString)
  }

  val aProcessedFuture = future.map(_+1)
  val aFlatFuture = future.flatMap {
    value => Future(value +2)
  }

  // filtered future
  val filteredFuture = future.filter(_ % 3 == 0) // no such element exception - predicate not satisfied

  val aNonSenseFuture = for {
    meaningOfLife <- future
    filteredMeaning <- filteredFuture
  } yield meaningOfLife + filteredMeaning

  val aNonSenseNumber: Int = Await.result(aNonSenseFuture, 3.seconds)


  println(aNonSenseNumber)


}
