package beginners.lectures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesPromises extends App {
  def calculateMeaningOfLife: Int = {
    Thread.sleep(420)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  }


}
