package AkkaEssentials.Recap

import scala.concurrent.Future

object ThreadModelLimitations extends App {

  // 1. OOP encapsulation is valid only in single threaded systems

  class BankAccount(private var amount: Int) {
    // volatile amount - but work for primitive types only

    def withdraw(money: Int): Unit = this.amount -= money

    def safeWithdraw(money: Int): Unit = this.synchronized {
      this.amount -= money
    }

    def deposit(money: Int): Unit = this.amount += money

    def getAmount: Int = amount

  }

  val account = new BankAccount(2000)
  for (_ <- 1 to 1000) {
    new Thread(() => account.withdraw(1)).start()
  }
  for (_ <- 1 to 1000) {
    new Thread(() => account.deposit(1)).start()
  }

  //  println(account.getAmount)
  // we can have locks. But still have a lot of deadlocks, livelocks

  // 2. Delegating something to a thread is a pain.
  // How do we send some information to a particular thread and want to give additional information
  // basically pass a new runnable to an existing runnable
  var task: Runnable = null
  val runningThread: Thread = new Thread(() => {
    while (true) {
      while (task == null) {
        runningThread.synchronized {
          println("[background] waiting for a task")
          runningThread.wait()
        }
        task.synchronized {
          println("[background] i have a task!!")
          task.run()
          task = null
        }
      }
    }
  })

  def delegateToBackgroundThread(r: Runnable): Unit = {
    if (task == null) task = r
    runningThread.synchronized {
      runningThread.notify()
    }
  }

  runningThread.start()
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println(42))
  Thread.sleep(1000)
  delegateToBackgroundThread(() => println("This should run in the background"))

  // Questions
  // Other signals
  // Multiple tasks like a queue
  // who gave the signal
  // What if something crashes who handles the errors

  // The needs is
  // can safely receive messages
  // can identify the sender
  // can guard against the errors

  // 3. Tracing and dealing with errors in a multiTheaded enc is a pain
  // we want o count the 1million numbers in 10 threads

  val futures = (0 to 9).map(i => 100000 * i until 100000 * (i + 1))
    .map(range => Future {
      if (range.contains(546735)) throw new RuntimeException("invalid number")
      range.sum
    })
  val sumFuture = Future.reduceLeft(futures)(_ + _)
  sumFuture.onComplete(println)

  // Error handling is tough and monumental task
  // Delegation is a second class citizen and should not have to be done on a blocking manner

}
