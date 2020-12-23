package parallelprogramming

object Intro extends App {

  /**
   * OS has many processes running in parallel
   * A process cannot access another process's memory
   * Ech process contain independent concurrency units called threads
   * each thread has a program counter and its own stack. the memory of the process is shared
   * Each JVM process starts the main thread. it executes the main program in scala
   * To start additional threads
   * Define a thread subclass
   * instantiate a new thread object
   * call start on the thread object
   *
   * thread maintain their stack in the heap of the process
   */

  class HelloThread extends Thread {
    override def run(): Unit = {
      println("Hello world")
    }
  }

  val t = new HelloThread
  //starts a new thread
  //  t.start()

  // the main thread when it runs this waits for the t to finish before proceeding ahead from here
  //  t.join()

  // sometimes we want to ensure that the thread runs fully before going to the new thread
  // Atomicity in threads

  private var uidCount = 0L

  def getUniqueId: Long = {
    uidCount += 1
    // atomicity is breaking here. since the increments are not atomic
    // Two threads read it simultaneously before it is written
    // So both read the value 0 and write back 1
    uidCount
  }

  def startThread: Runnable = {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId
        println(uids)
      }
    }
    t.start()
    t
  }
//  startThread
//  startThread
//  startThread

  /**
   * Each object has a monitor. Only a single thread can hold the monitor for an object
   */
  private val x = AnyRef
  def startSynchronizedThread: Runnable = x.synchronized {
    val t = new Thread {
      override def run(): Unit = {
        val uids = for (i <- 0 until 10) yield getUniqueId
        println(uids)
      }
    }
    t.start()
    t
  }
  startSynchronizedThread
  startSynchronizedThread
  startSynchronizedThread



}
