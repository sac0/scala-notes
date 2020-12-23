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

  //  val t = new HelloThread
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

  //  startSynchronizedThread
  //  startSynchronizedThread
  //  startSynchronizedThread
  /**
   * Invocations synchronized can nest. Use more fine grained
   * Take a monitor on account a1 and then a2 and do the transfer of funds
   */
  /**
   * AccountA.synchronized { AccountB.synchronized {transfer amount}}
   * This is prone to deadlock - Manage same order
   * access the amount to transfer by checking if id's of accounts
   * if a < b transfer(a,b,n) else transfer(b,a,-n)
   */
  /**
   * Evaluating parallel programs
   * Calculate worst case asymptotic performance
   * Performance is a factor of
   * processor speed
   * cache control - false sharing associativity
   * number of processors
   * memory latency
   * Runtime - garbage collection, jit, threas scheduling
   *
   * Steps to mitigate
   * multiple repetitions
   * statistical treatment - eliminating outliers, calculating mean and variance
   * ensuring steady state warmup
   * https://dri.es/files/oopsla07-georges.pdf - Georges Buytaert statistically rigorous java evaluation
   *
   */
  /**
   * First the jvm interprets the program
   * parts of the program are converted to bytecode
   * later additional optimizations
   * scalameter gives a function withWarmer
   * Run scalameter with the following options
   * IgnoringGC
   * OutlierElimination
   * Calculate the count GC Pauses that occurred
   * Memory footprint
   */
  /**
   * Choice of data structures
   * we cannot use list since splitting it and concatenating are linear operations
   * we can use arrays or trees - trees do not have good locality
   */

  import org.scalameter._

  (1 to 20).foreach { _ =>
    val time = measure {
      (0 to 100000).toArray
    }
    println(s"Time taken to construct array is $time")
  }
  /**
   * operatives that are associative but not commutative
   * string concatenation and matrix multiplications of same size square matrices
   * floating point addition is commutative but not associative
   * we can also have functions like f(x,y)= x^2 + y^2
   * 
   */


}
