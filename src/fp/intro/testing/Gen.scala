package fp.intro.testing

import fp.intro.laziness.Stream
import fp.intro.state._
import fp.intro.parallelism._
import fp.intro.parallelism.Par.Par
import Gen.{pint2, _}
import Prop._
import java.util.concurrent.{ExecutorService, Executors}


/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  /**
   * step-1. Read Intro for test case minification
   * trait Prop { def check: Either[???,SuccessCount] }
   * What shall we return in failure cases. As a general rule,
   * we shouldn’t use String to represent data that we want to compute with.
   * But in our case it is only to show it to the tester on the screen
   * We do not care about the type
   * def check: Either[(FailedCase, SuccessCount), SuccessCount]
   */
  type FailedCase = String
  type SuccessCount = Int

  type TestCases = Int
  type MaxSize = Int

  /**
   * Step -5 define a result that is an either
   * type Result = Either[(FailedCase, SuccessCount), SuccessCount]
   * equivalent to Option[(FailedCase, SuccessCount)], that shows our intent very clearly.
   * Until now, we’ve only used the None case of Option to indicate failure
   */
  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    /**
     * Indicates that all tests passed
     * Indicates that one of the test cases falsified the property
     */
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  /**
   * Step-6
   *
   * We can see that forAll doesn’t have enough information to return a Prop.
   * Besides the number of test cases to try,
   * Prop.run must have all the information needed to generate test cases.
   * If it needs to generate random test cases using our current representation of Gen,
   * it’s going to need an RNG. Let’s go ahead and propagate that dependency to Prop
   * add RNG to test cases on the top
   * case class Prop(run: (TestCases,RNG) => Result)
   */

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  /**
   * step-8
   * def forAll[A](g: SGen[A])(f: A => Boolean): Prop
   * Can you see why it’s not possible to implement this function?
   * SGen is expecting to be told a size, but Prop doesn’t have any size information.
   * we simply need to add this as a dependency to Prop.
   * But since we want to put Prop in charge of invoking the underlying generators with various sizes,
   * we’ll have Prop accept a maximum size.
   * Prop will then generate test cases up to and including the maximum specified size.
   * type MaxSize = Int
   * case class Prop(run: (MaxSize,TestCases,RNG) => Result)
   */
  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  /**
   * Step -9
   * At this point, calling run directly on a Prop is rather cumbersome.
   * We can introduce a helper function for running our Prop values and
   * printing their result to the console in a useful format
   */
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1: Prop = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  val p2: Prop = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3: Prop = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )(ES) get
  }

  val S: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  val pint: Gen[Par[MaxSize]] = Gen.choose(0, 10) map Par.unit
  val p4: Prop =
    forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  val forkProp: Prop = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"

}


case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  /**
   * Step -4 we’d like a Gen[(String,String)] that generates pairs
   * where the second string contains only characters from the first.
   * Or that we had a Gen[Int] that chooses an integer between 0 and 11,
   * and we'd like to make a Gen[List[Double] that then generates lists of whatever length is chosen
   *
   * Generate a list of A's from an int generator - check both of em
   */

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  /* A version of `listOfN` that generates the size to use dynamically. */
  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size flatMap (n => listOfN(n))
  }

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  /**
   * Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
   */
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

}

/**
 * step-7
 * Test case minimization - we’d like our framework to find the smallest or simplest failing test case
 *  Shrinking — After we’ve found a failing test case, we run a
 * separate procedure to minimize the test case by successively decreasing its “size”
 * until it no longer fails. This is called shrinking, and it usually requires
 * us to write separate code for each data type to implement this minimization process.
 *
 *  Sized generation — Rather than shrinking test cases,
 * we simply generate our test cases in order of increasing size and complexity.
 * So we start small and increase the size until we find a failure.
 * This idea can be extended in various ways to allow the test runner to
 * make larger jumps in the space of possible sizes while still making
 * it possible to find the smallest failing test.
 *
 * ScalaCheck, incidentally, takes the first approach: shrinking. based on haskell quickcheck
 * Instead of modifying our Gen data type, for which
 * we’ve already written a number of useful combinators,
 * let’s introduce sized generation as a separate layer in our library.
 *
 *
 */
case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}


object Gen {
  /**
   * Step -2
   * Gen[A] was something that knows how to generate values of type A.
   * Implement Gen.choose using this representation of Gen.
   * It should generate integers in the range start to stopExclusive
   */

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // Always generates the value a
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  /**
   * Step -3 Generate a list of items
   */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val t = List.fill(n)(g.sample)
    Gen(State.sequence(t))
  }

  val uniform: Gen[Double] = Gen(State(RNG.double))

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d * (j - i)))

  /* Basic idea is to add 1 to the result of `choose` if it is of the wrong
 * parity, but we require some special handling to deal with the maximum
 * integer in the range.
 */
  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  /**
   * Implement union, for combining two generators of the same type into one,
   * by pulling values from each generator with equal likelihood.
   */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (b => if (b) g1 else g2)


  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double)).flatMap(d => if (d < g1Threshold) g1._1 else g2._1)
  }

  /**
   * Implement a listOf combinator that doesn’t accept an explicit size.
   * It should return an SGen instead of a Gen.
   * The implementation should generate lists of the requested size.
   */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(listOfN(_, g))


  /* Not the most efficient implementation, but it's simple.
   * This generates ASCII strings.
   */
  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  val smallInt: Gen[MaxSize] = Gen.choose(-10, 10)
  val maxProp: Prop = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val maxProp1: Prop = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  // We specify that every sorted list is either empty, has one element,
  // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
  val sortedProp: Prop = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  object ** {
    def unapply[A, B](p: (A, B)): Option[(Any, Any)] = Some(p)
  }

  /** A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   * computation for each element of the input list summed to produce the final
   * result. This is not the most compelling example, but it provides at least some
   * variation in structure to use for testing.
   *
   * Note that this has to be a `lazy val` because of the way Scala initializes objects.
   * It depends on the `Prop` companion object being created, which references `pint2`.
   */
  lazy val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))

  def genStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => s => i)

}

object Play extends App {

  //  case class Simple(seed: Long) extends RNG {
  //    def nextInt: (Int, RNG) = {
  //      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
  //      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
  //      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
  //      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  //    }
  //  }

  //  val t: (List[Int], RNG) = Gen.listOfN(6, Gen.choose(1, 30)).sample.run(Simple(123))
  //  println(t)
  //  val smallInt = Gen.choose(-10,10)
  //  val maxProp = forAll(listOf(smallInt)) { ns =>
  //    val max = ns.max
  //    !ns.exists(_ > max) }
  //  run(maxProp)

}
