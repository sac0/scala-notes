package fp.intro.testing

import fp.intro.laziness.Stream
import fp.intro.state._
import fp.intro.parallelism._
import fp.intro.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
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

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}


case class Gen[A](sample: State[RNG, A]) {
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
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

}

object Play extends App {
  val t:(List[Int],RNG) = Gen.listOfN(3,Gen.choose(1,3)).sample
  println()

}
