package fp.intro.algebra.monoid

import fp.intro.testing.Prop.forAll
import fp.intro.testing.{Gen, Prop}
import fp.intro.laziness.Stream

/**
 * introduction to purely algebraic structures.
 * We’ll consider a simple structure, the monoid,
 * which is defined only by its algebra. Other than satisfying the same laws,
 * instances of the monoid interface may have little/nothing to do with one another.
 * Nonetheless, we’ll see how this algebraic structure is often all we need
 * to write useful, polymorphic functions.
 */
/**
 * A monoid is the following
 *
 *  Some type A
 *  An associative binary operation, op, that takes two values of type A
 * and combines them into one:
 * op(op(x,y), z) == op(x, op(y,z)) for any choice of x: A, y:A,z:A -> matrix multiplication
 *  A value, zero: A, that is an identity for that operation: op(x, zero) == x and
 * op(zero, x) == x for any x: A
 *
 * Associative but not Commutative
 * We can define 𝑥⊕𝑦=𝑦. Then (𝑥⊕𝑦)⊕𝑧=𝑧=𝑥⊕(𝑦⊕𝑧) but 𝑦=𝑥⊕𝑦≠𝑦⊕𝑥=𝑥
 *
 * Commutative but not Associative
 * Consider the operation (𝑥,𝑦)↦𝑥𝑦+1 on the integers.
 */

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

/**
 * Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
 *
 * val intAddition: Monoid[Int]
 * val intMultiplication: Monoid[Int]
 * val booleanOr: Monoid[Boolean]
 * val booleanAnd: Monoid[Boolean]
 *
 * Give a Monoid instance for combining Option values.
 * def optionMonoid[A]: Monoid Option[A]
 */

object Monoid extends App {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override def zero: Option[A] = None
  }

  // We can get the dual of any monoid just by flipping the `op`.
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)

    val zero: A = m.zero
  }

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  //A function having the same argument and return type is sometimes called an endofunction.
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A): A => A = f compose g

    val zero: A => A = (a: A) => a
  }

  /**
   * Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
   */

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    // Associativity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p =>
      m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      forAll(gen)((a: A) =>
        m.op(a, m.zero) == a && m.op(m.zero, a) == a)
  }

  /**
   * Monoids with lists
   * def foldRight[B](z: B)(f: (A, B) => B): B
   * def foldLeft[B](z: B)(f: (B, A) => B): B
   *
   * What is the input and output are the same
   *
   * def foldRight[A](z: A)(f: (A, A) => A): A
   * def foldLeft[A](z: A)(f: (A, A) => A): A
   *
   */

  //  println(List("Hic", "Est", "Index").foldRight(stringMonoid.zero)(stringMonoid.op))
  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMapUsingMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)

  // Notice that this function does not require the use of `map` at all.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.isEmpty) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  // This ability to 'lift' a monoid any monoid to operate within
  // some context (here `Par`) is something we'll discuss more in
  // chapters 11 & 12
  /**
   * def par[A](m: Monoid[A]): Monoid Par A= new Monoid Par A fill [] {
   * def zero = Par.unit(m.zero)
   * def op(a: Par[A], b: Par[A]) = a.map2(b)(m.op)
   * }
   *
   * // we perform the mapping and the reducing both in parallel
   * def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
   * Par.parMap(v)(f).flatMap { bs =>
   * foldMapV(bs, par(m))(b => Par.lazyUnit(b))
   * }
   */
  def ordered(ints: IndexedSeq[Int]): Boolean = {

    val orderingMonoid = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] = {
        (a1, a2) match {
          case (Some((x1, y1, o1)), Some((x2, y2, o2))) => Some((x1 min x2, y1 max y2, o1 && o2 && y1 <= x2))
          case (x, None) => x
          case (None, x) => x
        }
      }

      def zero: Option[(Int, Int, Boolean)] = None
    }
    foldMapV(ints, orderingMonoid)(i => Some((i, i, true))).forall(_._3)
  }

  /**
   * If we split this string roughly in half, we might split it in the middle of a word.
   * In the case of our string, that would yield "lorem ipsum do" and "lor sit amet, ".
   * When we add up the results of counting the words in these strings,
   * we want to avoid double-counting the word dolor.
   */

  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC


  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def zero: WC = Stub("")

    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1), r2)
    }
  }

  def count(s: String): Int = {
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)

    // `unStub(s)` is 0 if `s` is empty, otherwise 1.
    def unStub(s: String) = s.length min 1

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unStub(s)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) =
        (A.op(x._1, y._1), B.op(x._2, y._2))
      val zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(f: A => B, g: A => B): A => B = a => B.op(f(a), g(a))
      val zero: A => B = _ => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero: Map[K, V] = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]): Map[K, V] =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
            b.getOrElse(k, V.zero)))
        }
    }


  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))



}


/**
 * Foldable is a higher-order type constructor or a higher-kinded type.
 */
trait Foldable[F[_]] {

  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {

  import Monoid._

  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(a) => f(z, a)
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
    case Leaf(a) => f(a, z)
    case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
  }
}

// Notice that in `TreeFoldable.foldMap`, we don't actually use the `zero`
// from the `Monoid`. This is because there is no empty tree.
// This suggests that there might be a class of types that are foldable
// with something "smaller" than a monoid, consisting only of an
// associative `op`. That kind of object (a monoid without a `zero`) is
// called a semigroup. `Tree` itself is not a monoid, but it is a semigroup.

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None => mb.zero
      case Some(a) => f(a)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
    case None => z
    case Some(a) => f(z, a)
  }

  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case None => z
    case Some(a) => f(a, z)
  }
}


