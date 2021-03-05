package fp.intro.algebra

import fp.intro.laziness.Stream


object Applicative {

  /**
   * What you may not have noticed is that a large number of the
   * useful combinators on Monad can be defined using only unit and map2.
   * The traverse combinator is one example—it doesn’t call flatMap directly
   * and is therefore agnostic to whether map2 is primitive or derived.
   * Furthermore, for many data types,
   * map2 can be implemented directly, without using flatMap.
   * All this suggests a variation on Monad—the Monad interface has
   * flatMap and unit as primitives, and derives map2,
   * but we can obtain a different abstraction by letting unit
   * and map2 be the primitives. We’ll see that this new abstraction,
   * called an applicative functor, is less powerful than a monad,
   * but we’ll also see that limitations come with benefits.
   *
   * def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
   * flatMap(ma)(a => map(mb)(b => f(a,b)))
   */

  /**
   * def join[A](f: F- F -A ): F[A]
   * This peels a layer of F. Unit adds a layer and map2 just combines layers.
   * There is not way to achieve join with just unit and map2 primitives
   * applicative has no means of implementing flatmap as well since there is no flattening function
   *
   * We might say that with Applicative,
   * the structure of our computation is fixed;
   * with Monad, the results of previous computations
   * may influence what computations to run next.
   *
   * If we want the result of one lookup to affect what lookup we do next, then we need flatMap or join.
   *
   *
   * The differences
   *  Applicative computations have fixed structure and simply sequence effects,
   * whereas monadic computations may choose structure dynamically,
   * based on the result of previous effects.
   *
   *  Applicative constructs context-free computations,
   * while Monad allows for context sensitivity
   *
   *  Monad makes effects first class;
   * they may be generated at “interpretation” time
   * rather than chosen ahead of time by the program.
   */

  /**
   * Advantages of applicative functors
   *
   * It is not as powerful as monad hence gives more flexibility on the implementations end
   */

  /**
   * Stream is not an applicative functor
   */
  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.constant(a) // The infinite, constant stream

    override def map2[A, B, C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                                                            f: (A, B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A): Success[A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      def unit[A](a: => A): M = M.zero

      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }

  /**
   * Applicative rules
   *
   * Obey functor laws
   * map(v)(id) == v
   * map(map(v)(g))(f) = map (v)(f compose g)
   *
   *
   * Left identity: map2(unit(()), fa)((_,a) => a) == fa
   * Right identity: map2(fa, unit(()))((a,_) => a) == fa
   * Associativity: product(product(fa, fb),fc) == map(product(fa, product(fb, fc)))(assoc)
   * Naturality: map2(a,b)(productF(f,g)) == product(map(a)(f), map(b)(g))
   *
   */

  /**
   * Applicatives compose, monads don't.
   *
   * Monads do compose, but the result might not be a monad.
   * In contrast, the composition of two applicatives is necessarily an applicative.
   * I suspect the intention of the original statement was that
   * "Applicativeness composes, while monadness doesn't." Rephrased,
   * "Applicative is closed under composition, and Monad is not."
   */


}

trait Applicative[F[_]] extends Functor[F] {
  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  // Here we simply use `map2` to lift `apply` and `unit` themselves from one
  // Applicative into the other.
  // If `self` and `G` both satisfy the laws, then so does the composite.
  // The full proof can be found at
  // https://github.com/runarorama/sannanir/blob/master/Applicative.v
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))

      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C) =
        self.map2(fga, fgb)(G.map2(_, _)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }

}

/**
 * Traversable vs Iterable
 * Think of it as the difference between blowing and sucking.
 *
 * When you have call a Traversables foreach, or its derived methods,
 * it will blow its values into your function one at a time - so it has control over the iteration.
 * With the Iterator returned by an Iterable though, you suck the values out of it,
 * controlling when to move to the next one yourself.
 *
 *
 * Traverse maintains structure - all folds do not
 * A traversal is similar to a fold in that both take some data structure
 * and apply a function to the data within in order to produce a result.
 * The difference is that traverse preserves the original structure,
 * whereas foldMap discards the structure and replaces it with the
 * operations of a monoid. Look at the signature
 * Tree Option A  => Option Tree A,
 * for instance. We’re preserving the Tree structure, not merely col- lapsing the values using some monoid.
 */
sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object EMonad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = eea match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }
}