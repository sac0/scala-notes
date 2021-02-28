package fp.intro.algebra

import fp.intro.parsers._
import fp.intro.testing._
import fp.intro.parallelism._
import fp.intro.state._
import fp.intro.parallelism.Par._
import fp.intro.laziness.Stream

/**
 * def map2[A,B,C](fa: Parser[A], fb: Parser[B])(f: (A,B) => C): Parser[C] =
 * fa flatMap (a => fb map (b => f(a,b)))
 *
 * def map2[A,B,C](fa: Option[A], fb: Option[B])(f: (A,B) => C): Option[C] =
 * fa flatMap (a => fb map (b => f(a,b)))
 */

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

/**
 * CAn extend functor. since all monads have map. but not all functors are not monads
 */
trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  /**
   * Map is not primitive. since unit and flatmap can be used to define map
   */
  def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = {
    // this is throwing a similar code error since RNG is also a monad if we go to duplicated code
    lma.foldRight(unit(List[A]()))((ma, b) => map2(ma, b)(_ :: _))
  }

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    sequence(la map f)

  // For `List`, the `replicateM` function will generate a list of lists.
  // It will contain all the lists of length `n` with elements selected from the
  // input list.
  // For `Option`, it will generate either `Some` or `None` based on whether the
  // input is `Some` or `None`. The `Some` case will contain a list of length `n`
  // that repeats the element in the input `Option`.
  // The general meaning of `replicateM` is described very well by the
  // implementation `sequence(List.fill(n)(ma))`. It repeats the `ma` monadic value
  // `n` times and gathers the results in a single value, where the monad `M`
  // determines how values are actually combined.

  // Recursive version:
  def _replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    if (n <= 0) unit(List[A]()) else map2(ma, _replicateM(n - 1, ma))(_ :: _)

  // Using `sequence` and the `List.fill` function of the standard library:
  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence(List.fill(n)(ma))


  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(g)

  def _flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] =
    compose((_: Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(ma => ma)

  /**
   * Implement the function filterM. It’s a bit like filter, except that instead of a function
   * from A => Boolean, we have an A => F[Boolean]. (Replacing various ordinary functions
   * like this with the monadic equivalent often yields interesting results.)
   * Implement this function, and then think about what it means for various data types.
   */
  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    ms.foldRight(unit(List[A]()))((x, y) =>
      compose(f, (b: Boolean) => if (b) map2(unit(x), y)(_ :: _) else y)(x))

  //  compose(compose(f, g), h) == compose(f, compose(g, h))


}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad: Monad[Gen] = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }


  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)

    override def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A) = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  // Since `State` is a binary type constructor, we need to partially apply it
  // with the `S` type argument. Thus, it is not just one monad, but an entire
  // family of monads, one for each type `S`. One solution is to create a class
  // `StateMonads` that accepts the `S` type argument and then has a _type member_
  // for the fully applied `State[S, A]` type inside:
  class StateMonads[S] {
    type StateS[A] = State[S, A]

    // We can then declare the monad for the `StateS` type constructor:
    val monad: Monad[StateS] = new Monad[StateS] {
      def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
        st flatMap f
    }
  }

  // But we don't have to create a full class like `StateMonads`. We can create
  // an anonymous class inline, inside parentheses, and project out its type member,
  // `lambda`:
  def stateMonad[S]: Monad[({
    type lambda[x] = State[S, x]
  })#lambda] = new Monad[({type lambda[x] = State[S, x]})#lambda] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](ida: Id[A])(f: A => Id[B]): Id[B] = ida flatMap f
  }

  def getState[S]: State[S,S] = State(s => (s,s))
  def setState[S](s: S): State[S,Unit] = State(_ => ((),s))

  val F: Monad[({
    type lambda[x] = State[Int, x]
  })#lambda] = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
      xs <- acc
      n  <- getState
      _  <- setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  // The action of Reader's `flatMap` is to pass the `r` argument along to both the
  // outer Reader and also to the result of `f`, the inner Reader. Similar to how
  // `State` passes along a state, except that in `Reader` the "state" is read-only.

  // The meaning of `sequence` here is that if you have a list of functions, you can
  // turn it into a function that takes one argument and passes it to all the functions
  // in the list, returning a list of the results.

  // The meaning of `join` is simply to pass the same value as both arguments to a
  // binary function.

  // The meaning of `replicateM` is to apply the same function a number of times to
  // the same argument, returning a list of the results. Note that if this function
  // is _pure_, (which it should be), this can be exploited by only applying the
  // function once and replicating the result instead of calling the function many times.
  // This means the Reader monad can override replicateM to provide a very efficient
  // implementation.

  def readerMonad[R]: Monad[({
    type f[x] = Reader[R, x]
  })#f] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
      Reader(r => f(st.run(r)).run(r))
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def ask[R]: Reader[R, R] = Reader(r => r)
}