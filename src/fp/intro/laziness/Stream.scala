package fp.intro.laziness

/**
 * Laziness is essential to not do unnecessary work and do the least work needed at the moment
 */

import fp.intro.laziness.Stream._

trait Stream[+A] {

  // The natural recursive solution
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _ => List()
  }

  /**
   * The above solution will stack overflow for large streams, since it's
   * not tail-recursive. Here is a tail-recursive implementation. At each
   * step we cons onto the front of the `acc` list, which will result in the
   * reverse of the stream. Then at the end we reverse the result to get the
   * correct order again.
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  /**
   * In order to avoid the `reverse` at the end, we could write it using a
   * mutable list buffer and an explicit loop instead. Note that the mutable
   * list buffer never escapes our `toList` method, so this function is
   * still _pure_.
   */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }

    go(this)
  }

  /**
   * The arrow `=>` in front of the argument type `B` means that
   * the function `f` takes its second argument by name and may choose not to evaluate it.
   * If `f` doesn't evaluate its second argument, the recursion never occurs.
   * See exists function for a slight variant of second function evaluation
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /**
   * Here `b` is the unevaluated recursive step that folds the tail of the stream.
   * If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
   */
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  /**
   * to think the following function will not evaluate everything but only the needed elements
   * is just mind boggling. We can add random function all over the place
   */
  def findUsingFilter(p: A => Boolean): Option[A] = filter(p).headOption


  /**
   * The incremental nature of stream transformations also has important consequences for memory usage.
   * Because intermediate streams aren’t generated, a transformation of the stream requires only
   * enough working memory to store and transform the current element. For instance,
   * in the transformation Stream(1,2,3,4).map(_ + 10).filter (_ % 2 == 0), the garbage collector
   * can reclaim the space allocated for the values 11 and 13 emitted by map as soon as
   * filter determines they are not needed. Of course, this is a simple example;
   * in other situations we might be dealing with larger numbers of elements, and the stream elements
   * themselves could be large objects that retain signifi- cant amounts of memory.
   * Being able to reclaim this memory as quickly as possible can cut down on the
   * amount of memory required by your program as a whole.
   */

  /**
   * Checkout list for mutable buffer implementations
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => empty
  }

  def takeWhile_1(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * Instead of using isEmpty. Implementing in terms of foldRight
   */
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  /**
   * Implement map, filter, append, and flatMap using foldRight.
   * The append method should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  /**
   * Note how s is passed in this function
   */
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)(cons(_, _))

  def flatmap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)


  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))


  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /**
   * `s startsWith s2` when corresponding elements of `s` and `s2` are all equal,
   * until the point that `s2` is exhausted. If `s` is exhausted first,
   * or we find an element that doesn't match, we terminate early. Using non-strictness,
   * we can compose these three separate logical steps--the zipping, the termination
   * when the second stream is exhausted, and the termination if a non matching element
   * is found or the first stream is exhausted.
   */
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  /**
   * The last element of `tails` is always the empty `Stream`,
   * so we handle this as a special case, by appending it to the output.
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  /**
   * The function can't be implemented using `unfold`,
   * since `unfold` generates elements of the `Stream` from left to right.
   * It can be implemented using `foldRight` though.
   * The implementation is just a `foldRight` that keeps the accumulated value
   * and the stream of intermediate results, which we `cons` onto during each iteration.
   * When writing folds, it's common to have more state in the fold
   * than is needed to compute the result.
   * Here, we simply extract the accumulated list once finished.
   */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  /**
   * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
   * Write a function that generates an infinite stream of integers, starting from n, then n+1, n+2
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8
   */
  def fibs: Stream[Int] = {
    def generateFibsFrom(a: Int, b: Int): Stream[Int] = cons(a + b, generateFibsFrom(b, a + b))

    cons(0, cons(1, generateFibsFrom(0, 1)))
  }

  /**
   * Option is used to indicate when the Stream should be terminated, if at all.
   * The function unfold is a very general Stream-building function.
   * The unfold function is an example of what’s sometimes called a corecursive function.
   * Whereas a recursive function consumes data, a corecursive function produces data.
   * whereas recursive functions terminate by recursing on smaller inputs,
   * corecursive functions need not terminate so long as they remain productive,
   * which just means that we can always evaluate more of the result in a finite amount of time.
   * The unfold function is productive as long as f terminates,
   * since we just need to run the function f one more time to generate the next element of the Stream.
   * Corecursion is also sometimes called guarded recursion, and productivity is also sometimes called cotermination.
   * These terms aren’t that important to our discussion, but you’ll hear them used sometimes
   * in the context of functional programming.
   */

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  /*
  The below two implementations use `fold` and `map` functions in the Option class to implement unfold, thereby doing away with the need to manually pattern match as in the above solution.
   */
  def unfoldViaFold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A])((p: (A, S)) => cons(p._1, unfold(p._2)(f)))

  def unfoldViaMap[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A, S)) => cons(p._1, unfold(p._2)(f))).getOrElse(empty[A])

  /*
  Scala provides shorter syntax when the first action of a function literal is to match on an expression.  The function passed to `unfold` in `fibsViaUnfold` is equivalent to `p => p match { case (f0,f1) => ... }`, but we avoid having to choose a name for `p`, only to pattern match on it.
  */
  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  // could also of course be implemented as constant(1)
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))
  
}
