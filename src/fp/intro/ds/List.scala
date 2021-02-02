package fp.intro.ds

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)


  /**
   * Although we could return `Nil` when the input list is empty, we choose to throw an exception instead. This is
   * a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug, and silently
   * returning a value just means this bug will be discovered later, further from the place where it was introduced.
   * It's generally good practice when pattern matching to use `_` for any variables you don't intend to use on the
   * right hand side of a pattern. This makes it clear the value isn't relevant.
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
   * If a function body consists solely of a match expression, we'll often put the match on the same line as the
   * function signature, rather than introducing another level of nesting.
   */
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }


  /** We should have multiple lists of arguments to create HOF so type inference can be carried forward (L->R)
   * def dropWhile[A](as: List[A])(f: A => Boolean) -> curried it dropWhile(xs)(x => x < 4)
   * normally dropWhile(xs, (x: Int) => x < 4)
   **/
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  /**
   * Note that this is no longer a constant time operation -> since we cannot mark the last as null directly
   * Note that we're copying the entire list up until the last element. Besides being inefficient, the natural recursive
   * which can lead to stack overflows. With lists, it's common to use a temporary, mutable buffer internal to the
   * function (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this). So long as the
   * buffer is allocated internal to the function, the mutation is not observable and RT is preserved.
   * Another common convention is to accumulate the output list in reverse order, then reverse it at the end, which
   * doesn't require even local mutation. We'll write a reverse function later in this chapter.
   */
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }

  /**
   * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
   * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
   * This is a deeper question that we’ll return to in chapter 5.
   *
   * No, this is not possible! The reason is because _before_ we ever call our function, `f`, we evaluate its argument,
   * which in the case of `foldRight` means traversing the list all the way to the end. We need _non-strict_ evaluation
   * to support early termination---we discuss this in chapter 5.
   */


  /**
   * We get back the original list! Why is that? As we mentioned earlier, one way of thinking about what `foldRight` "does"
   * is it replaces the `Nil` constructor of the list with the `z` argument, and it replaces the `Cons` constructor with
   * the given function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`, then we get back the input list.
   * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
   * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
   * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
   * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
   * Cons(1, Cons(2, Cons(3, Nil)))
   */

  /**
   * Compute the length of a list using foldRight.
   */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  def sum3(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)

  }

  def length2(as: List[Int]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  /** Reverse a list using fold */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((tail, head) => Cons(head, tail))
  }

  /**
   * As background, let's start with what foldLeft and foldRight do. For example, the result of foldLeft on the
   * list [1, 2, 3] with the operation * and starting value z is the value
   * ((z * 1) * 2) * 3
   *
   * We can think of foldLeft as consuming values of the list incrementally, left to right. In other words,
   * we initially start with the value z (which is what the result would be if the list were empty), then we reveal
   * to foldLeft that our list starts with 1 and the value becomes z * 1, then foldLeft sees our list next has 2 and
   * the value becomes (z * 1) * 2, and finally, after acting on 3, it becomes the value ((z * 1) * 2) * 3.
   *
   * 1    2    3
   * Initially:               z
   * After consuming 1:      (z * 1)
   * After consuming 2:     ((z * 1) * 2
   * After consuming 3:    (((z * 1) * 2) * 3
   * This final value is the value we want to achieve, except (as the exercise asks us) using foldRight instead.
   * Now note that, just as foldLeft consumes values of the list left to right, foldRight consumes values of the
   * list right to left. So on the list [1, 2, 3],
   *
   * This foldRight will act on 3 and [something], giving a [result]
   * Then it will act on 2 and the [result], giving [result2]
   * Finally it will act on 1 and [result2] giving the final expression
   * We want our final expression to be (((z * 1) * 2) * 3
   * In other words: using foldRight, we first arrive at what the result would be if the list were empty,
   * then the result if the list contained only [3], then the result if the list were [2, 3], and
   * finally the result for the list being [1, 2, 3].
   *
   * That is, these are the values we would like to arrive at, using foldRight:
   *
   * 1    2    3
   * Initially:                             z
   * After consuming 3:                 z * 3
   * After consuming 2:           (z * 2) * 3
   * After consuming 1:     ((z * 1) * 2) * 3
   * So we need to go from z to (z * 3) to (z * 2) * 3 to ((z * 1) * 2) * 3.
   *
   * As values, we cannot do this: there's no natural way to go from the value (z * 3) to the value (z * 2) * 3,
   * for an arbitrary operation *. (There is for multiplication as it's commutative and associative,
   * but we're only using * to stand for an arbitrary operation.)
   *
   * But as functions we may be able to do this! We need to have a function with a "placeholder" or "hole":
   * something that will take z and put it in the proper place.
   *
   * E.g. after the first step (after acting on 3) we have the placeholder function z => (z * 3). Or rather,
   * as a function must take arbitrary values and we've been using z for a specific value,
   * let's write this as t => (t * 3). (This function applied on input z gives the value (z * 3).)
   * After the second step (after acting on 2 and the result) we have the placeholder function t =>  (t * 2) * 3 maybe?
   * Can we go from the first placeholder function to the next? Let
   *
   * f1(t) = t * 3
   * and   f2(t) = (t * 2) * 3
   * What is f2 in terms of f1?
   *
   * f2(t) = f1(t * 2)
   * Yes we can! So the function we want takes 2 and f1 and gives f2. Let's call this g. We have g(2, f1) = f2
   * where f2(t) = f1(t * 2) or in other words
   *
   * g(2, f1) =
   * t => f1(t * 2)
   * Let's see if this would work if we carried it forward: the next step would be g(1, f2) = (t => f2(t * 1)) and
   * the RHS is same as t => f1((t * 1) * 2)) or t => (((t * 1) * 2) * 3).
   *
   * Looks like it works! And finally we apply z to this result.
   *
   * What should the initial step be? We apply g on 3 and f0 to get f1, where f1(t) = t * 3 as defined above but
   * also f1(t) = f0(t * 3) from the definition of g. So looks like we need f0 to be the identity function.
   *
   * Let's start afresh.
   *
   * Our foldLeft(List(1, 2, 3), z)(*) is ((z * 1) * 2) * 3
   * Types here: List(1, 2, 3) is type List[A]
   * z is of type B
   * * is of type (B, A) -> B
   * Result is of type B
   * We want to express that in terms of foldRight
   * As above:
   * f0 = identity. f0(t) = t.
   * f1 = g(3, f0). So f1(t) = f0(t * 3) = t * 3
   * f2 = g(2, f1). So f2(t) = f1(t * 2) = (t * 2) * 3
   * f3 = g(1, f2). So f3(t) = f2(t * 1) = ((t * 1) * 2) * 3
   * And finally we apply f3 on z and get the expression we want. Everything works out. So
   *
   * f3 = g(1, g(2, g(3, f0)))
   * which means f3 = foldRight(xs, f0)(g)
   *
   * Let's define g, this time instead of x * y using an arbitrary function s(x, y):
   *
   * first arg to g is of type A
   * second arg to g is of the type of these f's, which is B => B
   * So type of g is (A, (B=>B)) => (B=>B)
   * So g is:
   *
   * def g(a: A, f: B=>B): B=>B =
   * (t: B) => f(s(t, a))
   * Putting all this together
   *
   * def foldLeft[A, B](xs: List[A], z: B)(s: (B, A) => B): B = {
   * val f0 = (b: B) => b
   *
   * def g(a: A, f: B=>B): B=>B =
   * t => f(s(t, a))
   *
   * foldRight(xs, f0)(g)(z)
   * }
   * At this level of working through the book, I actually prefer this form as it's more explicit and easier to
   * understand. But to get closer to the form of the solution, we can inline the definitions of f0 and g
   * (we no longer need to declare the type of g as it's input to foldRight and the compiler infers it), giving:
   *
   * def foldLeft[A, B](xs: List[A], z: B)(s: (B, A) => B): B =
   * foldRight(xs, (b: B) => b)((a, f) => t => f(s(t, a)))(z)
   * which is exactly what is in the question just with different symbols. Similarly for foldRight in terms of foldLeft
   */

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  /** Implement append in terms of either foldLeft or foldRight. */

  def appendViaFold[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)(Cons(_, _))
  }

  /** Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
   */
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(List.append)
  }

  /**
   *
   * A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
   * use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
   * implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
   * mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
   */

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def map_1[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {

    foldRight(l, Nil: List[A])((h, t) => {
      if (f(h)) Cons(h, t) else t
    })
  }

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }

    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
   * This could also be implemented directly using `foldRight`.
   */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))
  // Use flatMap to implement filter.
  def filter_3[A](l: List[A])(f: A => Boolean): List[A] = {

    flatMap(l)(x=> {
      if(f(x)) List(x) else Nil
    })
  }
  /**
  To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
  syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
  objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
  right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
  variables introduced in the outer `match`.
  The discussion about stack usage from the explanation of `map` also applies here.
  */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }

  /**
  This function is usually called `zipWith`. The discussion about stack usage from the explanation of `map` also
  applies here. By putting the `f` in the second argument list, Scala can infer its type from the previous argument list.
  */
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

}