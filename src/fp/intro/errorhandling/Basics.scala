package fp.intro.errorhandling

object Basics extends App {


  def failingFn(): Int = {
    //    val y: Int = throw new Exception("fail!")
    val y = 5
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case _: Exception => 43
    }
  }

  /**
   * We can prove that y is not referentially transparent. Recall that any RT expression may be substituted with the
   * value it refers to, and this substitution should preserve program meaning.
   * If we substitute throw new Exception("fail!") for y in x + y => gives 43 since the exception is caught here
   *
   * Another way of understanding RT is that the meaning of RT expressions does not depend on context and may be
   * reasoned about locally, whereas the meaning of non-RT expressions is context-dependent and requires more global
   * reasoning. For instance, the mean- ing of the RT expression 42 + 5 doesnt depend on the larger expression it’s
   * embedded in it’s always and forever equal to 47
   */

  /**
   * There are two main problems with exceptions:
   *  As we just discussed, exceptions break RT and introduce context dependence, moving us away from the
   * simple reasoning of the substitution model and making it possible to write confusing exception-based code.
   * This is the source of the folklore advice that exceptions should be used only for error handling,
   * not for control flow.
   *  Exceptions are not type-safe. The type of failingFn, Int => Int tells us nothing about the fact that exceptions
   * may occur, and the compiler will certainly not force callers of failingFn to make a decision about how to
   * handle those exceptions. If we forget to check for an exception in failingFn, this won’t be detected until runtime.
   */

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty) throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  /**
   * Partial Functions
   * The mean function is an example of what’s called a partial function: it’s not defined for
   * some inputs. A function is typically partial because it makes some assumptions about its
   * inputs that arent implied by the input types.
   */

  /**
   * 1st Possibility
   * return some sort of bogus value of type Double/ Sentinel Value like Double.NaN
   *
   * Problems
   *  It allows errors to silently propagate—the caller can forget to check this condition and
   * won’t be alerted by the compiler, which might result in subsequent code not working properly.
   * Often the error won’t be detected until much later in the code.
   *
   *  Besides being error-prone, it results in a fair amount of boilerplate code at call sites, with explicit
   * if statements to check whether the caller has received a “real” result. This boilerplate is magnified if you
   * happen to be calling several functions, each of which uses error codes that must be checked
   * and aggregated in some way.
   *
   *  It’s not applicable to polymorphic code. For some output types, we might not even have a sentinel value
   * of that type even if we wanted to! Consider a function like max, which finds the maximum value in a sequence
   * according to a custom comparison function:defmax[A](xs:Seq[A])(greater:(A,A)=>Boolean): A.
   * If the input is empty, we can’t invent a value of type A. Nor can null be used here,
   * since null is only valid for non-primitive types, and A may in fact be a primitive like Double or Int.
   *
   *  It demands a special policy or calling convention of callers—proper use of the mean function would require
   * that callers do something other than call mean and make use of the result. Giving functions special policies
   * like this makes it difficult to pass them to higher-order functions, which must treat all arguments uniformly.
   */

  /**
   * 2nd Possibility
   * force the caller to supply an argument that tells us what to do in case we don’t know how to handle the input:
   * Here mean becomes a total function. It has a drawback of making the callers understand all the undefined cases
   * and limits them to return a double
   *
   * What if mean is called as part of a larger computation and we like to abort that computation if mean is undefined?
   * Or perhaps we’d like to take some completely different branch in the larger computation in this case?
   * Simply passing an onEmpty parameter does not give us this freedom.
   *
   * Answer : Option
   */

  /**
   * Either has only two cases, just like Option. The essential difference is that both cases
   * carry a value. The Either data type represents, in a very general way, values that can
   * be one of two things. We can say that it’s a disjoint union of two types. When we use it to
   * indicate success or failure, by convention the Right constructor is reserved for the
   * success case (a pun on “right,” meaning correct), and Left is used for failure. We’ve
   * given the left type parameter the suggestive name E (for error).
   */

  /**
   * Lets us design a simple try catch using the knowledge gathered
   * This doesnt capture the information of the error
   */

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case _: Exception => None
    }

  def TryWithEither[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }


}
