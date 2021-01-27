package fp.intro.errorhandling

object Basics extends App {


  def failingFn(): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x+y }
    catch { case e: Exception => 43 }
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




}
