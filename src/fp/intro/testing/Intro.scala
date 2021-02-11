package fp.intro.testing

object Intro {
  /**
   *  Test case minimization— In the event of a failing test,
   * the framework tries smaller sizes until it finds the smallest test case
   * that also fails, which is more illuminating for debugging purposes.
   * For instance, if a property fails for a list of size 10,
   * the framework tries smaller lists and reports the smallest list that fails the test.
   *
   *  Exhaustive test case generation— We call the set of values that could be produced
   * by some Gen[A]the domain. When the domain is small enough
   * for instance,if it’s all even integers less than 100,
   * we may exhaustively test all its values, rather than generate sample values.
   * If the property holds for all values in a domain, we have an actual proof,
   * rather than just the absence of evidence to the contrary.
   */

}
