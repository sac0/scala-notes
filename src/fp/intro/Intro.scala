package fp.intro


object Intro extends App {

  /**
   * What are side effects
   * Modifying a variable
   *  Modifying a data structure in place
   *  Setting a field on an object
   *  Throwing an exception or halting with an error  Printing to the console or reading user input
   *  Reading from or writing to a file
   *  Drawing on the screen
   *
   * Functional programming is a restriction on how we write the programs not what we can write. We can write all the
   * above functions
   * without side effects
   *
   * Because of their modularity, pure functions are easier to test, reuse, parallelize, generalize, and reason about.
   * Furthermore, pure functions are much less prone to bugs.
   */

  /** A program with side effects */
  case class CreditCard(card: String) {
    def charge(amount: Int): Charge = Charge(this, amount)
  }

  case class Payments() {
    //separating payments from credit card
    def charge(cc: CreditCard, price: Int): Charge = {
      // do some side effects
      Charge(cc, price)
    }
  }

  case class Coffee(size: String = "M", price: Int = 1)

  case class Charge(cc: CreditCard, amount: Int) {
    def combine(other: Charge): Charge = {
      if (cc == other.cc) Charge(cc, amount + other.amount)
      else throw new RuntimeException("Cant combine different cc's")
    }

    def coalesce(charges: List[Charge]): List[Charge] = charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }

  class Cafe {
    def buyCoffee(cc: CreditCard): Coffee = {
      val cup = Coffee()

      /**
       * This is an example of a side effect. This might start a string of transactions
       * This can also interact with the databases or the web backend. Call an API
       * buy coffee  returns  a coffee and all these things are happening on the side lines
       * These are side effects
       *
       * As a result of this side effect, the code is difficult to test. We don’t want our tests to actually contact
       * the credit card company and charge the card! This lack of testability is suggesting a design change: arguably,
       * CreditCard shouldn’t have any knowledge baked into it about how to contact the credit card company to actually
       * execute a charge, nor should it have knowledge of how to persist a record of this charge in our internal systems.
       * We can make the code more modular and testable by letting Credit- Card be ignorant of these concerns and passing
       * a Payments object into buyCoffee.
       */
      cc.charge(cup.price)
      cup
    }

    def buyCoffeeV2(cc: CreditCard, p: Payments): Coffee = {
      val cup = Coffee()

      /**
       * Side effects still persist in this case. but we can write a mock payment interface which encapsulates the various side effects
       * Core business logic becomes testable
       * We are forced to make payments an interface. Mocking it will be awkward. If it ws a class it was better why ?
       * So we can test the internal state of the class after calling the buy coffee
       * Final problem : It is difficult to reuse the buyCoffee. If alice buys 12 coffee we contact payments 12 times
       * Here we can implement buyCoffees but it still is an issue to ponder in non trivial cases
       */
      p.charge(cc, cup.price)
      cup
    }

    def buyCoffeeV3(cc: CreditCard): (Coffee, Charge) = {
      val cup = Coffee()

      /** We have separated the concern of payments from the coffee payments here
       * We have created an object charge which is easy to test in buyCoffee implementation
       * We can combine charges
       */
      (cup, Charge(cc, cup.price))
    }

    def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
      /**
       * Overall, this solution is a marked improvement—we’re now able to reuse buyCoffee directly to define the
       * buyCoffees function, and both functions are trivially testable without having to define complicated mock
       * implementations of some Payments interface!
       *
       * WE can implement handy functions like coalesce
       */
      val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffeeV3(cc))
      val (coffees, charges) = purchases.unzip
      (coffees, charges.reduce(_.combine(_)))
    }

    /**
     * But even so, surely at some point we must actually have an effect on the world and submit the Charge for
     * processing by some external system. And aren’t there other useful programs that necessitate side effects
     * or mutation? How do we write such programs?
     * In other cases we’ll find ways to structure code so that effects occur but are not observable.
     * (For example, we can mutate data that’s declared locally in the body of some function if we ensure
     * that it can’t be referenced outside that function, or we can write to a file as long as no enclosing
     * function can observe this occurring
     */

    /**
     * Unless we state otherwise, we’ll often use function to imply no side effects - VERY IMPORTANT
     */

    /**
     * What is referentially transparent
     * Like how in algebra we can replace the functions with values it outputs or refers to
     * In above case, buyCoffee returns a cup of coffee. Charge is a an extra task will be missed if we directly
     * pass a coffee instance
     *
     */

    /**
     * A simple impure function
     * scala> val x = new StringBuilder("Hello") x: java.lang.StringBuilder = Hello
     * scala> val r1 = x.append(", World").toString r1: java.lang.String = Hello, World
     * scala> val r2 = x.append(", World").toString r2: java.lang.String = Hello, World, World
     *
     * By the time r2 calls x.append, r1 will have already mutated the object referenced by x.
     * If this seems difficult to think about, that’s because it is.
     * Side effects make reasoning about program behavior more difficult.
     * Conversely, the substitution model is simple to reason about since effects of evaluation
     * StringBuilder changes x is not clear from the function
     */
    /**
     * A pure function is modular and composable because it separates the logic of the computation itself from
     * “what to do with the result” and “how to obtain the input”; it’s a black box.
     * Input is obtained in exactly one way: via the argument(s) to the function.
     * And the output is simply computed and returned. By keeping each of these concerns separate,
     * the logic of the computation is more reusable; we may reuse the logic wherever we want without worrying about
     * whether the side effect being done with the result or the side effect requesting the input are appropriate
     * in all contexts. We saw this in the buyCoffee example—by eliminating the side effect of payment processing
     * being done with the output, we were more easily able to reuse the logic of the function, both for purposes of
     * testing and for purposes of further composition (like when we wrote buyCoffees and coalesce).
     */
  }

}
