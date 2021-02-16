package fp.intro.parsers

import fp.intro.testing.Prop._
import fp.intro.testing.{Gen, Prop}

import language.higherKinds


case class ParseError()
trait Parsers[Parser[+_]] {
  self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /**
   * Step-1
   *  A Parser[Int] that recognizes zero or more 'a' characters, and whose
   * result value is the number of 'a' characters it has seen.
   * For instance, given "aa", the parser results in 2; given "" or "b123"
   * (a string not starting with 'a'), it results in 0; and so on.
   *
   *  A Parser[Int] that recognizes one or more 'a' characters,
   * and whose result value is the number of 'a' characters it has seen.
   * (Is this defined somehow in terms of the same combinators as the
   * parser for 'a' repeated zero or more times?) The parser should fail
   * when given a string without a starting 'a'. How would you like to
   * handle error reporting in this case? Could the API support giving
   * an explicit message like "Expected one or more 'a'" in the case of failure?
   *
   *  A parser that recognizes zero or more 'a',
   * followed by one or more 'b', and which results in the pair of counts
   * of characters seen. For instance, given "bbb", we get (0,3), given "aaaab",
   * we get (4,1), and so on.
   *
   *  If we’re trying to parse a sequence of zero or more "a" and are only
   * interested in the number of characters seen, it seems inefficient to have to build up,
   * say, a List[Char] only to throw it away and extract the length.
   * Could something be done about this?
   *

   *  We introduced a type ParseError earlier, but so far we haven’t chosen any functions for the API
   * \of ParseError and our algebra doesn’t have any way of letting the programmer control what
   * errors are reported. This seems like a limitation,
   * given that we’d like meaningful error messages from our parsers. Can you do something about it?
   *  Does a|b mean the same thing asb|a? This is a choice you get to make.
   * What are the consequences if the answer is yes? What about if the answer is no?
   *  Doesa|(b|c)mean the same thing as(a|b)|c?
   * If yes, is this a primitive law for your algebra, or is it implied by something simpler?
   *  Try to come up with a set of laws to specify your algebra.
   * You don’t necessarily need the laws to be complete;
   * just write down some laws that you expect should hold for any Parsers implementation.
   */

  /**
   * Answer Step-1
   * Here what are aiming for is the algebra between parsers hence we cannot count in a loop
   * We want to express an algebra for parsers hence the below definition
   * These are called combinators as they act on a value and either combine or change them to other of the same type
   * lets map and get a int parser
   */

  // many1(p) is just p followed by many(p)
  def many1[A](p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]= ???

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = ???

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  // This is run(succeed(a))(s) == Right(a)
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)


  /**
   * With slice, our parser that counts 'a' characters can now be written as char('a')
   * .many.slice.map(_.size) (assuming we add an alias for slice to ParserOps).
   * The _.size function here is now referencing the size method on String,
   * which takes constant time, rather than the size method on List,
   * which takes time proportional to the length of the list
   * (and requires us to actually construct the list)
   */
  def slice[A](p: Parser[A]): Parser[String]




  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
  }

  object Laws {
    /**
     * Step-2 add laws to check our library
     * map(p)(a => a) == p
     */
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }




}


object PlayParsers extends App {

}

//case class Location(input: String, offset: Int = 0) {
//
//  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
//  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')
//
//  def toError(msg: String): ParseError =
//    ParseError(List((this, msg)))
//
//  def advanceBy(n: Int) = copy(offset = offset+n)
//
//  /* Returns the line corresponding to this location */
//  def currentLine: String =
//    if (input.length > 1) input.lines.drop(line-1).next
//    else ""
//}
//
//case class ParseError(stack: List[(Location,String)] = List(),
//                      otherFailures: List[ParseError] = List()) {
//}