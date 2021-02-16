package fp.intro.parsers

class Intro {

  /**
   *
   * Parser is a type parameter that itself is a covariant type constructor
   * trait Parsers[ParseError, Parser[[+_]] {
   * def run[A](p: Parser[A])(input: String): Either[ParseError,A]
   * Here the Parser type constructor is applied to Char.
   * def char(c: Char): Parser[Char] }
   *
   *
   * We should now have this
   * Since the parser parses exactly this string. What we are trying to do here is write an algebra
   * run(char(c))(c.toString) == Right(c)
   *
   * we can give this or combinator nice infix syntax like s1 | s2 or alternately
   * s1 or s2, using implicits
   * in this case a string can be converted in to its parser type implicitly
   *
   *
   * def listOfN[A] (n: Int, p: Parser[A]): Parser[List[A]]
   * We made listOfN parametric in the choice of A, since it doesn’t seem like it should care whether we have a Parser[String], a Parser[Char], or some other type of parser. Here are some examples of what we expect from listOfN:
   * run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
   * run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
   * run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")
   *
   * check step-1 at this point
   */

}
