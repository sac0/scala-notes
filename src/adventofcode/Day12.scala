package adventofcode

object Day12 extends App{

//  val regex = raw"(\-?\d+)".r
//  println(regex.findAllIn(io.Source.fromResource("adventofcode/Day12.txt").toArray).map(_.toInt).sum)

  val number = raw"-?\d+".r

  val objekt = raw"\{[^{}]+\}".r
  val forbidden = """:\s*"red"""".r.unanchored

  def sum(s: String) =
    number.findAllIn(s).map(_.toInt).sum

  def simplify(s: String) =
    objekt.replaceAllIn(s, _.matched match {
      case forbidden() => "0"
      case str => sum(str).toString
    })

  var line = io.Source.fromResource("adventofcode/Day12.txt").toString()
  while(number.unapplySeq(line).isEmpty) {
    println("simplyfiing")
    line = simplify(line)
  }

  println(line)

}
