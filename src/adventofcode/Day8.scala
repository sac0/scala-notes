package adventofcode

object Day8 extends App{
  val regex = raw"(\\+)(x[^\\]{2}|.)".r

  val lines = io.Source.fromResource("adventofcode/Day8.txt").getLines;

  println(lines.map { line =>
    regex.findAllMatchIn(line).map { m =>
      val backslashes = m.group(1).size
      val evenNumber = backslashes % 2 == 0
      backslashes/2 + (if (evenNumber) 0 else m.group(2).size)
    }.sum + 2
  }.sum)
  println(io.Source.fromResource("adventofcode/Day8.txt").getLines.map(
    _.count(Seq('\\', '"').contains) + 2
  ).sum)
}
