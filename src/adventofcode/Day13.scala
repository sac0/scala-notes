package adventofcode

object Day13 extends App{

  val regex = raw"(.+) would (lose|gain) (\d+) happiness units by sitting next to (.+).".r

  val distances = io.Source.fromResource("adventofcode/Day13.txt").getLines.map { case regex(a, sign, distance, b) =>
    (a, b) -> distance.toInt * (if (sign == "lose") -1 else 1)
  }.toMap

  println(distances.keys.flatMap {
    case (a, b) => Seq(a, b)
  }.toVector.permutations.map( path =>
    (path :+ path.head).sliding(2).map { case Seq(a, b) =>
      distances(a, b) + distances(b, a)
    }.sum
  ).max)

  println(distances.keys.flatMap {
    case (a, b) => Seq(a, b)
  }.toVector.permutations.map { path =>
    val costs = (path :+ path.head).sliding(2).map { case Seq(a, b) =>
      distances(a, b) + distances(b, a)
    }.toVector
    costs.sum - costs.min
  }.max)

}
