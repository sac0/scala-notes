package adventofcode

object Day9 extends App{

  val lines = io.Source.fromResource("adventofcode/Day9.txt").getLines

  val regex = raw"(.+) to (.+) = (\d+)".r

  val distances = lines.map { case regex(a, b, distance) =>
    (a, b) -> distance.toInt
  }.toMap

  println(distances.keys.flatten((a)=>Seq(a._1,a._2)).toVector.permutations.map({(path)=>{
    path.sliding(2).map { case Seq(a, b) =>
      distances.getOrElse((a, b), distances(b, a))
    }.sum
  }}).max)
}
