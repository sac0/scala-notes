package adventofcode

object Day2 extends App {
  println(io.Source.fromResource("adventofcode/Day2.txt").getLines.map { line =>
    val nums = line.split("x").map(_.toInt)
    val Array(a, b, c) = nums
    2*(a*b + a*c + b*c) + nums.sorted.take(2).product
  }.sum)

  println(io.Source.fromResource("adventofcode/Day2.txt").getLines.map { line =>
    val nums = line.split("x").map(_.toInt)
    val Array(a, b, c) = nums
    nums.sorted.take(2).sum*2 + nums.product
  }.sum)


}
