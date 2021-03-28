package adventofcode

object Day3 extends App{
  println(io.Source.fromResource("adventofcode/Day3.txt").toList.scanLeft((0,0)) {
    {
      case (cur, '>')=> (cur._1+1,cur._2 )
      case (cur, '<')=> (cur._1-1,cur._2 )
      case (cur, '^')=> (cur._1,cur._2 +1)
      case (cur, 'v')=> (cur._1,cur._2 -1)
    }

  }.distinct.length)

  // since zipped is deprecated - thinking of anot
//  println(io.Source.fromResource("adventofcode/Day3.txt").toList.grouped(2).scanLeft( Seq(0 -> 0, 0 -> 0) )(
//    (a, b) => (a,b).zipped.map {
//      case ((x,y), '>') => (x+1, y)
//      case ((x,y), '<') => (x-1, y)
//      case ((x,y), '^') => (x, y+1)
//      case ((x,y), 'v') => (x, y-1)
//    }
//  ).flatten.toSeq.distinct.size)

//  io.Source.fromResource("adventofcode/Day3.txt").toList.grouped(2).take(5).foreach(println)

}
