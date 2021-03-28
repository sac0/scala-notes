package adventofcode


object Day10 extends App {
  def nextStr(ss: String): String = {
    val ans = ss.toList.foldLeft(("", ss.charAt(0), 0))((tup, b) => {
      if ((tup._2) == b) {
        (tup._1, b, tup._3 + 1)
      } else {
        (s"${tup._1}${tup._3}${tup._2}", b, 1)
      }
    })
    //    println(ans);
    ans._1 + s"${ans._3}${ans._2}"
  }

  var time = System.currentTimeMillis()
  var res = "1321131112"
  for (i <- 1 to 40) {
    res = nextStr(res)
  }
//  println(res.length)
  println(System.currentTimeMillis() - time);
//  time = System.currentTimeMillis()
//  var line = "1321131112"
//  val regex = raw"(\d)\1*".r
//
//  println(System.currentTimeMillis() - time);
  //  var line = "1321131112"
  //  val regex = raw"(\d)\1*".r
  //  (1 to 50).foreach { i =>
  //    line = regex.replaceAllIn(line, m =>
  //      m.matched.size + m.matched.take(1)
  //    )
  //  }
  //  println(line.size)

}
