package adventofcode

object Day5 extends App{

  val vowels = "aeiou".toSet
  def forbidden (ss:String):Boolean = Set("ab", "cd", "pq", "xy").contains(ss)
  def duplicate(ss: String) = ss(0) == ss(1)
  def hasThreeMatch (ss: String) = ss(0) == ss(2)

  println((io.Source.fromResource("adventofcode/Day5.txt").getLines map (line =>{

    if(line.toSeq.sliding(2).map(_.unwrap).exists(duplicate) &&
      !line.toSeq.sliding(2).map(_.unwrap).exists(forbidden) &&
      line.count(vowels) >= 3) {
      1
    } else {
      0
    }

  })).sum)

  def hasDuplicates(ss:Iterator[String]) = ss.distinct.size != ss.size
  def verifyLineNicety(line:String):Int = {

    val checkSeq = line.toSeq
    val checkDup = checkSeq.sliding(2).map(_.unwrap).exists( ss =>
      line.lastIndexOf(ss) - line.indexOf(ss) > 1
    )
    val hasThreeMatches = checkSeq.sliding(3).map(_.unwrap).exists(hasThreeMatch)

    if( checkDup && hasThreeMatches){
      1
    } else {
      0
    }
  }

  println((io.Source.fromResource("adventofcode/Day5.txt").getLines map verifyLineNicety).sum)

}
