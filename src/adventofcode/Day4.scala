package adventofcode


object Day4 extends App {
  val hash = java.security.MessageDigest.getInstance("MD5")

  def firstIntegerToGiveFiveZeroes(input: String, check: String, current: Integer): Int = {
    val md5Input = s"$input$current"
    if (convertBytesToHex(hash.digest(md5Input.getBytes()).toArray[Byte]).substring(0, check.length) == check) {
      current
    } else {
      firstIntegerToGiveFiveZeroes(input, check,current + 1)
    }
  }
  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

  println(firstIntegerToGiveFiveZeroes("bgvyzdsv","00000",0))
  println(firstIntegerToGiveFiveZeroes("bgvyzdsv","000000",0))

}
