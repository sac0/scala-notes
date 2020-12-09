package akkaessentials.playground

object Play extends App {

  println(List("", "123", 234).map {
//    case "" => "eomty"
    case s:String => "str"
    case _ => "something else"
  })

}
