package beginners.implicits

object TypeClasses extends App {
  trait HTMLWritable {
    def toHtml: String
  }
  case class User(name:String, age:Int, email:String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name (age: $age)</div>"
  }

  User("John",32,"some@g.com").toHtml


}
