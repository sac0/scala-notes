package advanced.lectures.part4implicits

/**
  * Created by Sachin.
  */
object ImplicitsIntro extends App {

  val pair = "Sachin" -> "555"
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name!"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  println("Peter".greet) // println(fromStringToPerson("Peter").greet)

//  class A {
//    def greet: Int = 2
//  }
//  implicit def fromStringToA(str: String): A = new A

  // implicit parameters
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10

  increment(2)
  // NOT default args

}
