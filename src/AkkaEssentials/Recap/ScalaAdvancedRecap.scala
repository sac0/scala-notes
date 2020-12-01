package AkkaEssentials.Recap

object ScalaAdvancedRecap extends App {

  //  partial functions

  val partialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 23 => 23
    case 0 => 12
  }

  // is is as if we have said
  val pf = (x: Int) => x match {
    case 1 => 42
    case 23 => 23
    case 0 => 12
  }

  val function: (Int => Int) = partialFunction // this is a valid fuctions

  // this means that we can use partial functions in the collections

  val modifiedList = List(1, 3, 5).map {
    case 1 => 42
    case _ => 0
  }

  val lifted = partialFunction.lift
  lifted(23) // => Some(23) else None

  // additional chaining of partialFunction
  // see how partial functions are being defined

  // see how this def has types
  val pfChain = partialFunction orElse[Int, Int] {
    case 60 => 9000
  }

  pfChain(23) // 23
  pfChain(60) // 9000
  //  pfChain(457) // throws a match error

  // type aliases
  type ReceiveFunction = PartialFunction[Any, Unit]

  def receive: ReceiveFunction = {
    case 1 => println("Hello")
    case _ => println("Confused")
  }

  // implicits


  // parameter skip
  implicit val timeout: Int = 3000

  def setTimeout(f: () => Unit)(implicit timeout: Int): Unit = f()

  setTimeout(() => println("timeout")) // second parameter skipped


  // String to person conversion
  case class Person(name: String) {
    def greet = s"Hi my name id $name"
  }

  implicit def fromStringToPerson(string: String): Person = Person(string)

  "Peter".greet // changed to the Persons type automatically

  // implicit classes
  implicit class Dog(name: String) {
    def bark(): Unit = println("bark!")
  }

  "Lassie".bark // works

  // organizer
  implicit val inverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
  List(1, 2, 3).sorted // ordering is implicit

  //imported scope with future with globals

  //  implicits in companion
  object Person {
    implicit val personOrdering: Ordering[Person] = Ordering.fromLessThan((a: Person, b: Person) => a.name.compareTo(b.name) < 0)
  }

  List(Person("Bob"), Person("Alice")).sorted


}
