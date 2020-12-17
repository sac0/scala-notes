package beginners.lectures.part2oop

/**
 * Created by Sachin.
 */
object Objects extends App {

  // SCALA DOES NOT HAVE CLASS-LEVEL FUNCTIONALITY ("static")
  object Person { // type + its only instance
    // "static"/"class" - level functionality
    private val N_EYES = 2

    def canFly: Boolean = false

    def canIAccessMagic(): Unit = {
      new Person("Harry").magic()
    }

    // factory method
    def apply(mother: Person, father: Person): Person = new Person("Bobbie")
  }

  class Person(val name: String) {
    // instance-level functionality
    println()

    private def magic(): Unit = {
      println("If this isnt magic what is")
    }
  }

  // COMPANIONS
  // uncomment the following line and play around the private val N_EYES
  //  println(Person.N_EYES)
  println(Person.canFly)

  // Scala object = SINGLETON INSTANCE
  val mary = new Person("Mary")
  val john = new Person("John")
  println(mary == john)

  val person1 = Person
  val person2 = Person
  println(person1 == person2)

  val bobbie = Person(mary, john)
  // Scala Applications = Scala object with
  // def main(args: Array[String]): Unit


  val k = 6.67e-11

}
