package beginners.implicits

import java.util.Date

object JSONSerialization extends App {

  case class User(name: String, age: String, email: String)

  case class Post(content: String, createdAt: Date)

  case class Feed(user: User, posts: List[Post])

}
