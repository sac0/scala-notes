package beginners.fp

object LazyEvaluation extends App {

  abstract class MyStream[+A] {
    def isEmpty:Boolean
    def head: A
    def tail: MyStream[A]
    def #::[B >: A](element: B):MyStream[B]
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]
    def foreach(f: A=>Unit):Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A=> Boolean):MyStream[A]
    def take(n:Int): MyStream[A]
    def takeAsList(n:Int):List[A]
  }


}
