package beginners.fp

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def -(elem: A): MySet[A]

  def --(anotherSet: MySet[A]): MySet[A]

  def &(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach(f: A => Unit): Unit

  def unary_! : MySet[A]
}


class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def foreach(f: A => Unit): Unit = ()

  override def apply(elem: A): Boolean = contains(elem)

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def -(elem: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def contains(elem: A): Boolean = {
    elem == head || tail.contains(elem)
  }

  override def +(elem: A): MySet[A] = {
    if (this contains elem) this
    else new NonEmptySet[A](elem, this)
  }

  override def ++(anotherSet: MySet[A]): MySet[A] = {
    tail ++ anotherSet + head
  }

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail: MySet[A] = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet(x))

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))

  override def apply(elem: A): Boolean = contains(elem)
}

class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  override def +(elem: A): MySet[A] = new PropertyBasedSet[A]((x: A) => {
    x == elem || property(x)
  })

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A]((x: A) => {
    anotherSet(x) || property(x)
  })

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def foreach(f: A => Unit): Unit = politelyFail

  override def &(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A]((x: A) => {
    anotherSet(x) && property(x)
  })

  override def --(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A]((x: A) => {
    !anotherSet(x) && property(x)
  })

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A]((x: A) => {
    predicate(x) && property(x)
  })

  override def -(elem: A): MySet[A] = filter(_ != elem)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  override def apply(elem: A): Boolean = contains(elem)
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }
    buildSet(values.toSeq, new EmptySet[A])
  }
}
