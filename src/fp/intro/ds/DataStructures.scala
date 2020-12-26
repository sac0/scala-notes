package fp.intro.ds

object DataStructures extends App {

  /**
   * A pure function must not change data in place or perform other side effects.
   * Therefore, functional data structures are by definition immutable.
   * For eg, the empty list (written List() or Nil in Scala) is as eternal and immutable as the int values 3 or 4
   */

  /**
   * In general we use a trait to declare a data type and make it sealed
   * A trait is a class that is meant to be added to some other class as a mixin. Unlike normal classes,
   * traits cannot have constructor parameters. Furthermore, no constructor arguments are passed to the
   * superclass of the trait. This is not necessary as traits are initialized after the superclass is initialized.
   */

  /**
   * Whenever you implement a reusable collection of behavior, you will have to decide whether you want to use a trait
   * or an abstract class. There is no firm rule, but this section contains a few guidelines to consider.
   *
   * If the behavior will not be reused, then make it a concrete class. It is not reusable behavior after all.
   *
   * If it might be reused in multiple, unrelated classes, make it a trait. Only traits can be mixed into different
   * parts of the class hierarchy.
   *
   * If you want to inherit from it in Java code, use an abstract class. Since traits with code do not have a close
   * Java analog, it tends to be awkward to inherit from a trait in a Java class. Inheriting from a Scala class,
   * meanwhile, is exactly like inheriting from a Java class. As one exception, a Scala trait with only abstract
   * members translates directly to a Java interface, so you should feel free to define such traits even if you
   * expect Java code to inherit from it. See Chapter 29 for more information on working with Java and Scala together.
   *
   * If you plan to distribute it in compiled form, and you expect outside groups to write classes inheriting from it,
   * you might lean towards using an abstract class. The issue is that when a trait gains or loses a member,
   * any classes that inherit from it must be recompiled, even if they have not changed. If outside clients will
   * only call into the behavior, instead of inheriting from it, then using a trait is fine.
   *
   * If efficiency is very important, lean towards using a class. Most Java runtimes make a virtual method invocation
   * of a class member a faster operation than an interface method invocation. Traits get compiled to interfaces and
   * therefore may pay a slight performance overhead. However, you should make this choice only if you know that the
   * trait in question constitutes a performance bottleneck and have evidence that using a class instead
   * actually solves the problem.
   *
   * If you still do not know, after considering the above, then start by making it as a trait.
   * You can always change it later, and in general using a trait keeps more options open.
   *
   * https://www.artima.com/pins1ed/traits.html#12.7
   */
  /**
   * Trivia
   *
   * https://stackoverflow.com/questions/10866639/difference-between-a-seq-and-a-list-in-scala
   * In Java terms, Scala's Seq would be Java's List, and Scala's List would be Java's LinkedList.
   *
   * Note that Seq is a trait, which is equivalent to Java's interface, but with the equivalent of up-and-coming
   * defender methods. Scala's List is an abstract class that is extended by Nil and ::,
   * which are the concrete implementations of List.
   *
   * So, where Java's List is an interface, Scala's List is an implementation.
   * Beyond that, Scala's List is immutable, which is not the case of LinkedList.
   * In fact, Java has no equivalent to immutable collections (the read only thing only guarantees the new object
   * cannot be changed, but you still can change the old one, and, therefore, the "read only" one).
   *
   * Scala's List is highly optimized by compiler and libraries, and it's a fundamental data type in
   * functional programming. However, it has limitations and it's inadequate for parallel programming.
   * These days, Vector is a better choice than List, but habit is hard to break.
   *
   * Seq is a good generalization for sequences, so if you program to interfaces, you should use that.
   * Note that there are actually three of them: collection.Seq, collection.mutable.Seq and collection.immutable.Seq,
   * and it is the latter one that is the "default" imported into scope.
   * There's also GenSeq and ParSeq. The latter methods run in parallel where possible, while the former is parent
   * to both Seq and ParSeq, being a suitable generalization for when parallelism of a code doesn't matter.
   * They are both relatively newly introduced, so people doesn't use them much yet.
   */
  // Todo finger tree


}
