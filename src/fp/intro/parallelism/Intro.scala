package fp.intro.parallelism

import fp.intro.parallelism.Par._

object Intro extends App {

  /**
   * IndexedSeq is a superclass of random-access sequences like Vector in the standard library.
   * Unlike lists, these sequences provide an efficient splitAt method for dividing
   * them into two parts at a particular index.
   */
  def sum(nums: IndexedSeq[Int]): Int =
    if (nums.size <= 1)
      nums.headOption getOrElse 0
    else {
      val (l, r) = nums.splitAt(nums.length / 2)
      sum(l) + sum(r)
    }

  /**
   * Look at the line sum(l) + sum(r), which invokes
   * sum on the two halves recursively. Just from looking at this single line,
   * we can see that any data type we might choose to represent our
   * parallel computations needs to be able to contain a result.
   * That result will have some meaningful type (in this case Int),
   * and we require some way of extracting this result.
   * Let’s apply this newfound knowledge to our design. For now, we can just
   * invent a container type for our result, Par[A] (for parallel ), and legislate the existence
   * of the functions we need:
   *
   * for taking an unevaluated A and returning a computation that might evaluate it in a separate thread
   * We call it unit because in a sense it creates a unit of parallelism that just wraps a single value
   * def unit[A](a: => A): Par[A]
   *
   * for extracting the resulting value from a parallel computation.
   * def get[A](a: Par[A]): A
   *
   */
  // This is an infinite loop. I've just written this to manage the type to match and not throw error elsewhere
  def get[A](a: Par[A]): A = get(a)

  def sumPar(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum(l))
      val sumR: Par[Int] = Par.unit(sum(r))
      get(sumL) + get(sumR)
    }

  /**
   * The problem with using concurrency primitives directly
   * What of java.lang.Thread and Runnable? Let’s take a look at these classes.
   * Here’s a partial excerpt of their API, transcribed into Scala:
   * trait Runnable { def run: Unit }
   * class Thread(r: Runnable) {
   * def start: Unit
   * def join: Unit
   * }
   * Begins running r in a separate thread.
   * Blocks the calling thread until r finishes running.
   * Already, we can see a problem with both of these types
   * none of the methods return a meaningful value.
   * Therefore, if we want to get any information out of a Runnable, it has to have some side effect,
   * like mutating some state that we can inspect.
   * This is bad for composition
   * we can’t manipulate Runnable objects generically since
   * we always need to know something about their internal behavior.
   * Thread also has the disadvantage that it maps directly onto
   * operating system threads, which are a scarce resource.
   * It would be preferable to create as many “logical threads”
   * as is natural for our problem, and later deal with mapping these onto actual OS threads.
   *
   * This kind of thing can be handled by something like java.util.concurrent.Future,
   * ExecutorService, and friends.
   * Why don’t we use them directly? Here’s a portion of their API:
   * class ExecutorService {
   * def submit[A](a: Callable[A]): Future[A]
   * }
   * trait Future[A] { def get: A }
   * Though these are a tremendous help in abstracting over physical threads,
   * these primitives are still at a much lower level of abstraction
   * than the library we want to create in this chapter.
   * A call to Future.get, for example, blocks the calling thread
   * until the ExecutorService has finished executing it, and its API
   * provides no means of composing futures.
   * Of course, we can build the implementation of our library on top of these tools
   * (and this is in fact what we end up doing later in the chapter),
   * but they don’t present a modular and compositional API that we’d want to use directly from functional programs.
   *
   *
   * We now have a choice about the meaning of unit and get—
   * unit could begin evaluating its argument immediately
   * in a separate (logical) thread,1 or it could simply hold
   * onto its argument until get is called and begin evaluation then.
   * But scala function evaluate from left to right so it would essentially become sequential
   *
   * So the two sides of the + sign won’t run in parallel
   * if we simply inline the sumL and sumR variables.
   *
   * if unit is concurrent we break referential transparency by losing parallism though the output is identical
   * Par.get(Par.unit(sum(l))) + Par.get(Par.unit(sum(r)))
   *
   * We can see that unit has a definite side effect,
   * but only with regard to get. That is, unit simply returns a Par[Int]
   * in this case, representing an asynchronous computation.
   * But as soon as we pass that Par to get, we explicitly wait for it,
   * exposing the side effect. So it seems that we want to avoid calling get,
   * or at least delay calling it until the very end.
   */

  /**
   * Let’s see if we can avoid the aforementioned pitfall of combining unit and get.
   * If we don’t call get, that implies that our sum function must return a Par[Int].
   */

  def sumPar2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sumPar2(l), sumPar2(r))(_ + _)
    }

  /**
   * it isn’t clear whether unit should accept its argument lazily anymore
   * Map2 takes 3 parallel computations and give a single Par as the output
   * What about map2—should it take its arguments lazily?
   * It would make sense for map2 to run both sides of the computation in parallel
   *
   * if map2 is strict - scala evaluates right to left
   * This has the rather unfortunate consequence that we’ll strictly construct
   * the entire left half of the tree of summations first before moving
   * on to (strictly) constructing the right half.
   * if map2 evaluates its argu- ments in parallel
   * (using whatever resource is being used to implement the parallelism,
   * like a thread pool), that implies the left half of our computation
   * will start executing before we even begin constructing the right half of our computation.
   *
   *
   * What if we keep map2 strict, but don’t have it begin execution immediately?
   *
   *
   * this implies a Par value is merely constructing a description of what needs
   * to be computed in parallel. Nothing actually occurs until we evaluate this
   * description, perhaps using a get-like function. The problem is that if
   * we construct our descriptions strictly, they’ll be rather heavy-weight objects
   *
   * map2(
   * map2(
   * unit(1),
   * unit(2))(_ + _),
   * map2(
   * unit(3),
   * unit(4))(_ + _))(_ + _)
   * )
   *
   * It seems we should make map2 lazy and have it begin immediate execution
   * of both sides in parallel. This also addresses the problem of giving
   * neither side priority over the other.
   */

  /**
   * Explicit Forking
   * Something doesn't feel right in our latest choice
   * Is it always the case that we want to evaluate the two argiments in parallel
   * Consider Par.map2(Par.unit(1), Par.unit(1))(_ + _)
   *
   * def fork[A](a: => Par[A]): Par[A]
   */
  def sumFork(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sumFork(l)), Par.fork(sumFork(r)))(_ + _)
    }

  /**
   * We can now make map2 strict
   * We have a way to indicate the results of the two parallel tasks should be combined.
   * Separate from this, we have the choice of whether a particular task should be performed asynchronously.
   * By keeping these concerns separate, we avoid having any sort of global policy for
   * parallelism attached to map2 and other combinators we write, which would mean
   * making tough (and ultimately arbitrary) choices about what global policy is best.
   *
   * def unit[A](a: A): Par[A]
   * def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
   *
   * fork signals evaluation in a separate thread
   * should it be immediately upon being called or to be evaluated later,
   * when the computation is forced using something like get.
   * In other words, should evaluation be the responsibility of fork or of get?
   * Should evaluation be eager or lazy?
   * When you’re unsure about a meaning to assign to some function in your API,
   * you can always continue with the design process—at some point
   * later the trade-offs of different choices of meaning may become clear.
   * Here we make use of a helpful trick
   * we’ll think about what sort of information is required to implement fork and get with various meanings.
   * If fork begins evaluating its argument immediately in parallel,
   * the implementation must clearly know something,
   * either directly or indirectly, about how to create threads or submit tasks
   * to some sort of thread pool. Moreover, this implies that the thread pool must be
   * globally accessible and properly initialized wherever we want to call fork.
   * This means we lose the ability to control the parallelism strategy used for different parts of our program.
   * And though there’s nothing inherently wrong with having a global resource for executing parallel tasks,
   * we can imagine how it would be useful to have more fine-grained control over what implementations
   * are used where (we might like for each subsystem of a large application to get its own thread pool with
   * different parameters, for example). It seems much more appropriate to give get the responsibility of
   * creating threads and submitting execution tasks.
   * Note that coming to these conclusions didn’t require knowing exactly how fork and get will be implemented,
   * or even what the representation of Par will be.
   * We just reasoned informally about the sort of information required to actually
   * spawn a parallel task, and examined the consequences of having Par values know about this information.
   * In contrast, if fork simply holds on to its unevaluated argument until later,
   * it requires no access to the mechanism for implementing parallelism.
   * It just takes an unevaluated Par and “marks” it for concurrent evaluation.
   * Let’s now assume this meaning for fork. With this model, Par itself doesn’t need to
   * know how to actually implement the parallelism. It’s more a description of a parallel computation
   * that gets interpreted at a later time by something like the get function.
   * This is a shift from before, where we were considering Par to be a container of a value that we could simply
   * This sort of indifference to representation is a hint that the operations are actually more general,
   * and can be abstracted to work for types other than just Par.
   *
   * ef unit[A](a: A): Par[A]
   * def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
   * def fork[A](a: => Par[A]): Par[A]
   * def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
   * def run[A](a: Par[A]): A
   *
   */

  /**
   * The simplest possible model for Par[A] might be ExecutorService => A.
   * This would obviously make run trivial to implement. But it might be nice
   * to defer the decision of how long to wait for a computation,
   * or whether to cancel it, to the caller of run.
   * So Par[A] becomes ExecutorService => Future[A], and run simply returns the Future
   *
   * Have look at fork. map2 and unit descriptions in the Par class
   */


}
