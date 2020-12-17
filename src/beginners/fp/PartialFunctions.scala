package beginners.fp

object PartialFunctions extends App {

  def byName(n: => Int):Int = n +1
  def byFunction(f : ()=> Int):Int = f() +1

  def method :Int = 42
  def parentMethod(): Int = 42

  /*
  calling byName and byFunction with the following
  int
  method
  parentMethod
  lambda
  paf - partially applied function
   */
  byName(3)
  byName(method)
  byName(parentMethod())
  byName(parentMethod) // same as parentMethod() -> this is called
//  byName(()=>42) doesnt work since that expected a value.
  byName((()=>42)()) // still fine

  // byName(parentMethod _) not ok since this reurns a function on evaluation
//  byFunction(method) method is not a function and doesnt have a parameter list per se not lifted
  byFunction(parentMethod) // compiler does eta expansion
  byFunction(()=> 46)
  byFunction(parentMethod _)// this is unnecessary
  
}
