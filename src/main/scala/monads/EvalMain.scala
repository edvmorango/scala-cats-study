package monads

import cats.Eval



object EvaluationMain extends App{


  val memoized = {
    println("Eager and memoized (will happen before the statements)")
    math.random()
  }

  lazy val lz = {
    println("Lazy and memoized" )
    math.random()
  }

  def eager = {
    println("Eager and not memoized")
    math.random()
  }


  println("\nEager Memoized \n")

  println(memoized)
  println(memoized)

  println("\nLazy \n")

  println(lz)
  println(lz)

  println("\nEager not memoized \n")
  println(eager)
  println(eager)




}


object EvalMain extends App {

  val now = Eval.now(math.random()) // eager + memoized

  val later = Eval.later(math.random()) // lazy + memoized

  val always = Eval.always(math.random()) // lazy + not memoized?


  println("Now:")
  println(now)
  println(now.value)

  println("\nLater:")
  println(later)
  println(later.value)
  println(later.value)


  println("\nAlways:")
  println(always)
  println(always.value)
  println(always.value)


}