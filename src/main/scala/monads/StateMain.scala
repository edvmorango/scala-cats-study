package monads

import java.time.LocalDateTime

import cats.data.State

object StateMain extends App{

  val someState = State[Int, String] { state =>
    // Actions always will be executed, but can be discarded.
    println(s"action executed - state input: $state")
    (state, s"${LocalDateTime.now()} - $state")
  }

  val sf = someState.run(10).value
  println(s"Get state and result: $sf")
  println

  val s = someState.runS(15).value
  println(s"Ignores result: $s")
  println

  val r = someState.runA(20).value
  println(s"Ignores state: $r")
  println

}
