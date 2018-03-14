package monads

import java.time.LocalDateTime

import cats.data.State

import scala.annotation.tailrec

object StateMain extends App {

  val someState = State[Int, String] { state =>
    // Actions always will be executed, but can be discarded.
    println(s"action executed - state input: $state")

    (state + 10, s"${LocalDateTime.now()} - $state")
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

  val square = State[Int, String] { s =>
    (s, s"Result Square: ${s * s}")
  }

  val sum10 = State[Int, String] { s =>
    (s, s"Result Sum: ${s + 10}")
  }

  val sumSquare = for {
    a <- square
    b <- sum10
  } yield (a, b)

  println(sumSquare.run(10).value)
  println("Hey: ", sumSquare.run(11).value)

}

object StateOperations extends App {

  // State and result are the same
  val state = State.get[Int]

  println("Get state:")
  val getValue = state.run(10).value
  println(getValue)
  state.get.map(v => println(s"get.map : $v"))
  println(state.run(20).value)
  println

  // set defines the value as constant
  val setState = State.set[Int](30)
  println("Set state:")
  println(setState.run(10).value)
  println(setState.run(20).value)
  println

  // set define result as constant
  val pureState = State.pure[Int, String]("Result")
  println("Pure State:")
  println(pureState.run(10).value)
  println(pureState.run(20).value)
  println

  // seems to be like State[Int, Double] { v => ... (v,...)}
  val inspectState = State.inspect[Int, Double](x => x / 2)
  println("Inspect State:")
  println(inspectState.run(10).value)
  println(inspectState.run(20).value)
  println(inspectState.run(30).value)
  println

  // Apply State to HoF and returns (HoF(state), Unit)
  val modifyState = State.modify[Int](_ + 1)
  println("Modify State:")
  println(modifyState.run(10).value)
  println(modifyState.run(21).value)

}

object StateComputation extends App {
  import State._
  // St stands to StateType
  type St = Int

  val comp: State[St, (String, Float, Int)] = for {
    half <- inspect[St, (String, Float, Int)](v => ("", v / 2, 0)) // This function can be a side effect?
    _ <- set[St](10) // Change the input to 10
    str <- get[St] // Gets the current state (10)
    _ <- modify[St](_ * 0)
    zero <- get[St]
  } yield (str.toString, half._2, zero)

  println(s"Comp: ${comp.run(100).value}")
  println(s"Comp: ${comp.run(50).value}")

}

object StateInterpreter extends App {
  import State._
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]

  def evalOne(op: String): CalcState[Int] = {
    op match {
      case "+" => binOp(_ + _)
      case "-" => binOp(_ - _)
      case "*" => binOp(_ * _)
      case "/" => binOp(_ / _)
      case n   => addOne(n.toInt)
    }
  }

  def evalAll(op: List[String]): CalcState[Int] = {
    op.foldLeft(0.pure[CalcState]) { (a, b) =>
      a.flatMap(_ => evalOne(b))
    }
  }

  def addOne(v: Int): CalcState[Int] = State[List[Int], Int] { cur =>
    (v :: cur, v)
  }

  def binOp(func: (Int, Int) => (Int)): CalcState[Int] = State[List[Int], Int] {
    case (a :: b :: tail) =>
      val res = func(a, b)
      (tail.::(res), func(a, b))
    case _ =>
      sys.error("Failed")
  }

  val operations: CalcState[Int] = for {
    _ <- evalOne("+")
    _ <- evalOne("-")
    _ <- evalOne("2")
    _ <- evalOne("*")
    r <- evalOne("1")
  } yield r

  val desugaredOperations: CalcState[Int] =
    // State is encapsulated
    // State updates happen into functions returning a State (CalcState[Int])
    evalOne("+").flatMap { fs =>
      println(s"First evaluation: $fs")
      evalOne("-").map { ss =>
        println(s"Second evaluation: $ss")
        ss
      }
    }

  val evalAllPart: CalcState[Int] = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    _ <- evalOne("3")
    _ <- evalOne("4")
    _ <- evalOne("+")
    r <- evalOne("*")
  } yield r

  println(operations.run(List(2, 3, 4)).value)
  println
  println(desugaredOperations.run(List(2, 3, 4)).value)
  println
  println(evalAll(List("1", "2", "+", "3", "4", "+", "*")).run(Nil).value)
  println(evalAllPart.run(Nil).value)
}
