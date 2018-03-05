package monads

import cats.data.{Writer, WriterT}
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.int._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object WriterMain extends App {


//  val s1 = "Step 1".pure

  val test = Writer(1, "Step 1")

  val v  = test.value
  val w  = test.written
  val r = test.run

  println(v)
  println(w)
  println(r)

  type Log[A] = Writer[Int, A]

  val monadicComp = for {
    a <- "Step 1".pure[Log]
    _ <- 1.tell
    b <-  ";Step 2".writer(2)
  } yield a + b

  println(monadicComp)

}
